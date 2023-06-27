#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#'     style: TRUE
#' ---

library(data.table)
library(INLA)
library(sf)

.center <- function(x)
    as.numeric(scale(x, scale = FALSE))

.center_scale <- function(x)
    as.numeric(scale(x))

inla.setOption(pardiso.license = "~/sys/licenses/pardiso.lic")

my_dt <- fread("../data/center_census_state_OUT_0901.csv")
my_dt <- my_dt[OUT == 0]

rm_vars <- names(my_dt) |>
    grep("^V[0-9]{1,2}", x = _)

set(my_dt, j = rm_vars, value = NULL)

my_dt[, `:=`(GEOID = gsub("1400000US", "", GEO_ID),
             pwhite = as.numeric(pwhite),
             hhinc = as.numeric(hhinc))]
my_dt <- my_dt[, log_inc := as.numeric(log(hhinc))]

my_dt <- my_dt[! is.na(USER_Capac)]
my_dt <- my_dt[(STATE == "Alaska" & hhinc <= 49321) |
               (STATE == "Hawaii" & hhinc <= 45399) |
               (hhinc <= 39461), ]

my_dt[, `:=`(log_inc = .center_scale(log_inc),
             pwhite  = .center_scale(pwhite),
             phs     = .center_scale(phs),
             punder5 = .center_scale(punder5),
             SNAP    = .center_scale(SNAP),
             WIC     = .center_scale(WIC),
             poor    = .center_scale(POOR),
             USER_Capac = .center_scale(USER_Capac))]

states_included <- unique(my_dt[["STATE"]])

my_dt[, website := paste0("webs_", WEBSITE.q)]
my_dt[, STATE := paste0("STATE_", gsub(" ", "_", STATE))]
my_dt[, sponsor := paste0("spon_", SPON_UC.cat)]
my_dt[, AGENCY := paste0("ag_", AGENCY.lab)]
my_dt[, FNS_REG := paste0("reg_", FNS_REG)]

my_dt[, AGENCY := gsub(" ", "_", AGENCY)]
my_dt[, sponsor := gsub(" ", "_", sponsor)]

covars <- c("STATE", "PARTY_bin", "poor", "FNS_REG",
            "SPON_UC.cat", "AGENCY.lab", "ONEAG",
            "WEBSITE.q", "log_inc",
            "pwhite", "phs", "punder5",
            "SNAP", "WIC")

cat_vars <- c("website", "AGENCY",
              "STATE",
              "FNS_REG", "sponsor")
vars <- vector(mode = "list", length = length(cat_vars))

regs <- unique(my_dt[["FNS_REG"]])
my_dt[, (regs) := lapply(regs,
                         function(x) as.numeric(FNS_REG == x))]

states <- unique(my_dt[["STATE"]])
my_dt[, (states) := lapply(states,
                           function(x) as.numeric(STATE == x))]

webs <- unique(my_dt[["website"]])
my_dt[, (webs) := lapply(webs,
                         function(x) as.numeric(website == x))]

agency <- unique(my_dt[["AGENCY"]])
my_dt[, (agency) := lapply(agency,
                           function(x) as.numeric(AGENCY == x))]

spon <- unique(my_dt[["sponsor"]])
my_dt[, (spon) := lapply(spon,
                         function(x) as.numeric(sponsor == x))]

my_states <- tigris::states(cb = TRUE)

crs_orig <- st_crs(my_states)
crs_new  <- 4326

my_states <- my_states |>
    subset(my_states$NAME %in% states_included) |>
    st_transform(crs_new)

borders <- my_states |>
    st_geometry() |>
    st_union() |>
    st_simplify(dTol = 10 * 1000) |>
    nngeo::st_remove_holes() |>
    st_cast("POLYGON") |>
    st_make_valid()

borders <- borders[!st_is_empty(borders)]

border_sp <-  as(borders, "Spatial")

locs <- my_dt[, .(X, Y)] |>
    as.matrix()

locs_sf <- locs |>
    as.data.frame() |>
    st_as_sf(coords = c(1, 2),
             crs = crs_orig) |>
    st_geometry() |>
    st_transform(crs_new)

locs <- st_coordinates(locs_sf)

my_dt[, `:=`(X_utm = locs[, 1],
             Y_utm = locs[, 2])]

max_edge <- .6

my_mesh <- inla.mesh.2d(locs,
                        max.edge = c(max_edge,
                                     5 * max_edge),
                        cutoff = max_edge / 10,
                        offset = c(max_edge,
                                   5 * max_edge))

plot(my_mesh, asp = 1)
points(locs,
       pch = 19,
       col = ifelse(my_dt[["USER_CACFP"]] == 1,
                    scales::alpha(2, .75),
                    scales::alpha("blue", .5)),
       cex = .5)

borders_tri <- inla.over_sp_mesh(border_sp,
                                 y = my_mesh,
                                 type = "centroid",
                                 ignore.CRS = TRUE)
num_tri <- length(my_mesh$graph$tv[, 1])
barrier_tri <- setdiff(1:num_tri, borders_tri)
poly_barrier <-
    inla.barrier.polygon(my_mesh,
                         barrier.triangles = barrier_tri)

plot(my_mesh, asp = 1)
plot(poly_barrier, add = TRUE,
     col = "gray70")

A <- inla.spde.make.A(my_mesh, loc = locs)
spde_cov <- inla.barrier.pcmatern(my_mesh,
                                  barrier.triangles =
                                      barrier_tri,
                                  prior.sigma = c(sqrt(5), .01),
                                  prior.range = c(1e-8,
                                                  .95))

mesh_index <- inla.spde.make.index(name = "field",
                                   n.spde = my_mesh$n)

stk_dat <- inla.stack(data = list(y = my_dt[["USER_CACFP"]]),
                      A = list(A, 1),
                      tag = "my_spde",
                      effects = list(
                          c(mesh_index,
                            list(Intercept = 1)),
                          list(
                              long = inla.group(locs[, 1]),
                              lat = inla.group(locs[, 2]),
                              party  = my_dt[["PARTY_bin"]],
                              state = factor(my_dt[["STATE"]]),
                              spon_12 = my_dt[["spon_1_or_2_Sponsors"]],
                              spon_3 = my_dt[["spon_3+_Sponsors"]],
                              oneag = my_dt[["ONEAG"]],
                              website_Mid = my_dt[["webs_Middle"]],
                              website_High =
                                  my_dt[["webs_Upper"]],
                              AGENCY_soc =
                                  my_dt[["ag_Social_Serv."]],
                              AGENCY_ph =
                                  my_dt[["ag_Pub._Health"]],
                              AGENCY_ee =
                                  my_dt[["ag_Early_Ed."]],
                              AGENCY_ag =
                                  my_dt[["ag_Agri."]],
                              FNS_REG_West =
                                  my_dt[["reg_West"]],
                              FNS_REG_Northeast =
                                  my_dt[["reg_Northeast"]],
                              FNS_REG_Mountain =
                                  my_dt[["reg_Mountain"]],
                              FNS_REG_Midatlantic =
                                  my_dt[["reg_Midatlantic"]],
                              FNS_REG_Southeast =
                                  my_dt[["reg_Southeast"]],
                              FNS_REG_Southwest =
                                  my_dt[["reg_Southwest"]],
                              log_inc = my_dt[["log_inc"]],
                              pwhite = my_dt[["pwhite"]],
                              phs = my_dt[["phs"]],
                              punder5 = my_dt[["punder5"]],
                              capacity = my_dt[["USER_Capac"]],
                              snap = my_dt[["SNAP"]],
                              wic = my_dt[["WIC"]]
                          )
                      ))

ids_out <- c(1:6, length(names(stk_dat$effects$data)))

f_s <-
    sprintf(
        "y ~ -1 + Intercept + %s + %s",
        paste(names(stk_dat$effects$data)[-ids_out],
              collapse = " + "),
        "f(field, model = spde_cov)"
    ) |>
    as.formula()

my_model <- inla(f_s, family = "binomial",
                 data = inla.stack.data(stk_dat),
                 verbose = TRUE,
                 control.predictor =
                     list(A = inla.stack.A(stk_dat),
                          compute = TRUE))

out <-
    dplyr::as_tibble(cbind(par =
                               rownames(my_model$summary.fixed),
                           exp(my_model$summary.fixed)[,
                                                       c(1, 3, 5)]))

colnames(out) <- c("Variable", "OR", "CI - lower", "CI - upper")

print(out, n = Inf)

fitted <- my_model$summary.linear.predictor

fitted <- fitted[grepl("^APredictor", rownames(fitted)), c(1, 3, 5)]
fitted <- transform(fitted,
                    mean = plogis(mean),
                    lower = plogis(`0.025quant`),
                    upper = plogis(`0.975quant`))

fitted <- fitted[, -c(2:3)]
rownames(fitted) <- NULL

head(fitted)
