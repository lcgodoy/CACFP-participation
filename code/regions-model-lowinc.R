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
    grep("^V[0-9]{2}", x = _)

set(my_dt, j = rm_vars, value = NULL)

my_dt[, `:=`(GEOID = gsub("1400000US", "", GEO_ID),
             pwhite = as.numeric(pwhite),
             hhinc = as.numeric(hhinc))]
my_dt <- my_dt[, log_inc := as.numeric(log(hhinc2))]

my_dt <- my_dt[(STATE == "Alaska" & hhinc <= 49321) |
               (STATE == "Hawaii" & hhinc <= 45399) |
               (hhinc <= 39461), ]

my_dt[, `:=`(log_inc = .center_scale(log_inc),
             pwhite  = .center_scale(pwhite),
             phs     = .center_scale(phs),
             punder5 = .center_scale(punder5),
             SNAP    = .center_scale(SNAP),
             WIC     = .center_scale(WIC),
             poor    = .center_scale(POOR))]

covars <- c("STATE", "PARTY_bin", "poor", "FNS_REG",
            "SPON_UC.cat", "AGENCY.lab", "ONEAG",
            "WEBSITE.q", "log_inc",
            "pwhite", "phs", "punder5",
            "SNAP", "WIC")

states_included <- unique(my_dt[["STATE"]])

my_dt[, website := paste0("webs_", WEBSITE.q)]
my_dt[, STATE := paste0("STATE_", gsub(" ", "_", STATE))]
my_dt[, sponsor := paste0("spon_", SPON_UC.cat)]
my_dt[, AGENCY := paste0("ag_", AGENCY.lab)]
my_dt[, FNS_REG := paste0("reg_", FNS_REG)]

my_dt[, AGENCY := gsub(" ", "_", AGENCY)]
my_dt[, sponsor := gsub(" ", "_", sponsor)]

regs <- unique(my_dt[["FNS_REG"]])
my_dt[, (regs) := lapply(regs,
                         function(x) fifelse(FNS_REG == x, 1,
                                             fifelse(FNS_REG == "reg_Midwest", -1, 0)))]

stk_dat <- inla.stack(data = list(y = my_dt[["USER_CACFP"]]),
                      A = list(1),
                      tag = "my_spde",
                      effects = list(
                          list(
                              Intercept = 1,
                              party  = my_dt[["PARTY_bin"]],
                              state = factor(my_dt[["STATE"]]),
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
                              snap = my_dt[["SNAP"]],
                              wic = my_dt[["WIC"]]
                          )
                      ))

ids_out <- c(1:6, length(names(stk_dat$effects$data)))

f_s <-
    sprintf(
        "y ~ -1 + Intercept + %s",
        paste(grep("^FNS_REG",
                   names(stk_dat$effects$data),
                   value = TRUE),
              collapse = " + ")
    ) |>
    as.formula()

my_model <- inla(f_s, family = "binomial",
                 data = inla.stack.data(stk_dat),
                 verbose = TRUE,
                 control.compute = list(config = TRUE),
                 control.predictor =
                     list(A = inla.stack.A(stk_dat)))

betas <- inla.posterior.sample(1000, my_model)

post <- lapply(betas, function(x) {
    t(tail(x[["latent"]], n = 7))
})

post <- do.call("rbind", post)

intercept <- post[, 1]
post[, 1] <- post[, 1] - apply(post[, 2:7], 1, sum)

colnames(post)[1] <- "FNS_REG_Midwest:1"

post <- cbind(intercept, post)

sig_level <- 1 - (.05 / NCOL(post))

post <- coda::as.mcmc(post)

xx <- cbind(apply(post, 2, mean), coda::HPDinterval(post, prob = sig_level))
colnames(xx)[1] <- "post_mean"

xx <- exp(xx)

xx <- cbind.data.frame(region = rownames(xx), as.data.frame(xx))

xx$region <- ifelse(xx$region == "intercept", "Overall", gsub("FNS_REG_", "", xx$region))

rownames(xx) <- NULL

openxlsx::write.xlsx(list("all_estimated_means" = xx),
                     file = "../data/results/model-regions-lowinc.xlsx",
                     overwrite = TRUE)
