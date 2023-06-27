``` r
library(data.table)
library(INLA)
#> Loading required package: Matrix
#> Loading required package: foreach
#> Loading required package: parallel
#> Loading required package: sp
#> This is INLA_22.12.16 built 2022-12-23 13:43:44 UTC.
#>  - See www.r-inla.org/contact-us for how to get help.
#>  - To enable PARDISO sparse library; see inla.pardiso()
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

.center <- function(x) {
  as.numeric(scale(x, scale = FALSE))
}

.center_scale <- function(x) {
  as.numeric(scale(x))
}

inla.setOption(pardiso.license = "~/sys/licenses/pardiso.lic")

my_dt <- fread("../data/center_census_state_OUT_0901.csv")
my_dt <- my_dt[OUT == 0]

rm_vars <- names(my_dt) |>
  grep("^V[0-9]{1,2}", x = _)

set(my_dt, j = rm_vars, value = NULL)

my_dt[, `:=`(
  GEOID = gsub("1400000US", "", GEO_ID),
  pwhite = as.numeric(pwhite),
  hhinc = as.numeric(hhinc)
)]
#> Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion
my_dt <- my_dt[, log_inc := as.numeric(log(hhinc))]

my_dt <- my_dt[!is.na(USER_Capac)]
my_dt <- my_dt[(STATE == "Alaska" & hhinc <= 49321) |
  (STATE == "Hawaii" & hhinc <= 45399) |
  (hhinc <= 39461), ]

my_dt[, `:=`(
  log_inc = .center_scale(log_inc),
  pwhite = .center_scale(pwhite),
  phs = .center_scale(phs),
  punder5 = .center_scale(punder5),
  SNAP = .center_scale(SNAP),
  WIC = .center_scale(WIC),
  poor = .center_scale(POOR),
  USER_Capac = .center_scale(USER_Capac)
)]

states_included <- unique(my_dt[["STATE"]])

my_dt[, website := paste0("webs_", WEBSITE.q)]
my_dt[, STATE := paste0("STATE_", gsub(" ", "_", STATE))]
my_dt[, sponsor := paste0("spon_", SPON_UC.cat)]
my_dt[, AGENCY := paste0("ag_", AGENCY.lab)]
my_dt[, FNS_REG := paste0("reg_", FNS_REG)]

my_dt[, AGENCY := gsub(" ", "_", AGENCY)]
my_dt[, sponsor := gsub(" ", "_", sponsor)]

covars <- c(
  "STATE", "PARTY_bin", "poor", "FNS_REG",
  "SPON_UC.cat", "AGENCY.lab", "ONEAG",
  "WEBSITE.q", "log_inc",
  "pwhite", "phs", "punder5",
  "SNAP", "WIC"
)

cat_vars <- c(
  "website", "AGENCY",
  "STATE",
  "FNS_REG", "sponsor"
)
vars <- vector(mode = "list", length = length(cat_vars))

regs <- unique(my_dt[["FNS_REG"]])
my_dt[, (regs) := lapply(
  regs,
  function(x) as.numeric(FNS_REG == x)
)]

states <- unique(my_dt[["STATE"]])
my_dt[, (states) := lapply(
  states,
  function(x) as.numeric(STATE == x)
)]

webs <- unique(my_dt[["website"]])
my_dt[, (webs) := lapply(
  webs,
  function(x) as.numeric(website == x)
)]

agency <- unique(my_dt[["AGENCY"]])
my_dt[, (agency) := lapply(
  agency,
  function(x) as.numeric(AGENCY == x)
)]

spon <- unique(my_dt[["sponsor"]])
my_dt[, (spon) := lapply(
  spon,
  function(x) as.numeric(sponsor == x)
)]

my_states <- tigris::states(cb = TRUE)
#> Retrieving data for the year 2021
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

crs_orig <- st_crs(my_states)
crs_new <- 4326

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

border_sp <- as(borders, "Spatial")

locs <- my_dt[, .(X, Y)] |>
  as.matrix()

locs_sf <- locs |>
  as.data.frame() |>
  st_as_sf(
    coords = c(1, 2),
    crs = crs_orig
  ) |>
  st_geometry() |>
  st_transform(crs_new)

locs <- st_coordinates(locs_sf)

my_dt[, `:=`(
  X_utm = locs[, 1],
  Y_utm = locs[, 2]
)]

max_edge <- .6

my_mesh <- inla.mesh.2d(locs,
  max.edge = c(
    max_edge,
    5 * max_edge
  ),
  cutoff = max_edge / 10,
  offset = c(
    max_edge,
    5 * max_edge
  )
)

plot(my_mesh, asp = 1)
points(locs,
  pch = 19,
  col = ifelse(my_dt[["USER_CACFP"]] == 1,
    scales::alpha(2, .75),
    scales::alpha("blue", .5)
  ),
  cex = .5
)
```

![](https://i.imgur.com/JRiy3dw.png)<!-- -->

``` r

borders_tri <- inla.over_sp_mesh(border_sp,
  y = my_mesh,
  type = "centroid",
  ignore.CRS = TRUE
)
num_tri <- length(my_mesh$graph$tv[, 1])
barrier_tri <- setdiff(1:num_tri, borders_tri)
poly_barrier <-
  inla.barrier.polygon(my_mesh,
    barrier.triangles = barrier_tri
  )
#> Warning in RGEOSUnaryPredFunc(spgeom, byid, "rgeos_isvalid"): Self-intersection
#> at or near point -97.071875349999999 41.402641840000001
#> mesh.polys is invalid
#> Warning in rgeos::gUnaryUnion(mesh.polys): Invalid objects found; consider
#> using set_RGEOS_CheckValidity(2L)
#> Warning in RGEOSUnaryPredFunc(spgeom, byid, "rgeos_isvalid"): Self-intersection
#> at or near point -127.51940361 35.052770180000003
#> mesh.polys is invalid
#> Warning in rgeos::gUnaryUnion(mesh.polys): Invalid objects found; consider
#> using set_RGEOS_CheckValidity(2L)

plot(my_mesh, asp = 1)
plot(poly_barrier,
  add = TRUE,
  col = "gray70"
)
```

![](https://i.imgur.com/LEqnI1M.png)<!-- -->

``` r

A <- inla.spde.make.A(my_mesh, loc = locs)
spde_cov <- inla.barrier.pcmatern(my_mesh,
  barrier.triangles =
    barrier_tri,
  prior.sigma = c(sqrt(5), .01),
  prior.range = c(
    1e-8,
    .95
  )
)

mesh_index <- inla.spde.make.index(
  name = "field",
  n.spde = my_mesh$n
)

stk_dat <- inla.stack(
  data = list(y = my_dt[["USER_CACFP"]]),
  A = list(A, 1),
  tag = "my_spde",
  effects = list(
    c(
      mesh_index,
      list(Intercept = 1)
    ),
    list(
      long = inla.group(locs[, 1]),
      lat = inla.group(locs[, 2]),
      party = my_dt[["PARTY_bin"]],
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
  )
)

ids_out <- c(1:6, length(names(stk_dat$effects$data)))

f_s <-
  sprintf(
    "y ~ -1 + Intercept + %s + %s",
    paste(names(stk_dat$effects$data)[-ids_out],
      collapse = " + "
    ),
    "f(field, model = spde_cov)"
  ) |>
  as.formula()

my_model <- inla(f_s,
  family = "binomial",
  data = inla.stack.data(stk_dat),
  verbose = TRUE,
  control.predictor =
    list(
      A = inla.stack.A(stk_dat),
      compute = TRUE
    )
)
#> Warning in inla.model.properties.generic(inla.trim.family(model), mm[names(mm) == : Model 'rgeneric' in section 'latent' is marked as 'experimental'; changes may appear at any time.
#>   Use this model with extra care!!! Further warnings are disabled.

out <-
  dplyr::as_tibble(cbind(
    par =
      rownames(my_model$summary.fixed),
    exp(my_model$summary.fixed)[
      ,
      c(1, 3, 5)
    ]
  ))

colnames(out) <- c("Variable", "OR", "CI - lower", "CI - upper")

print(out, n = Inf)
#> # A tibble: 24 × 4
#>    Variable               OR `CI - lower` `CI - upper`
#>    <chr>               <dbl>        <dbl>        <dbl>
#>  1 Intercept           2.24        1.45          3.46 
#>  2 party               1.10        0.807         1.50 
#>  3 spon_12             0.976       0.720         1.32 
#>  4 spon_3              2.52        1.85          3.43 
#>  5 oneag               0.897       0.608         1.34 
#>  6 website_Mid         1.25        0.948         1.64 
#>  7 website_High        0.680       0.482         0.961
#>  8 AGENCY_soc          0.655       0.401         1.06 
#>  9 AGENCY_ph           0.675       0.487         0.928
#> 10 AGENCY_ee           1.04        0.598         1.78 
#> 11 AGENCY_ag           1.52        1.05          2.20 
#> 12 FNS_REG_West        0.448       0.280         0.718
#> 13 FNS_REG_Northeast   0.559       0.305         1.04 
#> 14 FNS_REG_Mountain    1.05        0.622         1.77 
#> 15 FNS_REG_Midatlantic 0.188       0.0985        0.354
#> 16 FNS_REG_Southeast   0.371       0.213         0.650
#> 17 FNS_REG_Southwest   0.408       0.241         0.689
#> 18 log_inc             1.02        0.979         1.06 
#> 19 pwhite              0.815       0.768         0.865
#> 20 phs                 0.798       0.759         0.838
#> 21 punder5             1.02        0.983         1.07 
#> 22 capacity            1.32        1.26          1.37 
#> 23 snap                1.28        1.14          1.44 
#> 24 wic                 1.01        0.898         1.13

fitted <- my_model$summary.linear.predictor

fitted <- fitted[grepl("^APredictor", rownames(fitted)), c(1, 3, 5)]
fitted <- transform(fitted,
  mean = plogis(mean),
  lower = plogis(`0.025quant`),
  upper = plogis(`0.975quant`)
)

fitted <- fitted[, -c(2:3)]
rownames(fitted) <- NULL

head(fitted)
#>        mean     lower     upper
#> 1 0.6759819 0.4401556 0.8483318
#> 2 0.6984042 0.4607359 0.8638412
#> 3 0.5661858 0.3393285 0.7699309
#> 4 0.6188664 0.3826323 0.8111755
#> 5 0.6231911 0.3868730 0.8140487
#> 6 0.5840742 0.3533126 0.7846367
```

<sup>Created on 2023-06-27 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.2.2 (2022-10-31)
#>  os       macOS Ventura 13.4
#>  system   aarch64, darwin20
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       America/Sao_Paulo
#>  date     2023-06-27
#>  pandoc   3.1.2 @ /opt/homebrew/bin/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version  date (UTC) lib source
#>  class          7.3-20   2022-01-16 [1] CRAN (R 4.2.2)
#>  classInt       0.4-9    2023-02-28 [1] CRAN (R 4.2.0)
#>  cli            3.6.0    2023-01-09 [1] CRAN (R 4.2.0)
#>  codetools      0.2-18   2020-11-04 [1] CRAN (R 4.2.2)
#>  colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.2.0)
#>  curl           5.0.0    2023-01-12 [1] CRAN (R 4.2.0)
#>  data.table   * 1.14.8   2023-02-17 [1] CRAN (R 4.2.0)
#>  DBI            1.1.3    2022-06-18 [1] CRAN (R 4.2.0)
#>  digest         0.6.31   2022-12-11 [1] CRAN (R 4.2.0)
#>  dplyr          1.1.0    2023-01-29 [1] CRAN (R 4.2.0)
#>  e1071          1.7-13   2023-02-01 [1] CRAN (R 4.2.0)
#>  evaluate       0.20     2023-01-17 [1] CRAN (R 4.2.0)
#>  fansi          1.0.4    2023-01-22 [1] CRAN (R 4.2.0)
#>  farver         2.1.1    2022-07-06 [1] CRAN (R 4.2.0)
#>  fastmap        1.1.0    2021-01-25 [1] CRAN (R 4.2.0)
#>  foreach      * 1.5.2    2022-02-02 [1] CRAN (R 4.2.0)
#>  fs             1.6.1    2023-02-06 [1] CRAN (R 4.2.0)
#>  generics       0.1.3    2022-07-05 [1] CRAN (R 4.2.0)
#>  glue           1.6.2    2022-02-24 [1] CRAN (R 4.2.0)
#>  highr          0.10     2022-12-22 [1] CRAN (R 4.2.0)
#>  htmltools      0.5.4    2022-12-07 [1] CRAN (R 4.2.0)
#>  httr           1.4.4    2022-08-17 [1] CRAN (R 4.2.0)
#>  INLA         * 22.12.16 2022-12-23 [1] local
#>  iterators      1.0.14   2022-02-05 [1] CRAN (R 4.2.0)
#>  KernSmooth     2.23-20  2021-05-03 [1] CRAN (R 4.2.2)
#>  knitr          1.42     2023-01-25 [1] CRAN (R 4.2.0)
#>  lattice        0.20-45  2021-09-22 [1] CRAN (R 4.2.2)
#>  lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.2.0)
#>  magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.2.0)
#>  Matrix       * 1.5-1    2022-09-13 [1] CRAN (R 4.2.2)
#>  MatrixModels   0.5-1    2022-09-11 [1] CRAN (R 4.2.0)
#>  mime           0.12     2021-09-28 [1] CRAN (R 4.2.0)
#>  munsell        0.5.0    2018-06-12 [1] CRAN (R 4.2.0)
#>  nngeo          0.4.6    2022-05-29 [1] CRAN (R 4.2.0)
#>  pillar         1.8.1    2022-08-19 [1] CRAN (R 4.2.0)
#>  pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.2.0)
#>  proxy          0.4-27   2022-06-09 [1] CRAN (R 4.2.0)
#>  purrr          1.0.1    2023-01-10 [1] CRAN (R 4.2.0)
#>  R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.2.0)
#>  R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.2.0)
#>  R.oo           1.25.0   2022-06-12 [1] CRAN (R 4.2.0)
#>  R.utils        2.12.2   2022-11-11 [1] CRAN (R 4.2.0)
#>  R6             2.5.1    2021-08-19 [1] CRAN (R 4.2.0)
#>  rappdirs       0.3.3    2021-01-31 [1] CRAN (R 4.2.0)
#>  Rcpp           1.0.10   2023-01-22 [1] CRAN (R 4.2.0)
#>  reprex         2.0.2    2022-08-17 [1] CRAN (R 4.2.0)
#>  rgdal          1.6-4    2023-01-12 [1] CRAN (R 4.2.0)
#>  rgeos          0.6-1    2022-12-14 [1] CRAN (R 4.2.0)
#>  rlang          1.0.6    2022-09-24 [1] CRAN (R 4.2.0)
#>  rmarkdown      2.20     2023-01-19 [1] CRAN (R 4.2.0)
#>  s2             1.1.3    2023-04-27 [1] CRAN (R 4.2.0)
#>  scales         1.2.1    2022-08-20 [1] CRAN (R 4.2.0)
#>  sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.2.0)
#>  sf           * 1.0-12   2023-03-19 [1] CRAN (R 4.2.0)
#>  sp           * 1.6-0    2023-01-19 [1] CRAN (R 4.2.0)
#>  stringi        1.7.12   2023-01-11 [1] CRAN (R 4.2.0)
#>  stringr        1.5.0    2022-12-02 [1] CRAN (R 4.2.0)
#>  styler         1.9.1    2023-03-04 [1] CRAN (R 4.2.0)
#>  tibble         3.1.8    2022-07-22 [1] CRAN (R 4.2.0)
#>  tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.2.0)
#>  tigris         2.0.1    2023-01-30 [1] CRAN (R 4.2.0)
#>  units          0.8-2    2023-04-27 [1] CRAN (R 4.2.0)
#>  utf8           1.2.3    2023-01-31 [1] CRAN (R 4.2.0)
#>  uuid           1.1-0    2022-04-19 [1] CRAN (R 4.2.0)
#>  vctrs          0.5.2    2023-01-23 [1] CRAN (R 4.2.0)
#>  withr          2.5.0    2022-03-03 [1] CRAN (R 4.2.0)
#>  wk             0.7.2    2023-03-17 [1] CRAN (R 4.2.0)
#>  xfun           0.37     2023-01-31 [1] CRAN (R 4.2.0)
#>  xml2           1.3.3    2021-11-30 [1] CRAN (R 4.2.0)
#>  yaml           2.3.7    2023-01-23 [1] CRAN (R 4.2.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

</details>
