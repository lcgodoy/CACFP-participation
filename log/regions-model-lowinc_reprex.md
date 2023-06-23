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
  grep("^V[0-9]{2}", x = _)

set(my_dt, j = rm_vars, value = NULL)

my_dt[, `:=`(
  GEOID = gsub("1400000US", "", GEO_ID),
  pwhite = as.numeric(pwhite),
  hhinc = as.numeric(hhinc)
)]
#> Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion
my_dt <- my_dt[, log_inc := as.numeric(log(hhinc2))]

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
  poor = .center_scale(POOR)
)]

covars <- c(
  "STATE", "PARTY_bin", "poor", "FNS_REG",
  "SPON_UC.cat", "AGENCY.lab", "ONEAG",
  "WEBSITE.q", "log_inc",
  "pwhite", "phs", "punder5",
  "SNAP", "WIC"
)

states_included <- unique(my_dt[["STATE"]])

my_dt[, website := paste0("webs_", WEBSITE.q)]
my_dt[, STATE := paste0("STATE_", gsub(" ", "_", STATE))]
my_dt[, sponsor := paste0("spon_", SPON_UC.cat)]
my_dt[, AGENCY := paste0("ag_", AGENCY.lab)]
my_dt[, FNS_REG := paste0("reg_", FNS_REG)]

my_dt[, AGENCY := gsub(" ", "_", AGENCY)]
my_dt[, sponsor := gsub(" ", "_", sponsor)]

regs <- unique(my_dt[["FNS_REG"]])
my_dt[, (regs) := lapply(
  regs,
  function(x) {
    fifelse(
      FNS_REG == x, 1,
      fifelse(FNS_REG == "reg_Midwest", -1, 0)
    )
  }
)]

stk_dat <- inla.stack(
  data = list(y = my_dt[["USER_CACFP"]]),
  A = list(1),
  tag = "my_spde",
  effects = list(
    list(
      Intercept = 1,
      party = my_dt[["PARTY_bin"]],
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
  )
)

ids_out <- c(1:6, length(names(stk_dat$effects$data)))

f_s <-
  sprintf(
    "y ~ -1 + Intercept + %s",
    paste(
      grep("^FNS_REG",
        names(stk_dat$effects$data),
        value = TRUE
      ),
      collapse = " + "
    )
  ) |>
  as.formula()

my_model <- inla(f_s,
  family = "binomial",
  data = inla.stack.data(stk_dat),
  verbose = TRUE,
  control.compute = list(config = TRUE),
  control.predictor =
    list(A = inla.stack.A(stk_dat))
)

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
  overwrite = TRUE
)
```

<sup>Created on 2023-06-23 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>

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
#>  date     2023-06-23
#>  pandoc   3.1.2 @ /opt/homebrew/bin/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version    date (UTC) lib source
#>  class          7.3-20     2022-01-16 [1] CRAN (R 4.2.2)
#>  classInt       0.4-9      2023-02-28 [1] CRAN (R 4.2.0)
#>  cli            3.6.0      2023-01-09 [1] CRAN (R 4.2.0)
#>  coda           0.19-4     2020-09-30 [1] CRAN (R 4.2.0)
#>  codetools      0.2-18     2020-11-04 [1] CRAN (R 4.2.2)
#>  data.table   * 1.14.8     2023-02-17 [1] CRAN (R 4.2.0)
#>  DBI            1.1.3      2022-06-18 [1] CRAN (R 4.2.0)
#>  digest         0.6.31     2022-12-11 [1] CRAN (R 4.2.0)
#>  dplyr          1.1.0      2023-01-29 [1] CRAN (R 4.2.0)
#>  e1071          1.7-13     2023-02-01 [1] CRAN (R 4.2.0)
#>  evaluate       0.20       2023-01-17 [1] CRAN (R 4.2.0)
#>  fansi          1.0.4      2023-01-22 [1] CRAN (R 4.2.0)
#>  fastmap        1.1.0      2021-01-25 [1] CRAN (R 4.2.0)
#>  foreach      * 1.5.2      2022-02-02 [1] CRAN (R 4.2.0)
#>  fs             1.6.1      2023-02-06 [1] CRAN (R 4.2.0)
#>  generics       0.1.3      2022-07-05 [1] CRAN (R 4.2.0)
#>  glue           1.6.2      2022-02-24 [1] CRAN (R 4.2.0)
#>  htmltools      0.5.4      2022-12-07 [1] CRAN (R 4.2.0)
#>  INLA         * 22.12.16   2022-12-23 [1] local
#>  iterators      1.0.14     2022-02-05 [1] CRAN (R 4.2.0)
#>  KernSmooth     2.23-20    2021-05-03 [1] CRAN (R 4.2.2)
#>  knitr          1.42       2023-01-25 [1] CRAN (R 4.2.0)
#>  lattice        0.20-45    2021-09-22 [1] CRAN (R 4.2.2)
#>  lifecycle      1.0.3      2022-10-07 [1] CRAN (R 4.2.0)
#>  magrittr       2.0.3      2022-03-30 [1] CRAN (R 4.2.0)
#>  Matrix       * 1.5-1      2022-09-13 [1] CRAN (R 4.2.2)
#>  MatrixModels   0.5-1      2022-09-11 [1] CRAN (R 4.2.0)
#>  mnormt         2.1.1      2022-09-26 [1] CRAN (R 4.2.0)
#>  numDeriv       2016.8-1.1 2019-06-06 [1] CRAN (R 4.2.0)
#>  openxlsx       4.2.5.2    2023-02-06 [1] CRAN (R 4.2.0)
#>  pillar         1.8.1      2022-08-19 [1] CRAN (R 4.2.0)
#>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.2.0)
#>  proxy          0.4-27     2022-06-09 [1] CRAN (R 4.2.0)
#>  purrr          1.0.1      2023-01-10 [1] CRAN (R 4.2.0)
#>  R.cache        0.16.0     2022-07-21 [1] CRAN (R 4.2.0)
#>  R.methodsS3    1.8.2      2022-06-13 [1] CRAN (R 4.2.0)
#>  R.oo           1.25.0     2022-06-12 [1] CRAN (R 4.2.0)
#>  R.utils        2.12.2     2022-11-11 [1] CRAN (R 4.2.0)
#>  R6             2.5.1      2021-08-19 [1] CRAN (R 4.2.0)
#>  Rcpp           1.0.10     2023-01-22 [1] CRAN (R 4.2.0)
#>  reprex         2.0.2      2022-08-17 [1] CRAN (R 4.2.0)
#>  rlang          1.0.6      2022-09-24 [1] CRAN (R 4.2.0)
#>  rmarkdown      2.20       2023-01-19 [1] CRAN (R 4.2.0)
#>  sessioninfo    1.2.2      2021-12-06 [1] CRAN (R 4.2.0)
#>  sf           * 1.0-12     2023-03-19 [1] CRAN (R 4.2.0)
#>  sn             2.1.1      2023-04-04 [1] CRAN (R 4.2.0)
#>  sp           * 1.6-0      2023-01-19 [1] CRAN (R 4.2.0)
#>  stringi        1.7.12     2023-01-11 [1] CRAN (R 4.2.0)
#>  styler         1.9.1      2023-03-04 [1] CRAN (R 4.2.0)
#>  tibble         3.1.8      2022-07-22 [1] CRAN (R 4.2.0)
#>  tidyselect     1.2.0      2022-10-10 [1] CRAN (R 4.2.0)
#>  units          0.8-2      2023-04-27 [1] CRAN (R 4.2.0)
#>  utf8           1.2.3      2023-01-31 [1] CRAN (R 4.2.0)
#>  vctrs          0.5.2      2023-01-23 [1] CRAN (R 4.2.0)
#>  withr          2.5.0      2022-03-03 [1] CRAN (R 4.2.0)
#>  xfun           0.37       2023-01-31 [1] CRAN (R 4.2.0)
#>  yaml           2.3.7      2023-01-23 [1] CRAN (R 4.2.0)
#>  zip            2.2.2      2022-10-26 [1] CRAN (R 4.2.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

</details>
