``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(ggplot2)
library(ggrepel)

my_states <- readRDS("../data/states-map.rds")

names_out <-
  c(
    "Puerto Rico",
    "Commonwealth of the Northern Mariana Islands",
    "United States Virgin Islands",
    "American Samoa",
    "Guam"
  )

my_states <- my_states[!my_states$NAME %in% names_out, ]

my_states <- tigris::shift_geometry(my_states,
  geoid_column = "GEOID"
)

full <- readr::read_csv("../data/pred-state-full.csv") |>
  mutate(dataset = "full")
#> Rows: 48 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): STATE
#> dbl (2): p_est, p_obs
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

lowinc <- readr::read_csv("../data/pred-state-lowinc.csv") |>
  mutate(dataset = "low-income")
#> Rows: 48 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): STATE
#> dbl (2): p_est, p_obs
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

full <- full_join(my_states[, c("STATEFP", "STUSPS", "NAME")],
  full,
  by = c("NAME" = "STATE")
)

lowinc <- full_join(my_states[, c("STATEFP", "STUSPS", "NAME")],
  lowinc,
  by = c("NAME" = "STATE")
)

ub <- c(
  full$p_obs, full$p_est,
  lowinc$p_obs, lowinc$p_est
) |>
  max(na.rm = TRUE)

edit_1 <- c("NH", "VT")
edit_2 <-
  c(
    "MA", "RI", "CT",
    "NJ", "MD", "DE",
    "DC"
  )
x_range <- st_bbox(full)[c(1, 3)] |> diff()
y_range <- st_bbox(full)[c(2, 4)] |> diff()

full_2 <- full |>
  filter(STUSPS %in% edit_1) |>
  mutate(
    cnt = purrr::map(geometry, st_centroid),
    crd = purrr::map(cnt, st_coordinates),
    crd_x = purrr::map_dbl(crd, 1),
    crd_y = purrr::map_dbl(crd, 2)
  ) |>
  select(-crd, -cnt) |>
  as_tibble() |>
  st_as_sf()
full_2 <- full_2 |>
  mutate(
    end_x = crd_x + c(
      -.02, # VT
      -.007 # NH
    ) * x_range,
    end_y = crd_y + c(
      .05, # VT
      .075 # NH
    ) * y_range
  )

full_3 <- full |>
  filter(STUSPS %in% edit_2) |>
  mutate(
    cnt = purrr::map(geometry, st_centroid),
    crd = purrr::map(cnt, st_coordinates),
    crd_x = purrr::map_dbl(crd, 1),
    crd_y = purrr::map_dbl(crd, 2)
  ) |>
  select(-crd, -cnt) |>
  as_tibble() |>
  st_as_sf()

full_3 <- full_3 |>
  mutate(
    end_x = crd_x + c(
      .05, # MA
      .025, # NJ
      .03, # DE
      .05, # DC
      .046, # CT
      .08, # MD
      .038 # RI
    ) *
      x_range,
    end_y = crd_y + c(
      .015, # MA
      0, # NJ
      0, # DE
      -.03, # DC
      -.025, # CT
      .02, # MD
      .001 # RI
    ) * y_range
  )

ggplot(data = mutate(full, dataset = "(A)")) +
  geom_sf(aes(fill = p_obs),
    color = 1, lwd = .1
  ) +
  geom_sf_text(
    data = filter(
      mutate(full, dataset = "(A)"),
      !STUSPS %in% c(
        edit_1,
        edit_2
      )
    ),
    aes(label = STUSPS),
    size = 3, color = "white"
  ) +
  geom_segment(
    data = mutate(full_2, dataset = "(A)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_segment(
    data = mutate(full_3, dataset = "(A)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_label(
    data = mutate(full_2, dataset = "(A)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  geom_label(
    data = mutate(full_3, dataset = "(A)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = scales::percent,
    limits = c(0, ub),
    breaks = c(0, .25, .5, .75, .9),
    na.value = "gray70"
  ) +
  facet_wrap(~dataset) +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    legend.text = element_text(size = 16),
    legend.key.size = unit(x = 2, "cm"),
    legend.key.width = unit(x = .5, "cm")
  )
```

![](https://i.imgur.com/CdJlBQn.png)<!-- -->

``` r
ggsave("img/map-observed-full.eps",
  dpi = 300,
  width = 12, height = 12
)
#> Error in grDevices::postscript(file = filename, ..., onefile = FALSE, : cannot open file 'img/map-observed-full.eps'
ggsave("img/map-observed-full.pdf",
  dpi = 300,
  width = 12, height = 12
)
#> Error in grDevices::pdf(file = filename, ..., version = version): cannot open file 'img/map-observed-full.pdf'
ggsave("img/map-observed-full.png",
  dpi = 300,
  width = 12, height = 12
)
ggsave("img/map-observed-full.jpg",
  dpi = 300,
  width = 12, height = 12
)

ggplot(data = mutate(full, dataset = "(B)")) +
  geom_sf(aes(fill = p_est),
    color = 1, lwd = .1
  ) +
  geom_sf_text(
    data = filter(
      mutate(full, dataset = "(B)"),
      !STUSPS %in% c(
        edit_1,
        edit_2
      )
    ),
    aes(label = STUSPS),
    size = 3, color = "white"
  ) +
  geom_segment(
    data = mutate(full_2, dataset = "(B)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_segment(
    data = mutate(full_3, dataset = "(B)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_label(
    data = mutate(full_2, dataset = "(B)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  geom_label(
    data = mutate(full_3, dataset = "(B)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = scales::percent,
    limits = c(0, ub),
    breaks = c(0, .25, .5, .75, .9),
    na.value = "gray70"
  ) +
  facet_wrap(~dataset) +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
```

![](https://i.imgur.com/0G1mrq3.png)<!-- -->

``` r

ggsave("../img/map-predicted-full.eps",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-full.pdf",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-full.jpg",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-full.png",
  dpi = 300,
  width = 12, height = 12
)

ggplot(data = mutate(lowinc, dataset = "(C)")) +
  geom_sf(aes(fill = p_obs),
    color = 1, lwd = .1
  ) +
  geom_sf_text(
    data = filter(
      mutate(full, dataset = "(C)"),
      !STUSPS %in% c(
        edit_1,
        edit_2
      )
    ),
    aes(label = STUSPS),
    size = 3, color = "white"
  ) +
  geom_segment(
    data = mutate(full_2, dataset = "(C)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_segment(
    data = mutate(full_3, dataset = "(C)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_label(
    data = mutate(full_2, dataset = "(C)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  geom_label(
    data = mutate(full_3, dataset = "(C)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = scales::percent,
    limits = c(0, ub),
    breaks = c(0, .25, .5, .75, .9),
    na.value = "gray70"
  ) +
  facet_wrap(~dataset) +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
```

![](https://i.imgur.com/ZVCI6pw.png)<!-- -->

``` r

ggsave("../img/map-observed-lowinc.eps",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-observed-lowinc.pdf",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-observed-lowinc.jpg",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-observed-lowinc.png",
  dpi = 300,
  width = 12, height = 12
)

ggplot(data = mutate(lowinc, dataset = "(D)")) +
  geom_sf(aes(fill = p_est),
    color = 1, lwd = .1
  ) +
  geom_sf_text(
    data = filter(
      mutate(full, dataset = "(D)"),
      !STUSPS %in% c(
        edit_1,
        edit_2
      )
    ),
    aes(label = STUSPS),
    size = 3, color = "white"
  ) +
  geom_segment(
    data = mutate(full_2, dataset = "(D)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_segment(
    data = mutate(full_3, dataset = "(D)"),
    aes(
      x = crd_x, y = crd_y,
      xend = end_x, yend = end_y
    ),
    lwd = .5, color = "gray"
  ) +
  geom_label(
    data = mutate(full_2, dataset = "(D)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  geom_label(
    data = mutate(full_3, dataset = "(D)"),
    aes(
      x = end_x, y = end_y,
      label = STUSPS
    ),
    size = 3, color = "black"
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = scales::percent,
    limits = c(0, ub),
    breaks = c(0, .25, .5, .75, .9),
    na.value = "gray70"
  ) +
  facet_wrap(~dataset) +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
```

![](https://i.imgur.com/QTXNs0b.png)<!-- -->

``` r

ggsave("../img/map-predicted-lowinc.eps",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-lowinc.pdf",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-lowinc.jpg",
  dpi = 300,
  width = 12, height = 12
)
ggsave("../img/map-predicted-lowinc.png",
  dpi = 300,
  width = 12, height = 12
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
#>  package     * version date (UTC) lib source
#>  bit           4.0.5   2022-11-15 [1] CRAN (R 4.2.0)
#>  bit64         4.0.5   2020-08-30 [1] CRAN (R 4.2.0)
#>  class         7.3-20  2022-01-16 [1] CRAN (R 4.2.2)
#>  classInt      0.4-9   2023-02-28 [1] CRAN (R 4.2.0)
#>  cli           3.6.0   2023-01-09 [1] CRAN (R 4.2.0)
#>  colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.2.0)
#>  crayon        1.5.2   2022-09-29 [1] CRAN (R 4.2.0)
#>  curl          5.0.0   2023-01-12 [1] CRAN (R 4.2.0)
#>  DBI           1.1.3   2022-06-18 [1] CRAN (R 4.2.0)
#>  digest        0.6.31  2022-12-11 [1] CRAN (R 4.2.0)
#>  dplyr       * 1.1.0   2023-01-29 [1] CRAN (R 4.2.0)
#>  e1071         1.7-13  2023-02-01 [1] CRAN (R 4.2.0)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
#>  evaluate      0.20    2023-01-17 [1] CRAN (R 4.2.0)
#>  fansi         1.0.4   2023-01-22 [1] CRAN (R 4.2.0)
#>  farver        2.1.1   2022-07-06 [1] CRAN (R 4.2.0)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.2.0)
#>  fs            1.6.1   2023-02-06 [1] CRAN (R 4.2.0)
#>  generics      0.1.3   2022-07-05 [1] CRAN (R 4.2.0)
#>  ggplot2     * 3.4.1   2023-02-10 [1] CRAN (R 4.2.0)
#>  ggrepel     * 0.9.3   2023-02-03 [1] CRAN (R 4.2.0)
#>  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
#>  gtable        0.3.1   2022-09-01 [1] CRAN (R 4.2.0)
#>  highr         0.10    2022-12-22 [1] CRAN (R 4.2.0)
#>  hms           1.1.2   2022-08-19 [1] CRAN (R 4.2.0)
#>  htmltools     0.5.4   2022-12-07 [1] CRAN (R 4.2.0)
#>  httr          1.4.4   2022-08-17 [1] CRAN (R 4.2.0)
#>  KernSmooth    2.23-20 2021-05-03 [1] CRAN (R 4.2.2)
#>  knitr         1.42    2023-01-25 [1] CRAN (R 4.2.0)
#>  lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.0)
#>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
#>  mime          0.12    2021-09-28 [1] CRAN (R 4.2.0)
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.0)
#>  pillar        1.8.1   2022-08-19 [1] CRAN (R 4.2.0)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
#>  proxy         0.4-27  2022-06-09 [1] CRAN (R 4.2.0)
#>  purrr         1.0.1   2023-01-10 [1] CRAN (R 4.2.0)
#>  R.cache       0.16.0  2022-07-21 [1] CRAN (R 4.2.0)
#>  R.methodsS3   1.8.2   2022-06-13 [1] CRAN (R 4.2.0)
#>  R.oo          1.25.0  2022-06-12 [1] CRAN (R 4.2.0)
#>  R.utils       2.12.2  2022-11-11 [1] CRAN (R 4.2.0)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
#>  ragg          1.2.5   2023-01-12 [1] CRAN (R 4.2.0)
#>  rappdirs      0.3.3   2021-01-31 [1] CRAN (R 4.2.0)
#>  Rcpp          1.0.10  2023-01-22 [1] CRAN (R 4.2.0)
#>  readr         2.1.4   2023-02-10 [1] CRAN (R 4.2.0)
#>  reprex        2.0.2   2022-08-17 [1] CRAN (R 4.2.0)
#>  rlang         1.0.6   2022-09-24 [1] CRAN (R 4.2.0)
#>  rmarkdown     2.20    2023-01-19 [1] CRAN (R 4.2.0)
#>  scales        1.2.1   2022-08-20 [1] CRAN (R 4.2.0)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
#>  sf          * 1.0-12  2023-03-19 [1] CRAN (R 4.2.0)
#>  stringi       1.7.12  2023-01-11 [1] CRAN (R 4.2.0)
#>  stringr       1.5.0   2022-12-02 [1] CRAN (R 4.2.0)
#>  styler        1.9.1   2023-03-04 [1] CRAN (R 4.2.0)
#>  systemfonts   1.0.4   2022-02-11 [1] CRAN (R 4.2.0)
#>  textshaping   0.3.6   2021-10-13 [1] CRAN (R 4.2.0)
#>  tibble        3.1.8   2022-07-22 [1] CRAN (R 4.2.0)
#>  tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.2.0)
#>  tigris        2.0.1   2023-01-30 [1] CRAN (R 4.2.0)
#>  tzdb          0.3.0   2022-03-28 [1] CRAN (R 4.2.0)
#>  units         0.8-2   2023-04-27 [1] CRAN (R 4.2.0)
#>  utf8          1.2.3   2023-01-31 [1] CRAN (R 4.2.0)
#>  uuid          1.1-0   2022-04-19 [1] CRAN (R 4.2.0)
#>  vctrs         0.5.2   2023-01-23 [1] CRAN (R 4.2.0)
#>  viridisLite   0.4.1   2022-08-22 [1] CRAN (R 4.2.0)
#>  vroom         1.6.1   2023-01-22 [1] CRAN (R 4.2.0)
#>  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
#>  xfun          0.37    2023-01-31 [1] CRAN (R 4.2.0)
#>  xml2          1.3.3   2021-11-30 [1] CRAN (R 4.2.0)
#>  yaml          2.3.7   2023-01-23 [1] CRAN (R 4.2.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

</details>
