#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#'     style: TRUE
#' ---

library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)

my_states <- readRDS("../data/states-map.rds")

names_out <-
    c("Puerto Rico",
      "Commonwealth of the Northern Mariana Islands",
      "United States Virgin Islands",
      "American Samoa",
      "Guam")

my_states <- my_states[! my_states$NAME %in% names_out, ]

my_states <- tigris::shift_geometry(my_states,
                                    geoid_column = "GEOID")

full <- readr::read_csv("../data/pred-state-full.csv") |>
    mutate(dataset = "full")

lowinc <- readr::read_csv("../data/pred-state-lowinc.csv") |>
    mutate(dataset = "low-income")

full <- full_join(my_states[, c("STATEFP", "STUSPS", "NAME")],
                  full,
                  by = c("NAME" = "STATE"))

lowinc <- full_join(my_states[, c("STATEFP", "STUSPS", "NAME")],
                  lowinc,
                  by = c("NAME" = "STATE"))

ub <- c(full$p_obs, full$p_est,
        lowinc$p_obs, lowinc$p_est) |>
    max(na.rm = TRUE)

edit_1 <- c("NH", "VT")
edit_2 <-
    c("MA", "RI", "CT",
      "NJ", "MD", "DE",
      "DC")
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
    mutate(end_x = crd_x + c(
                               -.02, # VT
                               -.007 # NH
                           ) * x_range,
           end_y = crd_y + c(
                               .05, # VT
                               .075 # NH
                           ) * y_range)

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
    mutate(end_x = crd_x + c(
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
                           ) * y_range)

ggplot(data = mutate(full, dataset = "(A)")) +
    geom_sf(aes(fill = p_obs),
            color = 1, lwd = .1) +
    geom_sf_text(data = filter(mutate(full, dataset = "(A)"),
                               ! STUSPS %in% c(edit_1,
                                               edit_2)),
                 aes(label = STUSPS),
                 size = 3, color = "white") +
    geom_segment(data = mutate(full_2, dataset = "(A)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_segment(data = mutate(full_3, dataset = "(A)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_label(data = mutate(full_2, dataset = "(A)"),
              aes(x = end_x, y = end_y,
                  label = STUSPS),
              size = 3, color = "black") +
    geom_label(data = mutate(full_3, dataset = "(A)"),
               aes(x = end_x, y = end_y,
                   label = STUSPS),
               size = 3, color = "black") +
    scale_fill_viridis_c(option = "D",
                         labels = scales::percent,
                         limits = c(0, ub),
                         breaks = c(0, .25, .5, .75, .9),
                         na.value = "gray70") +
    facet_wrap(~ dataset) +
    labs(fill = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          legend.text = element_text(size = 16),
          legend.key.size = unit(x = 2, "cm"),
          legend.key.width = unit(x = .5, "cm"))
ggsave("img/map-observed-full.eps", dpi = 300,
       width = 12, height = 12)
ggsave("img/map-observed-full.pdf", dpi = 300,
       width = 12, height = 12)
ggsave("img/map-observed-full.png", dpi = 300,
       width = 12, height = 12)
ggsave("img/map-observed-full.jpg", dpi = 300,
       width = 12, height = 12)

ggplot(data = mutate(full, dataset = "(B)")) +
    geom_sf(aes(fill = p_est),
            color = 1, lwd = .1) +
    geom_sf_text(data = filter(mutate(full, dataset = "(B)"),
                               ! STUSPS %in% c(edit_1,
                                               edit_2)),
                 aes(label = STUSPS),
                 size = 3, color = "white") +
    geom_segment(data = mutate(full_2, dataset = "(B)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_segment(data = mutate(full_3, dataset = "(B)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_label(data = mutate(full_2, dataset = "(B)"),
              aes(x = end_x, y = end_y,
                  label = STUSPS),
              size = 3, color = "black") +
    geom_label(data = mutate(full_3, dataset = "(B)"),
               aes(x = end_x, y = end_y,
                   label = STUSPS),
               size = 3, color = "black") +
    scale_fill_viridis_c(option = "D",
                         labels = scales::percent,
                         limits = c(0, ub),
                         breaks = c(0, .25, .5, .75, .9),
                         na.value = "gray70") +
    facet_wrap(~ dataset) +
    labs(fill = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank())

ggsave("../img/map-predicted-full.eps", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-full.pdf", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-full.jpg", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-full.png", dpi = 300,
       width = 12, height = 12)

ggplot(data = mutate(lowinc, dataset = "(C)")) +
    geom_sf(aes(fill = p_obs),
            color = 1, lwd = .1) +
    geom_sf_text(data = filter(mutate(full, dataset = "(C)"),
                               ! STUSPS %in% c(edit_1,
                                               edit_2)),
                 aes(label = STUSPS),
                 size = 3, color = "white") +
    geom_segment(data = mutate(full_2, dataset = "(C)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_segment(data = mutate(full_3, dataset = "(C)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_label(data = mutate(full_2, dataset = "(C)"),
              aes(x = end_x, y = end_y,
                  label = STUSPS),
              size = 3, color = "black") +
    geom_label(data = mutate(full_3, dataset = "(C)"),
               aes(x = end_x, y = end_y,
                   label = STUSPS),
               size = 3, color = "black") +
    scale_fill_viridis_c(option = "D",
                         labels = scales::percent,
                         limits = c(0, ub),
                         breaks = c(0, .25, .5, .75, .9),
                         na.value = "gray70") +
    facet_wrap(~ dataset) +
    labs(fill = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank())

ggsave("../img/map-observed-lowinc.eps", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-observed-lowinc.pdf", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-observed-lowinc.jpg", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-observed-lowinc.png", dpi = 300,
       width = 12, height = 12)

ggplot(data = mutate(lowinc, dataset = "(D)")) +
    geom_sf(aes(fill = p_est),
            color = 1, lwd = .1) +
    geom_sf_text(data = filter(mutate(full, dataset = "(D)"),
                               ! STUSPS %in% c(edit_1,
                                               edit_2)),
                 aes(label = STUSPS),
                 size = 3, color = "white") +
    geom_segment(data = mutate(full_2, dataset = "(D)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_segment(data = mutate(full_3, dataset = "(D)"),
                 aes(x = crd_x, y = crd_y,
                     xend = end_x, yend = end_y),
                 lwd = .5, color = "gray") +
    geom_label(data = mutate(full_2, dataset = "(D)"),
              aes(x = end_x, y = end_y,
                  label = STUSPS),
              size = 3, color = "black") +
    geom_label(data = mutate(full_3, dataset = "(D)"),
               aes(x = end_x, y = end_y,
                   label = STUSPS),
               size = 3, color = "black") +
    scale_fill_viridis_c(option = "D",
                         labels = scales::percent,
                         limits = c(0, ub),
                         breaks = c(0, .25, .5, .75, .9),
                         na.value = "gray70") +
    facet_wrap(~ dataset) +
    labs(fill = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank())

ggsave("../img/map-predicted-lowinc.eps", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-lowinc.pdf", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-lowinc.jpg", dpi = 300,
       width = 12, height = 12)
ggsave("../img/map-predicted-lowinc.png", dpi = 300,
       width = 12, height = 12)
