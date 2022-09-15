frogs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv")

library(tidyverse)
library(sf)
library(osmdata)
library(lubridate)
library(patchwork)

# Transform date column to month
frogs <- mutate(frogs, Month = month(mdy(SurveyDate),
  label = TRUE, abbr = FALSE
))


# Code from https://rpubs.com/scolando/Tidy-Tuesday-08-02-2022
crane_prairie <- opq(bbox = c(-121.7920729, 43.7938767, -121.76501, 43.81433)) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

cr_pr_w <- crane_prairie$osm_polygons %>%
  sf::st_transform(crs = "EPSG: 32610")


# Make plots for the frogs in each month separately -----------------------
p_sep <- frogs %>%
  filter(Month == "September") %>%
  ggplot() +
  labs(subtitle = "September")

p_oct <- frogs %>%
  filter(Month == "October") %>%
  ggplot() +
  labs(subtitle = "October") +
  theme(axis.text.x = element_blank())

# This plot contains north arrow and scalebar
p_nov <- frogs %>%
  filter(Month == "November") %>%
  ggplot() +
  labs(subtitle = "November") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    width = unit(0.5, "cm"),
    height = unit(0.5, "cm")
  )

# Combine the three plots together, adding data layers and using frogs instead
# of points
frog_plot <- p_sep + p_oct + p_nov +
  plot_annotation(title = "Where are the frogs??") &
  geom_sf(data = cr_pr_w, fill = "lightblue", color = "azure3", size = 0.1) &
  emoGG::geom_emoji(aes(x = UTME_83, y = UTMN_83), emoji = "1f438", size = 0.05) &
  theme_bw() &
  theme(
    axis.title = element_blank(),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave("2022-09-10-frogs/2022_09_10_frogs_Selina.png",
       frog_plot,
       width = 16,
       height = 9,
       units = "cm")

# Other things I tried ----------------------------------------------------

# summarize number of frogs
frogs %>%
  mutate(Month = month(SurveyDate, label = TRUE, abbr = FALSE)) %>%
  ggplot() +
  geom_sf(data = cr_pr_w, fill = "azure3", color = "azure4", size = 0.1) +
  emoGG::geom_emoji(aes(x = UTME_83, y = UTMN_83), emoji = "1f438", size = 0.05) +
  facet_wrap(~Month) +
  theme_void() +
  theme(
    axis.title = element_blank()
  ) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    width = unit(0.5, "cm"),
    height = unit(0.5, "cm")
  )
