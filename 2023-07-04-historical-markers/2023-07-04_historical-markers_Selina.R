library(tidyverse)
library(sf)

historical_markers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv")
no_markers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv")

hm <- historical_markers %>%
  select(year_erected, latitude_minus_s, longitude_minus_w, state_or_prov, city_or_town) %>%
  mutate(state_or_prov = tolower(state_or_prov)) %>%
  group_by(city_or_town)

usa <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

ggplot(usa) +
  geom_sf(color = "grey50", fill = "white", size = 0.125) +
  geom_point(
    data = hm %>%
      filter(
        state_or_prov != "hawaii" & state_or_prov != "alaska" & state_or_prov != "puerto rico"
        # !is.na(year_erected)
      ),
    aes(x = longitude_minus_w, y = latitude_minus_s, color = year_erected),
    size = .01
  ) +
  paletteer::scale_color_paletteer_c(
    # "ggthemes::Blue-Green Sequential",
    # "grDevices::YlGnBu",
    "grDevices::ag_Sunset",
    # "grDevices::Green-Brown",
    # "grDevices::Tropic",
    # "grDevices::TealRose",
    na.value = "lightgrey",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  labs(
    title = "Some states don't like historical markers",
    subtitle = "The South and East seem to be crazy about markers,\nthe North and West not so much",
    caption = "Data: Historical Marker Database USA Index, Figure: Selina Baldauf",
    color = "Year when landmark\nwas erected"
  ) +
  ggthemes::theme_map()


# Another try with a coord polar plot -------------------------------------

counts <- historical_markers %>%
  group_by(state_or_prov) %>%
  count() %>%
  mutate()

counts %>%
  ggplot(aes(x = state_or_prov, y = n, fill = n)) +
  geom_col() +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  )
