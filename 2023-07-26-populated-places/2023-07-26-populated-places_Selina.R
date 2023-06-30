library(tidyverse)

us_place_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv")
us_place_history <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv")

us_places <- full_join(us_place_names, us_place_history)

# Prepare US Hexbin map -----------------------------------------------------------
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Read spatial hexagones
us_hex <- geojsonsf::geojson_sf("2023-07-26-populated-places/us_states_hexgrid.geojson")

# Reformate the state column
us_hex <- us_hex %>%
  rename(state_name = google_name) %>%
  mutate(state_name = str_replace(state_name, " \\(United States\\)", ""))

# Find centroids of each state (for plotting of the state name abbreviation)
centers <- us_hex %>%
  select(iso3166_2, geometry) %>%
  sf::st_centroid()

# Prepare data ------------------------------------------------------------

# Count no of landmarks in each state
landmarks <- us_places %>%
  group_by(state_name) %>%
  count()

# Combine with the spatial data
landmarks_us_hex <- left_join(us_hex, landmarks, by = "state_name")

# Make the plot -----------------------------------------------------------
ggplot(landmarks_us_hex) +
  geom_sf(aes(fill = n), color = "NA") +
  geom_sf_text(data = centers, aes(label = iso3166_2), size = 3) +
  paletteer::scale_fill_paletteer_c("grDevices::BluYl", direction = -1) +
  labs(
    fill = "", title = "Number of popular landmarks in the US",
    subtitle = "Not sure what this map should tell me in general",
    caption = "Data from somewhere"
  ) +
  theme_void()
