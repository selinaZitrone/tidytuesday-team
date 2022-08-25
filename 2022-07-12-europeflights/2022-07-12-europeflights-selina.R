library(tidyverse)
library(ggbump)
library(paletteer)
library(ggtext) # to use element_markdown



# Get the data and prepare------------------------------------------------------

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

# make names to lower for easier typing:
names(flights) <- tolower(names(flights))

# summarize flights per country and year
flights2 <- flights |>
  group_by(state_name, year) |>
  summarize(n = sum(flt_tot_1))

# remove marocco and israel (not complete)
flights2 <- flights2 |> filter(!(state_name %in% c("Israel", "Morocco")))

# Add population of the countries

# data from https://en.wikipedia.org/wiki/List_of_European_countries_by_population

populations <- tibble(
  state_name = flights |> arrange(state_name) |> pull(state_name) |> unique(),
  population_mio = c(
    2.79, 2.96, 9.03, 11.58, 3.48, 6.84, 3.88, 0.91, 10.52, 5.88, 1.33, 5.55, 67.87,
    3.69, 83.70, 10.68, 9.69, 5.12, 9.56, 58.89, 1.89, 2.80, 0.63, 0.52, 2.60,
    0.62, 32.88, 17.68, 5.44, 38.01, 10.36, 1.84, 19.19, 6.80, 5.43, 2.11, 47.43,
    10.48, 8.75, 84.68, 41.13, 66.33
  )
)

# Add population to flights2
flights2 <- flights2 |>
  left_join(populations, by = "state_name")

# Add a column for flights by 1 mio inhabitants:
flights2 <- flights2 |> mutate(
  per_mio = n/population_mio
)

# calculate the rank for the countries with most flights per mio inhabitants for
# every year
flights2 <- flights2 |>
  group_by(year) |>
  mutate(rank = rank(per_mio, ties.method = "random")) |>
  ungroup()

# calculate the ranks for the first year only and add it as a new column
ranks_fy <- flights2 |> filter(year == min(year)) |> rename(rank_one = rank) |>
  select(state_name, rank_one)
# add rank one to flights
flights2 <- flights2 |> left_join(ranks_fy, by = "state_name")

# Calculate the rank change over the years for each country and add it as a new
# column
flights2 <- flights2 |>
  select(state_name, year, rank, rank_one) |>
  pivot_wider(names_from = year, values_from = rank, id_cols = c(rank_one, state_name)) |>
  mutate(
    rank_change = `2022` - `2016`
  ) |>
  pivot_longer(`2016`:`2022`, names_to = "year", values_to = "rank", names_transform = as.integer) |>
  group_by(state_name) |>
  ungroup()


# Make the plot -----------------------------------------------------------

p <- flights2 |>
  mutate(state_name = fct_reorder(state_name, -rank_one)) |>
  droplevels() |>
  ggplot(aes(
    year, rank,
    group = state_name, color = rank_change, size = abs(rank_change)
  )) +
  geom_bump() +
  geom_point(size = 1) +
  geom_text(
    data = flights2 %>% filter(year == min(year)),
    aes(x = year - .1, label = state_name), size = 4, hjust = 1
  ) +
  geom_text(
    data = flights2 %>% filter(year == max(year)),
    aes(x = year + .1, label = state_name), size = 4, hjust = 0
  ) +
  scale_x_continuous(
    limits = c(2013, 2025),
    breaks = seq(2016, 2022, 1),
    position = "bottom",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  +scale_color_paletteer_c(palette = "grDevices::Tropic", direction = -1) +
  scale_size_continuous(range = c(0.5, 1.5)) +
  labs(
    title = "Which country flies most?",
    subtitle = "Ranks (y-axis) of the number of flights per million inhabitants from 2016 to 2022.<br> Some countries **<span style = 'color:#841859;'> lose </span>** and some **<span style = 'color:#009B9F;'> win </span>**"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    plot.title = element_markdown(color = "white", size = "18"),
    plot.subtitle = element_markdown(color = "white", size = 14),
    plot.background = element_rect(fill = "grey20", color = NA),
    panel.background = element_rect(fill = "grey20", color = NA)
  )

ggsave("2022-07-12-europeflights/2022-07-19-europeflights-selina.png", p, width = 8.27, height = 11.69 )




