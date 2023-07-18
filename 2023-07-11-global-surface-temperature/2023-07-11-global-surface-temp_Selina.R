library(tidyverse)


# Read the data -----------------------------------------------------------
# Values are deviations from 1951-1980 means
global_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv")
# northern hemisphere
nh_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv")
# Southern hemisphere
sh_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv")

zonann_temps <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv")

# filter only interesting cols
global_temps <- global_temps %>% select(Year, `J-D`, DJF, MAM, JJA, SON)
nh_temps <- nh_temps %>% select(Year, `J-D`, DJF, MAM, JJA, SON)
sh_temps <- sh_temps %>% select(Year, `J-D`, DJF, MAM, JJA, SON)

temps <- bind_rows(global = global_temps, nh_temps = nh_temps, sh_temps = sh_temps, .id = "type")

# Global vars -------------------------------------------------------------
stripes_palette <- "ggthemes::Classic Red-Blue"

theme_strip <- theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

# Bars -------------------------------------------------------
# Adding the reference period
zonann_temps %>%
  ggplot(aes(x = Year, y = Glob, fill = Glob)) +
  geom_col(width = 1) +
  scale_fill_gradient2(
    low = "#26456E",
    high = "#9C0824",
    breaks = c(-1, 0, 1), limits = c(-1.2, 1.2),
  ) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  annotate(
    geom = "rect", xmin = 1951, xmax = 1980, ymin = -0.2, ymax = 0.2,
    fill = NA, color = "gray20"
  ) +
  labs(
    title = "We are doomed",
    subtitle = "Global mean annual temperature deviation from reference period (1951-1980)"
  ) +
  theme_strip #+
  transition_time(Year) +
  shadow_mark() +
  enter_grow() +
  enter_fade()


# Warming stripes ---------------------------------------------------------

zonann_temps %>%
  ggplot(aes(x = Year, y = 1, fill = Glob)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#26456E",
    high = "#9C0824",
    breaks = c(-1, 0, 1), limits = c(-1.2, 1.2),
  ) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  annotate(
    geom = "rect", xmin = 1951, xmax = 1980, ymin = 0.45, ymax = 1.55,
    fill = NA, color = "gray20"
  ) +
  labs(
    title = "We are doomed",
    subtitle = "Global mean annual temperature deviation from reference period (1951-1980)"
  ) +
  theme_strip




# Lineplot with animation -------------------------------------------------
library(gganimate)

zonann_temps %>%
  # select(Year, Glob, NHem, SHem) %>%
  # pivot_longer(!Year) %>%
  ggplot(aes(x = Year, y = Glob)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    y = "Difference in temperature rel. to reference"
  ) +
  theme(axis.title.x = element_blank()) +
  gganimate::transition_reveal(Year)

## save as gif
animation <- animate(anim)
anim_save(filename = "animation.gif")


# Also tried --------------------------------------------------------------

# Which regions are warming in particular? --------------------------------
# Fine scale
zonann_temps %>%
  select(!c(Glob, NHem, SHem)) %>%
  pivot_longer(!Year, names_to = "region", values_to = "temp") %>%
  filter(!region %in% c(
    "90S-24S",
    "24S-24N",
    "24N-90N"
  )) %>%
  mutate(region = fct_relevel(region, rev(c(
    "64N-90N",
    "44N-64N",
    "24N-44N",
    "EQU-24N",
    "24S-EQU",
    "44S-24S",
    "64S-44S",
    "90S-64S"
  )))) %>%
  ggplot(aes(y = region, x = Year, fill = temp)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#26456E",
    high = "#9C0824",
    breaks = c(-1, 0, 1), limits = c(-1, 1),
  )

# Coarse scale
zonann_temps %>%
  select(!c(Glob, NHem, SHem)) %>%
  pivot_longer(!Year, names_to = "region", values_to = "temp") %>%
  filter(region %in% c(
    "90S-24S",
    "24S-24N",
    "24N-90N"
  )) %>%
  mutate(region = fct_relevel(region, rev(c(
    rev(c(
      "90S-24S",
      "24S-24N",
      "24N-90N"
    ))
  )))) %>%
  ggplot(aes(y = region, x = Year, fill = temp)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#26456E",
    high = "#9C0824",
    breaks = c(-1, 0, 1), limits = c(-1, 1),
  )
