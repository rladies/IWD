library(tidyverse)
library(jsonlite)
chapters_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/rladies/meetup_archive/main/data/chapters_meetup.json")
events_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json")

# Map of chapters ---------------------------------------------------------

chapters <- chapters_data |>
  as_tibble() |>
  select(lat, lon)

world <- map_data("world") |>
  filter(region != "Antarctica")

ggplot() +
  geom_map(
    data = world,
    mapping = aes(
      x = long,
      y = lat,
      map_id = region
    ),
    map = world, fill = "#a7a9ac",
    colour = "white", linewidth = 0.1
  ) +
  geom_point(
    data = chapters,
    mapping = aes(x = lon, y = lat),
    col = "#88398a", size = 0.4,
    fill = alpha("#88398A", 0.6),
    pch = 21
  ) +
  labs(
    title = "R-Ladies Chapters",
    caption = "Source: meetup.com (March 2025)"
  ) +
  coord_sf() +
  theme_void(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(1.4),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0)
  )

ggsave("plots/chapters_maps/2025-chapter-map.png", width = 5.5, height = 3, bg = "white")

# Events bar chart --------------------------------------------------------

events_bar_data <- events_data |>
  mutate(year = as.character(year(date))) |>
  count(year) |>
  filter(year != 2038)

ggplot(events_bar_data) +
  geom_col(aes(x = year, y = n), fill = "#88398A") +
  geom_text(aes(x = year, y = n + 40, label = n), colour = "#88398A") +
  labs(title = "R-Ladies Events", x = "", y = "Number of events",
       caption = "Source: meetup.com (March 2025)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(1.4),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0)
  )

ggsave("plots/chapters_maps/2025-events-bar.png", width = 5.5, height = 3, bg = "white")

# Events bar chart --------------------------------------------------------

first_events_bar_data <- events_data |>
  group_by(group_urlname) |> 
  slice_min(date) |> 
  ungroup() |> 
  mutate(year = as.character(year(date))) |>
  count(year) |>
  filter(year != 2038)

ggplot(first_events_bar_data) +
  geom_col(aes(x = year, y = n), fill = "#88398A") +
  geom_text(aes(x = year, y = n + 8, label = n), colour = "#88398A") +
  labs(title = "New R-Ladies Chapters", x = "", 
       y = "Number of new chapters",
       caption = "Source: meetup.com (March 2025)") +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      face = "bold", size = rel(1.4),
      colour = "#88398A",
      margin = margin(
        t = 0, r = 0, b = 6.6,
        l = 0, unit = "pt"
      )
    ),
    plot.caption = element_text(hjust = 0)
  )

ggsave("plots/chapters_maps/2025-first-events-bar.png", width = 5.5, height = 3, bg = "white")
