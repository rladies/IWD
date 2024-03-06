library(tidyverse)
library(jsonlite)
chapters_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/rladies/meetup_archive/main/data/chapters_meetup.json")
events_data <- jsonlite::fromJSON('https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json')

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
    caption = "Source: meetup.com (March 2024)"
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

ggsave("plots/chapters_maps/2024-chapter-map.png", width = 5.5, height = 3, bg = "white")

select(1,2,3,7,8)%>% 
  rename(chapter=group_urlname)%>%
  mutate(location=ifelse(location=="Online event","online","inperson"),
         title=sub(".*-- ","",title),
         title=gsub("\\s*\\([^\\)]+\\)","",title)) %>%
  filter(!str_detect(title,regex("canceled|cancelled",ignore_case=T)),
         !chapter%in%c("RLadiesJeddah","muhq_deleted@4633@rladies-ushuaia",
                       "muhq_deleted@9919@notopic@508502","notopic@544550"))%>%
  arrange(desc(date))%>%
  filter(year(date)<2024)%>%
  mutate(year=year(date))