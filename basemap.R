library(ggplot2)
library(maptools)
data(wrld_simpl)

# This code generates the world map and is taken from the R-Ladies Colombo code
# (https://github.com/rladiescolombo/R-Ladies_world_map)
p <- ggplot() +
  geom_polygon(
    data = wrld_simpl,
    aes(x = long, y = lat, group = group), fill = "thistle", colour = "white"
  ) +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = seq(-180, 180, 120)) +
  scale_y_continuous(breaks = seq(-90, 90, 100))
  
  # Then you use:
  
  p <- p +
  geom ...
  
  
  # Then you save the picture:
  
  ggsave(here::here("Map_name.png"), width = 15, height = 7)
  
  
  
  
