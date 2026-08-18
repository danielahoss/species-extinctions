# Supplementary Fig. 1: geographic distribution of the study locations
# Study points (jittered up to +/-2 degrees) on a Mollweide world map.

library(tidyverse)
library(sf)
library(rnaturalearth)

rm(list = ls())
if (!is.null(dev.list())) dev.off()

effects_brm <- read.csv(here::here("data", "moderators_Synthesis_MA.csv"), header = TRUE) %>%
  distinct(study_ID, .keep_all = TRUE)

sf::sf_use_s2(FALSE)
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  st_union()
sf::sf_use_s2(TRUE)

set.seed(42)
effects_sf <- effects_brm %>%
  mutate(longitude = longitude + runif(n(), -2, 2),
         latitude  = latitude  + runif(n(), -2, 2)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=moll")

# projected y-coordinates of each latitude line (from st_transform)
lat_labels <- data.frame(
  x     = -18560000,
  y     = c(-6876759, -3643854, 0, 3643854, 6876759),
  label = c("60°S", "30°S","0°",  "30°N", "60°N")
)

# projected x-coordinates of each longitude line at equator
lon_labels <- data.frame(
  x     = c(-13404708, -8936472, -4468236, 0, 4468236, 8936472, 13404708),
  y     = -7500000,
  label = c("120°W", "80°W", "40°W", "0°", "40°E", "80°E", "120°E")
)

map_supp <- ggplot() +

  geom_sf(data = world_sf,
          fill = "#DAC17C", colour = NA) +

  geom_sf(data = effects_sf,
          shape = 21, size = 1, stroke = 0.7,
          fill = "black", colour = "black", alpha = 0.95) +

  annotate("text", x = lat_labels$x, y = lat_labels$y, label = lat_labels$label,
           color = "grey30", size = 3, hjust = 0.5, family = "Helvetica") +

  annotate("text", x = lon_labels$x, y = lon_labels$y, label = lon_labels$label,
           color = "grey30", size = 3, hjust = 0.5, family = "Helvetica") +


  coord_sf(crs = "+proj=moll", expand = FALSE, clip = "off",
           xlim = c(-18040096, 18040096),
           ylim = c(-7200000,   9020048)) +

  theme_void() +
  theme(
    panel.background  = element_rect(fill = "#1A6B9A", color = "black"),
    plot.background   = element_rect(fill = "#DAC17C", color = "black"),
    panel.grid.major  = element_line(color = "grey60", linewidth = 0.3),
    plot.margin       = margin(10, 25, 25, 30)
  ) 
 
 

map_supp

ggsave(here::here("figures", "SupplementaryFigure_01_map.pdf"), map_supp,
       width = 250, height = 140, units = "mm", dpi = 300)
 