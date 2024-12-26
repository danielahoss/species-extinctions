require(tidyverse)

rm(list = ls())
# Loading the data ----
effects_brm <- read.csv(here::here("data", "processed", "moderators_Synthesis_MA.csv"), header = T) 


effects_brm <- effects_brm %>% filter (study_ID != "ID_42")
# simple map
world <- map_data('world')

  

ggplot() +
  geom_polygon(data = world, 
               aes(long, lat, group = group),
               colour = NA, fill = '#DAC17C', linewidth = 0) +
  
  geom_point(data = effects_brm %>%
               distinct((study_ID), .keep_all = TRUE) %>%
               filter(country == "Brazil"),
             shape = 21, size = 3, alpha = 0.7,
             position=position_jitter(h = 1.5, w = .4),
             aes(x = longitude, y = latitude, fill=factor(study_ID))) +
  
  geom_point(data = effects_brm %>%
               distinct((study_ID), .keep_all = TRUE),  
             color = 'black', shape = 21, size = 3, alpha = 0.7,
             position=position_jitter(h = 0.9, w = 1),
             aes(x = longitude, y = latitude, fill=factor(study_ID)) ) +
  # 
  scale_fill_manual(values = viridis::viridis(38, option = "magma")) +
  
  coord_map('mollweide', 
            ylim = c(-60, 90), xlim = c(-180, 180)) +
  scale_x_continuous(name = 'Longitude', breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(name = 'Latitude', breaks = c(0, -23.5, 23.5, -60, 60)) +
  #scale_size_area(name = "country") +
  theme_bw( ) +
  theme(legend.position = 'none')

  # theme(panel.grid.major = element_line(colour = 'black', linewidth = 0.2), 
  #       panel.border = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       legend.position = 'none', #'top',
  #       plot.background = element_rect(fill = "white", color = "black"), #  plot.background = element_rect(fill = "#B7C5B9", color = "black"),
  #       legend.text = element_text(size = 16, face = 'plain'),
  #       legend.title = element_text(size = 18, face = 'bold')) +
  # guides(colour = guide_legend(title.position = 'top', title.hjust = 0.5),
  #        shape = guide_legend(title.position = 'top', title.hjust = 0.5, size = 3),
  #        size = guide_legend(title.position = 'top', title.hjust = 0.5))





 ggsave("Supp_fig_Map.pdf",  path = "figures",
        width = 290, height = 200, units = 'mm')

