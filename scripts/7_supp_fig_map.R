library(ggplot2)
library(dplyr)
library(viridis)

# Create a simplified filtered dataset
filtered_data <- effects_brm %>%
  distinct(study_ID, .keep_all = TRUE) %>%
  mutate(color_ref = ref)  # create a column for consistent color mapping

ggplot() +
  geom_polygon(data = world, 
               aes(long, lat, group = group),
               colour = NA, fill = '#DAC17C') +
  
  geom_point(data = filtered_data,
             shape = 21, size = 3, alpha = 0.8,
             position = position_jitter(width = 1, height = 1),
             aes(x = longitude, y = latitude, fill = color_ref, color = color_ref)) +
  
  scale_fill_manual(values = viridis(38, option = "magma")) +
  scale_color_manual(values = viridis(38, option = "magma")) +
  
  coord_map('mollweide', ylim = c(-60, 90), xlim = c(-180, 180)) +
  scale_x_continuous(name = 'Longitude', breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(name = 'Latitude', breaks = c(0, -23.5, 23.5, -60, 60)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 10)) +
  guides(fill = guide_legend(ncol = 3))





# new ---------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(maps)

# Prepare the world map
world <- map_data('world')

# Process your data
effects_brm <- effects_brm %>%
  distinct(study_ID, .keep_all = TRUE) %>%
  mutate(refs_ID = paste0(" (", study_ID, ") ")) %>% 
  unite("ref", c("author", "publ_year", "refs_ID", "country"), sep = " ", remove = FALSE) %>%
  arrange(country, publ_year) %>%  
  mutate(ref = factor(ref, levels = unique(ref)))

# Plot
ggplot() +
  geom_polygon(data = world, 
               aes(long, lat, group = group),
               colour = NA, fill = '#DAC17C') +
  
  geom_point(data = effects_brm,
             shape = 21, size = 3, alpha = 0.8,
             position = position_jitter(width = 1, height = 1.2),
             aes(x = longitude, y = latitude, fill = ref, color = ref)) +
  
  scale_fill_manual(values = viridis(38, option = "magma")) +
  scale_color_manual(values = viridis(38, option = "magma")) +
  
  coord_map('mollweide', ylim = c(-60, 90), xlim = c(-180, 180)) +
  scale_x_continuous(name = 'Longitude', breaks = seq(-180, 180, by = 30)) +
  scale_y_continuous(name = 'Latitude', breaks = c(0, -23.5, 23.5, -60, 60)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 10, face = 'plain')) +
  guides(fill = guide_legend(ncol = 3))

