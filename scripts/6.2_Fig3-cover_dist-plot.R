# Community-level cover & additional disturbances
# Figure 3

# 1. Load Libraries and Set Up Environment --------------------------------
library(patchwork)
library(tidyverse)   
library(gridExtra)
library(brms)        


# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_cover<- read.csv(here::here("data", "df_cover_brm.csv"), header = T) 

df_cover %>% names


# removal_disturbance -----------------------------------------------------
df_cover$add_dist <- "no"

df_cover$add_dist[df_cover$warming == "yes"] <- "warming"
df_cover$add_dist[df_cover$NPK_addition == "yes"] <- "NPK_addition"


# 3. Upload model and plot figures ----------------------------------------


mod_cover_add_dist <- read_rds(here::here("model_output", "mod_cover_warmnutr.rds")) 

mod_cover_add_dist %>% summary

## plot disturbance

cover_fitted <- cbind(mod_cover_add_dist$data, 
                      fitted(mod_cover_add_dist, re_formula = NA)) %>% 
  as_tibble() %>% 
  left_join(df_cover,  
            by = c("study_ID", "block",
                   "plot", "time_pad", 
                   "cover", "add_dist",
                   "removed_propo"))

## plot 
Fig.3b_cover_add_dist <- 
  df_cover %>% 
  
  ggplot(aes(x = removed_propo, fill = add_dist, color = add_dist)) +
  
  # Plot add_dist = "no" first (as background)
  geom_point(data = df_cover %>% filter(add_dist == "no"),
             aes(y = cover),
             size = 1) +
  # Plot add_dist = "yes" on top (foreground)
  geom_point(data = df_cover %>% filter(add_dist == "warming"),
             aes(y = cover),
             size = 1) + 
  # Plot add_dist = "yes" on top (foreground)
  geom_point(data = df_cover %>% filter(add_dist == "NPK_addition"),
             aes(y = cover),
             size = 1) + 
  
  geom_smooth(data = cover_fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity") +
  
  scale_x_continuous(
    labels = function(x) ifelse(
      x == 0, "0% \ncontrol and \npre removals", 
      scales::percent(x, scale = 100))) + 
  
  scale_y_continuous(
    trans = 'log', 
    breaks = c(0, 2, 4, 8, 16,32, 64, 128,
               256)) +
  
  scale_colour_manual(
    name = "",
    labels = c("species removal",
               "species removal +\nnutrient addition",
               "species removal +\nwarming"),
    values = c("#6AC9B5","#682555", "darkorange")) + 
  
  scale_fill_manual(
    name = "",
    labels = c("species removal",
               "species removal +\nnutrient addition",
               "species removal +\nwarming"),
    values = c("#6AC9B5","#682555", "darkorange")) + 
  
  labs(x = 'Proportion of species removed',
       y = 'Total species cover') +
  
  coord_cartesian(xlim = c(0, .32), clip = "off") + 
  
  theme(
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(.63, .16),
    legend.margin = margin(1, 1, 1, 1),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    
    text = element_text(size = 10, family = "Helvetica", colour = "black"))

Fig.3b_cover_add_dist

# ggsave("Fig.3b_cover-add_dist.pdf", 
#  path = "figures", width = 90, height = 100, units = 'mm')


# To reproduce this figure, you need the figures from the script 5.2_Fig3-biomass_dist-plot.R
Fig.3a_biom_add_dist + Fig.3b_cover_add_dist + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'),
        text = element_text(size = 7, family = "Helvetica"))

# ggsave("Fig.3.pdf",  path = "figures",      width = 200, height = 100, units = 'mm')


