# Community-level biomass & additional disturbances
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
     

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom<- read.csv(here::here("data", "df_biom_brm.csv"), header = T) 

df_biom %>% head

df_biom$add_dist <- "no"
df_biom$add_dist[df_biom$drought == "yes"] <- "drought"
df_biom$add_dist[df_biom$NPK_addition == "yes"] <- "nutrient"
df_biom$add_dist[df_biom$N_addition == "yes"] <- "nutrient"

as.factor(df_biom$add_dist) %>% levels


df_biom$add_dist <- factor(df_biom$add_dist, levels = c("drought", "no", "nutrient"))
df_biom$add_dist <- relevel(df_biom$add_dist, ref = "no")  # Change reference level

as.factor(df_biom$study_ID[which(df_biom$add_dist == "drought" )]) %>% levels
# "ID_215" -> drought

as.factor(df_biom$study_ID[which(df_biom$add_dist == "nutrient" )]) %>% levels



# 3.1 Model including drought ---------------------------------------------

mod_biomass_add_dist <- read_rds(here::here("model_output", "model_biom_droughtnutr.rds")) 
mod_biomass_add_dist %>% summary
mod_biomass_add_dist %>% get_prior()

biom_fitted <- cbind(mod_biomass_add_dist$data, fitted(mod_biomass_add_dist, re_formula = NA)) %>% 
  as_tibble() %>% 
  left_join(df_biom, by = c("study_ID", "block",
                            "plot", "time_pad", 
                            "biomass", "add_dist",
                            "removed_propo"))
biom_fitted %>% head


biom_exp_coef <- coef(mod_biomass_add_dist)

biom_exp_coef2 <- bind_cols(
  biom_exp_coef$study_ID[,,'Intercept'] %>%
    as_tibble() %>%
    mutate(Intercept = Estimate,
           Intercept_lower = Q2.5,
           Intercept_upper = Q97.5,
           study_ID = rownames(biom_exp_coef$study_ID[,,'Intercept'])
    ) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  biom_exp_coef$study_ID[,,'add_distnutrient:removed_propo'] %>% # 
    as_tibble() %>%
    mutate(Slope = Estimate,
           Slope_lower = Q2.5,
           Slope_upper = Q97.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5))   %>%
  inner_join(df_biom %>%
               group_by(study_ID) %>%
               summarise(xmin = min(removed_propo),
                         xmax = max(removed_propo),
                         cxmin = min(removed_propo),
                         cxmax = max(removed_propo)),
             by = 'study_ID')  %>% 
  inner_join(biom_fitted %>%
               dplyr::distinct(study_ID, author, publ_year, .keep_all= FALSE) %>% 
               mutate(refs_ID = paste0(" (", study_ID, ") ")) %>% 
               unite("ref",c("author", "publ_year", "refs_ID"), sep = " ", remove = T),
             by = 'study_ID')


biom_fix <- as.data.frame(fixef(mod_biomass_add_dist))


df_biom$add_dist <- factor(df_biom$add_dist, levels = c("drought", "no", "nutrient"))
df_biom$add_dist <- relevel(df_biom$add_dist, ref = "no")  # Change reference level

Fig.3a_biom_add_dist <- 
  df_biom %>% 
  
  ggplot(aes(x = removed_propo, 
             fill = add_dist, 
             color = add_dist)) +
  
  # Plot disturbance = "no" first (as background)
  geom_point(data = df_biom %>% filter(add_dist == "no"),
             aes(y = biomass),
             size = 1) +
  
  # Plot disturbance = "nutrient" on top (foreground)
  geom_point(data = df_biom %>% filter(add_dist == "nutrient"),
             aes(y = biomass),
             size = 1) + 
    # Plot disturbance = "drought" on top (foreground)
    geom_point(data = df_biom %>% filter(add_dist == "drought"),
               aes(y = biomass),
               size = 1) + 
    
  geom_smooth(data = biom_fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity") +
  
  scale_x_continuous(
    labels = function(x) ifelse(
      x == 0, "0% \ncontrol and \npre removals", 
      scales::percent(x, scale = 100))) +  
  
  scale_y_continuous(trans = 'log', 
                     breaks = c(0, 2, 4, 8, 16,32, 64, 128,
                                256, 512, 1024, 2048)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  coord_cartesian(xlim = c(0, 1), clip = "off") + 
  
  scale_colour_manual(name = "",
                      labels = c("species removal",
                                 "species removal +\nnutrient addition",
                                 "species removal +\ndrought"),
                      values = c("#CD7233", "#682555", "#5E3D2D")) +  
                              
  scale_fill_manual(name = "",
                    labels = c("species removal",
                       "species removal +\nnutrient addition",
                               "species removal +\ndrought"),
                    values = c("#CD7233", "#682555", "#5E3D2D")) + 
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
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

Fig.3a_biom_add_dist

# ggsave("Fig.3a_biom_add_dist.pdf", path = "figures", 
#         width = 90, height = 100, units = 'mm')

