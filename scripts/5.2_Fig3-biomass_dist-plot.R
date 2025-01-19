# Community-level biomass & additional disturbances
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom<- read.csv(here::here("data", "df_biom_brm.csv"), header = T) 

df_biom %>% names

                   
# removal_disturbance -----------------------------------------------------
df_biom$disturbance <- "no"

# do not use drought
# "drought",
df_biom$disturbance[apply(df_biom[,c(
  "drought", "NPK_addition", "N_addition")] == "yes", 1, any)] <- "yes"

as.factor(df_biom$disturbance) %>% levels


df_biom$add_dist <- "no"

df_biom$add_dist[df_biom$drought == "yes"] <- "drought"
df_biom$add_dist[df_biom$NPK_addition == "yes"] <- "nutrient"
df_biom$add_dist[df_biom$N_addition == "yes"] <- "nutrient"

as.factor(df_biom$add_dist) %>% levels





# 3. Upload model and plot figures ----------------------------------------


# 3.1 Model including drought ---------------------------------------------

mod_biomass_add_dist <- read_rds(here::here("model_output", "mod_biomass_add_dist.rds")) 
mod_biomass_add_dist %>% summary


biom_fitted <- cbind(mod_biomass_add_dist$data, fitted(mod_biomass_add_dist, re_formula = NA)) %>% 
  as_tibble() %>% 
  inner_join(df_biom %>% 
               distinct(study_ID,
                        block, 
                        plot,
                        time_pad, 
                        removed_propo,
                        richness, 
                        biomass, 
                        disturbance, 
                        add_dist),
             by = c('study_ID', 'block', 'plot', 
                    'removed_propo', 'disturbance',
                    "biomass"))
biom_fitted %>% head



biom_exp_coef <- coef(mod_biomass_add_dist)

biom_exp_coef2 <- bind_cols(biom_exp_coef$study_ID[,,'Intercept'] %>%
                              as_tibble() %>%
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     study_ID = rownames(biom_exp_coef$study_ID[,,'Intercept'])) %>%
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            biom_exp_coef$study_ID[,,'disturbanceyes'] %>%
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
             by = 'study_ID')



Fig.3a_biom_add_dist <- 
  df_biom %>% 
  
  ggplot(aes(x = removed_propo, 
             fill = disturbance, 
             color = disturbance,
             shape = disturbance)) +
  
  #geom_point(data = df_biom, aes(y = biomass, colour = disturbance), size = 1, alpha = 0.4) +
  
  geom_jitter(aes(y = biomass, 
                  color = disturbance),
              width = 0.001, height = 0, size = 1,  alpha = 0.8) +
  
  geom_smooth(data = biom_fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity") +
  
  # Scale axis
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  
  scale_y_continuous(trans = 'log', 
                     breaks = c(0, 2, 4, 8, 16,32, 64, 128,
                                256, 512, 1024, 2048)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  coord_cartesian(xlim = c(0, 1), clip = "off") + 
  
  scale_colour_manual(name = "",
                      labels = c("species removal",
                                 "species removal +\nadd disturbance"),
                      values = c( "#996633","#452800")) +
  
  scale_fill_manual(name = "",
                    labels = c("species removal",
                               "species removal +\nadd disturbance"),
                    values = c( "#996633","#452800")) +
  
  scale_shape_manual(name = "",
                     labels = c("species removal",
                                "species removal +\nadd disturbance"),
                     values = c(19, 19)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  theme(
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(.8, .12),
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

ggsave("ExtendedData_Fig.3a_biom_add_dist.pdf", path = "figures", 
       width = 90, height = 100, units = 'mm')


# 3.2 Model excluding drought ---------------------------------------------
# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom<- read.csv(here::here("data", "df_biom_brm.csv"), header = T) 

df_biom %>% names


# removal_disturbance -----------------------------------------------------
df_biom$disturbance <- "no"

# do not use drought
# "drought",
df_biom$disturbance[apply(df_biom[,c(
  "NPK_addition", "N_addition")] == "yes", 1, any)] <- "yes"

as.factor(df_biom$drought) %>% levels
as.factor(df_biom$disturbance) %>% levels
as.factor(df_biom$add_dist) %>% levels

df_biom$disturbance[df_biom$drought == "yes"] <- "no"

df_biom$add_dist <- "no"
df_biom$add_dist[df_biom$add_dist == "drought"] <- "no"

df_biom$add_dist[as.factor(df_biom$drought == "drought")] <- "no"

mod_biom_no_drought <- read_rds(here::here("model_output", "mod_biomass_add_dist_no-drought.rds")) 
mod_biom_no_drought %>% summary

# plot ----------------------------------------------------------

biom_fitted <- cbind(mod_biom_no_drought$data, fitted(mod_biom_no_drought, re_formula = NA)) %>% 
  as_tibble() %>% 
  inner_join(df_biom %>% 
               distinct(study_ID,
                        block, 
                        plot,
                        time_pad, 
                        removed_propo,
                        richness, 
                        biomass, 
                        disturbance, 
                        add_dist),
             by = c('study_ID', 'block', 'plot', 
                    'removed_propo', 'disturbance',
                    "biomass"))
biom_fitted %>% head


as.factor(biom_fitted$disturbance) %>% levels
as.factor(biom_fitted$add_dist) %>% levels

biom_exp_coef <- coef(mod_biom_no_drought)

biom_exp_coef2 <- bind_cols(biom_exp_coef$study_ID[,,'Intercept'] %>%
                              as_tibble() %>%
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     study_ID = rownames(biom_exp_coef$study_ID[,,'Intercept'])) %>%
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            biom_exp_coef$study_ID[,,'disturbanceyes'] %>%
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
             by = 'study_ID')

## plot disturbance
df_biom %>% head

dev.off()


Fig.3a_biom_add_dist <- 
  df_biom %>% 
  
  ggplot(aes(x = removed_propo, 
             fill = disturbance, 
             color = disturbance,
             shape = disturbance)) +
  
  #geom_point(data = df_biom, aes(y = biomass, colour = disturbance), size = 1, alpha = 0.4) +
  
  geom_jitter(aes(y = biomass, 
                  color = disturbance),
              width = 0.01, height = 0.001, size = 1,  alpha = 0.8) +
  
  geom_smooth(data = biom_fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity") +
  
  # Scale axis
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  
  scale_y_continuous(trans = 'log', 
                     breaks = c(0, 2, 4, 8, 16,32, 64, 128,
                                256, 512, 1024, 2048)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  coord_cartesian(xlim = c(0, 1), clip = "off") + 
  
  scale_colour_manual(name = "",
                      labels = c("species removal",
                                 "species removal +\nadd disturbance"),
                      values = c( "#996633","#452800")) +
  
  scale_fill_manual(name = "",
                    labels = c("species removal",
                               "species removal +\nadd disturbance"),
                    values = c( "#996633","#452800")) +
  
  scale_shape_manual(name = "",
                     labels = c("species removal",
                                "species removal +\nadd disturbance"),
                     values = c(19, 19)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  theme(
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(.8, .12),
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

ggsave("Fig.3a_biomass-add_dist_no_drought.pdf", path = "figures", 
       width = 90, height = 100, units = 'mm')


