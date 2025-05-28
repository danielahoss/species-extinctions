# Meta-Analysis Moderators 
# model and model checks


# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(brms) 
library(tidybayes)
library(ggdist)
require(viridis)

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = T) 


effects <- effects_brm %>% 
  ungroup() %>% 
  group_by(study_ID) %>% 
  mutate(ES_ij = 1:n()) %>% 
  ungroup() %>%
  select(ES_ij,
         study_ID,                        
         experiment_duration,            
         country,                        
         latitude,                        
         longitude,                       
         altitude_m,                     
         mean_annual_ppt_mm,              
         mean_annual_temperature_Celsius,
         author,                          
         publ_year,                      
         eff_size_ctrl,                   
         eff_size_rem,                   
         sd_eff_size_ctrl,                
         sd_eff_size_rem,     
         removal_method_category,
         n_control,                       
         n_removal,                      
         se_eff_size_ctrl,                
         se_eff_size_rem)  


# Supplemetary Fig. 2 - mean annual precipitation ----------------------------------------

mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds")) 

fitted_ppt <- mod_ppt$data %>%
  bind_cols(fitted(mod_ppt, re_formula = NA, scale = 'linear')) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_ppt$data), names(effects)),
    relationship = "many-to-many"
  ) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(study_num) %>%
  mutate(study_ID = factor(study_ID, levels = unique(study_ID))) %>%
  select(-study_num)  


fig_moderators_ppt <- ggplot() +
  geom_point(data = fitted_ppt,
             aes(x = mean_annual_ppt_mm, y = yi, colour = study_ID),
             size = 2, alpha = 1) +
  
  geom_ribbon(data = fitted_ppt,
              aes(x = mean_annual_ppt_mm,
                  ymin = (Q2.5),
                  ymax = (Q97.5)),
              alpha = 0.2) +
  #  fixed effect
  geom_line(data = fitted_ppt,
            aes(x = mean_annual_ppt_mm, y = (Estimate)),
            linewidth = 1,
            colour = "black") + 
  
  labs(x = 'Mean Annual Precipitation (mm)',
       y = 'Log Response Ratio') +
  
  scale_colour_viridis_d(option = "plasma")+
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

fig_moderators_ppt
# ggsave("Supplemetary Fig. 2 - mean annual precipitation.pdf", fig_moderators_ppt, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')

# Supplemetary Fig. 3 - mean annual temperature ----------------------------------------
mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds")) 
mod_temp %>% summary

effects <- effects %>%
  mutate(study_ID = if_else(
    study_ID == "ID_239" & mean_annual_temperature_Celsius == 6.8,
    "ID_239_cold",
    study_ID
  ))

#effects$study_ID [(effects$study_ID == "ID_239" & effects$temp_pad == 0.85 )] <- "ID_239_cold"

fitted_temp <- mod_temp$data %>%
  bind_cols(fitted(mod_temp, re_formula = NA, scale = 'linear')) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_temp$data), names(effects)),
    relationship = "many-to-many"
  ) %>%
  mutate(study_ID = if_else(study_ID == "ID_239_cold", "ID_239", study_ID)) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(study_num) %>%
  mutate(study_ID = factor(study_ID, levels = unique(study_ID))) %>%
  select(-study_num)  


fig_moderators_temp <- ggplot() +
  geom_point(data = fitted_temp,
             aes(x = mean_annual_temperature_Celsius, y = yi,
                 colour = study_ID),
             size = 2, alpha = 1) +
  
  geom_ribbon(data = fitted_temp,
              aes(x = mean_annual_temperature_Celsius,
                  ymin = (Q2.5),
                  ymax = (Q97.5)),
              alpha = 0.2) +
  #  fixed effect
  geom_line(data = fitted_temp,
            aes(x = mean_annual_temperature_Celsius, y = (Estimate)),
            linewidth = 1,
            colour = "black") +
  
  labs(x = 'Mean Annual Temperature (ºC)',
       y = 'Log Response Ratio') +
  
  scale_colour_viridis_d(option = "plasma") +
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

fig_moderators_temp
# ggsave("Supplemetary Fig. 3 - mean annual temperature.pdf", fig_moderators_temp, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')

# Supplemetary Fig. 4 - abs(latitude) ------------------------------------------------

mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds")) 
mod_lat %>% summary

fitted_lat <- mod_lat$data %>%
bind_cols(fitted(mod_lat, re_formula = NA, scale = 'linear')) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = 'study_ID',
    relationship = "many-to-many"
  ) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(study_num) %>%
  mutate(study_ID = factor(study_ID, levels = unique(study_ID))) %>%
  select(-study_num)  


fig_moderators_lat <- ggplot() +
  geom_point(data = fitted_lat,
             aes(x = latitude_abs, y = yi,
                 colour = study_ID),
             size = 2, alpha = 1) +
  
  geom_ribbon(data = fitted_lat,
              aes(x = latitude_abs,
                  ymin = (Q2.5),
                  ymax = (Q97.5)),
              alpha = 0.2) +
  #  fixed effect
  geom_line(data = fitted_lat,
            aes(x = latitude_abs, y = (Estimate)),
            linewidth = 1,
            colour = "black") +
  
  coord_cartesian(ylim = c(-1.05, .5)) +
  
  labs(x = 'Absolute Latitude',
       y = 'Log Response Ratio') +
  
  scale_colour_viridis_d(option = "plasma") +
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

fig_moderators_lat
# ggsave("Supplemetary Fig. 4 - abs(latitude).pdf", fig_moderators_lat, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')




# Supplemetary Fig. 5 - response_variable ---------------------------------------
mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds")) 

mod_resp %>% summary


fitted_response_variable <- mod_resp$data %>%
  bind_cols(fitted(mod_resp, re_formula = NA, scale = 'linear')) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_resp$data), names(effects)),
    relationship = "many-to-many"
  ) %>%
  
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(study_num) %>%
  mutate(study_ID = factor(study_ID, levels = unique(study_ID))) %>%
  select(-study_num)  



fig_mod_resp <- ggplot() +
  geom_boxplot(data = fitted_response_variable,
               aes(x = response_variable, y = yi)) +
  
  labs(x = 'Response variable',
       y = 'Log Response Ratio') +
  
  scale_colour_viridis_d(option = "plasma") +
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) 

fig_moderators_resp_var <-  
  fig_mod_resp + geom_jitter(data = fitted_response_variable, 
                             aes(x = response_variable, y = yi, colour = study_ID), 
                             width = 0.3,  # Adjust the horizontal jitter
                             size = 2)    

fig_moderators_resp_var
# ggsave("Supplemetary Fig. 5 - response_variable.pdf", fig_moderators_resp_var, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')




# Supplemetary Fig. 6 - Experiment duration  ----------------------------------------
mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds")) 

fitted_time <- mod_time$data %>%
  bind_cols(fitted(mod_time, re_formula = NA, scale = 'linear')) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_time$data), names(effects)),
    relationship = "many-to-many"
  ) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(study_num) %>%
  mutate(study_ID = factor(study_ID, levels = unique(study_ID))) %>%
  select(-study_num)  


fig_moderators_time <- ggplot() +
  geom_point(data = fitted_time,
             aes(x = experiment_duration, y = yi,
                 colour = study_ID),
             size = 2, alpha = 1) +
  
  geom_ribbon(data = fitted_time,
              aes(x = experiment_duration,
                  ymin = (Q2.5),
                  ymax = (Q97.5)),
              alpha = 0.2) +
  #  fixed effect
  geom_line(data = fitted_time,
            aes(x = experiment_duration, y = (Estimate)),
            linewidth = 1,
            colour = "black") +
  
  scale_x_continuous(limits = c(.5, 22), breaks = c(1, 5, 10, 15, 21)) +
  
  labs(x = 'Experiment duration',
       y = 'Log Response Ratio') +
  
  scale_colour_viridis_d(option = "plasma") +
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

fig_moderators_time
# ggsave("Supplemetary Fig. 6 - Experiment duration.pdf", fig_moderators_time, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')




