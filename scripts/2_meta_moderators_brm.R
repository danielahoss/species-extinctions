# -----------------------------------------------------------
# Meta-Analysis Moderators
# -----------------------------------------------------------

# --- 1. Load Libraries and Set Up Environment ---
library(tidyverse)
library(brms) 
library(tidybayes)
library(ggdist)
require(viridis)

# Clear workspace 
rm(list = ls())

# --- 2. Load and Preprocess Data ---
effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = T) 

# standardizing variables
moderators_df_pad <- effects_brm %>%
  mutate(
    lat_pad = round(as.numeric(scale(as.numeric(latitude_abs), center = TRUE, scale = FALSE)),2),
    lon_pad = round(as.numeric(scale(as.numeric(longitude), center = TRUE, scale = FALSE)),2),
    ppt_pad = round(as.numeric(scale(as.numeric(mean_annual_ppt_mm), center = TRUE, scale = FALSE)),2),
    temp_pad = round(as.numeric(scale(as.numeric(mean_annual_temperature_Celsius), center = TRUE, scale = FALSE)),2),
    alt_pad = round(as.numeric(scale(as.numeric(altitude_m), center = TRUE, scale = FALSE)),0),
    time_pad = as.numeric(scale(as.numeric(experiment_duration), center = TRUE, scale = FALSE)))
 

df_pad <- effects_brm %>%
  select(study_ID,                        
         experiment_duration,            
         country,                        
         latitude,                        
         latitude_abs,                   
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
         n_control,                       
         n_removal,                      
         se_eff_size_ctrl,                
         se_eff_size_rem)  



# Model_mean annual ppt -----------------------------------------

# mod_ppt <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + ppt_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       control = list(max_treedepth = 14),
#       cores = 4, chains = 4, file = "ma_moderators_ppt")
mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds")) 

mod_ppt %>% summary
pp_check(mod_ppt) 


# plot Model_mean annual ppt ------------------------------------

fitted_ppt <- cbind(mod_ppt$data,
                    fitted(mod_ppt, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))


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
            colour = "black") + # coord_cartesian(ylim = c(-1, 1), xlim = c(min(fitted_ppt$mean_annual_ppt_mm), max(fitted_ppt$mean_annual_ppt_mm)))+
                  
  labs(x = 'Mean Annual Precipitation (mm)',
       y = expression(paste('Log Response Ratio'))) +
  
 scale_colour_viridis_d(option = "plasma")+

  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        #legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

fig_moderators_ppt
# ggsave("fig_moderators_ppt.pdf", fig_moderators_ppt, path = ("figures/ExtendedData"), 
#        width = 200, height = 200, units = 'mm')

# Model_mean annual temperature ---------------------------------

# mod_temp <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + temp_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_temp")
mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds")) 

mod_temp %>% summary

mod_temp %>% pp_check


# plot Model_mean annual temperature ----------------------------

fitted_temp <- cbind(mod_temp$data,
                     fitted(mod_temp, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               #filter(!is.na(mean_annual_temperature_Celsius)) %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))

fig_moderators_temp <- ggplot() +
  geom_point(data = fitted_temp,
             aes(x = mean_annual_temperature_Celsius, y = yi,
                 colour = study_ID),
             size = 2, alpha = 1) +
  
  geom_ribbon(data = fitted_temp,
              aes(x = temp_pad,
                  ymin = (Q2.5),
                  ymax = (Q97.5)),
              alpha = 0.2) +
  #  fixed effect
  geom_line(data = fitted_temp,
            aes(x = temp_pad, y = (Estimate)),
            linewidth = 1,
            colour = "black") +
  
  labs(x = 'Mean Annual Temperature (ºC)',
       y = expression(paste('Log Response Ratio'))) +
  
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
# ggsave("fig_moderators_temp.pdf", fig_moderators_temp, path = ("figures/ExtendedData"), 
#        width = 200, height = 200, units = 'mm')




# Model_time_pad ------------------------------------------------

# mod_time <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + time_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4, file = "ma_moderators_time")
mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds")) 

mod_time %>% summary


pp_check(mod_time) 


# plot Model_time_pad -------------------------------------------

fitted_time <- cbind(mod_time$data, 
                     fitted(mod_time, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))

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
  
  scale_x_continuous(limits = c(.5, 9.5), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
 
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
# ggsave("fig_moderators_time.pdf", fig_moderators_time, path = ("figures/ExtendedData"), 
#        width = 200, height = 200, units = 'mm')


# Model_latitude ------------------------------------------------


# mod_lat <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + lat_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_lat")

mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds")) 
mod_lat %>% summary


pp_check(mod_lat) 
conditional_effects(mod_lat)



# plot Model_latitude -------------------------------------------


fitted_lat <- cbind(mod_lat$data, 
                    fitted(mod_lat, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))


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
# ggsave("fig_moderators_lat.pdf", fig_moderators_lat, path = ("figures/ExtendedData"), 
#        width = 200, height = 200, units = 'mm')



# Model_response_variable ---------------------------------------

# mod_resp <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 0 + response_variable + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_response_variable")
mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds")) 

mod_resp %>% summary

conditional_effects(mod_resp)
pp_check(mod_resp) 


# plot Model_response_variable ----------------------------------


fitted_response_variable <- cbind(mod_resp$data, 
                                  fitted(mod_resp, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))

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
# ggsave("fig_moderators_resp_var.pdf", fig_moderators_resp_var, path = ("figures/ExtendedData"), 
#         width = 200, height = 200, units = 'mm') 




# Model_effect_type ---------------------------------------------



# mod_effect_type <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 0 + effect_type + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_effect_type")
mod_effect_type <- read_rds(here::here("model_output", "ma_moderators_effect_type.rds")) 

mod_effect_type %>% summary

conditional_effects(mod_effect_type)

pp_check(mod_effect_type) 



# plot Model_effect_type ----------------------------------------

fitted_effect_type <- cbind(mod_effect_type$data,
                            fitted(mod_effect_type, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  inner_join(df_pad %>% 
               distinct(study_ID, .keep_all = TRUE), 
             relationship = "many-to-many",
             by = c('study_ID'))

fig_mod_effect_type <- ggplot() +
  geom_boxplot(data = fitted_effect_type,
               aes(x = effect_type, y = yi)) +
  
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

fig_moderators_effect_type <- fig_mod_effect_type + geom_jitter(data = fitted_effect_type, 
                 aes(x = effect_type, y = yi, colour = study_ID), 
                 width = 0.3,  # Adjust the horizontal jitter for plotting visibility
                 size = 2)     

fig_moderators_effect_type
# ggsave("fig_moderators_effect_type.pdf", fig_moderators_effect_type, path = ("figures/ExtendedData"), 
#         width = 200, height = 200, units = 'mm')

