# Meta-Analysis Moderators 
# model and model checks


# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(brms) 
library(tidybayes)
library(ggdist)

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

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




# 3. Model_mean annual ppt ------------------------------------------------


# mod_ppt <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + ppt_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       control = list(max_treedepth = 14),
#       cores = 4, chains = 4, file = "ma_moderators_ppt")
mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds")) 

mod_ppt %>% summary
pp_check(mod_ppt) 



# 4. Model_mean annual temperature ---------------------------------

# mod_temp <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + temp_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_temp")
mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds")) 

mod_temp %>% summary

mod_temp %>% pp_check



# 5. Model_time_pad ------------------------------------------------

# mod_time <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + time_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4, file = "ma_moderators_time")
mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds")) 

mod_time %>% summary


pp_check(mod_time) 



# 6. Model_latitude ------------------------------------------------


# mod_lat <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 1 + lat_pad + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_lat")

mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds")) 
mod_lat %>% summary


pp_check(mod_lat) 
conditional_effects(mod_lat)




# 7. Model_response_variable ---------------------------------------

# mod_resp <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 0 + response_variable + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_response_variable")
mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds")) 

mod_resp %>% summary

conditional_effects(mod_resp)
pp_check(mod_resp) 



# 8. Model_effect_type ---------------------------------------------



# mod_effect_type <-
#   brm(data = moderators_df_pad,
#       yi | se(sei) ~ 0 + effect_type + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4 , file = "ma_moderators_effect_type")
mod_effect_type <- read_rds(here::here("model_output", "ma_moderators_effect_type.rds")) 

mod_effect_type %>% summary

conditional_effects(mod_effect_type)

pp_check(mod_effect_type) 



