# Meta-Analysis Moderators 
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(DHARMa)
library(here)

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = T)

effects <- effects_brm %>% 
  ungroup() %>% 
  group_by(study_ID) %>% 
  mutate(ES_ij = 1:n()) %>% 
  ungroup() %>%
  mutate(
    lat_pad = round(as.numeric(scale(as.numeric(latitude_abs), center = TRUE, scale = FALSE)),2),
    ppt_pad = round(as.numeric(scale(as.numeric(mean_annual_ppt_mm), center = TRUE, scale = FALSE)),2),
    temp_pad = round(as.numeric(scale(as.numeric(mean_annual_temperature_Celsius), center = TRUE, scale = FALSE)),2),
    alt_pad = round(as.numeric(scale(as.numeric(altitude_m), center = TRUE, scale = FALSE)),0),
    time_pad = as.numeric(scale(as.numeric(experiment_duration), center = TRUE, scale = FALSE)))


# Supplementary Fig. 2 - mean annual precipitation ------------------------

mod_ppt <- brm(
  data    = effects,
  formula = yi | se(sei) ~ 1 + ppt_pad + (1 | study_ID/ES_ij),
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_ppt")
)

mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds")) 

mod_ppt %>% summary

pp_check(mod_ppt) 
conditional_effects(mod_ppt)


model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_ppt)),
  observedResponse = effects$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_ppt)), 1, mean),
  integerResponse = FALSE)


plot(model.check)  # qq and residuals

testDispersion(model.check)


# Supplementary Fig. 3 - mean annual temperature --------------------------


effects_temp <- effects
effects_temp$study_ID [(effects_temp$study_ID == "ID_239" & effects_temp$temp_pad == 0.85 )] <- "ID_239_cold"


mod_temp <- brm(
  data    = effects_temp,
  formula = yi | se(sei) ~ 1 + temp_pad + (1 | study_ID/ES_ij),
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_temp")
)
mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds")) 

mod_temp %>% summary

mod_temp %>% pp_check

mod_temp %>% conditional_effects()


# mod_temp was fitted on effects_temp (with ID_239 split) and drops NA temp_pad rows.
# Use mod_temp$data$yi to ensure row alignment.
model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(mod_temp)),
  observedResponse        = mod_temp$data$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_temp)), 1, mean),
  integerResponse         = FALSE
)


plot(model.check)  # qq and residuals

testDispersion(model.check)

# Supplementary Fig. 4 - abs(latitude) ------------------------------------


mod_lat <- brm(
  data    = effects,
  formula = yi | se(sei) ~ 1 + lat_pad + (1 | study_ID/ES_ij),
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_lat")
)

mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds")) 

mod_lat %>% summary

pp_check(mod_lat) 
conditional_effects(mod_lat)

model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_lat)),
  observedResponse = effects$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_lat)), 1, mean),
  integerResponse = FALSE)


plot(model.check)  # qq and residuals

testDispersion(model.check)

# Supplementary Fig. 5 - Experiment duration ------------------------------

mod_time <- brm(
  data    = effects,
  formula = yi | se(sei) ~ 1 + time_pad + (1 | study_ID/ES_ij),
  iter    = 4000, warmup = 1000,
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_time")
)
mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds")) 

mod_time %>% summary
pp_check(mod_time) 
mod_time %>% conditional_effects()

model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_time)),
  observedResponse = effects$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_time)), 1, mean),
  integerResponse = FALSE)


plot(model.check)  # qq and residuals

testDispersion(model.check)



# Supplementary Fig. 6 - response_variable --------------------------------


mod_resp <- brm(
  data    = effects,
  formula = yi | se(sei) ~ 0 + response_variable + (1 | study_ID/ES_ij),
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_response_variable")
)
mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds")) 

mod_resp %>% summary
pp_check(mod_resp) 
conditional_effects(mod_resp)

model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_resp)),
  observedResponse = effects$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_resp)), 1, mean),
  integerResponse = FALSE)


plot(model.check)  # qq and residuals

testDispersion(model.check)



# covariables  ----------------------------------------------------------


mod_covars <- brm(
  data    = effects,
  formula = yi | se(sei) ~ 1 + lat_pad + ppt_pad + temp_pad + (1 | study_ID/ES_ij),
  cores   = 4, chains = 4,
  backend = "cmdstanr",
  seed    = 321,
  file    = here("model_output", "ma_moderators_covariates")
)
mod_covars <- read_rds(here::here("model_output", "ma_moderators_covariates.rds")) 
mod_covars %>% summary
mod_covars %>% pp_check()

mod_covars %>% conditional_effects()


# mod_covars drops rows with NA temp_pad automatically; use mod_covars$data
# to ensure observedResponse matches the rows the model actually used.
model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(mod_covars)),
  observedResponse        = mod_covars$data$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_covars)), 1, mean),
  integerResponse         = FALSE
)


plot(model.check)  # qq and residuals

testDispersion(model.check)


