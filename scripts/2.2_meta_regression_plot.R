# Meta-Analysis Moderators
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(viridis)
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
  select(ES_ij,
         study_ID,
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
         yi,
         eff_size_ctrl,
         eff_size_rem,
         sd_eff_size_ctrl,
         sd_eff_size_rem,
         removal_method_category,
         n_control,
         n_removal,
         se_eff_size_ctrl,
         se_eff_size_rem)

# Shared theme ------------------------------------------------------------

theme_mods <- theme(
  legend.background     = element_rect(fill = "white"),
  legend.box.background = element_rect(fill = "white"),
  legend.position       = "none",
  panel.background      = element_rect(fill = "white"),
  panel.grid.major      = element_blank(),
  panel.grid.minor      = element_blank(),
  axis.text             = element_text(colour = "black"),
  axis.line             = element_line(colour = "black"),
  axis.title            = element_text(colour = "black"),
  axis.ticks            = element_line(colour = "black"),
  plot.background       = element_rect(fill = "white", color = NA)
)

# Supplementary Fig. 3 — Mean annual precipitation -----------------------

mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds"))

fitted_ppt <- mod_ppt$data %>%
  bind_cols(fitted(mod_ppt, re_formula = NA, scale = "linear")) %>%
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
  arrange(mean_annual_ppt_mm) %>%      
  mutate(study_ID = factor(study_ID,
                           levels = unique(study_ID[order(study_num)]))) %>%
  select(-study_num)

fig_moderators_ppt <- ggplot() +
  geom_ribbon(data = fitted_ppt,
              aes(x = mean_annual_ppt_mm, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.2) +
  geom_line(data = fitted_ppt,
            aes(x = mean_annual_ppt_mm, y = Estimate),
            linewidth = 1, colour = "black") +
  geom_point(data = fitted_ppt,
             aes(x = mean_annual_ppt_mm, y = yi, colour = study_ID),
             size = 2, alpha = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Mean Annual Precipitation (mm)", y = "Log Response Ratio") +
  scale_colour_viridis_d(option = "plasma") +
  theme_mods

fig_moderators_ppt
ggsave("SupplementaryFigure_03_precipitation.pdf", 
       fig_moderators_ppt,
       path = here::here("figures"), 
       width = 200, height = 200, units = "mm", device = cairo_pdf)


# Supplementary Fig. 2 — Mean annual temperature -------------------------

mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds"))
mod_temp %>% summary

# ID_239 has two sites at different temperatures; distinguish them for the model
effects_temp <- effects %>%
  mutate(study_ID = if_else(
    study_ID == "ID_239" & mean_annual_temperature_Celsius == 6.8,
    "ID_239_cold",
    study_ID
  ))

fitted_temp <- mod_temp$data %>%
  bind_cols(fitted(mod_temp, re_formula = NA, scale = "linear")) %>%
  as_tibble() %>%
  inner_join(
    effects_temp %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_temp$data), names(effects_temp)),
    relationship = "many-to-many"
  ) %>%
  mutate(study_ID = if_else(study_ID == "ID_239_cold", "ID_239", study_ID)) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(mean_annual_temperature_Celsius) %>%   
  mutate(study_ID = factor(study_ID, levels = unique(study_ID[order(study_num)]))) %>%
  select(-study_num)

fig_moderators_temp <- ggplot() +
  geom_ribbon(data = fitted_temp,
              aes(x = mean_annual_temperature_Celsius, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.2) +
  geom_line(data = fitted_temp,
            aes(x = mean_annual_temperature_Celsius, y = Estimate),
            linewidth = 1, colour = "black") +
  geom_point(data = fitted_temp,
             aes(x = mean_annual_temperature_Celsius, y = yi, colour = study_ID),
             size = 2, alpha = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Mean Annual Temperature (\u00b0C)", y = "Log Response Ratio") +
  scale_colour_viridis_d(option = "plasma") +
  theme_mods

fig_moderators_temp
ggsave("SupplementaryFigure_02_temperature.pdf",
       fig_moderators_temp,
       path = here::here("figures"), 
       width = 200, height = 200, units = "mm", device = cairo_pdf)


# Supplementary Fig. 4 — Absolute latitude --------------------------------

mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds"))
mod_lat %>% summary

fitted_lat <- mod_lat$data %>%
  bind_cols(fitted(mod_lat, re_formula = NA, scale = "linear")) %>%
  as_tibble() %>%
  inner_join(
    effects %>% distinct(study_ID, ES_ij, .keep_all = TRUE),
    by = intersect(names(mod_lat$data), names(effects)),
    relationship = "many-to-many"
  ) %>%
  mutate(
    study_ID = as.character(study_ID),
    study_num = as.numeric(gsub("ID_", "", study_ID))
  ) %>%
  arrange(latitude_abs) %>%            
  mutate(study_ID = factor(study_ID, levels = unique(study_ID[order(study_num)]))) %>%
  select(-study_num)

fig_moderators_lat <- ggplot() +
  geom_ribbon(data = fitted_lat,
              aes(x = latitude_abs, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.2) +
  geom_line(data = fitted_lat,
            aes(x = latitude_abs, y = Estimate),
            linewidth = 1, colour = "black") +
  geom_point(data = fitted_lat,
             aes(x = latitude_abs, y = yi, colour = study_ID),
             size = 2, alpha = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Absolute Latitude (\u00b0)", y = "Log Response Ratio") +
  scale_colour_viridis_d(option = "plasma") +
  theme_mods

fig_moderators_lat
ggsave("SupplementaryFigure_04_latitude.pdf", fig_moderators_lat,
       path = here::here("figures"), width = 200, height = 200, units = "mm", device = cairo_pdf)

# Supplementary Fig. 5 — Experiment duration ------------------------------

mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds"))

fitted_time <- mod_time$data %>%
  bind_cols(fitted(mod_time, re_formula = NA, scale = "linear")) %>%
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
  arrange(experiment_duration) %>%      # sort by x for smooth ribbon/line
  mutate(study_ID = factor(study_ID, levels = unique(study_ID[order(study_num)]))) %>%
  select(-study_num)

fig_moderators_time <- ggplot() +
  geom_ribbon(data = fitted_time,
              aes(x = experiment_duration, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.2) +
  geom_line(data = fitted_time,
            aes(x = experiment_duration, y = Estimate),
            linewidth = 1, colour = "black") +
  geom_point(data = fitted_time,
             aes(x = experiment_duration, y = yi, colour = study_ID),
             size = 2, alpha = 1) +
  scale_x_continuous(limits = c(.5, 22), breaks = c(1, 5, 10, 15, 21)) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Experiment duration (years)", y = "Log Response Ratio") +
  scale_colour_viridis_d(option = "plasma") +
  theme_mods

fig_moderators_time
ggsave("SupplementaryFigure_05_duration.pdf", fig_moderators_time,
       path = here::here("figures"), width = 200, height = 200, units = "mm", device = cairo_pdf)

# Supplementary Fig. 6 — Response variable --------------------------------

mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds"))
mod_resp %>% summary

fitted_response_variable <- mod_resp$data %>%
  bind_cols(fitted(mod_resp, re_formula = NA, scale = "linear")) %>%
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

# Population-level predictions per response variable category
# sei must be supplied (required by the se() term); value does not affect the
# population-level mean since re_formula = NA and scale = "linear"
newdata_resp <- data.frame(
  response_variable = c("biomass", "cover"),
  sei = mean(mod_resp$data$sei)
)
pred_resp <- fitted(mod_resp, newdata = newdata_resp, re_formula = NA, scale = "linear") %>%
  as_tibble() %>%
  bind_cols(newdata_resp)

fig_moderators_resp_var <- ggplot() +
  geom_jitter(data = fitted_response_variable,
              aes(x = response_variable, y = yi, colour = study_ID),
              width = 0.2, size = 2, alpha = 0.7) +
  geom_pointrange(data = pred_resp,
                  aes(x = response_variable, y = Estimate,
                      ymin = Q2.5, ymax = Q97.5),
                  linewidth = 1, size = 0.6, colour = "black") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Response variable", y = "Log Response Ratio") +
  scale_colour_viridis_d(option = "plasma") +
  theme_mods

fig_moderators_resp_var
ggsave("SupplementaryFigure_06_response_variable.pdf", fig_moderators_resp_var,
       path = here::here("figures"), width = 200, height = 200, units = "mm", device = cairo_pdf)


