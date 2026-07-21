# Climate interaction robustness check (Reviewer #3, Point 3)
# --------------------------------------------------------------------------
# Tests whether the effect of species removal on productivity depends on
# climate, by adding a removed_propo x climate interaction to the
# productivity model. This is the minimal analysis R3 requested as a fallback
# to full random climate slopes ("at least test for an interaction between
# climate and the proportion of species removed").
#
# Design decisions:
#   - Biomass and cover are modelled SEPARATELY (units differ; matches the
#     separate models reported in Supplementary Table 1), so this doubles as a
#     response to the pooling concern.
#   - Temperature and precipitation are tested in SEPARATE models (as with the
#     meta-analysis moderators), so that studies missing one climate variable
#     do not drop out of the test for the other.
#   - Base structure mirrors scripts/3.1_productivity_model.R:
#       lognormal, (removed_propo | study_ID/block/plot) + (1 | time_length_years)
#   - A non-significant interaction (95% CrI spanning 0) supports the
#     conclusion that the removal effect is consistent across the climate
#     gradient.
# Output: model_output/int_{biom,cover}_{temp,ppt}.rds

# 1. Libraries ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(DHARMa)
library(here)

rm(list = ls())

seed <- 321

# 2. Load and prepare data ----------------------------------------------------

df_biom  <- read.csv(here::here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here::here("data", "df_cover_brm_2.csv"), header = TRUE)

# Same exclusion as the productivity model: abundance-only manipulations from
# ID_29 (removed_propo is not meaningful for those treatments).
df_biom <- df_biom %>%
  filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction",
                                      "abundance_richness_reduction")))

# Centre and scale climate predictors (z-scores) for numerical stability and
# so interaction coefficients are interpretable per standard deviation.
prep <- function(d, resp) {
  d %>%
    mutate(
      block        = as.character(block),
      productivity = .data[[resp]],
      temp_z       = as.numeric(scale(mean_annual_temperature_Celsius)),
      ppt_z        = as.numeric(scale(mean_annual_ppt_mm))
    )
}

df_biom  <- prep(df_biom,  "biomass")
df_cover <- prep(df_cover, "cover")

# Pooled biomass + cover dataset (mirrors the main productivity model in 3.1).
# Climate is z-scored on the pooled data so the scaling is common to both.
df_pool <- bind_rows(
  df_biom  %>% select(study_ID, block, plot, time_length_years, removed_propo,
                      productivity, mean_annual_temperature_Celsius, mean_annual_ppt_mm),
  df_cover %>% select(study_ID, block, plot, time_length_years, removed_propo,
                      productivity, mean_annual_temperature_Celsius, mean_annual_ppt_mm)
) %>%
  mutate(
    temp_z = as.numeric(scale(mean_annual_temperature_Celsius)),
    ppt_z  = as.numeric(scale(mean_annual_ppt_mm))
  )

# 3. Fit models ---------------------------------------------------------------
# Helper keeps the base structure identical across the four fits and only
# swaps the response data and the climate moderator.

fit_interaction <- function(data, climate, tag) {
  data <- data %>% filter(!is.na(.data[[climate]]))

  form <- bf(as.formula(paste0(
    "productivity ~ removed_propo * ", climate,
    " + (removed_propo | study_ID / block / plot) + (1 | time_length_years)"
  )))

  brm(
    form,
    family  = lognormal(),
    data    = data,
    seed    = seed,
    cores   = 4, chains = 4,
    iter    = 6000, warmup = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 14),
    file    = here::here("model_output", tag),
    backend = "cmdstanr"
  )
}

int_biom_temp  <- fit_interaction(df_biom,  "temp_z", "int_biom_temp")
int_biom_ppt   <- fit_interaction(df_biom,  "ppt_z",  "int_biom_ppt")
int_cover_temp <- fit_interaction(df_cover, "temp_z", "int_cover_temp")
int_cover_ppt  <- fit_interaction(df_cover, "ppt_z",  "int_cover_ppt")
int_pool_temp  <- fit_interaction(df_pool,  "temp_z", "int_pool_temp")
int_pool_ppt   <- fit_interaction(df_pool,  "ppt_z",  "int_pool_ppt")

# 4. Summarise the interaction terms -----------------------------------------
# The row of interest is the removed_propo:climate interaction. If its 95% CrI
# spans zero, climate does not significantly moderate the removal effect.

interaction_summary <- function(model, climate, response) {
  fx  <- fixef(model)
  row <- grep(paste0("removed_propo:", climate), rownames(fx), value = TRUE)
  tibble(
    response    = response,
    moderator   = climate,
    term        = row,
    estimate    = round(fx[row, "Estimate"], 3),
    l95         = round(fx[row, "Q2.5"], 3),
    u95         = round(fx[row, "Q97.5"], 3),
    crosses_0   = fx[row, "Q2.5"] < 0 & fx[row, "Q97.5"] > 0
  )
}

results <- bind_rows(
  interaction_summary(int_biom_temp,  "temp_z", "biomass"),
  interaction_summary(int_biom_ppt,   "ppt_z",  "biomass"),
  interaction_summary(int_cover_temp, "temp_z", "cover"),
  interaction_summary(int_cover_ppt,  "ppt_z",  "cover"),
  interaction_summary(int_pool_temp,  "temp_z", "pooled"),
  interaction_summary(int_pool_ppt,   "ppt_z",  "pooled")
)

print(as.data.frame(results))

# 5. Diagnostics --------------------------------------------------------------
# Run per model as needed (convergence + posterior predictive check).

for (m in list(int_biom_temp, int_biom_ppt, int_cover_temp, int_cover_ppt,
               int_pool_temp, int_pool_ppt)) {
  print(summary(m))
  print(pp_check(m))
}
