# Climate interaction models
# --------------------------------------------------------------------------
# productivity ~ removed_propo * climate + (removed_propo | study_ID/block/plot)
#                                         + (1 | time_length_years), lognormal()
# Temperature and precipitation are tested in separate models, for biomass and
# cover.
#
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

df_biom <- df_biom %>%
  filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction",
                                      "abundance_richness_reduction")))

# Climate predictors as z-scores
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

# 3. Fit models ---------------------------------------------------------------

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
    iter    = 7000, warmup = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 14),
    file    = here::here("model_output", tag),
    backend = "cmdstanr"
  )
}

int_biom_temp  <- fit_interaction(df_biom,  "temp_z", "int_biom_temp")
int_biom_ppt   <- fit_interaction(df_biom,  "ppt_z",  "int_biom_ppt")
int_cover_temp <- fit_interaction(df_cover, "temp_z", "int_cover_temp")
int_cover_ppt  <- fit_interaction(df_cover, "ppt_z",  "int_cover_ppt")

# 4. Interaction terms --------------------------------------------------------

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
  interaction_summary(int_cover_ppt,  "ppt_z",  "cover")
)

print(as.data.frame(results))

# 5. Diagnostics --------------------------------------------------------------

for (m in list(int_biom_temp, int_biom_ppt, int_cover_temp, int_cover_ppt)) {
  print(summary(m))
  print(pp_check(m))
}

