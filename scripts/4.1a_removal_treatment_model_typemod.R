# Removal-treatment model with measurement type as a predictor — Fig 3
# ---------------------------------------------------------------------------
# The removal-treatment model (removed_propo x removal_trt interaction) with
# measurement type added as a predictor of both the mean and the variance:
#   productivity ~ 0 + removed_propo + removal_trt + removed_propo:removal_trt
#                    + measure
#                    + (0 + removed_propo + removal_trt + removed_propo:removal_trt
#                         | study_ID/block/plot)
#                    + (1 | time_length_years),
#   sigma ~ measure ,  lognormal()
#
# Fully separate biomass/cover models are not feasible here because some
# treatment x response-type cells contain a single study (e.g. cover x
# subordinate = 1). Dominant is the reference treatment level.
#
# Output: model_output/model_remtrt_typemod.rds
# Plot:   scripts/4.2_removal_treatment_plot_typemod.R

library(tidyverse)
library(brms)
library(here)

seed <- 321

df_biom <- read.csv(here("data", "df_biomass_trt.csv"), header = TRUE) %>%
  mutate(block = as.character(block), productivity = biomass, measure = "biomass")
df_cover <- read.csv(here("data", "df_cover_trt.csv"), header = TRUE) %>%
  mutate(block = as.character(block), productivity = cover, measure = "cover")

df <- bind_rows(df_biom, df_cover) %>%
  mutate(removal_trt = factor(removal_trt),
         measure     = factor(measure, levels = c("biomass", "cover")))

set.seed(seed)
model_remtrt_typemod <- brm(
  bf(productivity ~ 0 + removed_propo + removal_trt + removed_propo:removal_trt + measure +
       (0 + removed_propo + removal_trt + removed_propo:removal_trt |
          study_ID / block / plot) +
       (1 | time_length_years),
     sigma ~ measure),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 8000, warmup = 3000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file    = here("model_output", "model_remtrt_typemod"),
  backend = "cmdstanr"
)

model_remtrt_typemod %>% summary()
model_remtrt_typemod %>% pp_check()
