# Removal-treatment model with measurement type as a predictor (Path A) — Fig 3
# ---------------------------------------------------------------------------
# Round 3 revision. Takes the LOO-selected removal-treatment model
# (scripts/4.1, fit3_inter: removed_propo x removal_trt interaction) and adds
# measurement type as a predictor of BOTH the mean and the variance:
#   productivity ~ 0 + removed_propo + removal_trt + removed_propo:removal_trt
#                    + measure
#                    + (0 + removed_propo + removal_trt + removed_propo:removal_trt
#                         | study_ID/block/plot)
#                    + (1 | time_length_years),
#   sigma ~ measure ,  lognormal()
#
# Model SELECTION stays on the combined data (a question about structure, which
# the units concern does not affect); adding measure on mean + sigma addresses
# the units concern (reviewers' route c). Fully separate models are infeasible
# here because some treatment x response-type cells contain a single study
# (cover x subordinate = 1). The treatment conclusions are unchanged: dominant
# removal drives the largest, clearly credible decline (-1.97 [-2.67, -1.31];
# ~-63% at 50% removal), with shallower, more uncertain effects for traits and
# subordinate removals.
#
# Output: model_output/model_remtrt_typemod.rds
# Plot:   scripts/4.2c_removal_treatment_plot_typemod.R

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

# slopes per treatment (dominant is the reference level)
emtrends(model_remtrt_typemod, ~ removal_trt, var = "removed_propo")
