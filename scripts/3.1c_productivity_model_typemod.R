# Type-moderator productivity model (Path A) — canonical Fig 2 model
# ---------------------------------------------------------------------------
# Round 3 revision (Reviewer #2 Comment 2.2 / Reviewer #3 Comment 3.2).
# Rather than pooling biomass and cover blindly (scripts/3.1) or fitting them
# fully separately (scripts/3.1b), we include measurement type as a moderator
# of BOTH the mean and the variance of productivity — the reviewers' route (c):
#   productivity ~ removed_propo * measure + (removed_propo | study_ID/block/plot)
#                                           + (1 | time_length_years),
#   sigma ~ measure ,  lognormal()
# The removed_propo:measure term is a formal in-model test of whether the
# productivity response to species removal differs by measurement type
# (it does not: 0.16, 95% CrI -0.88 to 1.26), and sigma ~ measure gives each
# response type its own residual dispersion (biomass 0.27 vs cover 0.33).
# This approach is used uniformly for Fig 2 and Fig 3 (removal treatments),
# where fully separate models are infeasible (cover x subordinate = 1 study).
#
# Output: model_output/model_removed_propo_typemod.rds
# Plot:   scripts/3.2c_productivity_plot_typemod.R

library(tidyverse)
library(brms)
library(here)

seed <- 321

# 1. Data ---------------------------------------------------------------------

df_biom <- read.csv(here("data", "df_biom_brm_2.csv"), header = TRUE) %>%
  # same ID_29 exclusion as the other productivity models
  filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction",
                                      "abundance_richness_reduction"))) %>%
  transmute(study_ID, block = as.character(block), plot, time_length_years,
            removed_propo, productivity = biomass, measure = "biomass")

df_cover <- read.csv(here("data", "df_cover_brm_2.csv"), header = TRUE) %>%
  transmute(study_ID, block = as.character(block), plot, time_length_years,
            removed_propo, productivity = cover, measure = "cover")

df <- bind_rows(df_biom, df_cover) %>%
  mutate(measure = factor(measure, levels = c("biomass", "cover")))

# 2. Fit ----------------------------------------------------------------------

set.seed(seed)
model_typemod <- brm(
  bf(productivity ~ removed_propo * measure +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years),
     sigma ~ measure),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 10000, warmup = 3000,
  control = list(adapt_delta = 0.99, max_treedepth = 14),
  file    = here("model_output", "model_removed_propo_typemod"),
  backend = "cmdstanr"
)

# 3. Diagnostics --------------------------------------------------------------

model_typemod %>% summary()
model_typemod %>% pp_check()

# Consistency test and per-type residual dispersion
fixef(model_typemod)["removed_propo:measurecover", ]   # ~ 0 => response consistent
fixef(model_typemod)["sigma_measurecover", ]           # > 0 => cover more dispersed
