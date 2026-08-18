# Type-moderator productivity model — canonical Fig 2 model
# ---------------------------------------------------------------------------
# Biomass and cover are combined in a single model with measurement type as a
# moderator of both the mean and the residual variance:
#   productivity ~ removed_propo * measure + (removed_propo | study_ID/block/plot)
#                                           + (1 | time_length_years),
#   sigma ~ measure ,  lognormal()
# The removed_propo:measure term tests whether the productivity response to
# species removal differs between biomass and cover; sigma ~ measure gives each
# response type its own residual dispersion. The same structure is used for the
# removal-treatment model (Fig 3), where fully separate biomass/cover models are
# infeasible (e.g. cover x subordinate = 1 study).
#
# Output: model_output/model_removed_propo_typemod.rds
# Plot:   scripts/3.2a_productivity_plot_typemod.R

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
