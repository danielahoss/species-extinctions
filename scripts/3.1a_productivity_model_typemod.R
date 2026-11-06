# Productivity model (Fig 2): biomass and cover in a single model
# ---------------------------------------------------------------------------
# productivity ~ removed_propo * measure + (removed_propo | study_ID/block/plot)
#                                         + (1 | time_length_years),
# sigma ~ measure, lognormal()
#
# Output: model_output/model_removed_propo_typemod_3.rds
# Plot:   scripts/3.1b_productivity_plot_typemod.R

library(tidyverse)
library(brms)
library(here)

seed <- 321

# 1. Data ---------------------------------------------------------------------

df_biom <- read.csv(here("data", "df_biom_brm_2.csv"), header = TRUE) %>%
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
  iter    = 30000, warmup = 5000,
  control = list(adapt_delta = 0.99, max_treedepth = 14),
  file    = here("model_output", "model_removed_propo_typemod_3"),
  backend = "cmdstanr"
)

# 3. Diagnostics --------------------------------------------------------------

model_typemod %>% summary()
model_typemod %>% pp_check()

fixef(model_typemod)["removed_propo:measurecover", ]
fixef(model_typemod)["sigma_measurecover", ]


