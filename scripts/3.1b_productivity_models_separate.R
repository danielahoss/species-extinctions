# Separate productivity models: biomass and cover fit independently
# ---------------------------------------------------------------------------
# Round 3 revision (Reviewer #2 Comment 2.2 / Reviewer #3 Comment 3.2):
# instead of pooling biomass and cover into a single lognormal model
# (scripts/3.1_productivity_model.R), we fit the two response types as two
# separate models with the SAME structure, and present them side by side
# (scripts/3.2b_productivity_plot_separate.R). This is the analysis both
# reviewers requested ("model biomass and percent cover separately").
#
# Structure is identical to the pooled model:
#   productivity ~ removed_propo + (removed_propo | study_ID/block/plot)
#                                 + (1 | time_length_years),  lognormal()
# Only the response data differ between the two fits.
#
# Output: model_output/model_biomass_2.rds, model_output/model_cover_2.rds

library(tidyverse)
library(brms)
library(here)

seed <- 321   # matches scripts/3.1_productivity_model.R

# 1. Data ---------------------------------------------------------------------

df_biom  <- read.csv(here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here("data", "df_cover_brm_2.csv"), header = TRUE)

# Same exclusion as the pooled model: abundance-only manipulations from ID_29,
# for which removed_propo (proportion of *species* removed) is not meaningful.
id29_filter <- function(d) {
  if (!"removal_treatment" %in% names(d)) return(d)
  d %>% filter(!(study_ID == "ID_29" &
                   removal_treatment %in% c("abundance_reduction",
                                            "abundance_richness_reduction")))
}

df_biom <- df_biom %>% id29_filter() %>%
  mutate(block = as.character(block), productivity = biomass)
df_cover <- df_cover %>% id29_filter() %>%
  mutate(block = as.character(block), productivity = cover)

prod_form <- bf(productivity ~ removed_propo +
                  (removed_propo | study_ID / block / plot) +
                  (1 | time_length_years))

# 2. Biomass model ------------------------------------------------------------

set.seed(seed)
model_biomass <- brm(
  prod_form,
  family  = lognormal(),
  data    = df_biom,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 14),
  file    = here("model_output", "model_biomass_2"),
  backend = "cmdstanr"
)
model_biomass %>% summary()

# 3. Cover model --------------------------------------------------------------

set.seed(seed)
model_cover <- brm(
  prod_form,
  family  = lognormal(),
  data    = df_cover,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 14),
  file    = here("model_output", "model_cover_2"),
  backend = "cmdstanr"
)
model_cover %>% summary()

# 4. The two removal slopes (log scale) ---------------------------------------

fixef(model_biomass)["removed_propo", ]
fixef(model_cover)["removed_propo", ]
