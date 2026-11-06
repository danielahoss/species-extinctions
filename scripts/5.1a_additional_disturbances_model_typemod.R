# Additional-disturbances model
# ---------------------------------------------------------------------------
# productivity ~ add_dist * removed_propo + measure
#                + (removed_propo | study_ID/block/plot) + (1 | time_length_years),
# sigma ~ measure, lognormal()
# add_dist: no (reference), drought, nutrient addition, warming
#
# Output: model_output/model_dist_typemod.rds
# Plot:   scripts/5.1b_additional_disturbances_plot_typemod.R

library(tidyverse)
library(brms)
library(here)

seed <- 321

# 1. Data ---------------------------------------------------------------------

prep <- function(file, meas) {
  read.csv(here("data", file), header = TRUE) %>%
    mutate(add_dist = "no",
           add_dist = if_else(drought  == "yes", "drought",  add_dist),
           add_dist = if_else(nutrient == "yes", "nutrient", add_dist),
           add_dist = if_else(warming  == "yes", "warming",  add_dist)) %>%
    transmute(study_ID, block = as.character(block), plot, time_length_years,
              removed_propo, productivity = .data[[meas]], measure = meas, add_dist)
}

df <- bind_rows(prep("df_biom_brm_2.csv", "biomass"),
                prep("df_cover_brm_2.csv", "cover")) %>%
  mutate(measure  = factor(measure,  levels = c("biomass", "cover")),
         add_dist = factor(add_dist, levels = c("no", "drought", "nutrient", "warming")))

stopifnot(nrow(df) == 7861)
print(table(df$add_dist))

# 2. Fit ----------------------------------------------------------------------

set.seed(seed)
model_dist_typemod <- brm(
  bf(productivity ~ add_dist * removed_propo + measure +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years),
     sigma ~ measure),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 12000, warmup = 3000,
  control = list(adapt_delta = 0.995, max_treedepth = 14),
  file    = here("model_output", "model_dist_typemod"),
  backend = "cmdstanr"
)

# 3. Diagnostics --------------------------------------------------------------

model_dist_typemod %>% summary()
model_dist_typemod %>% pp_check()
message("max R-hat: ", round(max(brms::rhat(model_dist_typemod), na.rm = TRUE), 4),
        " | divergences: ",
        sum(subset(brms::nuts_params(model_dist_typemod), Parameter == "divergent__")$Value))


