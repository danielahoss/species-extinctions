# Type-moderator additional-disturbances model
# ---------------------------------------------------------------------------
# Same type-moderator specification as the productivity model
# (scripts/3.1a), extended with the additional disturbance applied in each
# experiment (drought, nutrient addition, warming; "no" = no additional
# disturbance, the reference level). The disturbance interacts with the
# proportion removed, and measurement type enters the mean and the variance:
#   productivity ~ add_dist * removed_propo + measure
#                  + (removed_propo | study_ID/block/plot) + (1 | time_length_years),
#   sigma ~ measure ,  lognormal()
#
# Output: model_output/model_dist_typemod.rds
# Plot:   scripts/5.2_additional_disturbances_plot_typemod.R

library(tidyverse)
library(brms)
library(here)

seed <- 321

# 1. Data ---------------------------------------------------------------------
# Build the additional-disturbance factor from the yes/no columns. Order is such
# that, where a row carries more than one flag, the later assignment wins; in
# this dataset the categories are mutually exclusive.

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
# Higher iter / adapt_delta than the main productivity model: the deep
# plot-level random-effects terms need more sampling to converge (max R-hat
# 1.006, no divergent transitions at these settings).

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
