# Additional disturbances model (biomass + cover combined)
# productivity ~ add_dist * removed_propo +
#   (removed_propo | study_ID/block/plot) + (1 | time_length_years)
#
# add_dist levels (biomass): drought, no, nutrient
# add_dist levels (cover):   warming, no, nutrient
# Reference level: "no" (no additional disturbance)

library(tidyverse)
library(brms)
library(patchwork)
library(DHARMa)
library(here)

rm(list = ls())

seed <- 321

# 1. Load and prepare data ----------------------------------------------------

df_biom  <- read.csv(here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here("data", "df_cover_brm_2.csv"), header = TRUE)

# Biomass: drought or nutrient addition as additional disturbance
df_biom$add_dist <- "no"
df_biom$add_dist[df_biom$drought  == "yes"] <- "drought"
df_biom$add_dist[df_biom$nutrient == "yes"] <- "nutrient"
df_biom$add_dist <- relevel(factor(df_biom$add_dist), ref = "no")

# Cover: warming or nutrient addition as additional disturbance
df_cover$add_dist <- "no"
df_cover$add_dist[df_cover$warming  == "yes"] <- "warming"
df_cover$add_dist[df_cover$nutrient == "yes"] <- "nutrient"
df_cover$add_dist <- relevel(factor(df_cover$add_dist), ref = "no")

df_biom_add <- df_biom %>%
  select(study_ID, block, plot, time_length_years, experiment_duration,
         pre_removal, add_dist, country, latitude, longitude, altitude_m,
         mean_annual_ppt_mm, mean_annual_temperature_Celsius, latitude_abs,
         author, publ_year, removal_method_category, removal_treatment,
         removal_treatment_category, response_variable, n_removed, removed_propo,
         biomass, richness, ricmin, ricmean, ricmax,
         remov_propo_min, remov_propo_mean, remov_propo_max,
         n_remov_min, n_remov_mean, n_remov_max) %>%
  mutate(block = as.character(block), productivity = biomass)

df_cover_add <- df_cover %>%
  select(study_ID, block, plot, time_length_years, experiment_duration,
         pre_removal, add_dist, country, latitude, longitude, altitude_m,
         mean_annual_ppt_mm, mean_annual_temperature_Celsius, latitude_abs,
         author, publ_year, removal_method_category, removal_treatment,
         removal_treatment_category, response_variable, n_removed, removed_propo,
         cover, richness, ricmin, ricmean, ricmax,
         remov_propo_min, remov_propo_mean, remov_propo_max,
         n_remov_min, n_remov_mean, n_remov_max) %>%
  mutate(block = as.character(block), productivity = cover)

df <- bind_rows(df_biom_add, df_cover_add)

# 2. Fit model ----------------------------------------------------------------
#
#

fit_dist <- brm(
  bf(productivity ~ add_dist * removed_propo +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years)),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 3000, warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file    = here("model_output", "additional_disturbances_1"),
  backend = "cmdstanr"
)

# 3. Diagnostics --------------------------------------------------------------

fit_dist %>% summary()
fit_dist %>% pp_check()
fit_dist %>% pp_check(type = "stat", stat = "mean") # does my model get the average right?
fit_dist %>% mcmc_plot(type = "trace")
fit_dist %>% conditional_effects()

# -- 3a. DHARMa ---------------------------------------------------------------
# Use fit_dist$data as observed response: lognormal drops rows with
# productivity <= 0 or any NA predictor, so nrow(fit_dist$data) may differ
# from nrow(df)

model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(fit_dist)),
  observedResponse        = fit_dist$data$productivity,
  fittedPredictedResponse = apply(t(posterior_epred(fit_dist)), 1, mean),
  integerResponse         = FALSE
)

plot(model.check)
testUniformity(model.check)
testDispersion(model.check)
testOutliers(model.check)

# -- 3b. Styled residual boxplots ---------------------------------------------
# Join extra covariates from df (brms only keeps formula variables)

residuals_df <- fit_dist$data %>%
  left_join(
    df %>%
      select(study_ID, block, plot, time_length_years,
             removed_propo, add_dist, response_variable,
             latitude, mean_annual_ppt_mm,
             mean_annual_temperature_Celsius) %>%
      mutate(add_dist = as.character(add_dist)) %>%
      distinct(),
    by = c("study_ID", "block", "plot", "time_length_years",
           "removed_propo", "add_dist")
  ) %>%
  mutate(resid = model.check$scaledResiduals)

bp <- function(data, x, labx = x, xangle = 0) {
  data[[x]] <- as.factor(data[[x]])
  ggplot(data, aes(x = .data[[x]], y = resid)) +
    geom_boxplot() +
    geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed") +
    labs(x = labx, y = "Scaled residuals") +
    theme(
      text             = element_text(size = 8, family = "Helvetica", colour = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid       = element_blank(),
      axis.title       = element_text(face = "bold"),
      axis.text        = element_text(colour = "black"),
      axis.text.x      = element_text(angle = xangle,
                                      hjust = if (xangle > 0) 1 else 0.5),
      axis.line        = element_line(colour = "black"),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

plot1 <- bp(residuals_df, "study_ID",        "Study ID",                 xangle = 45)
plot2 <- bp(residuals_df, "time_length_years","Time (years)")
plot3 <- bp(residuals_df, "add_dist",        "Additional disturbance")
plot4 <- bp(residuals_df, "response_variable","Response variable")
plot5 <- residuals_df %>%
  mutate(removed_propo_bin = cut(removed_propo, breaks = 5)) %>%
  bp("removed_propo_bin", "Proportion removed (binned)")

(plot2 + plot3 + plot4 + plot5 + plot1 +
    plot_annotation(tag_levels = "a") +
    plot_layout(ncol = 2))

# ggsave("Extended Data Fig - additional disturbances residuals.pdf",
#        path = here("git_publ", "figures", "ExtendedData"),
#        width = 180, height = 220, units = "mm")
