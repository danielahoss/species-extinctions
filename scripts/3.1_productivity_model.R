# Combined productivity model (biomass + cover)
# Fits: productivity ~ removed_propo + (removed_propo | study_ID/block/plot) + (1 | time_length_years)
# Output: model_output/model_removed_propo.rds

library(tidyverse)
library(brms)
library(DHARMa)
library(here)

rm(list = ls())

seed <- 321

# 1. Load and prepare data ----------------------------------------------------

df_biom  <- read.csv(here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here("data", "df_cover_brm_2.csv"), header = TRUE)

df_biom_trt <- df_biom %>%
  select(study_ID, block, author, publ_year, plot, time_length_years,
         response_variable, removed_propo, removal_treatment, biomass) %>%
  mutate(block = as.character(block), productivity = biomass)

df_cover_trt <- df_cover %>%
  select(study_ID, block, author, publ_year, plot, time_length_years,
         response_variable, removed_propo, removal_treatment, cover) %>%
  mutate(block = as.character(block), productivity = cover)

df <- bind_rows(df_biom_trt, df_cover_trt) %>%
  # Exclude abundance-only manipulation treatments from ID_29:
  # removed_propo reflects proportion of *species* removed, which is not
  # meaningful for abundance_reduction / abundance_richness_reduction treatments
  filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction",
                                      "abundance_richness_reduction")))

# 2. Fit model ----------------------------------------------------------------


#
mod_rem_propo <- brm(
  bf(productivity ~ removed_propo +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years)),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 14),
  file    = "model_output/model_removed_propo_2",
  backend = "cmdstanr"
)


# 3. Diagnostics --------------------------------------------------------------

# -- 3a. Convergence ----------------------------------------------------------
mod_rem_propo %>% summary()
mod_rem_propo %>% mcmc_plot(type = "trace")

# -- 3b. Posterior predictive check -------------------------------------------
mod_rem_propo %>% pp_check()
mod_rem_propo %>% pp_check(type = "stat", stat = "mean")

# -- 3c. DHARMa residuals -----------------------------------------------------
model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(mod_rem_propo)),
  observedResponse        = mod_rem_propo$data$productivity,
  fittedPredictedResponse = apply(t(posterior_epred(mod_rem_propo)), 1, mean),
  integerResponse         = FALSE
)

plot(model.check)           # QQ + residuals vs fitted
testUniformity(model.check)
testDispersion(model.check)
testOutliers(model.check)

# -- 3d. Styled residual boxplots by key covariates ---------------------------
# Use mod_rem_propo$data to match the rows actually used in the model
# (lognormal drops zeros, so nrow may differ from the original df)

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
      axis.text.x      = element_text(angle = xangle, hjust = if (xangle > 0) 1 else 0.5),
      axis.line        = element_line(colour = "black"),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# Join response_variable back from original data (brms only keeps formula vars)
residuals_df <- mod_rem_propo$data %>%
  left_join(
    bind_rows(df_biom_trt, df_cover_trt) %>%
      select(study_ID, block, plot, time_length_years,
             removed_propo, response_variable) %>%
      distinct(),
    by = c("study_ID", "block", "plot", "time_length_years", "removed_propo")
  ) %>%
  mutate(resid = model.check$scaledResiduals)

plot1 <- bp(residuals_df, "study_ID", "Study ID", xangle = 45)
plot2 <- bp(residuals_df, "time_length_years",  "Time (years)")
plot3 <- bp(residuals_df, "response_variable",  "Response variable")
plot4 <- residuals_df %>%
  mutate(removed_propo_bin = cut(removed_propo, breaks = 5)) %>%
  bp("removed_propo_bin", "Proportion removed (binned)")

(plot2 + plot3 + plot4 + plot1 +
    plot_annotation(tag_levels = "a") +
    plot_layout(ncol = 2))

# ggsave("Extended Data Fig. 8 - productivity residuals.pdf",
#        path = here("git_publ", "figures", "ExtendedData"),
#        width = 180, height = 180, units = "mm")
