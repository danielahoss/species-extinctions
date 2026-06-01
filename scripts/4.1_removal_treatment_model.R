# Removal treatment model (biomass + cover combined)
# Compares three model structures via LOO:
#   fit1       : productivity ~ removed_propo (no treatment identity)
#   fit2_add   : productivity ~ removed_propo + removal_trt (additive)
#   fit3_inter : productivity ~ removed_propo * removal_trt (interaction) ← best
# Final model: fit3_inter_traits_3.rds

library(brms)
library(tidyverse)
library(patchwork)
library(DHARMa)
library(emmeans)
library(here)

rm(list = ls())

seed <- 321

# 1. Load and prepare data ----------------------------------------------------

df_trt_biom  <- read.csv(here("data", "df_biomass_trt.csv"), header = TRUE)
df_trt_cover <- read.csv(here("data", "df_cover_trt.csv"),   header = TRUE)

df_trt_biom  <- df_trt_biom  %>% mutate(block = as.character(block),
                                         productivity = biomass)
df_trt_cover <- df_trt_cover %>% mutate(block = as.character(block),
                                         productivity = cover)

df <- bind_rows(df_trt_biom, df_trt_cover)

as.factor(df$removal_trt) %>% levels


df %>% count(removal_trt)

df %>% distinct(study_ID, removal_trt) %>% count(removal_trt)

df %>%
  group_by(removal_trt) %>%
  summarise(n_obs = n(), n_studies = n_distinct(study_ID))

df %>%
  filter(removal_trt == "subordinate") %>% 
  group_by(study_ID, removal_trt) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  arrange(removal_trt, study_ID)


df %>%
  count(study_ID, removal_trt) %>%
  pivot_wider(names_from = removal_trt, values_from = n, values_fill = 0)

# 2. Fit models ---------------------------------------------------------------

# Model 1: removed_propo only (baseline)
fit1 <- brm(
  bf(productivity ~ removed_propo +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years)),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file    = here("model_output", "fit1_3"),
  backend = "cmdstanr"
)

# Model 2: additive effect of removal treatment
fit2_add <- brm(
  bf(productivity ~ 0 + removed_propo + removal_trt +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years)),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 5000, warmup = 2000,
  control = list(adapt_delta = 0.99),
  file    = here("model_output", "fit2_add_traits_4"),
  backend = "cmdstanr"
)

# Model 3: interaction between removed_propo and removal treatment (best model)
fit3_inter <- brm(
  bf(productivity ~ 0 + removed_propo + removal_trt +
       removed_propo:removal_trt +
       (0 + removed_propo + removal_trt +
          removed_propo:removal_trt | study_ID / block / plot) +
       (1 | time_length_years)),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 6000, warmup = 2000,
  control = list(adapt_delta = 0.99),
  file    = here("model_output", "fit3_inter_traits_3"),
  backend = "cmdstanr"
)

# 3. Model comparison (LOO) ---------------------------------------------------
#

fit1       <- add_criterion(fit1,       "loo")
fit2_add   <- add_criterion(fit2_add,   "loo")
fit3_inter <- add_criterion(fit3_inter, "loo")

comp <- loo_compare(fit1, fit2_add, fit3_inter)
print(comp, simplify = FALSE)

loo_R2(fit1)
loo_R2(fit2_add)
loo_R2(fit3_inter)

# 4. Best model summary and marginal effects ----------------------------------

fit3_inter %>% summary()
fit3_inter %>% conditional_effects()

# Slopes per treatment type
emtrends(fit3_inter, ~ removal_trt, var = "removed_propo")

# 5. Diagnostics (fit3_inter) -------------------------------------------------

fit3_inter %>% pp_check()
fit3_inter %>% mcmc_plot(type = "trace")

# -- 5a. DHARMa ---------------------------------------------------------------
model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(fit3_inter)),
  observedResponse        = fit3_inter$data$productivity,
  fittedPredictedResponse = apply(t(posterior_epred(fit3_inter)), 1, mean),
  integerResponse         = FALSE
)

plot(model.check)
testUniformity(model.check)
testDispersion(model.check)
testOutliers(model.check)

# -- 5b. Styled residual boxplots ---------------------------------------------
# Join extra covariates from df (brms only keeps formula variables)
residuals_df <- fit3_inter$data %>%
  left_join(
    df %>% select(study_ID, block, plot, time_length_years,
                  removed_propo, removal_trt, response_variable,
                  latitude, mean_annual_ppt_mm,
                  mean_annual_temperature_Celsius) %>% distinct(),
    by = c("study_ID", "block", "plot", "time_length_years",
           "removed_propo", "removal_trt")
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

plot1 <- bp(residuals_df, "study_ID",        "Study ID",           xangle = 45)
plot2 <- bp(residuals_df, "time_length_years","Time (years)")
plot3 <- bp(residuals_df, "removal_trt",     "Removal treatment")
plot4 <- bp(residuals_df, "response_variable","Response variable")
plot5 <- residuals_df %>%
  mutate(removed_propo_bin = cut(removed_propo, breaks = 5)) %>%
  bp("removed_propo_bin", "Proportion removed (binned)")

(plot2 + plot3 + plot4 + plot5 + plot1 +
    plot_annotation(tag_levels = "a") +
    plot_layout(ncol = 2))

# ggsave("Extended Data Fig. 9 - removal treatment residuals.pdf",
#        path = here("git_publ", "figures", "ExtendedData"),
#        width = 180, height = 220, units = "mm")
