# Removal-treatment model comparison (LOO)
# ---------------------------------------------------------------------------
#   fit1       : productivity ~ removed_propo
#   fit2_add   : productivity ~ removed_propo + removal_trt
#   fit3_inter : productivity ~ removed_propo * removal_trt (model_remtrt_typemod, 4.1a)
# All include measure in the mean and sigma ~ measure.
# Data: data/df_biomass_trt.csv, data/df_cover_trt.csv (see 4.0)

library(brms)
library(tidyverse)
library(here)
library(loo)

rm(list = ls())

seed <- 321

# 1. Load and prepare data ----------------------------------------------------

df_biom <- read.csv(here("data", "df_biomass_trt.csv"), header = TRUE) %>%
  mutate(block = as.character(block), productivity = biomass, measure = "biomass")
df_cover <- read.csv(here("data", "df_cover_trt.csv"), header = TRUE) %>%
  mutate(block = as.character(block), productivity = cover, measure = "cover")

df <- bind_rows(df_biom, df_cover) %>%
  mutate(removal_trt = factor(removal_trt),
         measure     = factor(measure, levels = c("biomass", "cover")))

# 2. Fit models ---------------------------------------------------------------

# Model 1: removed_propo only (baseline)
fit1 <- brm(
  bf(productivity ~ removed_propo + measure +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years),
     sigma ~ measure),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 12000, warmup = 2000,
  control = list(adapt_delta = 0.995, max_treedepth = 12),
  file    = here("model_output", "fit1_7"),
  backend = "cmdstanr"
)

# Model 2: additive effect of removal treatment
fit2_add <- brm(
  bf(productivity ~ 0 + removed_propo + removal_trt + measure +
       (removed_propo | study_ID / block / plot) +
       (1 | time_length_years),
     sigma ~ measure),
  family  = lognormal(),
  data    = df,
  seed    = seed,
  cores   = 4, chains = 4,
  iter    = 12000, warmup = 2000,
  control = list(adapt_delta = 0.995, max_treedepth = 12),
  file    = here("model_output", "fit2_add_traits_7"),
  backend = "cmdstanr"
)

# Model 3: interaction (fitted in 4.1a)
fit3_inter <- readRDS(here("model_output", "model_remtrt_typemod.rds"))

fit1
fit2_add
fit3_inter

# 3. Model comparison (LOO) ---------------------------------------------------

loo_file <- here("model_output", "remtrt_comparison_loo_v5.rds")
if (file.exists(loo_file)) {
  loos <- readRDS(loo_file)
} else {
  loos <- list(
    fit1       = loo(fit1,       cores = 4),
    fit2_add   = loo(fit2_add,   cores = 4),
    fit3_inter = loo(fit3_inter, cores = 4))
  saveRDS(loos, loo_file)
}

comp <- loo_compare(loos)
print(comp, simplify = FALSE)

map(loos, ~ pareto_k_table(.x))

loo_R2(fit1, ndraws = 1000)
loo_R2(fit2_add, ndraws = 1000)
loo_R2(fit3_inter, ndraws = 1000)

# 4. Diagnostics --------------------------------------------------------------

library(posterior)
library(DHARMa)
library(bayesplot)

models <- list(fit1 = fit1, fit2_add = fit2_add, fit3_inter = fit3_inter)

# -- 4a. Convergence and effective sample size --------------------------------
conv <- map_dfr(models, function(m) {
  draws <- summarise_draws(as_draws_array(m), "rhat", "ess_bulk", "ess_tail")
  tibble(
    n_params       = nrow(draws),
    max_rhat       = max(draws$rhat, na.rm = TRUE),
    n_rhat_gt_1.01 = sum(draws$rhat > 1.01, na.rm = TRUE),
    min_ess_bulk   = min(draws$ess_bulk, na.rm = TRUE),
    min_ess_tail   = min(draws$ess_tail, na.rm = TRUE),
    divergent      = sum(subset(nuts_params(m), Parameter == "divergent__")$Value)
  )
}, .id = "model")
print(conv)

# Parameters that fail the criteria
map(models, function(m) {
  summarise_draws(as_draws_array(m), "rhat", "ess_bulk", "ess_tail") %>%
    filter(rhat > 1.01 | ess_bulk < 400 | ess_tail < 400) %>%
    arrange(desc(rhat))
})

# -- 4b. Posterior predictive checks and traces --------------------------------
map(models, pp_check, ndraws = 100)
map(models, mcmc_plot, type = "trace", variable = "^b_", regex = TRUE)

# -- 4c. DHARMa residuals and heteroscedasticity -------------------------------
set.seed(seed)
dharma_check <- function(m, ndraws_use = 1000) {
  idx <- sample(seq_len(ndraws(m)), ndraws_use)
  dh <- createDHARMa(
    simulatedResponse       = t(posterior_predict(m, draw_ids = idx)),
    observedResponse        = brms::get_y(m),
    fittedPredictedResponse = rowMeans(t(posterior_epred(m, draw_ids = idx))),
    integerResponse         = FALSE
  )
  plot(dh)
  print(testUniformity(dh, plot = FALSE))
  print(testDispersion(dh, plot = FALSE))
  print(testOutliers(dh, plot = FALSE))
  print(testQuantiles(dh, plot = FALSE))
  dh
}
dh_list <- map(models, dharma_check)

# Scaled residual spread by removal treatment
map(names(models), function(n) {
  models[[n]]$data %>%
    mutate(resid = dh_list[[n]]$scaledResiduals) %>%
    group_by(across(any_of("removal_trt"))) %>%
    summarise(mean_resid = mean(resid), sd_resid = sd(resid), .groups = "drop")
})

