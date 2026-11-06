# Supplementary Table — Removal treatment model comparison
# Compares three model structures via LOO-ELPD and reports fixed-effect summaries:
#   fit1: productivity ~ removed_propo (proportion only)
#   fit2: productivity ~ removed_propo + removal_trt (additive)
#   fit3: productivity ~ removed_propo * removal_trt (interaction, highest ELPD)
# All three include measure (biomass vs cover) in the mean and sigma ~ measure.
# fit1 and fit2 are fitted in scripts/4.1c_removal_treatment_model_comparison.R;
# fit3 is model_remtrt_typemod from scripts/4.1a (loaded there as fit3_inter).

# 1. Libraries ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(loo)
library(flextable)
library(officer)
library(here)

rm(list = ls())

# 2. Load models --------------------------------------------------------------

fit1 <- read_rds(here::here("model_output", "fit1_7.rds"))
fit2 <- read_rds(here::here("model_output", "fit2_add_traits_7.rds"))
fit3 <- read_rds(here::here("model_output", "model_remtrt_typemod.rds"))

# 3. LOO comparison -----------------------------------------------------------

loos <- readRDS(here::here("model_output", "remtrt_comparison_loo_v5.rds"))

comp <- loo_compare(loos)

elpd_df <- as.data.frame(comp) %>%
  rownames_to_column("fit_name") %>%
  mutate(model_id = case_when(
    fit_name == "fit3_inter" ~ "fit3",
    fit_name == "fit2_add"   ~ "fit2",
    fit_name == "fit1"       ~ "fit1"
  )) %>%
  select(model_id, elpd_diff, se_diff)

# 4. Fixed effects ------------------------------------------------------------

param_labels <- c(
  Intercept                            = "Intercept (β0)",
  removed_propo                        = "Proportion removed (β1)",
  removal_trtdominant                  = "Intercept — dominant (βdominant)",
  removal_trtsubordinate               = "Intercept — subordinate (βsubordinate)",
  removal_trttraits                    = "Intercept — trait-based (βtraits)",
  `removed_propo:removal_trtsubordinate` = "Slope deviation — subordinate (β1×sub)",
  `removed_propo:removal_trttraits`      = "Slope deviation — trait-based (β1×traits)",
  measurecover                         = "Measure: cover vs biomass (βcover)",
  sigma_Intercept                      = "Residual SD, log scale (σ intercept, biomass)",
  sigma_measurecover                   = "Residual SD: cover vs biomass"
)

# Row order within each model (fixef() order differs between models)
param_order <- unname(param_labels[c(
  "Intercept", "removed_propo", "removal_trtdominant", "removal_trtsubordinate",
  "removal_trttraits", "removed_propo:removal_trtsubordinate",
  "removed_propo:removal_trttraits", "measurecover", "sigma_Intercept",
  "sigma_measurecover")])

model_labels <- c(
  fit3 = "Model 1: proportion removed × removal treatment (interaction)",
  fit1 = "Model 2: proportion removed only",
  fit2 = "Model 3: proportion removed + removal treatment (additive)"
)

extract_fe <- function(mod, model_id) {
  as.data.frame(fixef(mod)) %>%
    rownames_to_column("raw_param") %>%
    mutate(
      model_id  = model_id,
      Parameter = recode(raw_param, !!!param_labels)
    ) %>%
    select(model_id, Parameter, Estimate, Est.Error, Q2.5, Q97.5)
}

fe_rows <- bind_rows(
  extract_fe(fit3, "fit3"),
  extract_fe(fit1, "fit1"),
  extract_fe(fit2, "fit2")
) %>%
  mutate(model_id = factor(model_id, levels = c("fit3", "fit1", "fit2"))) %>%
  arrange(model_id, match(Parameter, param_order)) %>%
  left_join(elpd_df, by = "model_id") %>%
  mutate(
    Model = recode(model_id, !!!model_labels),
    across(c(Estimate, Est.Error, Q2.5, Q97.5, elpd_diff, se_diff),
           ~ round(.x, 3))
  ) %>%
  select(Model, elpd_diff, se_diff, Parameter, Estimate, Est.Error, Q2.5, Q97.5)

# 4b. Diagnostics reported in the caption (computed, not typed) ----------------
# R-hat is the maximum over population-level terms, residual-variance terms,
# group-level SDs and correlations (individual random effects r_* and z_* excluded).

diag_one <- function(mod) {
  s  <- posterior::summarise_draws(posterior::as_draws_array(mod), "rhat")
  s  <- s[!grepl("^(r_|z_|lp__|lprior)", s$variable), ]
  np <- nuts_params(mod)
  list(max_rhat = max(s$rhat, na.rm = TRUE),
       n_div    = sum(np$Value[np$Parameter == "divergent__"]),
       n_draws  = posterior::ndraws(mod))
}

diags <- list(fit3 = diag_one(fit3), fit1 = diag_one(fit1), fit2 = diag_one(fit2))

pareto_pct <- sapply(loos, function(l) 100 * mean(loo::pareto_k_values(l) > 0.7))
pk_range   <- sprintf("%.1f-%.1f%%", min(pareto_pct), max(pareto_pct))

fmt_draws <- function(d) format(d$n_draws, big.mark = ",")
div_text <- function(d) {
  if (d$n_div == 0) "no divergent transitions"
  else sprintf("%d divergent transition%s in %s post-warmup draws", d$n_div,
               ifelse(d$n_div == 1, "", "s"), fmt_draws(d))
}

rhat_text <- sprintf(
  "Maximum R̂ was %.3f for the interaction model, %.3f for the proportion-only model and %.3f for the additive model",
  diags$fit3$max_rhat, diags$fit1$max_rhat, diags$fit2$max_rhat)

diag_sentence <- if (diags$fit1$n_div == 0 && diags$fit2$n_div == 0) {
  sprintf("%s; the interaction model had %s and the other two models had none.",
          rhat_text, div_text(diags$fit3))
} else {
  sprintf("%s; the interaction model had %s, the proportion-only model had %s and the additive model had %s.",
          rhat_text, div_text(diags$fit3), div_text(diags$fit1), div_text(diags$fit2))
}

print(diag_sentence)
print(pareto_pct)

# 5. Build flextable ----------------------------------------------------------

col_labels <- c(
  Model     = "Model",
  elpd_diff = "ELPD diff",
  se_diff   = "SE diff",
  Parameter = "Parameter",
  Estimate  = "Estimate",
  Est.Error = "Posterior SD",
  Q2.5      = "Q2.5",
  Q97.5     = "Q97.5"
)

ft <- flextable(fe_rows) %>%
  set_header_labels(values = col_labels) %>%

  merge_v(j = c("Model", "elpd_diff", "se_diff")) %>%
  valign(j = c("Model", "elpd_diff", "se_diff"), valign = "top") %>%

  bold(part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%

  width(j = "Model",     width = 2.5) %>%
  width(j = "elpd_diff", width = 0.7) %>%
  width(j = "se_diff",   width = 0.65) %>%
  width(j = "Parameter", width = 2.3) %>%
  width(j = "Estimate",  width = 0.75) %>%
  width(j = "Est.Error", width = 0.85) %>%
  width(j = "Q2.5",      width = 0.65) %>%
  width(j = "Q97.5",     width = 0.65) %>%

  colformat_num(j = c("Estimate", "Est.Error", "Q2.5", "Q97.5",
                      "elpd_diff", "se_diff"), digits = 3) %>%
  align(j = c("Estimate", "Est.Error", "Q2.5", "Q97.5",
              "elpd_diff", "se_diff"), align = "right") %>%

  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1)) %>%

  set_caption(paste(
    "Supplementary Table S6. Removal treatment model comparison.",
    "Models are compared using expected log predictive density (ELPD) estimated by approximate (Pareto-smoothed importance sampling) leave-one-out cross-validation.",
    "Lognormal likelihood was assumed for all models.",
    "All models include measurement type (biomass or cover) as a predictor of both the mean and the residual standard deviation.",
    "ELPD diff and SE diff are relative to the model with the highest ELPD (Proportion removed × removal treatment - interaction).",
    "Estimates are posterior means with posterior standard deviation (Posterior SD) and 95% credible intervals (Q2.5, Q97.5).",
    "Estimates are on the log scale, and residual SD terms are on the log link scale (log σ);",
    "intercepts refer to biomass and the cover effect is given by the measure term.",
    diag_sentence,
    sprintf("Pareto-k > 0.7 occurred for %s of observations across the models, so the LOO comparison should be interpreted cautiously.", pk_range),
    "All models in this table were fitted to the subset of observations with a defined removal treatment (7,248 of 7,765 observations, 24 studies),",
    "so the proportion-only estimate differs from the productivity model in Supplementary Table 3."
  ))

ft

# 6. Save to Word -------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Supplementary Table S6: Removal treatment model comparison",
               style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = here::here("SupplementaryTable_S6_model_comparison.docx"))

message("Saved: SupplementaryTable_S6_model_comparison.docx")
