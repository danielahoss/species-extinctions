# Supplementary Table — Removal treatment model comparison
# Compares three model structures via LOO-ELPD and reports fixed-effect summaries:
#   fit1: productivity ~ removed_propo (proportion only)
#   fit2: productivity ~ removed_propo + removal_trt (additive)
#   fit3: productivity ~ removed_propo * removal_trt (interaction, best fit)

# 1. Libraries ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(loo)
library(flextable)
library(officer)
library(here)

rm(list = ls())

# 2. Load models --------------------------------------------------------------

fit1 <- read_rds(here::here("model_output", "fit1_3.rds"))
fit2 <- read_rds(here::here("model_output", "fit2_add_traits_4.rds"))
fit3 <- read_rds(here::here("model_output", "fit3_inter_traits_3.rds"))

# 3. LOO comparison -----------------------------------------------------------

loo3 <- loo(fit3)
loo2 <- loo(fit2)
loo1 <- loo(fit1)

comp <- loo_compare(loo3, loo2, loo1)

elpd_df <- as.data.frame(comp) %>%
  rownames_to_column("fit_name") %>%
  mutate(model_id = case_when(
    fit_name == "fit3" ~ "fit3",
    fit_name == "fit2" ~ "fit2",
    fit_name == "fit1" ~ "fit1"
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
  `removed_propo:removal_trttraits`      = "Slope deviation — trait-based (β1×traits)"
)

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
  arrange(model_id) %>%
  left_join(elpd_df, by = "model_id") %>%
  mutate(
    Model = recode(model_id, !!!model_labels),
    across(c(Estimate, Est.Error, Q2.5, Q97.5, elpd_diff, se_diff),
           ~ round(.x, 3))
  ) %>%
  select(Model, elpd_diff, se_diff, Parameter, Estimate, Est.Error, Q2.5, Q97.5)

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
    "Supplementary Table. Removal treatment model comparison.",
    "Three models are compared using leave-one-out cross-validation (LOO-ELPD).",
    "ELPD diff and SE diff are relative to the best-fitting model (Model 3).",
    "Estimates are posterior means with posterior standard deviation (Posterior SD)",
    "and 95% credible intervals (Q2.5, Q97.5) on the log scale."
  ))

ft

# 6. Save to Word -------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Supplementary Table — Removal treatment model comparison",
               style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = here::here("SupplementaryTable_model_comparison.docx"))

message("Saved: SupplementaryTable_model_comparison.docx")
