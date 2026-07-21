# Supplementary tables for Round 3 revision
# --------------------------------------------------------------------------
# Table A. Representativeness of the plot-level subset (Reviewer #3, Point 1):
#          compares the 24 studies with raw plot-level data against the 14
#          studies with effect sizes only, and reports the pooled meta-analytic
#          effect for each subset.
# Table B. Climate x removal interaction models (Reviewer #3, Point 3):
#          fixed-effects summaries for the four robustness models fit in
#          scripts/3.3_climate_interaction_model.R.
# Style matches scripts/8_supp_table_productivity_models.R (flextable -> docx).

library(tidyverse)
library(brms)
library(flextable)
library(officer)
library(here)

rm(list = ls())

std_border <- fp_border(width = 1)

style_ft <- function(ft, model_col = NULL) {
  ft <- ft %>%
    bold(part = "header") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = std_border) %>%
    hline_bottom(part = "header", border = std_border) %>%
    hline_bottom(part = "body",   border = std_border) %>%
    valign(valign = "top", part = "body")
  if (!is.null(model_col)) ft <- merge_v(ft, j = model_col)
  ft
}

# ===========================================================================
# Table A — Representativeness of the plot-level subset
# ===========================================================================

eff0 <- read.csv(here::here("data", "effects_brm.csv"))
biom <- read.csv(here::here("data", "df_biom_brm_2.csv"))
cov  <- read.csv(here::here("data", "df_cover_brm_2.csv"))
plot_ids <- union(unique(biom$study_ID), unique(cov$study_ID))

study <- eff0 %>%
  group_by(study_ID) %>%
  summarise(yi   = mean(yi, na.rm = TRUE),
            temp = mean(mean_annual_temperature_Celsius, na.rm = TRUE),
            ppt  = mean(mean_annual_ppt_mm, na.rm = TRUE),
            lat  = mean(latitude_abs, na.rm = TRUE),
            dur  = mean(experiment_duration, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(group = ifelse(study_ID %in% plot_ids, "in", "out"))

vars <- tribble(
  ~var,    ~digits, ~label,
  "temp",  1,       "Mean annual temperature (°C)",
  "ppt",   0,       "Mean annual precipitation (mm)",
  "lat",   1,       "Absolute latitude (°)",
  "dur",   1,       "Experiment duration (years)"
)

# Robust Bayesian two-group comparison per variable (Kruschke's BEST):
#   value ~ group with a Student-t likelihood and group-specific variance.
# We report the posterior mean difference (with raw data minus without) and its
# 95% credible interval; an interval excluding zero indicates a credible
# difference. This replaces the earlier frequentist Wilcoxon test so that the
# whole table is consistent with the Bayesian framework used throughout.
compare_row <- function(v, digits, label) {
  d <- study %>% transmute(y = .data[[v]], group) %>% filter(!is.na(y))
  m <- brm(
    bf(y ~ 0 + group, sigma ~ 0 + group),
    data    = d, family = student(),
    seed    = 321, chains = 4, cores = 4, iter = 4000, warmup = 1000,
    refresh = 0, backend = "cmdstanr",
    file    = here::here("model_output", paste0("repr_diff_", v))
  )
  post <- as.data.frame(m)
  diff <- post[["b_groupin"]] - post[["b_groupout"]]   # with raw - without raw
  fmt  <- paste0("%.", digits, "f")
  tibble(
    Variable = label,
    `With raw data (n = 24)`    = sprintf(fmt, mean(d$y[d$group == "in"])),
    `Without raw data (n = 14)` = sprintf(fmt, mean(d$y[d$group == "out"])),
    `Mean difference [95% CrI]` = sprintf(
      paste0(fmt, " [", fmt, ", ", fmt, "]"),
      median(diff), quantile(diff, 0.025), quantile(diff, 0.975))
  )
}

tabA_top <- pmap_dfr(list(vars$var, vars$digits, vars$label), compare_row)

# Pooled meta-analytic effect per subset (from the brms subset fits)
pooled <- function(rds, n) {
  m  <- read_rds(here::here("model_output", rds))
  fx <- fixef(m)["Intercept", ]
  sprintf("%.3f [%.3f, %.3f]", fx["Estimate"], fx["Q2.5"], fx["Q97.5"])
}

tabA_pooled <- tibble(
  Variable = "Pooled effect (log RR) [95% CrI]",
  `With raw data (n = 24)`    = pooled("meta_subset_plot24.rds"),
  `Without raw data (n = 14)` = pooled("meta_subset_meta14.rds"),
  `Mean difference [95% CrI]` = ""
)

tabA <- bind_rows(tabA_top, tabA_pooled)

ftA <- flextable(tabA) %>%
  style_ft() %>%
  width(j = "Variable", width = 2.8) %>%
  width(j = c("With raw data (n = 24)", "Without raw data (n = 14)"), width = 1.4) %>%
  width(j = "Mean difference [95% CrI]", width = 1.7) %>%
  set_caption(paste(
    "Supplementary Table S2. Representativeness of the plot-level dataset.",
    "The 24 studies with raw plot-level data are compared with the 14 studies",
    "for which only effect sizes were available (“without raw data”).",
    "Values are group means; the final column gives the posterior mean difference",
    "(with raw data minus without) with its 95% credible interval, from a robust",
    "Bayesian two-group model (Student-t likelihood, group-specific variance); an",
    "interval excluding zero indicates a credible difference.",
    "The final row gives the pooled meta-analytic effect (posterior mean and 95%",
    "credible interval) from the random-effects model refit on each subset; for",
    "reference, the full 38-study estimate is",
    sprintf("%s.", pooled("meta_brm_multi_id.rds")),
    "Site-level species richness was recorded only where raw plot data were",
    "available and therefore cannot be compared across groups."
  ))

docA <- read_docx() %>%
  body_add_par("Supplementary Table S2: Representativeness of the plot-level dataset",
               style = "heading 1") %>%
  body_add_flextable(ftA)
print(docA, target = here::here("SupplementaryTable_S2_representativeness.docx"))
message("Saved: SupplementaryTable_S2_representativeness.docx")

# ===========================================================================
# Table B — Climate x removal interaction models
# ===========================================================================

int_files <- c(
  "Biomass × temperature"      = "int_biom_temp.rds",
  "Biomass × precipitation"    = "int_biom_ppt.rds",
  "Cover × temperature"        = "int_cover_temp.rds",
  "Cover × precipitation"      = "int_cover_ppt.rds"
)

if (!all(file.exists(here::here("model_output", int_files)))) {
  message("Climate interaction models not all present yet; skipping Table B. ",
          "Re-run this script after scripts/3.3 finishes.")
} else {

  # Readable labels for the fixed-effect rows
  relabel <- function(x) {
    x %>%
      str_replace("^b_", "") %>%
      str_replace("Intercept", "Intercept") %>%
      str_replace("removed_propo:temp_z", "% removed × temperature") %>%
      str_replace("removed_propo:ppt_z",  "% removed × precipitation") %>%
      str_replace("^removed_propo$", "% removed") %>%
      str_replace("^temp_z$", "Temperature (z)") %>%
      str_replace("^ppt_z$",  "Precipitation (z)")
  }

  one_model <- function(label, rds) {
    m  <- read_rds(here::here("model_output", rds))
    fx <- fixef(m) %>% as.data.frame() %>% rownames_to_column("Parameter")
    fx %>%
      transmute(
        model     = label,
        Parameter = relabel(Parameter),
        Estimate  = sprintf("%.3f", Estimate),
        `Posterior SD` = sprintf("%.3f", Est.Error),
        Q2.5      = sprintf("%.3f", Q2.5),
        Q97.5     = sprintf("%.3f", Q97.5)
      )
  }

  tabB <- imap_dfr(int_files, ~ one_model(.y, .x))

  ftB <- flextable(tabB) %>%
    set_header_labels(model = "Model") %>%
    style_ft(model_col = "model") %>%
    width(j = "model",     width = 2.0) %>%
    width(j = "Parameter", width = 2.2) %>%
    width(j = c("Estimate", "Posterior SD", "Q2.5", "Q97.5"), width = 0.85) %>%
    set_caption(paste(
      "Supplementary Table S5. Climate × removal interaction models.",
      "Fixed-effects summaries (posterior mean, posterior standard deviation, and",
      "95% credible interval) for biomass and cover models, each fit with a",
      "% removed × climate interaction. Climate predictors are standardised",
      "(z-scores). Temperature and precipitation are tested in separate models so",
      "that studies missing one variable do not drop from the test for the other.",
      "The base structure matches the productivity model (lognormal family;",
      "random intercepts and slopes for % removed within plots, blocks, and",
      "studies, and a random intercept for time). An interaction credible interval",
      "spanning zero indicates that the removal effect does not vary significantly",
      "with that climate variable."
    ))

  docB <- read_docx() %>%
    body_add_par("Supplementary Table S5: Climate × removal interaction models",
                 style = "heading 1") %>%
    body_add_flextable(ftB)
  print(docB, target = here::here("SupplementaryTable_S5_climate_interactions.docx"))
  message("Saved: SupplementaryTable_S5_climate_interactions.docx")
}
