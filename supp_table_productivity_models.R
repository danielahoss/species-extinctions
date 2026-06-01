# Supplementary Table — Productivity model summaries
# mod_rem_propo : combined biomass + cover ~ proportion removed
# fit3_df       : productivity ~ removal treatment × proportion removed
#
# Correlations between random effects are omitted to keep the table concise;
# they are available in the full model summary via summary(mod).

# 1. Libraries ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(flextable)
library(officer)
library(here)

rm(list = ls())

# 2. Load models --------------------------------------------------------------

mod_rem_propo <- readr::read_rds(here::here("model_output_reanalysis", "model_removed_propo.rds"))
fit3_df       <- readr::read_rds(here::here("model_output_reanalysis", "fit3_inter_v2_traits.rds"))

# 3. Helper: extract fixed effects + random-effect SDs (no correlations) ------

extract_model_summary <- function(mod, model_label, include_random = TRUE) {

  # Fixed effects
  fe <- as.data.frame(fixef(mod)) %>%
    rownames_to_column("raw_param")

  if (include_random) {
    # Random effects — SDs only (drop cor() rows)
    re <- summary(mod)$random
    re_rows <- bind_rows(lapply(names(re), function(grp) {
      df  <- re[[grp]]
      rn  <- rownames(df)
      keep <- !grepl("^cor\\(", rn)
      if (!any(keep)) return(NULL)
      df  <- df[keep, , drop = FALSE]
      rn  <- rn[keep]
      data.frame(
        raw_param = paste0("sd__", grp, "__", rn),
        Estimate  = df[, "Estimate"],
        Est.Error = df[, "Est.Error"],
        Q2.5      = df[, "l-95% CI"],
        Q97.5     = df[, "u-95% CI"],
        stringsAsFactors = FALSE
      )
    }))
    fe <- bind_rows(fe, re_rows)
  }

  fe %>%
    mutate(
      model     = model_label,
      Parameter = case_when(

        # ---- fixed effects: mod_rem_propo ----
        raw_param == "Intercept"     ~ "Intercept (\u03b20)",
        raw_param == "removed_propo" ~ "Proportion removed (\u03b21, slope)",

        # ---- fixed effects: fit3_df ----
        raw_param == "removal_trtdominant"                  ~ "Intercept \u2014 dominant (\u03b2dominant)",
        raw_param == "removal_trtsubordinate"               ~ "Intercept \u2014 subordinate (\u03b2subordinate)",
        raw_param == "removal_trttraits"                    ~ "Intercept \u2014 functional group (\u03b2traits)",
        raw_param == "removed_propo:removal_trtsubordinate" ~ "Slope deviation \u2014 subordinate (\u03b21\u00d7sub)",
        raw_param == "removed_propo:removal_trttraits"      ~ "Slope deviation \u2014 functional group (\u03b21\u00d7traits)",

        # ---- random SDs: study level ----
        raw_param == "sd__study_ID__sd(Intercept)"                              ~ "\u03c4 intercept \u2014 study",
        raw_param == "sd__study_ID__sd(removed_propo)"                          ~ "\u03c4 slope \u2014 study",
        raw_param == "sd__study_ID__sd(removal_trtdominant)"                    ~ "\u03c4 intercept dominant \u2014 study",
        raw_param == "sd__study_ID__sd(removal_trtsubordinate)"                 ~ "\u03c4 intercept subordinate \u2014 study",
        raw_param == "sd__study_ID__sd(removal_trttraits)"                      ~ "\u03c4 intercept functional group \u2014 study",
        raw_param == "sd__study_ID__sd(removed_propo:removal_trtsubordinate)"   ~ "\u03c4 slope deviation subordinate \u2014 study",
        raw_param == "sd__study_ID__sd(removed_propo:removal_trttraits)"        ~ "\u03c4 slope deviation functional group \u2014 study",

        # ---- random SDs: block level ----
        raw_param == "sd__study_ID:block__sd(Intercept)"                              ~ "\u03c4 intercept \u2014 block",
        raw_param == "sd__study_ID:block__sd(removed_propo)"                          ~ "\u03c4 slope \u2014 block",
        raw_param == "sd__study_ID:block__sd(removal_trtdominant)"                    ~ "\u03c4 intercept dominant \u2014 block",
        raw_param == "sd__study_ID:block__sd(removal_trtsubordinate)"                 ~ "\u03c4 intercept subordinate \u2014 block",
        raw_param == "sd__study_ID:block__sd(removal_trttraits)"                      ~ "\u03c4 intercept functional group \u2014 block",
        raw_param == "sd__study_ID:block__sd(removed_propo:removal_trtsubordinate)"   ~ "\u03c4 slope deviation subordinate \u2014 block",
        raw_param == "sd__study_ID:block__sd(removed_propo:removal_trttraits)"        ~ "\u03c4 slope deviation functional group \u2014 block",

        # ---- random SDs: plot level ----
        raw_param == "sd__study_ID:block:plot__sd(Intercept)"                              ~ "\u03c4 intercept \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removed_propo)"                          ~ "\u03c4 slope \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removal_trtdominant)"                    ~ "\u03c4 intercept dominant \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removal_trtsubordinate)"                 ~ "\u03c4 intercept subordinate \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removal_trttraits)"                      ~ "\u03c4 intercept functional group \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removed_propo:removal_trtsubordinate)"   ~ "\u03c4 slope deviation subordinate \u2014 plot",
        raw_param == "sd__study_ID:block:plot__sd(removed_propo:removal_trttraits)"        ~ "\u03c4 slope deviation functional group \u2014 plot",

        # ---- random SDs: time ----
        grepl("time_length_years", raw_param) ~ "\u03c4 time",

        TRUE ~ raw_param
      )
    ) %>%
    select(model, Parameter, Estimate, Est.Error, Q2.5, Q97.5)
}

# 4. Build combined table -----------------------------------------------------

model_specs <- list(
  list(mod = mod_rem_propo, include_random = FALSE,
       label = "Combined productivity model\n(biomass + cover ~ proportion removed)"),
  list(mod = fit3_df,       include_random = FALSE,
       label = "Removal treatment model\n(productivity ~ treatment \u00d7 proportion removed)")
)

all_rows <- bind_rows(lapply(model_specs, function(x)
  extract_model_summary(x$mod, x$label, x$include_random)
)) %>%
  mutate(across(c(Estimate, Est.Error, Q2.5, Q97.5), ~ round(.x, 3)))

# 5. Build flextable ----------------------------------------------------------

col_labels <- c(
  model     = "Model",
  Parameter = "Parameter",
  Estimate  = "Estimate",
  Est.Error = "Posterior SD",
  Q2.5      = "Q2.5",
  Q97.5     = "Q97.5"
)

# Row indices for each model (for shading)
n_mod1 <- sum(all_rows$model == all_rows$model[1])

ft <- flextable(all_rows) %>%
  set_header_labels(values = col_labels) %>%

  # Merge model column vertically
  merge_v(j = "model") %>%

  # Alternate background between models
  bg(i = seq_len(n_mod1),                   bg = "#F7F7F7", part = "body") %>%
  bg(i = (n_mod1 + 1):nrow(all_rows),       bg = "#FFFFFF", part = "body") %>%

  # Bold header
  bold(part = "header") %>%

  # Font
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%

  # Column widths (inches)
  width(j = "model",     width = 2.2) %>%
  width(j = "Parameter", width = 2.6) %>%
  width(j = "Estimate",  width = 0.8) %>%
  width(j = "Est.Error", width = 0.85) %>%
  width(j = "Q2.5",      width = 0.7) %>%
  width(j = "Q97.5",     width = 0.7) %>%

  # Borders: line above header, below header, below last row only
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1)) %>%

  # Vertical alignment
  valign(valign = "top", part = "body") %>%

  set_caption(paste(
    "Supplementary Table. Bayesian productivity model summaries.",
    "Estimates are posterior means with posterior standard deviation (Posterior SD)",
    "and 95% credible intervals (Q2.5, Q97.5). Both models use a lognormal family.",
    "Intercept parameters are on the log scale.",
    "\u03c4 = standard deviation of the corresponding random effect.",
    "Correlations between random effects are omitted; full model summaries are",
    "available in the model output files."
  ))

ft

# 6. Save to Word -------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Supplementary Table \u2014 Productivity model summaries",
               style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = here::here("SupplementaryTable_productivity_models.docx"))

message("Saved: SupplementaryTable_productivity_models.docx")
