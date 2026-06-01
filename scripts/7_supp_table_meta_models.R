# Supplementary Table — Meta-analysis model summaries
# Extracts fixed effects and random-effect SDs for all meta-analytic models
# and writes a formatted Word document via flextable + officer.

# 1. Libraries ----------------------------------------------------------------

library(tidyverse)
library(brms)
library(flextable)
library(officer)
library(here)

rm(list = ls())

# 2. Load models --------------------------------------------------------------

mod_meta   <- read_rds(here::here("model_output", "meta_brm_multi_id.rds"))
mod_ppt    <- read_rds(here::here("model_output", "ma_moderators_ppt.rds"))
mod_temp   <- read_rds(here::here("model_output", "ma_moderators_temp.rds"))
mod_lat    <- read_rds(here::here("model_output", "ma_moderators_lat.rds"))
mod_resp   <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds"))
mod_time   <- read_rds(here::here("model_output", "ma_moderators_time.rds"))
mod_covars <- read_rds(here::here("model_output", "ma_moderators_covariates.rds"))

# 3. Helper: extract fixed + random effects -----------------------------------

extract_model_summary <- function(mod, model_label) {

  # Fixed effects
  fe <- as.data.frame(fixef(mod)) %>%
    rownames_to_column("raw_param")

  fe %>%
    mutate(
      model     = model_label,
      Parameter = case_when(
        raw_param == "Intercept"                                                  ~ "Intercept (\u03b20)",
        raw_param == "ppt_pad"                                                    ~ "Precipitation (centered, mm/yr)",
        raw_param == "temp_pad"                                                   ~ "Temperature (centered, \u00b0C)",
        raw_param == "lat_pad"                                                    ~ "Latitude (centered, \u00b0)",
        raw_param == "latitude_abs"                                               ~ "Latitude (absolute, \u00b0)",
        raw_param == "time_pad"                                                   ~ "Experiment duration (centered, yr)",
        raw_param == "response_variablebiomass"                                   ~ "Response variable: biomass",
        raw_param == "response_variablecover"                                     ~ "Response variable: cover",
        grepl("sd__study_ID__", raw_param) & !grepl("ES_ij", raw_param)          ~ "\u03c4 study",
        grepl("sd__study_ID:ES_ij__", raw_param)                                  ~ "\u03c4 effect size (within study)",
        TRUE                                                                      ~ raw_param
      )
    ) %>%
    select(model, Parameter, Estimate, Est.Error, Q2.5, Q97.5)
}

# 4. Build combined table -----------------------------------------------------

model_specs <- list(
  list(mod = mod_meta,   label = "Overall effect (no moderator)"),
  list(mod = mod_ppt,    label = "Moderator: precipitation"),
  list(mod = mod_temp,   label = "Moderator: temperature"),
  list(mod = mod_lat,    label = "Moderator: latitude"),
  list(mod = mod_resp,   label = "Moderator: response variable"),
  list(mod = mod_time,   label = "Moderator: experiment duration"),
  list(mod = mod_covars, label = "Moderators: latitude + precipitation + temperature")
)

all_rows <- bind_rows(lapply(model_specs, function(x)
  extract_model_summary(x$mod, x$label)
))

# Round numerics
all_rows <- all_rows %>%
  mutate(across(c(Estimate, Est.Error, Q2.5, Q97.5), ~ round(.x, 3)))

# 5. Build flextable ----------------------------------------------------------

# Column display names
col_labels <- c(
  model     = "Model",
  Parameter = "Parameter",
  Estimate  = "Estimate",
  Est.Error = "Posterior SD",
  Q2.5      = "Q2.5",
  Q97.5     = "Q97.5"
)

ft <- flextable(all_rows) %>%
  set_header_labels(values = col_labels) %>%

  # Merge repeated model cells vertically
  merge_v(j = "model") %>%

  # Bold header
  bold(part = "header") %>%

  # Font
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%

  # Column widths (inches)
  width(j = "model",     width = 2.4) %>%
  width(j = "Parameter", width = 2.4) %>%
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

  # Caption
  set_caption("Supplementary Table. Bayesian meta-analytic model summaries. Estimates are posterior means with posterior standard deviation (SD) and 95% credible intervals (Q2.5, Q97.5). All models use a normal likelihood with known sampling variance se(sei). Random effects represent between-study (\u03c4 study) and within-study between-effect-size (\u03c4 effect size) standard deviations.")

ft

# 6. Save to Word -------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Supplementary Table — Meta-analysis model summaries",
               style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = here::here("SupplementaryTable_meta_models.docx"))

message("Saved: SupplementaryTable_meta_models.docx")
