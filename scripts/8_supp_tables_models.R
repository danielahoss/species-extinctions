# Supplementary model-summary tables (Round 3, type-moderator approach)
# ---------------------------------------------------------------------------
# Produces:
#   S3  Productivity model (type-moderator)            -> ..._S3_productivity_model.docx
#   S4  Separate biomass & cover models (sensitivity)  -> ..._S4_separate_models.docx
#   S7  Removal-treatment model (type-moderator)       -> ..._S7_removal_treatment_model.docx
#   S8  Additional-disturbances model (type-moderator) -> ..._S8_additional_disturbances_model.docx
# Fixed effects + residual-variance (sigma) parameters; random-effect SDs are
# omitted for concision (full summaries in the model_output .rds files).

library(tidyverse)
library(brms)
library(flextable)
library(officer)
library(here)

rm(list = ls())

# ---- helper: tidy fixed effects (incl. sigma_*) of one model ----------------
fx_tbl <- function(mod, label) {
  as.data.frame(fixef(mod)) %>%
    rownames_to_column("param") %>%
    transmute(model = label, param,
              Estimate = round(Estimate, 3), `Posterior SD` = round(Est.Error, 3),
              Q2.5 = round(Q2.5, 3), Q97.5 = round(Q97.5, 3))
}

relabel <- function(p) {
  dplyr::case_when(
    p == "Intercept"                            ~ "Intercept, log scale (β₀)",
    p == "removed_propo"                        ~ "Proportion removed (slope)",
    p == "measurecover"                         ~ "Cover vs biomass (mean offset)",
    p == "removed_propo:measurecover"           ~ "Proportion removed × cover",
    p == "sigma_Intercept"                      ~ "Residual log-SD, biomass (σ)",
    p == "sigma_measurecover"                   ~ "Residual log-SD, cover vs biomass",
    p == "removal_trtdominant"                  ~ "Intercept: dominant",
    p == "removal_trtsubordinate"               ~ "Intercept: subordinate",
    p == "removal_trttraits"                    ~ "Intercept: trait-based",
    p == "removed_propo:removal_trtsubordinate" ~ "Slope deviation: subordinate",
    p == "removed_propo:removal_trttraits"      ~ "Slope deviation: trait-based",
    p == "add_distdrought"                      ~ "Intercept offset: drought",
    p == "add_distnutrient"                     ~ "Intercept offset: nutrient",
    p == "add_distwarming"                      ~ "Intercept offset: warming",
    p == "add_distdrought:removed_propo"        ~ "Slope deviation: drought",
    p == "add_distnutrient:removed_propo"       ~ "Slope deviation: nutrient",
    p == "add_distwarming:removed_propo"        ~ "Slope deviation: warming",
    TRUE ~ p
  )
}

pref_order <- c("Intercept, log scale (β₀)",
                "Intercept: dominant", "Intercept: subordinate", "Intercept: trait-based",
                "Intercept offset: drought", "Intercept offset: nutrient",
                "Intercept offset: warming",
                "Proportion removed (slope)",
                "Slope deviation: subordinate", "Slope deviation: trait-based",
                "Slope deviation: drought", "Slope deviation: nutrient",
                "Slope deviation: warming",
                "Cover vs biomass (mean offset)", "Proportion removed × cover",
                "Residual log-SD, biomass (σ)", "Residual log-SD, cover vs biomass")

style_ft <- function(dat, has_model_col = TRUE) {
  dat <- dat %>% mutate(Parameter = relabel(param),
                        .ord = match(Parameter, pref_order),
                        .ord = ifelse(is.na(.ord), 99, .ord)) %>%
    arrange(model, .ord)
  keep <- if (has_model_col)
    c("model", "Parameter", "Estimate", "Posterior SD", "Q2.5", "Q97.5")
  else c("Parameter", "Estimate", "Posterior SD", "Q2.5", "Q97.5")
  dat <- dat %>% select(all_of(keep))
  ft <- flextable(dat) %>%
    bold(part = "header") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 1)) %>%
    valign(valign = "top", part = "body") %>%
    width(j = "Parameter", width = 2.6) %>%
    width(j = c("Estimate", "Posterior SD", "Q2.5", "Q97.5"), width = 0.85)
  if (has_model_col) ft <- ft %>% merge_v(j = "model") %>% width(j = "model", width = 1.7)
  ft
}

save_tbl <- function(ft, heading, file) {
  read_docx() %>%
    body_add_par(heading, style = "heading 1") %>%
    body_add_flextable(ft) %>%
    print(target = here(file))
  message("Saved: ", file)
}

# ============================================================================
# S3  Productivity model (type-moderator)
# ============================================================================
m_prod <- read_rds(here("model_output", "model_removed_propo_typemod.rds"))
ftS3 <- fx_tbl(m_prod, "prod") %>% style_ft(has_model_col = FALSE) %>%
  set_caption(paste(
    "Supplementary Table S3. Productivity model (measurement type as a predictor",
    "of the mean and variance). Lognormal family; measurement type (biomass vs.",
    "cover) enters the mean, interacts with proportion removed, and is allowed its",
    "own residual variance (σ ~ measurement type). Estimates are posterior means",
    "with posterior SD and 95% credible intervals (Q2.5, Q97.5); intercept and slope",
    "are on the log scale. Random-effect SDs are omitted for concision."))
save_tbl(ftS3, "Supplementary Table S3: Productivity model",
         "SupplementaryTable_S3_productivity_model.docx")

# ============================================================================
# S4  Separate biomass & cover models (sensitivity)
# ============================================================================
m_b <- read_rds(here("model_output", "model_biomass_2.rds"))
m_c <- read_rds(here("model_output", "model_cover_2.rds"))
ftS4 <- bind_rows(fx_tbl(m_b, "Biomass"), fx_tbl(m_c, "Cover")) %>%
  style_ft(has_model_col = TRUE) %>%
  set_caption(paste(
    "Supplementary Table S4. Separate biomass and cover models (sensitivity",
    "analysis for Comments 2.2 and 2.3). Each response type is fit in its own",
    "lognormal model with the same structure. The proportion-removed slopes are",
    "indistinguishable between response types, matching the combined model (S3).",
    "Estimates are posterior means with 95% credible intervals; log scale."))
save_tbl(ftS4, "Supplementary Table S4: Separate biomass and cover models",
         "SupplementaryTable_S4_separate_models.docx")

# ============================================================================
# S6  Removal-treatment model (type-moderator)
# ============================================================================
m_trt <- read_rds(here("model_output", "model_remtrt_typemod.rds"))
ftS7 <- fx_tbl(m_trt, "trt") %>% style_ft(has_model_col = FALSE) %>%
  set_caption(paste(
    "Supplementary Table S7. Removal-treatment model (measurement type as a",
    "predictor of the mean and variance). The selected structure (proportion",
    "removed × removal treatment; dominant is the reference) with measurement type",
    "added to the mean and variance. Dominant is the reference treatment, so the",
    "'proportion removed' slope is the dominant-removal slope and the deviations are",
    "relative to it. Log scale; random-effect SDs omitted for concision."))
save_tbl(ftS7, "Supplementary Table S7: Removal-treatment model",
         "SupplementaryTable_S7_removal_treatment_model.docx")

# ============================================================================
# S8  Additional-disturbances model (type-moderator)
# ============================================================================
m_dist <- read_rds(here("model_output", "model_dist_typemod.rds"))
ftS8 <- fx_tbl(m_dist, "dist") %>% style_ft(has_model_col = FALSE) %>%
  set_caption(paste(
    "Supplementary Table S8. Additional-disturbances model (measurement type as a",
    "predictor of the mean and variance). Productivity as a function of proportion",
    "removed interacting with the additional disturbance applied (no additional",
    "disturbance is the reference; drought, nutrient addition and warming enter as",
    "offsets), with measurement type added to the mean and variance. The",
    "'proportion removed' slope is therefore the slope with no additional",
    "disturbance, and the deviations are relative to it. Estimates are posterior",
    "means with posterior SD and 95% credible intervals (Q2.5, Q97.5); intercept",
    "and slopes are on the log scale. Random-effect SDs are omitted for concision."))
save_tbl(ftS8, "Supplementary Table S8: Additional-disturbances model",
         "SupplementaryTable_S8_additional_disturbances_model.docx")
