# Supplementary figures regenerated from the type-moderator models
# ---------------------------------------------------------------------------
#   Supplementary Fig. 9  productivity model residuals (DHARMa)
#   Supplementary Fig. 10 overall removal slope per treatment (halfeye)
#   Supplementary Fig. 11 removal-treatment model residuals (DHARMa)
#   Supplementary Fig. 12 additional-disturbances figure (built in scripts/5.2;
#                         Supplementary Fig. 13 is the PRISMA flow diagram)
# Models: model_removed_propo_typemod, model_remtrt_typemod (scripts 3.1c/4.1c)

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(DHARMa)
library(patchwork)
library(here)

rm(list = ls())
set.seed(321)

# ---- helpers ----------------------------------------------------------------
bp <- function(data, x, labx = x, xangle = 0) {
  data <- data[!is.na(data[[x]]), ]
  data[[x]] <- as.factor(data[[x]])
  ggplot(data, aes(x = .data[[x]], y = resid)) +
    geom_boxplot(outlier.size = 0.4) +
    geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed") +
    labs(x = labx, y = "Scaled residuals") +
    theme(text = element_text(size = 8, family = "Helvetica", colour = "black"),
          panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
          axis.title = element_text(face = "bold"), axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = xangle, hjust = if (xangle > 0) 1 else 0.5),
          axis.line = element_line(colour = "black"),
          plot.background = element_rect(fill = "white", color = NA))
}

resid_df <- function(m) {
  chk <- createDHARMa(
    simulatedResponse       = t(posterior_predict(m, ndraws = 1000)),
    observedResponse        = m$data$productivity,
    fittedPredictedResponse = apply(t(posterior_epred(m, ndraws = 1000)), 1, mean),
    integerResponse         = FALSE)
  m$data %>% mutate(resid = chk$scaledResiduals,
                    removed_bin = cut(removed_propo,
                                      breaks = c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf),
                                      labels = c("0-20%", "20-40%", "40-60%",
                                                 "60-80%", ">80%")))
}

# ---- Supplementary Fig. 9: productivity residuals ---------------------------
# The model data carries only removed_propo/measure, so richness, experiment
# duration and removal method are re-attached from the raw CSVs in the SAME row
# order the model was built with (biomass rows, ID_29 abundance treatments
# excluded, then cover rows).
mp <- read_rds(here("model_output", "model_removed_propo_typemod.rds"))
db <- read.csv(here("data", "df_biom_brm_2.csv")) %>%
  filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction", "abundance_richness_reduction"))) %>%
  transmute(study_ID, removed_propo, richness, experiment_duration, removal_method_category)
dc <- read.csv(here("data", "df_cover_brm_2.csv")) %>%
  transmute(study_ID, removed_propo, richness, experiment_duration, removal_method_category)
cov9 <- bind_rows(db, dc)
stopifnot(nrow(cov9) == nrow(mp$data),
          max(abs(cov9$removed_propo - mp$data$removed_propo)) < 1e-9)
rp <- resid_df(mp) %>%
  bind_cols(cov9 %>% select(richness, experiment_duration, removal_method_category)) %>%
  mutate(rich_bin = cut(richness, breaks = c(-Inf, 10, 20, 30, 40, Inf),
                        labels = c("1-10", "11-20", "21-30", "31-40", ">40")))
fig9a <- (bp(rp, "rich_bin", "Species richness") +
          bp(rp, "experiment_duration", "Experiment duration (years)") +
          bp(rp, "removal_method_category", "Removal method", xangle = 30) +
          bp(rp, "measure", "Response variable") +
          bp(rp, "removed_bin", "Proportion removed (binned)") +
          bp(rp, "study_ID", "Study ID", xangle = 90)) +
  plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)
ggsave(here("figures", "SupplementaryFigure_09_productivity_residuals.pdf"),
       fig9a, width = 180, height = 220, units = "mm", device = cairo_pdf)
message("saved Supplementary Fig. 9")
rm(mp, rp, db, dc, cov9); gc()

# ---- Supplementary Fig. 11: removal-treatment residuals ---------------------
mt <- read_rds(here("model_output", "model_remtrt_typemod.rds"))
rt <- resid_df(mt)
fig9 <- (bp(rt, "time_length_years", "Time (years)") +
         bp(rt, "removal_trt", "Removal treatment") +
         bp(rt, "measure", "Response variable") +
         bp(rt, "removed_bin", "Proportion removed (binned)") +
         bp(rt, "study_ID", "Study ID", xangle = 45)) +
  plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)
ggsave(here("figures", "SupplementaryFigure_11_removal_treatment_residuals.pdf"),
       fig9, width = 180, height = 210, units = "mm", device = cairo_pdf)
message("saved Supplementary Fig. 11")

# ---- Supplementary Fig. 10: overall removal slope per treatment -------------
d <- as_draws_df(mt)
sl <- tibble(
  Dominant    = d$b_removed_propo,
  Subordinate = d$b_removed_propo + d$`b_removed_propo:removal_trtsubordinate`,
  Traits      = d$b_removed_propo + d$`b_removed_propo:removal_trttraits`) %>%
  pivot_longer(everything(), names_to = "removal_trt", values_to = "slope") %>%
  mutate(removal_trt = factor(removal_trt, levels = c("Dominant", "Subordinate", "Traits")))
cvC <- c(Dominant = "#882255", Subordinate = "#4477AA", Traits = "#997700")

# posterior probability of a negative slope per treatment (for the caption)
sl %>% group_by(removal_trt) %>% summarise(pp_negative = round(mean(slope < 0), 3),
                                           .groups = "drop") %>% print()

fig10 <- ggplot(sl, aes(x = slope, y = removal_trt, fill = removal_trt, colour = removal_trt)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95)) +
  geom_text(data = distinct(sl, removal_trt), aes(label = removal_trt, colour = removal_trt),
            x = 0.1, y = Inf, hjust = 0, vjust = 1.2, size = 3.5, family = "Helvetica",
            inherit.aes = FALSE) +
  facet_grid(removal_trt ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = cvC) + scale_colour_manual(values = cvC) +
  coord_cartesian(xlim = c(-6, 2.5), clip = "off") +
  labs(x = "Overall slope (proportion removed, log scale)", y = NULL) +
  theme_classic() +
  theme(legend.position = "none", strip.text = element_blank(),
        strip.background = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.x = element_text(face = "bold"),
        text = element_text(size = 10, family = "Helvetica"))
ggsave(here("figures", "SupplementaryFigure_10_overall_slopes.pdf"),
       fig10, width = 110, height = 100, units = "mm", device = cairo_pdf)
message("saved Supplementary Fig. 10")

# ---- Supplementary Fig. 12: additional disturbances -------------------------
# Built in its own script (type-moderator disturbance model):
#   scripts/5.2_additional_disturbances_plot_typemod.R
#   -> figures/SupplementaryFigure_12_additional_disturbances.pdf
# (Supplementary Fig. 13 is the PRISMA flow diagram, prepared separately.)
