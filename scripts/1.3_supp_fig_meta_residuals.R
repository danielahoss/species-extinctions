# Supplementary Fig. 7: meta-analysis residuals (readable precip + study-ID axes)
suppressMessages({library(tidyverse); library(brms); library(DHARMa); library(patchwork); library(here)})

set.seed(321)

mod_meta    <- readRDS(here("model_output", "meta_brm_multi_id.rds"))
effects_brm <- read.csv(here("data", "effects_brm.csv"))

chk <- createDHARMa(
  simulatedResponse       = t(posterior_predict(mod_meta)),
  observedResponse        = effects_brm$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_meta)), 1, mean),
  integerResponse         = FALSE)

residuals_df <- tibble(resid = chk$scaledResiduals) %>%
  bind_cols(as_tibble(mod_meta$data)) %>%
  inner_join(
    effects_brm %>% select(study_ID, experiment_duration, removal_method_category,
                           response_variable, effect_type, latitude_abs,
                           mean_annual_ppt_mm, mean_annual_temperature_Celsius) %>%
      mutate(temp_pad = round(scale(mean_annual_temperature_Celsius,
                                    center = TRUE, scale = FALSE))),
    by = "study_ID", relationship = "many-to-many")

bp <- function(data, x, labx = x, xangle = 0, xsize = 8) {
  data[[x]] <- as.factor(data[[x]])
  ggplot(data, aes(x = .data[[x]], y = resid)) +
    geom_boxplot(outlier.size = 0.4) +
    geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed") +
    labs(x = labx, y = "Scaled residuals") +
    theme(text = element_text(size = 8, family = "Helvetica", colour = "black"),
          panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
          axis.title = element_text(face = "bold"), axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = xangle, hjust = if (xangle > 0) 1 else 0.5,
                                     size = xsize),
          axis.line = element_line(colour = "black"),
          plot.background = element_rect(fill = "white", color = NA))
}

p1 <- bp(residuals_df, "experiment_duration", "Experiment duration")
p2 <- bp(residuals_df, "removal_method_category", "Removal method") +
  scale_x_discrete(labels = c("chemical", "chemical/\nclipping", "clipping",
                              "pulled out", "pulled out/\nchemical"))
p3 <- bp(filter(residuals_df, !is.na(mean_annual_temperature_Celsius)),
         "temp_pad", "Mean annual temp. (°C)")
p4 <- bp(residuals_df %>% mutate(lat_r = round(latitude_abs, 0)), "lat_r", "Absolute latitude")
p5 <- bp(residuals_df %>% mutate(ppt_bin = cut(mean_annual_ppt_mm,
           breaks = c(-Inf, 400, 600, 800, 1000, Inf),
           labels = c("<400", "400-600", "600-800", "800-1000", ">1000"))),
         "ppt_bin", "Mean annual precip. (mm)")
p6 <- bp(residuals_df, "study_ID", "Study ID", xangle = 90, xsize = 5)
p7 <- bp(residuals_df, "response_variable", "Response variable")
p8 <- bp(residuals_df, "effect_type", "Effect type")

p_resid <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 +
  plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)

ggsave(here("figures", "SupplementaryFigure_07_meta_residuals.pdf"),
       p_resid, width = 180, height = 240, units = "mm", device = cairo_pdf)
message("saved Fig 7")
