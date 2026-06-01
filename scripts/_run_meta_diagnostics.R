
  library(tidyverse)
  library(patchwork)
  library(brms)
  library(DHARMa)
  library(bayesplot)
  library(here)

figures_path <- here("figures", "ExtendedData")

# ── Data ------------------------------------------------------------------
effects_brm <- read.csv(here("data", "effects_brm.csv"), header = TRUE)

effects <- effects_brm %>%
  group_by(study_ID) %>%
  mutate(ES_ij = 1:n()) %>%
  ungroup() %>%
  select(study_ID, ES_ij, yi, sei)

mod_meta <- read_rds(here("model_output", "meta_brm_multi_id.rds"))

# ── 1. pp_check -----------------------------------------------------------
p_pp <- pp_check(mod_meta) +
  theme(text = element_text(size = 8, family = "Helvetica"))
# ggsave("Extended Data Fig - meta-analysis pp_check.pdf", plot = p_pp,
       # path = figures_path, width = 120, height = 90, units = "mm", device = cairo_pdf)

# ── 2. Trace plots --------------------------------------------------------
p_trace <- mcmc_plot(mod_meta, type = "trace") +
  theme(text = element_text(size = 7, family = "Helvetica"))
# ggsave("Extended Data Fig - meta-analysis trace.pdf", plot = p_trace,
#        path = figures_path, width = 200, height = 200, units = "mm", device = cairo_pdf)

# ── 3. Rhat + ESS --------------------------------------------------------
# Use posterior package to compute Rhat and ESS per variable
draws_arr <- posterior::as_draws_array(mod_meta)
rhat_vals <- posterior::rhat(draws_arr)        
ess_vals  <- posterior::ess_bulk(draws_arr)    
total_draws <- prod(dim(draws_arr)[1:2])       
ess_ratio <- ess_vals / total_draws            

p_rhat <- mcmc_rhat(rhat_vals) +
  theme(text = element_text(size = 8, family = "Helvetica"))
p_neff <- mcmc_neff(ess_ratio) +
  theme(text = element_text(size = 8, family = "Helvetica"))
# ggsave("Extended Data Fig - meta-analysis Rhat.pdf", plot = p_rhat,
#        path = figures_path, width = 100, height = 80, units = "mm", device = cairo_pdf)
# ggsave("Extended Data Fig - meta-analysis ESS.pdf", plot = p_neff,
#        path = figures_path, width = 100, height = 80, units = "mm", device = cairo_pdf)

# ── 4. DHARMa QQ ---------------------------------------------------------
model.check <- createDHARMa(
  simulatedResponse       = t(posterior_predict(mod_meta)),
  observedResponse        = effects_brm$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_meta)), 1, mean),
  integerResponse         = FALSE)

# cairo_pdf(file.path(figures_path, "Extended Data Fig - meta-analysis DHARMa_QQ.pdf"),
#           width = 7, height = 4, family = "Helvetica")
plot(model.check)
dev.off()

# ── 5. Residual boxplots --------------------------------------------------

model_data     <- mod_meta$data %>% as_tibble()
residuals_df   <- tibble(resid = model.check$scaledResiduals) %>% bind_cols(model_data)
predicted_vals <- predict(mod_meta) %>% as_tibble()

df_pad <- effects_brm %>%
  select(study_ID, experiment_duration, removal_method_category,
         response_variable, effect_type, latitude_abs,
         mean_annual_ppt_mm, mean_annual_temperature_Celsius) %>%
  mutate(
    ppt_pad  = round(scale(as.numeric(mean_annual_ppt_mm),              center = TRUE, scale = FALSE)),
    temp_pad = round(scale(as.numeric(mean_annual_temperature_Celsius), center = TRUE, scale = FALSE))
  )

residuals_df <- residuals_df %>%
  mutate(predicted = predicted_vals$Estimate) %>%
  inner_join(df_pad, by = "study_ID", relationship = "many-to-many")

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
      axis.text.x      = element_text(angle = xangle, hjust = if (xangle > 0) 1 else 0.5),
      axis.line        = element_line(colour = "black"),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

p1 <- bp(residuals_df, "experiment_duration",     "Experiment duration")
p2 <- bp(residuals_df, "removal_method_category", "Removal method") +
        scale_x_discrete(labels = c("chemical", "chemical/\nclipping",
                                    "clipping", "pulled out", "pulled out/\nchemical"))
p3 <- bp(filter(residuals_df, !is.na(mean_annual_temperature_Celsius)),
         "temp_pad", "Mean annual temp. (°C)")
p4 <- bp(residuals_df %>% mutate(lat_r = round(latitude_abs, 0)),
         "lat_r", "Absolute latitude")
p5 <- bp(residuals_df, "ppt_pad",           "Mean annual precip. (mm)")
p6 <- bp(residuals_df, "study_ID",          "Study ID", xangle = 45)
p7 <- bp(residuals_df, "response_variable", "Response variable")
p8 <- bp(residuals_df, "effect_type",       "Effect type")

p_resid <- (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 +
  plot_annotation(tag_levels = "a") + plot_layout(ncol = 2))

# ggsave("Extended Data Fig. 7 - meta-analysis residuals.pdf", plot = p_resid,
#         path = figures_path, width = 180, height = 240, units = "mm", device = cairo_pdf)

# ── 6. Funnel plot --------------------------------------------------------

post     <- as_draws_df(mod_meta)
mu_mean  <- mean(post$b_Intercept)
mu_ci95  <- quantile(post$b_Intercept, c(0.025, 0.975))
tau_mean <- mean(post$sd_study_ID__Intercept)

sei_seq  <- seq(0, max(effects$sei) * 1.05, length.out = 500)
contours <- tibble(
  sei   = sei_seq,
  lo95  = mu_mean - 1.96  * sei_seq,
  hi95  = mu_mean + 1.96  * sei_seq,
  lo99  = mu_mean - 2.576 * sei_seq,
  hi99  = mu_mean + 2.576 * sei_seq,
  lo_pi = mu_mean - 1.96  * sqrt(sei_seq^2 + tau_mean^2),
  hi_pi = mu_mean + 1.96  * sqrt(sei_seq^2 + tau_mean^2)
)

funnel_plot <- ggplot() +
  geom_ribbon(data = contours, aes(y = sei, xmin = lo99, xmax = hi99),
              orientation = "y", fill = "#D9D9D9", alpha = 0.9) +
  geom_ribbon(data = contours, aes(y = sei, xmin = lo95, xmax = hi95),
              orientation = "y", fill = "#BDBDBD", alpha = 0.9) +
  geom_ribbon(data = contours, aes(y = sei, xmin = lo_pi, xmax = hi_pi),
              orientation = "y", fill = "#9ECAE1", alpha = 0.25,
              linetype = "dashed", colour = "#2171B5", linewidth = 0.3) +
  annotate("rect", ymin = -Inf, ymax = Inf,
           xmin = mu_ci95[1], xmax = mu_ci95[2],
           fill = "#4292C6", alpha = 0.15) +
  geom_vline(xintercept = 0,       linetype = "dotted", colour = "grey50",  linewidth = 0.4) +
  geom_vline(xintercept = mu_mean, linetype = "dashed", colour = "#2171B5", linewidth = 0.5) +
  geom_point(data = effects, aes(x = yi, y = sei), size = 1.8, alpha = 0.8) +
  scale_y_reverse(limits = c(max(effects$sei) * 1.05, 0), expand = c(0, 0)) +
  labs(x = "Log response ratio", y = "Standard error") +
  theme(
    text             = element_text(size = 8, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid       = element_blank(),
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(colour = "black"),
    axis.line        = element_line(colour = "black"),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# ggsave("Extended Data Fig - meta-analysis funnel plot.pdf", plot = funnel_plot,
#        path = figures_path, width = 100, height = 100, units = "mm", device = cairo_pdf)


