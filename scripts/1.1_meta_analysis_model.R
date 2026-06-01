# Meta-Analysis 
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(patchwork)
library(brms)
library(DHARMa)
library(here)

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

effects_brm <- read.csv(here::here( "data", "effects_brm.csv"), header = TRUE) 

effects <- effects_brm %>% 
  ungroup() %>% 
  group_by(study_ID) %>% 
  mutate(ES_ij = 1:n()) %>% 
  ungroup() %>% 
  select(study_ID, ES_ij, yi, sei)

# 3. Model ----------------------------------------------------------------


mod_meta <-
  brm(data = effects,
      yi | se(sei) ~ 1 +(1 | study_ID/ES_ij),
      cores = 4, chains = 4,
      backend = 'cmdstanr',
      seed = 321,
      file = "model_output/meta_brm_multi_id")

mod_meta <- read_rds(here::here("model_output", "meta_brm_multi_id.rds"))
sqrt(0.32)
(1-exp(-0.11))*100    
(1-exp(-0.20))*100      
(1-exp(-0.01))*100   

-0.56*0.5
(1-(exp(-0.11*0.5)))*100
exp(-0.28)
0.7557837-1
# 4. Model output and diagnostics -----------------------------------------

mod_meta %>% summary()

mod_meta %>% get_prior()

pp_check(mod_meta) # Sample data from the generative model and compare with real data

mcmc_plot(mod_meta, type = 'trace') # No divergences to plot


# 5. Residual diagnostics using DHARMa ------------------------------------

model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_meta)),
  observedResponse = effects_brm$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_meta)), 1, mean),
  integerResponse = FALSE)


plot(model.check)  # qq and residuals



testDispersion(model.check)



model_data <- mod_meta$data %>%
  as_tibble()

residuals_model <- model.check$scaledResiduals %>%
  as_tibble() %>%
  bind_cols(model_data) %>%
  rename(resid = value) 

predicted <- predict(mod_meta) %>%
  as_tibble()

# standardizing variables
df_pad <- effects_brm %>% 
  select(study_ID,                        
         experiment_duration,   
         removal_method_category,
         response_variable,
         effect_type,
         country,                        
         latitude,                        
         latitude_abs,                   
         longitude,                       
         altitude_m,                     
         mean_annual_ppt_mm,              
         mean_annual_temperature_Celsius,
         author,                          
         publ_year,                      
         eff_size_ctrl,                   
         eff_size_rem,                   
         sd_eff_size_ctrl,                
         sd_eff_size_rem,                
         n_control,                       
         n_removal,                      
         se_eff_size_ctrl,                
         se_eff_size_rem) %>%
  mutate(
    lat_pad = round(scale(as.numeric(latitude_abs), center = TRUE, scale = FALSE)),
    lon_pad = round(scale(as.numeric(longitude), center = TRUE, scale = FALSE)),
    ppt_pad = round(scale(as.numeric(mean_annual_ppt_mm), center = TRUE, scale = FALSE)),
    temp_pad = round(scale(as.numeric(mean_annual_temperature_Celsius), center = TRUE, scale = FALSE)),
    alt_pad = round(scale(as.numeric(altitude_m), center = TRUE, scale = FALSE)),
    time_pad = as.numeric(scale(as.numeric(experiment_duration), center = FALSE, scale = FALSE)))

# add predicted values to residual df
residuals_model <- residuals_model %>%
  mutate(predicted = predicted$Estimate) %>%
  as_tibble() %>%
  inner_join(df_pad ,
             by = c("study_ID"),relationship =
               "many-to-many")



# 6. plot DHARMA residual checks ------------------------------------------

boxplot_residuals <- function(data, x, labx = x, laby = "Scaled residuals") {
  x_var <- as.name(x)
  
  data[[x]] <- as.factor(data[[x]])
  
  plot <- ggplot(data, aes(x = !!x_var, y = resid)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.25, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.75, linetype = "dashed") +
    labs(x = labx, y = laby) +
    theme(
      text = element_text(size = 8, family = "Helvetica", colour = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(colour = "black"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(plot)
}

# plotResiduals(model.check, form = df_pad$study_ID)
plot1 <- boxplot_residuals(data = residuals_model, x = "study_ID" ,
                           labx = "Study ID") 

# plotResiduals(model.check, form = df_pad$time_pad)
plot2 <-boxplot_residuals(data = residuals_model, x = "experiment_duration", 
                          labx = "Experiment duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$removal_method_category)
plot3 <- boxplot_residuals(data = residuals_model, x = "removal_method_category", 
                           labx = "Removal method") +
  scale_x_discrete(labels = c("chemical", "chemical/\nclipping", 
                              "clipping","pulled out", "pulled out/\nchemical")) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  

# plotResiduals(model.check, form = df_pad$ppt_pad)
plot4 <- boxplot_residuals(data = residuals_model, x = "ppt_pad", 
                           labx = "Mean annual orecipitation (mm) - centered values") +
  theme(axis.text.x = element_text(angle =0 , hjust = 0.5))

# plotResiduals(model.check, form = df_pad$temp_pad)
residuals_model_temp <- residuals_model %>% filter(!is.na(mean_annual_temperature_Celsius))
plot5 <- boxplot_residuals(data = residuals_model_temp, x = "temp_pad", 
                           labx = "Mean annual temperature (ºC) - centered values") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$latitude_abs)
plot6 <- residuals_model %>% mutate(
  latitude_abs_round = round(latitude_abs,0)) %>% 
  boxplot_residuals( x = "latitude_abs_round", 
                     labx = "Absolute latitude") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


layout <- "
AB
CD
EE
FF
"

(plot2 + plot3 +  plot5 + plot6  + 
    plot4 + plot1 +
    plot_annotation(tag_levels = 'a')) +
  plot_layout(design = layout)


# ggsave("Extended Data Fig 7- meta-analysis residuals.pdf",  path = ("figures"), width = 180, height = 170, units = 'mm')

# dev.off()


# 7. Bayesian Funnel Plot & Egger's Test ----------------------------------
#
# Purpose: assess publication bias / small-study effects.
# The funnel plot shows observed effect sizes (yi) vs. their standard error
# (sei, inverted so precise studies are at the top). Under no publication
# bias, points should scatter symmetrically around the pooled effect within
# the sampling-error contours.
#
# Three reference regions are shown:
#   - Dark grey  : 95 % region (expected scatter from sampling error alone)
#   - Light grey : 99 % region
#   - Blue band  : 95 % posterior credible interval of the overall effect (mu)
#
# The Bayesian Egger's test adds sei as a predictor. A non-zero coefficient
# for sei (95 % CI excluding 0) indicates funnel asymmetry / publication bias.


# -- 7a. Extract posteriors -----------------------------------------------

post <- brms::as_draws_df(mod_meta)

mu_draws  <- post$b_Intercept
tau_draws <- post$sd_study_ID__Intercept   # between-study heterogeneity (tau)

mu_mean  <- mean(mu_draws)
mu_ci95  <- quantile(mu_draws, c(0.025, 0.975))
tau_mean <- mean(tau_draws)



# -- 7b. Build funnel contour data ----------------------------------------

sei_seq <- seq(0, max(effects$sei) * 1.05, length.out = 500)

contours <- tibble(
  sei    = sei_seq,
  # Sampling-error-only contours (assume no heterogeneity)
  lo95   = mu_mean - 1.96  * sei_seq,
  hi95   = mu_mean + 1.96  * sei_seq,
  lo99   = mu_mean - 2.576 * sei_seq,
  hi99   = mu_mean + 2.576 * sei_seq,
  # 95 % prediction interval — accounts for between-study tau
  lo_pi  = mu_mean - 1.96  * sqrt(sei_seq^2 + tau_mean^2),
  hi_pi  = mu_mean + 1.96  * sqrt(sei_seq^2 + tau_mean^2)
)


# -- 7c. Bayesian funnel plot ---------------------------------------------

funnel_plot <- ggplot() +
  # 99 % sampling-error region (outermost, lightest)
  geom_ribbon(data = contours,
              aes(y = sei, xmin = lo99, xmax = hi99),
              orientation = "y",
              fill = "#D9D9D9", alpha = 0.9) +
  # 95 % sampling-error region
  geom_ribbon(data = contours,
              aes(y = sei, xmin = lo95, xmax = hi95),
              orientation = "y",
              fill = "#BDBDBD", alpha = 0.9) +
  # 95 % prediction interval (includes tau — shows expected scatter under the
  # fitted heterogeneity, not just sampling error)
  geom_ribbon(data = contours,
              aes(y = sei, xmin = lo_pi, xmax = hi_pi),
              orientation = "y",
              fill = "#9ECAE1", alpha = 0.25, linetype = "dashed",
              colour = "#2171B5", linewidth = 0.3) +
  # Posterior 95 % CI of the overall effect (vertical band)
  annotate("rect",
           ymin = -Inf, ymax = Inf,
           xmin = mu_ci95[1], xmax = mu_ci95[2],
           fill = "#4292C6", alpha = 0.15) +
  # Null effect reference
  geom_vline(xintercept = 0,
             linetype = "dotted", colour = "grey50", linewidth = 0.4) +
  # Posterior mean overall effect
  geom_vline(xintercept = mu_mean,
             linetype = "dashed", colour = "#2171B5", linewidth = 0.5) +
  # Observed effect sizes
  geom_point(data = effects, aes(x = yi, y = sei),
             size = 1.8, alpha = 0.8, colour = "black") +
  # SE axis: 0 (most precise) at top, max at bottom
  scale_y_reverse(
    limits = c(max(effects$sei) * 1.05, 0),
    expand = c(0, 0)
  ) +
  labs(
    x = "Log response ratio",
    y = "Standard error"
  ) +
  theme(
    text            = element_text(size = 8, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title      = element_text(face = "bold"),
    axis.text       = element_text(colour = "black"),
    axis.line       = element_line(colour = "black"),
    plot.background = element_rect(fill = "white", color = NA)
  )

funnel_plot

# ggsave("Fig1_supp_funnel_plot.pdf", path = "figures", width = 100, height = 100, units = "mm")


