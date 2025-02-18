# Meta-Analysis 
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(patchwork)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = TRUE) 
 

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



# 3. Model ----------------------------------------------------------------
# prior only
# set.seed(2024)
# mod_meta_prior <-
#   brm(data = effects_brm,
#       yi | se(sei) ~ 1 + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4,
#       sample_prior = "only", file = "meta_brm_prior")

mod_meta_prior <- read_rds(here::here("model_output", "meta_brm_prior.rds"))

pp_check(mod_meta_prior)

# View summary
print(prior_summary)


# set.seed(2024)
# mod_meta <-
#   brm(data = effects_brm,
#       yi | se(sei) ~ 1 + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4, file = "meta_brm")
mod_meta <- read_rds(here::here("model_output", "meta_brm.rds"))
pp_check(mod_meta)
# 4. Model output and diagnostics -----------------------------------------

mod_meta %>% summary()

pp_check(mod_meta) # Posterior predictive check

# No divergences to plot
mcmc_plot(mod_meta, type = 'trace')


# 5. Residual diagnostics using DHARMa ------------------------------------

model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_meta)),
  observedResponse = effects_brm$yi,
  fittedPredictedResponse = apply(t(posterior_epred(mod_meta)), 1, mean),
  integerResponse = TRUE)


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


# add predicted values to residual df
residuals_model <- residuals_model %>%
  mutate(predicted = predicted$Estimate) %>%
  as_tibble() %>%
  inner_join(df_pad ,
             by = c("study_ID"),relationship =
               "many-to-many")



# 6. plot DHARMA residual checks ------------------------------------------
# pdf("residuals_biomass.pdf")  # Open a PDF
#   par(mfrow=c(3,1))



boxplot_residuals <- function(data, x, labx = x, laby = "Scaled residuals") {
  # Get x variable
  x_var <- as.name(x)
  
  # Ensure x is treated as a factor
  data[[x]] <- as.factor(data[[x]])
  
  # Create the plot
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
  
  # Return the plot
  return(plot)
}
# plotResiduals(model.check, form = df_pad$study_ID)
plot1 <- boxplot_residuals(data = residuals_model, x = "study_ID" ) 

# plotResiduals(model.check, form = df_pad$time_pad)
plot2 <-boxplot_residuals(data = residuals_model, x = "experiment_duration", 
                          labx = "Experiment duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$removal_method_category)
plot3 <- boxplot_residuals(data = residuals_model, x = "removal_method_category", 
                  labx = "Removal method") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$ppt_pad)
plot4 <- boxplot_residuals(data = residuals_model, x = "ppt_pad", 
labx = "Mean Annual Precipitation (mm) - centered values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$temp_pad)
residuals_model_temp <- residuals_model %>% filter(!is.na(mean_annual_temperature_Celsius))
plot5 <- boxplot_residuals(data = residuals_model_temp, x = "temp_pad", 
labx = "Mean Annual Temperature (ºC) - centered values") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_pad$latitude_abs)
plot6 <- residuals_model %>% mutate(
  latitude_abs_round = round(latitude_abs,0)
) %>% 
boxplot_residuals( x = "latitude_abs_round", 
labx = "Absolute latitude") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
# plotResiduals(model.check, form = df_pad$lat_pad)
plot7 <- boxplot_residuals(data = residuals_model, x = "lat_pad", 
labx = "Absolute latitude - centered values") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



 
 layout <- "
AB
CD
EF
GG
"
 
 (plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + 
     plot1 + 
     plot_annotation(tag_levels = 'a')) +
   plot_layout(design = layout)
 
 
# ggsave("Fig1_extended_Data_residuals.pdf",  path = ("figures"), width = 180, height = 170, units = 'mm')

# dev.off()





