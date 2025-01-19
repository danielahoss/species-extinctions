# Meta-Analysis 
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
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
    lat_pad = round(as.numeric(scale(as.numeric(latitude_abs), center = TRUE, scale = FALSE)),2),
    lon_pad = round(as.numeric(scale(as.numeric(longitude), center = TRUE, scale = FALSE)),2),
    ppt_pad = round(as.numeric(scale(as.numeric(mean_annual_ppt_mm), center = TRUE, scale = FALSE)),2),
    temp_pad = round(as.numeric(scale(as.numeric(mean_annual_temperature_Celsius), center = TRUE, scale = FALSE)),2),
    alt_pad = round(as.numeric(scale(as.numeric(altitude_m), center = TRUE, scale = FALSE)),0),
    time_pad = as.numeric(scale(as.numeric(experiment_duration), center = TRUE, scale = FALSE)))



# 3. Model ----------------------------------------------------------------

# set.seed(2024)
# mod_meta <-
#   brm(data = effects_brm,
#       yi | se(sei) ~ 1 + (1 | study_ID),
#       iter = 4000, warmup = 1000,
#       cores = 4, chains = 4, file = "meta_brm")
mod_meta <- read_rds(here::here("model_output", "meta_brm.rds"))

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

boxplot_residuals <- function(data, x, laby = "Scaled residuals") {
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
    labs(x = x, y = laby) +
    theme(
      text = element_text(size = 10, family = "Helvetica", colour = "black"),
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

# pdf("residuals_biomass.pdf")  # Open a PDF
#   par(mfrow=c(3,1))



# plotResiduals(model.check, form = df_pad$study_ID)
boxplot_residuals(data = residuals_model, x = "study_ID" ) 

# plotResiduals(model.check, form = df_pad$time_pad)
boxplot_residuals(data = residuals_model, x = "experiment_duration")

# plotResiduals(model.check, form = df_pad$removal_method_category)
boxplot_residuals(data = residuals_model, x = "removal_method_category")

# plotResiduals(model.check, form = df_pad$ppt_pad)
boxplot_residuals(data = residuals_model, x = "ppt_pad")

# plotResiduals(model.check, form = df_pad$temp_pad)
boxplot_residuals(data = residuals_model, x = "temp_pad")

# plotResiduals(model.check, form = df_pad$latitude_abs)
boxplot_residuals(data = residuals_model, x = "latitude_abs")

# plotResiduals(model.check, form = df_pad$lat_pad)
boxplot_residuals(data = residuals_model, x = "lat_pad")

# dev.off()

# plot DHARMA residual checks -----------------------------------


# pdf("model_meta_resid1.pdf")  # Open a PDF
#   par(mfrow=c(3,1))
  #with(residuals_model, boxplot(resid ~ study_ID, xlab = "Studies"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
  #plotResiduals(model.check, form = effects_brm$study_ID)
  plot1 <- ggplot(residuals_model, aes(x = study_ID, y = resid)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.25, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.75, linetype = "dashed") +
    labs(x = "Studies", y = "Scaled residuals") +
     theme(
      text = element_text(size = 10, family = "Helvetica", colour = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      axis.title = element_text(face="bold"),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(colour = "black"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA))
  
  
# with(residuals_model, boxplot(resid ~ round(latitude_abs,2), xlab = "Absolute Latitude"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$latitude_abs))
  plot2 <- ggplot(residuals_model, aes(x = as.factor(round(latitude_abs,2)), y = resid)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.25, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.75, linetype = "dashed") +
    labs(x = "Absolute Latitude", y = "Scaled residuals") +
    theme(
      text = element_text(size = 10, family = "Helvetica", colour = "black"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      axis.title = element_text(face="bold"),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(colour = "black"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA))
  
  
# with(residuals_model, boxplot(resid ~ lon_pad, xlab = "Longitude (centered values)"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(df_pad$lon_pad) ) 
plot3 <- ggplot(residuals_model, aes(x = as.factor(round(longitude,2)), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Longitude", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))

# with(residuals_model, boxplot(resid ~ alt_pad, xlab = "Altitude (centered values)"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$altitude_m))
filtered_alt_pad <- residuals_model %>% 
  filter(!is.na(alt_pad)) # Filter out NA values in temp_pad
plot4 <- ggplot(filtered_alt_pad, aes(x = as.factor(altitude_m), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Altitude (m)", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))


# with(residuals_model, boxplot(resid ~ ppt_pad, 
#                                    xlab = "Mean annual precipitation (mm, Centered)"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$mean_annual_ppt_mm))             
plot5 <- ggplot(residuals_model, aes(x = as.factor(ppt_pad), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Mean annual precipitation (mm, Centered)", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))


# with(residuals_model, boxplot(resid ~ temp_pad, xlab = "Mean annual temperature (ºC, Centered)"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$mean_annual_temperature_Celsius))

filtered_temp_pad <- residuals_model %>% 
  filter(!is.na(temp_pad)) # Filter out NA values in temp_pad

plot6 <- ggplot(filtered_temp_pad, aes(x = as.factor(temp_pad), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Mean annual temperature (ºC, Centered)", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))

# with(residuals_model, boxplot(resid ~ removal_method_category, xlab = "Removal method"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$removal_method_category))        
plot7 <- ggplot(residuals_model, aes(x = as.factor(removal_method_category), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Removal method", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))

# with(residuals_model, boxplot(resid ~ response_variable, xlab = "Response variable"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$response_variable))              
plot8 <- ggplot(residuals_model, aes(x = as.factor(response_variable), y = resid)) +
  geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.2) +  # Add jittered points
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Response variable", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))

  
# with(residuals_model, boxplot(resid ~ time_pad, xlab = "Experiment duration"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$experiment_duration))            
plot9 <- ggplot(residuals_model, aes(x = as.factor(experiment_duration), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Experiment duration (years)", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))


# with(residuals_model, boxplot(resid ~ effect_type, xlab = "Effect size type"));abline(h = 0.25, lty = 2);abline(h = 0.5, lty = 2);abline(h = 0.75, lty = 2)
# plotResiduals(model.check, form = as.factor(effects_brm$effect_type))
plot10 <- ggplot(residuals_model, aes(x = as.factor(effect_type), y = resid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Effect size type", y = "Scaled residuals") +
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))


plots1to3 <- grid.arrange(plot1, plot2, plot3, nrow = 3)
# ggsave("Fig1_extended_Data_residuals1.pdf", plots1to3, path = ("figures/ExtendedData"), width = 180, height = 170, units = 'mm')

plots4to6 <- grid.arrange(plot4, plot5, plot6, nrow = 3)
# ggsave("Fig1_extended_Data_residuals2.pdf", plots4to6, path = ("figures/ExtendedData"), width = 180, height = 170, units = 'mm')

plots7to10 <- grid.arrange(plot7, plot8, plot9, plot10, nrow = 2)
# ggsave("Fig1_extended_Data_residuals3.pdf", plots7to10, path = ("figures/ExtendedData"), width = 180, height = 170, units = 'mm')

