# -----------------------------------------------------------
# Community-level biomass model
# -----------------------------------------------------------

# --- 1. Load Libraries and Set Up Environment ---
library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# --- 2. Load and Preprocess Data ---
df_biom<- read.csv(here::here("data", "df_biom_brm.csv"), header = T) 

# --- 3. Model ---
# slope and intercept varying among time_pad within blocks within studies  
# and plots within blocks within studies

# set.seed(2024)
# mod_biom <- brm(bf(biomass ~ removed_propo +
#                 (removed_propo | study_ID / block / time_pad) +
#                 (1 | study_ID:block:plot)),
#            data = df_biom, family = lognormal(),
#            iter = 4000, warmup = 1000,
#            control = list(max_treedepth = 12,
#                           adapt_delta = 0.99),
#            cores = 4, file = "model_biomass2")


# saveRDS(mod_biom, here::here("model_output",  "model_biomass.rds"))
mod_biom <- read_rds(here::here("model_output", "model_biomass.rds")) 

# Model output
mod_biom %>% summary()

# 4. Model Diagnostics ------------------------------------------
# Residual diagnostics using DHARMa
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_biom)),
  observedResponse = df_biom$biomass,
  fittedPredictedResponse = apply(t(posterior_epred(mod_biom)), 1, mean),
  integerResponse = TRUE)

# plot(model.check)  # qq and residuals
# 
# pp_check(mod_biom, ndraws = 100)  # Posterior predictive check
# 
# testDispersion(model.check)
# 
# # No divergences to plot
# mcmc_plot(mod_biom, type = 'trace')
# 
# # examine fit to individual studies
# pp_check(mod_biom, type = 'scatter_avg_grouped', group = 'study_ID') +
#   geom_abline(intercept = 0, slope = 1, lty = 2)
# 

model_data <- mod_biom$data %>%
  as_tibble() 
  

residuals_model <- model.check$scaledResiduals %>%
  as_tibble() %>%
  bind_cols(model_data) %>%
  rename(resid = value) 

predicted <- predict(mod_biom) %>%
  as_tibble()

# # add predicted values to residual df
residuals_model <- residuals_model %>%
  mutate(predicted = predicted$Estimate) %>%
  as_tibble() %>%
  inner_join(df_biom %>% 
               select(-block,
                      -plot, 
                      -removed_propo,
                      -biomass, 
                      -time_pad),
             by = c("study_ID"),relationship =
               "many-to-many")




# plot DHARMA residual checks -----------------------------------


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

# pdf("residuals_biomass.pdf")  # Open a PDF
#   par(mfrow=c(3,1))

# EDIT THE LABS X -----------------------------------------------



# plotResiduals(model.check, form = df_biom$study_ID)
plot1 <- boxplot_residuals(data = residuals_model, x = "study_ID", 
                  labx = "Studies")

# plotResiduals(model.check, form = df_biom$time_pad)
plot2 <- boxplot_residuals(data = residuals_model, x = "experiment_duration", 
                  labx = "Experiment duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# plotResiduals(model.check, form = df_biom$removal_method_category)
plot3 <- boxplot_residuals(data = residuals_model, x = "removal_method_category", 
                  labx = "Species removal method")

# plotResiduals(model.check, form = df_biom$ppt_pad)
plot4 <- boxplot_residuals(data = residuals_model, x = "ppt_pad", 
                  labx = "Mean annual precipitation (mm)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_biom$temp_pad)
plot5 <- boxplot_residuals(data = residuals_model, x = "temp_pad", 
                  labx = "Mean annual temperature (ºC)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_biom$remov_propo_mean)
plot6 <- boxplot_residuals(data = residuals_model, x = "remov_propo_mean", 
                  labx = "Average proportion of species removed") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_biom$n_remov_min)
plot7 <- boxplot_residuals(data = residuals_model, x = "n_remov_min",
                  labx = "Minimum number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_biom$n_remov_max)
plot8 <- boxplot_residuals(data = residuals_model, x = "n_remov_max", 
                  labx = "Maximum number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_biom$n_remov_mean)
plot9 <- boxplot_residuals(data = residuals_model, x = "n_remov_mean", 
                  labx = "Average number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



# plotResiduals(model.check, form = df_biom$mean_ric_control_pad)
plot10 <- ggplot(residuals_model, aes(x = (mean_ric_control_pad), y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  labs(x = "Mean richness of control (centered)", y = "Scaled residuals") +
  theme(
    text = element_text(size = 8, family = "Helvetica", colour = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))


# dev.off()

plots1to9 <- grid.arrange(plot6, plot3, plot4,
                          plot1, plot5, plot2,
                          plot7, plot8, plot9, nrow = 3)
# ggsave("Fig2a_extended_Data_residuals1.pdf", plots1to9, path = ("figures/ExtendedData"), width = 180, height = 170, units = 'mm')


# ggsave("Fig2a_extended_Data_residuals2.pdf", plot10, path = ("figures/ExtendedData"), width = 180, height = 170, units = 'mm')

