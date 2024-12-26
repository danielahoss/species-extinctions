# -----------------------------------------------------------
# Community-level cover model
# -----------------------------------------------------------

# --- 1. Load Libraries and Set Up Environment ---
library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# --- 2. Load and Preprocess Data ---
df_cover<- read.csv(here::here("data", "df_cover_brm.csv"), header = T) 

# --- 3. Model ---
# slope and intercept varying among time_pad within blocks within studies  
# and plots within blocks within studies

set.seed(2024)
mod_cover <- brm(bf(cover ~ removed_propo +
                     (removed_propo | study_ID / block / time_pad) +
                     (1 | study_ID:block:plot)),
                data = df_cover, family = lognormal(),
                iter = 4000, warmup = 1000,
                control = list(max_treedepth = 12,
                               adapt_delta = 0.99),
                cores = 4, file = "model_cover")


# saveRDS(mod_cover, here::here("model_output",  "model_cover.rds"))
mod_cover <- read_rds(here::here("model_output", "model_cover.rds")) 

# Model output
mod_cover %>% summary()

# 4. Model Diagnostics ------------------------------------------
# Residual diagnostics using DHARMa
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_cover)),
  observedResponse = df_cover$cover,
  fittedPredictedResponse = apply(t(posterior_epred(mod_cover)), 1, mean),
  integerResponse = TRUE)

# plot(model.check)  # qq and residuals
# 
# pp_check(mod_cover, ndraws = 100)  # Posterior predictive check
# 
# testDispersion(model.check)
# 
# # No divergences to plot
# mcmc_plot(mod_cover, type = 'trace')
# 
# # examine fit to individual studies
# pp_check(mod_cover, type = 'scatter_avg_grouped', group = 'study_ID') +
#   geom_abline(intercept = 0, slope = 1, lty = 2)
# 

model_data <- mod_cover$data %>%
  as_tibble() 


residuals_model <- model.check$scaledResiduals %>%
  as_tibble() %>%
  bind_cols(model_data) %>%
  rename(resid = value) 

predicted <- predict(mod_cover) %>%
  as_tibble()

# # add predicted values to residual df
residuals_model <- residuals_model %>%
  mutate(predicted = predicted$Estimate) %>%
  as_tibble() %>%
  inner_join(df_cover %>% 
               select(-block,
                      -plot, 
                      -removed_propo,
                      -cover, 
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


# pdf("residuals_cover.pdf")  # Open a PDF
#   par(mfrow=c(3,1))



# plotResiduals(model.check, form = df_cover$study_ID)
boxplot_residuals(data = residuals_model, x = "study_ID")

# plotResiduals(model.check, form = df_cover$time_pad)
boxplot_residuals(data = residuals_model, x = "experiment_duration")

# plotResiduals(model.check, form = df_cover$removal_method_category)
boxplot_residuals(data = residuals_model, x = "removal_method_category")

# plotResiduals(model.check, form = df_cover$ppt_pad)
boxplot_residuals(data = residuals_model, x = "ppt_pad")

# plotResiduals(model.check, form = df_cover$temp_pad)
boxplot_residuals(data = residuals_model, x = "temp_pad")

# plotResiduals(model.check, form = df_cover$ricmean)
boxplot_residuals(data = residuals_model, x = "ricmean")

# plotResiduals(model.check, form = df_cover$ricmax)
boxplot_residuals(data = residuals_model, x = "ricmax")

# plotResiduals(model.check, form = df_cover$ricmin)
boxplot_residuals(data = residuals_model, x = "ricmin")

# plotResiduals(model.check, form = df_cover$remov_propo_min)
boxplot_residuals(data = residuals_model, x = "remov_propo_min")

# plotResiduals(model.check, form = df_cover$remov_propo_max)
boxplot_residuals(data = residuals_model, x = "remov_propo_max")

# plotResiduals(model.check, form = df_cover$remov_propo_mean)
boxplot_residuals(data = residuals_model, x = "remov_propo_mean")

# plotResiduals(model.check, form = df_cover$n_remov_min)
boxplot_residuals(data = residuals_model, x = "n_remov_min")

# plotResiduals(model.check, form = df_cover$n_remov_max)
boxplot_residuals(data = residuals_model, x = "n_remov_max")

# plotResiduals(model.check, form = df_cover$n_remov_mean)
boxplot_residuals(data = residuals_model, x = "n_remov_mean")

# dev.off()
