# Community-level biomass 
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)  
library(ggplot2)  
library(patchwork)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom <- read.csv(here::here( "data", "df_biom_brm.csv"), header = T) 
df_biom %>% head
as.factor(df_biom$removal_method_category) %>% levels


# 3. Model ----------------------------------------------------------------

# slope and intercept varying among plots within blocks within studies  
# and varying intercept for time

# set.seed(123)
# mod_biom <- brm(
#   bf(biomass ~ removed_propo +
#        (removed_propo | study_ID / block / plot) +
#        (1 | time_pad)),
#   data = df_biom, family = lognormal(),
#   iter = 6000, warmup = 1000,
#   control = list(max_treedepth = 12,
#                  adapt_delta = 0.99),
#   cores = 6, chains = 4, file = "model_output/model_biomass")


mod_biom <- read_rds(here::here("model_output", "model_biomass.rds")) 




# 4. Model output and diagnostics -----------------------------------------


mod_biom %>% summary()

conditional_effects(mod_biom)


pp_check(mod_biom) + scale_x_continuous(trans = "log") # Posterior predictive check


# No divergences to plot
mcmc_plot(mod_biom, type = 'trace')

# examine fit to individual studies
# pp_check(mod_biom, type = 'scatter_avg_grouped', group = 'study_ID') +
#   geom_abline(intercept = 0, slope = 1, lty = 2)



# 5. Residual diagnostics using DHARMa ------------------------------------
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_biom)),
  observedResponse = df_biom$biomass,
  fittedPredictedResponse = apply(t(posterior_epred(mod_biom)), 1, mean),
  integerResponse = TRUE)


# plot(model.check)  # qq and residuals
# testDispersion(model.check)


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


# 6. plot DHARMA residual checks ------------------------------------------



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


plotResiduals(model.check, form = df_biom$removal_method_category)
plotResiduals(model.check, form = df_biom$ricmean)
plotResiduals(model.check, form = df_biom$time_pad)

p1 <- boxplot_residuals(
  data = residuals_model, x = "removal_method_category", 
  labx = "Removal method") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p3 <- boxplot_residuals(
  data = residuals_model, x = "ricmean", labx = "Average species richness") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p2 <- boxplot_residuals(data = residuals_model, x = "experiment_duration", 
                        labx = "Experiment duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

resid <- p2 + p3 + p1 +   plot_annotation(tag_levels = 'a') +
  plot_layout(nrow = 3)
resid


# ggsave("Extended Data Fig. 8 - Fig2a_residuals.pdf", resid, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')

