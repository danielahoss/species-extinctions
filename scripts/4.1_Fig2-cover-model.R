# Community-level cover 
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

require(tidyverse)
require(brms)
library(DHARMa)
require(patchwork)

# Clear workspace
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------
df_cover <- read.csv(here::here( "data", "df_cover_brm.csv"), header = T) 
df_cover %>% head


as.factor(df_cover$removal_treatment_category) %>% levels

# 3. Model ----------------------------------------------------------------

 
# set.seed(123)
# mod_cover <- brm(bf(cover ~ removed_propo + (removed_propo | study_ID / block / plot) +
#                       (1 | time_pad)),
#                  family = lognormal(),
#                    control = list(max_treedepth = 11,
#                                   adapt_delta = 0.99),
#                  iter = 5000, warmup = 1000,
#                  data = df_cover,
#                  cores = 6, chains = 4,
#                  file = "model_output/model_cover")
mod_cover <- read_rds(here::here("model_output", "model_cover.rds")) 


mod_cover %>% get_prior()


# 4. Model output and diagnostics -----------------------------------------


mod_cover %>% summary

conditional_effects(mod_cover)


pp_check(mod_cover) + scale_x_continuous(trans = "log")


mcmc_plot(mod_cover, type = 'trace')


# examine fit to individual studies
 pp_check(mod_cover, type = 'scatter_avg_grouped', group = 'study_ID') +
   geom_abline(intercept = 0, slope = 1, lty = 2)

ggplot() +
  facet_wrap(~study_ID) +
  geom_density(data = df_cover,
               aes(x = cover)) +
  scale_x_continuous(trans = 'log')


# model fit to central tendency
pp_check(mod_cover, type = 'stat_grouped', group = 'study_ID')



# 5. Residual diagnostics using DHARMa ------------------------------------


model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_cover)),
  observedResponse = df_cover$cover,
  fittedPredictedResponse = apply(t(posterior_epred(mod_cover)), 1, mean),
  integerResponse = TRUE)

plot(model.check)  # qq and residuals
 
# testDispersion(model.check)
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

# pdf("residuals_cover.pdf")  # Open a PDF
#   par(mfrow=c(3,1))


plotResiduals(model.check, form = df_cover$removal_method_category)
plotResiduals(model.check, form = df_cover$removal_treatment_category)                  
plotResiduals(model.check, form = df_cover$ricmean)
plotResiduals(model.check, form = df_cover$time_pad)

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

resid <-  p2 + p3 + p1 +   plot_annotation(tag_levels = 'a') +
  plot_layout(nrow = 3)
resid


# ggsave("Extended Data Fig. 9 - Fig2cd_residuals.pdf", resid, path = ("figures/ExtendedData"), width = 200, height = 200, units = 'mm')

