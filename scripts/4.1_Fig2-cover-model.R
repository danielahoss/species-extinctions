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

# 3. Model ----------------------------------------------------------------

# slope and intercept varying among time_pad within blocks within studies  
# and plots within blocks within studies

# set.seed(2024)
# mod_cover <- brm(bf(cover ~ removed_propo + (removed_propo | study_ID / block / time_pad) +
#                       (1 | study_ID:block:plot)),
#                  family = lognormal(),
#                  control = list(max_treedepth = 12,
#                                 adapt_delta = 0.99),
#                  iter = 7000, warmup = 2000, 
#                  data = df_cover,
#                  cores = 4, chains = 4,
#                  file = "model_cover") 

mod_cover <- read_rds(here::here("model_output", "model_cover.rds")) 


# 4. Model output and diagnostics -----------------------------------------


mod_cover %>% summary

conditional_effects(mod_cover)

pp_check(mod_cover)
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

# plot(model.check)  # qq and residuals
 
# testDispersion(model.check)
# 

# examine fit to individual studies
# pp_check(mod_cover, type = 'scatter_avg_grouped', group = 'study_ID') +
# geom_abline(intercept = 0, slope = 1, lty = 2)
 

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

# plotResiduals(model.check, form = df_cover$study_ID)
plot1 <- boxplot_residuals(data = residuals_model, x = "study_ID", 
                           labx = "Studies")

# plotResiduals(model.check, form = df_cover$time_pad)
plot2 <- boxplot_residuals(data = residuals_model, x = "experiment_duration", 
                           labx = "Experiment duration") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# plotResiduals(model.check, form = df_cover$removal_method_category)
plot3 <- boxplot_residuals(data = residuals_model, x = "removal_method_category", 
                           labx = "Species removal method")

# plotResiduals(model.check, form = df_cover$ppt_pad)
plot4 <- boxplot_residuals(data = residuals_model, x = "ppt_pad", 
                           labx = "Mean annual precipitation (mm)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_cover$temp_pad)
plot5 <- boxplot_residuals(data = residuals_model, x = "temp_pad", 
                           labx = "Mean annual temperature (ºC)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_cover$remov_propo_mean)
plot6 <- boxplot_residuals(data = residuals_model, x = "remov_propo_mean", 
                           labx = "Average proportion of species removed") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_cover$n_remov_min)
plot7 <- boxplot_residuals(data = residuals_model, x = "n_remov_min",
                           labx = "Minimum number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_cover$n_remov_max)
plot8 <- boxplot_residuals(data = residuals_model, x = "n_remov_max", 
                           labx = "Maximum number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# plotResiduals(model.check, form = df_cover$n_remov_mean)
plot9 <- boxplot_residuals(data = residuals_model, x = "n_remov_mean", 
                           labx = "Average number of species removed")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



resid2cd <- # plot10 + 
  plot6  +  plot4 + 
  plot5 +  plot2 + 
  plot7 +  plot8 +  plot9 + plot1 + plot3 +   plot_annotation(tag_levels = 'a') +
  plot_layout(nrow = 4)
resid2cd

ggsave("Fig2b_extended_Data_residuals.pdf", resid2cd,
       path = ("figures"), width = 200, height = 200, units = 'mm')

# simple way of checking residuals ----------------------------------------

alpha_q0_ba_dat <- mod_cover$data %>% 
  as_tibble()


resid_alpha_q0_ba<- residuals(mod_cover) %>% 
  as_tibble() %>% 
  bind_cols(alpha_q0_ba_dat) %>% 
  rename(resid = Estimate) %>% 
  select(-Est.Error, -Q2.5, -Q97.5) %>% 
  inner_join(df_cover %>% 
               group_by(study_ID) %>% 
               summarise(time_mean = mean(time_length_years),
                         ricmean = mean(richness),
                         ricmin = min(richness),
                         ricmax = max(richness),
                         xmin = min(removed_propo),
                         xmax = max(removed_propo)),
             by = 'study_ID')

alpha_q0_ba_predicted <- predict(mod_cover) %>% 
  as_tibble()

# add predicted values to residual df 
resid_alpha_q0_ba <- resid_alpha_q0_ba %>% 
  mutate(predicted = alpha_q0_ba_predicted$Estimate)

resid_alpha_q0_ba_n <- left_join(resid_alpha_q0_ba, 
                                 df_cover, by = c("study_ID", "block",
                                                  "plot", "time_pad", 
                                                  "cover", 
                                                  "removed_propo"))

resid <- resid_alpha_q0_ba_n

# residuals look ok
par(mfrow=c(2,4))

with(resid, plot(resid ~ removed_propo));abline(h = 0, lty = 2)
with(resid, plot(resid ~ richness));abline(h = 0, lty = 2)
with(resid, plot(resid ~ ricmin));abline(h = 0, lty = 2)
with(resid, plot(resid ~ ricmax));abline(h = 0, lty = 2)
with(resid, plot(resid ~ ricmean));abline(h = 0, lty = 2)
with(resid, plot(resid ~ time_pad));abline(h = 0, lty = 2)
with(resid, boxplot(resid ~ study_ID));abline(h = 0, lty = 2)
with(resid, plot(plot_value_m2, predicted));abline(c(0,1), lty = 2)

