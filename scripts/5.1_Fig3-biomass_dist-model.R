# Community-level biomass & additional disturbances
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom <- read.csv(here::here( "data", "df_biom_brm.csv"), header = T) 
df_biom %>% head

df_biom$add_dist <- "no"
df_biom$add_dist[df_biom$drought == "yes"] <- "drought"
df_biom$add_dist[df_biom$NPK_addition == "yes"] <- "nutrient"
df_biom$add_dist[df_biom$N_addition == "yes"] <- "nutrient"

as.factor(df_biom$add_dist) %>% levels


df_biom$add_dist <- factor(df_biom$add_dist, levels = c("drought", "no", "nutrient"))
df_biom$add_dist <- relevel(df_biom$add_dist, ref = "no")  # Change reference level

as.factor(df_biom$study_ID[which(df_biom$add_dist == "drought" )]) %>% levels
# "ID_215" -> drought

as.factor(df_biom$study_ID[which(df_biom$add_dist == "nutrient" )]) %>% levels

# "ID_168" "ID_245" "ID_250"
  


# 3. Model ----------------------------------------------------------------
# set.seed(123)
# mod_biomass_add_dist <- brm(bf(biomass ~  add_dist * removed_propo +
#                                  (removed_propo | study_ID / block /plot ) +
#                                  (1 | time_pad)),
#                             data = df_biom, family = lognormal(),
#                             iter = 5000, warmup = 1000,
#                             control = list(adapt_delta = 0.99),
#                             cores = 6, chains = 4,
#                             file = "model_output/model_biom_droughtnutr")

mod_biomass_add_dist <- read_rds(here::here("model_output", "model_biom_droughtnutr.rds")) 

# Model output and diagnostics -----------------------------------------
mod_biomass_add_dist %>% summary
mod_biomass_add_dist %>% get_prior()
conditional_effects(mod_biomass_add_dist)

pp_check(mod_biomass_add_dist, ndraws = 100)  # Posterior predictive check


# No divergences to plot
mcmc_plot(mod_biomass_add_dist, type = 'trace')

# examine fit to individual studies
# pp_check(mod_biomass_add_dist, type = 'scatter_avg_grouped', group = 'study_ID') +
#   geom_abline(intercept = 0, slope = 1, lty = 2)


# Residual diagnostics using DHARMa ------------------------------------
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_biomass_add_dist)),
  observedResponse = df_biom$biomass,
  fittedPredictedResponse = apply(t(posterior_epred(mod_biomass_add_dist)), 1, mean),
  integerResponse = TRUE)

 plot(model.check)  # qq and residuals
 testDispersion(model.check)

model_data <- mod_biomass_add_dist$data %>%
  as_tibble() 


residuals_model <- model.check$scaledResiduals %>%
  as_tibble() %>%
  bind_cols(model_data) %>%
  rename(resid = value) 

predicted <- predict(mod_biomass_add_dist) %>%
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



# plot DHARMA residual checks ------------------------------------------

plotResiduals(model.check, form = df_biom$disturbance)

 plotResiduals(model.check, form = df_biom$add_dist)

