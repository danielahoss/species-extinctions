# Community-level cover & additional disturbances
# model and model checks

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_cover<- read.csv(here::here("data", "df_cover_brm.csv"), header = T) 

df_cover$add_dist <- "no"

df_cover$add_dist[df_cover$warming == "yes"] <- "warming"
df_cover$add_dist[df_cover$NPK_addition == "yes"] <- "NPK_addition"



df_cover$add_dist <- factor(df_cover$add_dist, levels = c("warming", "no", "NPK_addition"))
df_cover$add_dist <- relevel(df_cover$add_dist, ref = "no")  # Change reference level

# 3. Model ----------------------------------------------------------------

# set.seed(123)
# mod_cover_add_dist <- brm(bf(cover ~ add_dist * removed_propo +
#                                (removed_propo | study_ID / block / plot)+
#                                (1 | time_pad)),
#                           data = df_cover, family = lognormal(),
#                           iter = 5000, warmup = 2000,
#                           control = list(adapt_delta = 0.99),
#                           cores = 6, chains = 4,
#                           file = "model_output/mod_cover_warmnutr")


# 4. Model output and diagnostics -----------------------------------------
mod_cover_add_dist <- read_rds(here::here("model_output", "mod_cover_warmnutr.rds")) 


# Model output and diagnostics -----------------------------------------
mod_cover_add_dist %>% summary


conditional_effects(mod_cover_add_dist)

pp_check(mod_cover_add_dist, ndraws = 100)  # Posterior predictive check


#  divergences to plot
mcmc_plot(mod_cover_add_dist, type = 'trace')


# 5. Residual diagnostics using DHARMa ------------------------------------
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_cover_add_dist)),
  observedResponse = df_cover$cover,
  fittedPredictedResponse = apply(t(posterior_epred(mod_cover_add_dist)), 1, mean),
  integerResponse = TRUE)


# testDispersion(model.check)

model_data <- mod_cover_add_dist$data %>%
  as_tibble() 


residuals_model <- model.check$scaledResiduals %>%
  as_tibble() %>%
  bind_cols(model_data) %>%
  rename(resid = value) 

predicted <- predict(mod_cover_add_dist) %>%
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



plotResiduals(model.check, form = df_cover$add_dist)



