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

df_cover %>% names

as.factor(df_cover$warming) %>% levels                        
as.factor(df_cover$mowing) %>% levels 
as.factor(df_cover$NPK_addition) %>% levels  
# as.factor(df_cover$NSP) %>% levels                            
# as.factor(df_cover$N_addition) %>% levels                     
# as.factor(df_cover$drought) %>% levels                        


# removal_disturbance -----------------------------------------------------
df_cover$disturbance <- "no"

# do not use warming
# "warming",
df_cover$disturbance[apply(df_cover[,c(
  "warming", "NPK_addition")] == "yes", 1, any)] <- "yes"

as.factor(df_cover$disturbance) %>% levels


df_cover$add_dist <- "no"

df_cover$add_dist[df_cover$warming == "yes"] <- "warming"
df_cover$add_dist[df_cover$NPK_addition == "yes"] <- "NPK_addition"


as.factor(df_cover$add_dist) %>% levels


df_cover$removed_propo[which(df_cover$disturbance == "yes" )] %>% max
df_cover$removed_propo[which(df_cover$disturbance == "yes" )] %>% min

df_cover$removed_propo[which(df_cover$disturbance == "no" )] %>% max
df_cover$removed_propo[which(df_cover$disturbance == "no" )] %>% min

df_cover_dist <- df_cover %>% filter(removed_propo <= 0.35)
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "yes" )] %>% max
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "yes" )] %>% min
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "no" )] %>% max
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "no" )] %>% min

# 3. Model ----------------------------------------------------------------

# set.seed(2024)
# mod_cover_add_dist <- brm(bf(cover ~ disturbance * removed_propo +
#                                (removed_propo | study_ID / block / time_pad) +
#                                (1 | study_ID:block:plot)),
#                           data = df_cover_dist, family = lognormal(),
#                           iter = 4000, warmup = 2000,
#                           control = list(adapt_delta = 0.9), 
#                           cores = 4, chains = 4,
#                           file = "mod_cover_add_dist")

# 4. Model output and diagnostics -----------------------------------------

mod_cover_add_dist <- read_rds(here::here("model_output", "mod_cover_add_dist.rds")) 

# Model output and diagnostics -----------------------------------------
mod_cover_add_dist %>% summary


conditional_effects(mod_cover_add_dist)

pp_check(mod_cover_add_dist, ndraws = 100)  # Posterior predictive check


#  divergences to plot
mcmc_plot(mod_cover_add_dist, type = 'trace')

# examine fit to individual studies
# pp_check(mod_cover_add_dist, type = 'scatter_avg_grouped', group = 'study_ID') +
#   geom_abline(intercept = 0, slope = 1, lty = 2)


# 5. Residual diagnostics using DHARMa ------------------------------------
model.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod_cover_add_dist)),
  observedResponse = df_cover_dist$cover,
  fittedPredictedResponse = apply(t(posterior_epred(mod_cover_add_dist)), 1, mean),
  integerResponse = TRUE)

# plot(model.check)  # qq and residuals

# Here, we get too many residuals around 0.5, which means that we are 
# not getting as many residuals as we would expect in the tail of the 
# distribution than expected from the fitted model.

If you see this pattern, note that a common reason for underdispersion 
is overfitting, i.e. your model is too complex. 

Other possible explanations 
to check for include zero-inflation (best to check by comparing to a
ZIP model, but see also DHARMa::testZeroInflation), non-independence 
of the data (e.g. temporal autocorrelation, check via 
DHARMa:: testTemporalAutocorrelation) that your predictors can use 
to overfit, or that your data-generating process is simply not a Poisson process.

From a technical side, underdispersion is not as concerning as 
over dispersion, as it will usually bias p-values to the conservative 
side, but if your goal is to get a good power, you may want to consider 
a simpler model. If that is not helping, you can move to a distribution
for underdispersed count data (e.g. Conway-Maxwell-Poisson, generalized Poisson).
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
  inner_join(df_cover_dist %>% 
               select(-block,
                      -plot, 
                      -removed_propo,
                      -cover, 
                      -time_pad),
             by = c("study_ID"),relationship =
               "many-to-many")



# 6. plot DHARMA residual checks ------------------------------------------





plotResiduals(model.check, form = df_cover_dist$disturbance)

plotResiduals(model.check, form = df_cover_dist$add_dist)



