# > mod %>% summary
# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: plot_value_m2 ~ removed_propo + (removed_propo | study_ID/block/time_pad) + (1 | study_ID:block:plot) 
# Data: df_cover (Number of observations: 7113) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~study_ID (Number of levels: 11) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# sd(Intercept)                    1.94      0.48     1.23     3.11 1.00     1773
# sd(removed_propo)                0.92      0.37     0.33     1.79 1.00     1512
# cor(Intercept,removed_propo)     0.17      0.32    -0.48     0.74 1.00     3242
# Tail_ESS
# sd(Intercept)                    2617
# sd(removed_propo)                1597
# cor(Intercept,removed_propo)     2408
# 
# ~study_ID:block (Number of levels: 112) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# sd(Intercept)                    0.05      0.03     0.00     0.12 1.02      340
# sd(removed_propo)                0.35      0.09     0.18     0.53 1.01      486
# cor(Intercept,removed_propo)    -0.01      0.49    -0.90     0.93 1.02      205
# Tail_ESS
# sd(Intercept)                    1086
# sd(removed_propo)                 652
# cor(Intercept,removed_propo)      406
# 
# ~study_ID:block:plot (Number of levels: 847) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.35      0.01     0.33     0.37 1.00     1503     2228
# 
# ~study_ID:block:time_pad (Number of levels: 816) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# sd(Intercept)                    0.14      0.01     0.12     0.17 1.00     2385
# sd(removed_propo)                0.10      0.03     0.03     0.17 1.02      440
# cor(Intercept,removed_propo)     0.62      0.26     0.06     0.98 1.01      587
# Tail_ESS
# sd(Intercept)                    2784
# sd(removed_propo)                 642
# cor(Intercept,removed_propo)     1460
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept         4.28      0.58     3.14     5.40 1.00      834     1464
# removed_propo    -1.19      0.39    -1.98    -0.41 1.00     2632     2598
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.38      0.00     0.38     0.39 1.00     4963     3452
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message:
#   There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# > 


# -----------------------------------------------------------
#  Species-level biomass data model
# -----------------------------------------------------------

# --- 1. Load Libraries and Set Up Environment ---
require(tidyverse)
require(brms)
library(DHARMa)

rm(list = ls())
#####data#### 
#data.R: 

df_cover <- read.csv(here::here( "data", "df_cover_brm.csv"), header = T) 
df_cover %>% head


# model --------------------------------------------------------------------
(df_cover$plot_value_m2) %>% summary
as.factor(df_cover$study_ID) %>% summary
df2 <- df_cover %>% filter(study_ID != "ID_251")
df3 <- df2 %>% filter(study_ID != "ID_240")
df3$plot_value_m2 %>% summary
plot(df2$removed_propo,df2$plot_value_m2)
mod <- brm(bf(plot_value_m2 ~ removed_propo + (removed_propo | study_ID / block / time_pad) +
                (1 | study_ID:block:plot)),
           family = lognormal(),
          control = list(max_treedepth = 11,
                         adapt_delta = 0.9),
           #iter = 5000,# warmup = 2000, 
           data = df_cover,
        #   cores = 4, chains = 1,
          file = "teste3" ) # model_cover_lognorm

mod <- read_rds(here::here("model_output", "model_cover_lognorm.rds")) 

mod %>% summary
exp(4.75)

nd <- data.frame(removed_propo = 0 )
predict(mod, re_formula = NA, newdata = nd)
# 121 cover

nd <- data.frame(removed_propo = 0.5 )
predict(mod, re_formula = NA, newdata = nd)


# 7500/121
# 100 - 61.98347



modb <- read_rds(here::here("m_biom_lognorm.rds")) # "mod.rds"

modb %>% summary
nd <- data.frame(removed_propo = 0 )
predict(modb, re_formula = NA, newdata = nd)
213

nd <- data.frame(removed_propo = 0.5 )
predict(modb, re_formula = NA, newdata = nd)
122

12200/213
100 -  57.277

# checking -------------------------------------------------------------
1 - exp(-1.14*0.5)

4.75-0.96

(exp(-1.14))*0.5

exp(-0.96*0.5+5.32)

sd(df_cover$plot_value_m2)
mean(df_cover$plot_value_m2)

(187*0.31)/233
123/234
mod %>% summary

exp(5.32)

2.611696*0.5


(exp(4.75))*0.5

nd <- data.frame(removed_propo = 0.5 )
predict(modb, re_formula = NA, newdata = nd)

115.5843 - 100
75.97455 - x

75.97455*100

7597.455/115.5843

100 - 65.73086
# plot checks -------------------------------------------------------------


conditional_effects(mod)



# residuals check ---------------------------------------------------------

mcmc_plot(mod, type = 'trace')

ggplot() +
  facet_wrap(~study_ID) +
  geom_density(data = df_cover,
               aes(x = plot_value_m2)) +
  scale_x_continuous(trans = 'log')


# study-level variation in overdispersion for the win :)
pp_check(mod)
pp_check(mod) + scale_x_continuous(trans = "log")

# examine fit to individual time series
pp_check(mod, type = 'scatter_avg_grouped', group = 'study_ID') +
  # scale_x_continuous(trans = 'log2') +
  # scale_y_continuous(trans = 'log2') +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# another look (model fit to central tendency)
pp_check(mod, type = 'stat_grouped', group = 'study_ID')




# plot residuals ------------------------------------------------


# more model inspection
alpha_q0_ba_dat <- mod$data %>% 
  as_tibble()

# residual check
resid_alpha_q0_ba<- residuals(mod) %>% 
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

alpha_q0_ba_predicted <- predict(mod) %>% 
  as_tibble()

# add predicted values to residual df 
resid_alpha_q0_ba <- resid_alpha_q0_ba %>% 
  mutate(predicted = alpha_q0_ba_predicted$Estimate)

resid_alpha_q0_ba_n <- left_join(resid_alpha_q0_ba, 
                                 df_cover, by = c("study_ID", "block",
                                                  "plot", "time_pad", 
                                                  "plot_value_m2", 
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

