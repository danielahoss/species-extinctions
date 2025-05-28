# Meta-Analysis
# Figure 1

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
library(tidybayes)   
library(ggdist)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------


effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = TRUE) 

effects_brm %>% head

# 3. Upload model and plot figures ----------------------------------------
mod_meta <- read_rds(here::here("model_output", "meta_brm_multi_id.rds"))

df_mod <- effects_brm %>%
  unite("ref", c(author, publ_year), sep = " ", na.rm = TRUE, remove = FALSE) %>%
  right_join(
    mod_meta %>%
      spread_draws(b_Intercept, r_study_ID[study_ID,]) %>%
      mutate(mu = b_Intercept + r_study_ID) %>%
      ungroup() %>%
      mutate(study_ID = str_replace_all(study_ID, "[.]", " ")),
    by = "study_ID", relationship = "many-to-many"
  ) %>%
  mutate(
    study_ID = str_replace_all(study_ID, "[[:punct:]]+", " ") %>%
      str_squish() %>%
      str_replace_all(" ", "_")) %>% 
  unite("resp_data", c(response_variable, effect_type), sep = " ", na.rm = TRUE) %>%
  arrange(mu)


# plot Fig 1 ----------------------------------------------------
Fig.1 <- ggplot(df_mod, aes(x = mu, y = (reorder(ref, mu)),
                            color = resp_data,
                            shape = resp_data)) +
  
  # overall effect size
  geom_vline(xintercept = fixef(mod_meta)[1, 3:4], color = "black", linetype = 2) +
  
  # credible interval
  geom_rect(xmin = fixef(mod_meta)[1, 3], xmax = fixef(mod_meta)[1, 4], 
            ymin = -Inf, ymax = Inf, 
            fill = "grey90", colour = "grey90") +
  
  # Add a vertical dashed line at x = 0
  geom_vline(xintercept = 0, color = "black", 
             linetype = 2) +
  
  # Add a vertical line at the first fixed effect value
  geom_vline(xintercept = fixef(mod_meta)[1, 1], color = "black",
             linewidth = 1) +
  
  stat_halfeye(.width = .9, size = .5, 
               fill = "transparent", point_size = 3) +
  
  geom_text( df_mod %>%
               distinct(ref, country, .keep_all = TRUE),
             mapping=aes(x = -1.09, label = country), family = "Helvetica",
             nudge_x = -0.00, color = "black",  size = 3, 
             hjust = 0, vjust = .5) +
  
  geom_text(label = 'Country',
            x = -1.09, y = 39, color = "black",
            nudge_x = -0.07, 
            hjust = 0, 
            vjust = .5,
            size = 3, family = "Helvetica") +
  
  geom_text(label = 'Reference',
            x = -1.28, y = 39, color = "black",
            size = 3, family = "Helvetica") +
  
  labs(x = "Changes in productivity", y = "") +
  
  coord_cartesian(xlim = c(-1,1),
                  ylim = c(0,39)
                  , clip = "off") + 
  
  scale_colour_manual(name = "Response variable - Study level data",
                      labels = c("biomass - meta-analysis",
                                 "biomass - raw data",
                                 "cover - meta-analysis",
                                 "cover - raw data"),
                      values = c("#CD7233", "#CD7233",  "#6AC9B5", "#6AC9B5")) +
  
  scale_shape_manual(name = "Response variable - Study level data",
                     labels = c("biomass - meta-analysis",
                                "biomass - raw data",
                                "cover - meta-analysis",
                                "cover - raw data"),
                     values = c(1, 19, 1, 19)) +
  
  scale_x_continuous(labels = scales::percent_format(scale = 100), 
                     breaks = c(-1, -0.5, 0 , .5, 1)) +
  
  scale_y_discrete(labels = ~ paste0(
    .x, " (", df_mod$study_ID[match(.x, df_mod$ref)], ")")) + 
  
  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.key.width = unit(0.22,"cm"),
    legend.position = c(.8, .10),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA))

# save
# ggsave("Fig1.png", Fig.1, path = "figures", width = 200, height = 200, units = 'mm')

