# Meta-Analysis
# Figure 1

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(here)

# Clear workspace
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------


effects_brm <- read.csv(here::here("data", "effects_brm.csv"), header = TRUE)

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

# --- Classify each study by its 95% CrI and group rows into net-effect blocks ---
# Grouping is by the statistical class (not the point estimate), so a study with
# a strongly negative median but a CrI overlapping zero is placed in
# "Compensation", where it belongs.
study_summ <- df_mod %>%
  group_by(ref) %>%
  summarise(m  = median(mu),
            lo = quantile(mu, 0.025),
            hi = quantile(mu, 0.975), .groups = "drop") %>%
  mutate(net = factor(case_when(hi < 0 ~ "Decline",
                                lo > 0 ~ "Overcompensation",
                                TRUE   ~ "Compensation"),
                      levels = c("Decline", "Compensation", "Overcompensation")))

# Row order: declines at the bottom, compensation in the middle,
# overcompensation at the top; within each block ordered by effect size.
ref_levels <- study_summ %>% arrange(net, m) %>% pull(ref)
df_mod <- df_mod %>%
  left_join(study_summ %>% select(ref, net), by = "ref") %>%
  mutate(ref = factor(ref, levels = ref_levels))

# Shaded band spanning each net-effect group (positions on the discrete y axis)
bands <- tibble(ref = ref_levels) %>%
  mutate(row = row_number()) %>%
  left_join(study_summ %>% select(ref, net), by = "ref") %>%
  group_by(net) %>%
  summarise(ymin = min(row) - 0.5, ymax = max(row) + 0.5,
            ymid = (min(row) + max(row)) / 2, .groups = "drop") %>%
  mutate(fill  = c(Decline = "#F4CCCC", Compensation = "#F0F0F0",
                   Overcompensation = "#D9EAD3")[as.character(net)],
         label = c(Decline = "Declines", Compensation = "Compensation",
                   Overcompensation = "Overcompensation")[as.character(net)])


# plot Fig 1 ----------------------------------------------------
Fig.1 <- ggplot(df_mod, aes(x = mu, y = ref,
                            color = resp_data,
                            fill  = resp_data,
                            shape = resp_data)) +

  # Net-effect group bands (drawn first, behind everything)
  geom_rect(data = bands, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = bands$fill, alpha = 0.55) +

  # Right-margin group labels
  annotate("text", x = 1.04, y = bands$ymid, label = bands$label,
           hjust = 0, vjust = 0.5, size = 3, fontface = "bold",
           family = "Helvetica") +
  annotate("text", x = 1.04, y = 39, label = "Net result",
           hjust = 0, vjust = 0.5, size = 3, fontface = "bold",
           family = "Helvetica") +

  # 95% CI of overall effect (shaded rect + dashed bounds)
  geom_rect(xmin = fixef(mod_meta)[1, 3], xmax = fixef(mod_meta)[1, 4],
            ymin = -Inf, ymax = Inf,
            fill = "grey90", colour = "grey90") +
  
  geom_vline(xintercept = fixef(mod_meta)[1, 3:4], color = "grey90", linetype = 2) +

  geom_vline(xintercept = 0, color = "black", linetype = 2) +

  # overall effect estimate
  geom_vline(xintercept = fixef(mod_meta)[1, 1], color = "black", linewidth = 1) +
  
  # Forest plot: point estimate + 95% credible interval only (no density slab,
  # whose tail would visually cross zero even when the 95% CrI does not).
  stat_pointinterval(.width = .95, point_size = 2.2, interval_size_range = c(0.4, 0.9)) +
  
  geom_text( df_mod %>%
               distinct(ref, country, .keep_all = TRUE),
             mapping = aes(x = -1.09, label = country), family = "Helvetica",
             color = "black", size = 3,
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
                      values = c("#D55E00", "#D55E00", "#009E73", "#009E73")) +
  scale_fill_manual(values = c("#D55E00", "#D55E00", "#009E73", "#009E73"),
                    guide  = "none") +
  
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
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 78, 5.5, 5.5),
    plot.background = element_rect(fill = "white", color = NA))

Fig.1

ggsave("Fig1_meta_analysis.tiff", Fig.1,
       path = here::here("figures"), width = 200, height = 200, units = "mm",
       dpi = 300, device = ragg::agg_tiff, compression = "lzw")

