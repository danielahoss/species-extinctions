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
  mutate(net = factor(case_when(hi < 0 ~ "Undercompensation",
                                lo > 0 ~ "Overcompensation",
                                TRUE   ~ "Compensation"),
                      levels = c("Undercompensation", "Compensation", "Overcompensation")))

# Row order: undercompensation at the bottom, compensation in the middle,
# overcompensation at the top; within each block ordered by effect size. A blank
# spacer level is inserted between groups to create the visual gaps.
lev_under <- study_summ %>% filter(net == "Undercompensation") %>% arrange(m) %>% pull(ref)
lev_comp  <- study_summ %>% filter(net == "Compensation")      %>% arrange(m) %>% pull(ref)
lev_over  <- study_summ %>% filter(net == "Overcompensation")  %>% arrange(m) %>% pull(ref)
ref_levels <- c(lev_under, "__gap1__", lev_comp, "__gap2__", lev_over)

df_mod <- df_mod %>%
  left_join(study_summ %>% select(ref, net), by = "ref") %>%
  mutate(ref = factor(ref, levels = ref_levels))

# Group headers, placed just above the top row of each block
row_of  <- setNames(seq_along(ref_levels), ref_levels)
ntop    <- length(ref_levels)
headers <- tibble(
  label = c("Undercompensation", "Compensation", "Overcompensation"),
  y     = c(max(row_of[lev_under]), max(row_of[lev_comp]), max(row_of[lev_over])) + 0.9)


# plot Fig 1 ----------------------------------------------------
 Fig.1 <- ggplot(df_mod, aes(x = mu, y = ref,
                            color = resp_data,
                            fill  = resp_data,
                            shape = resp_data)) +


  # 95% CI of overall effect + reference lines, clipped to the data rows
  # (0.5 .. ntop + 0.5) so they do not extend up into the header area
  annotate("rect", xmin = fixef(mod_meta)[1, 3], xmax = fixef(mod_meta)[1, 4],
           ymin = 0.5, ymax = ntop + 0.5, fill = "grey90", colour = "grey90") +

  annotate("segment", x = fixef(mod_meta)[1, 3:4], xend = fixef(mod_meta)[1, 3:4],
           y = 0.5, yend = ntop + 0.5, color = "grey90", linetype = 2) +

  annotate("segment", x = 0, xend = 0, y = 0.5, yend = ntop + 0.5,
           color = "black", linetype = 2) +

  # overall effect estimate
  annotate("segment", x = fixef(mod_meta)[1, 1], xend = fixef(mod_meta)[1, 1],
           y = 0.5, yend = ntop + 0.5, color = "black", linewidth = 1) +
  
  # Forest plot: point estimate + 95% credible interval only (no density slab,
  # whose tail would visually cross zero even when the 95% CrI does not).
  stat_pointinterval(.width = .95, point_size = 2.2, interval_size_range = c(0.4, 0.9)) +
  
  geom_text( df_mod %>%
               distinct(ref, country, .keep_all = TRUE),
             mapping = aes(x = -1.09, label = country), family = "Helvetica",
             color = "black", size = 3,
             hjust = 0, vjust = .5) +
  
  geom_text(label = 'Country',
            x = -1.09, y = ntop + 3, color = "black",
            nudge_x = -0.07,
            hjust = 0,
            vjust = .5,
            size = 3.2, family = "Helvetica") +

  geom_text(label = 'Reference',
            x = -1.28, y = ntop + 3, color = "black",
            size = 3.2, family = "Helvetica") +
  
  labs(x = "Changes in productivity", y = "") +
  
  coord_cartesian(xlim = c(-1,1),
                  ylim = c(0, ntop + 4),
                  clip = "off") +
  
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
  
  # x-axis kept on the log response ratio scale (point positions unchanged),
  # but tick labels show the back-transformed proportional change in
  # productivity: pct = (exp(LRR) - 1) * 100. Ticks sit at round % values.
  scale_x_continuous(breaks = log(c(0.5, 0.75, 1, 1.5, 2)),
                     labels = c("-50%", "-25%", "0%", "+50%", "+100%")) +
  
  scale_y_discrete(drop = FALSE, labels = ~ ifelse(
    .x %in% c("__gap1__", "__gap2__"), "",
    paste0(.x, " (", df_mod$study_ID[match(.x, df_mod$ref)], ")"))) +
  

  theme(
    text = element_text(size = 10, family = "Helvetica", colour = "black"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.key.width = unit(0.22,"cm"),
    legend.position = c(0.58, 0.15),
    legend.justification = c(0, 0.5),
    legend.direction = "vertical",
    # legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.margin = margin(2, 4, 2, 2),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 78, 5.5, 5.5),
    plot.background = element_rect(fill = "white", color = NA)) + 
  
# Group headers above each block + parent header.
# Two layers per header: a white box (colour = white, so its border is
# invisible) with black text drawn on top. This avoids the black box outline
# that label.size = 0 leaves in the ragg/TIFF renderer.
  annotate("label", x = 0, y = headers$y, label = headers$label,
           hjust = 0.5, vjust = 0, size = 3, fontface = "bold",
           family = "Helvetica", colour = "white", fill = "white",
           label.padding = unit(0.12, "lines")) +
  annotate("text", x = 0, y = headers$y, label = headers$label,
           hjust = 0.5, vjust = 0, size = 3, fontface = "bold",
           family = "Helvetica", colour = "black") +

  annotate("label", x = 0, y = ntop + 3, label = "Community outcome",
           hjust = 0.5, vjust = 0.5, size = 3.2, fontface = "bold",
           family = "Helvetica", colour = "white", fill = "white",
           label.padding = unit(0.12, "lines")) +
  annotate("text", x = 0, y = ntop + 3, label = "Community outcome",
           hjust = 0.5, vjust = 0.5, size = 3.2, fontface = "bold",
           family = "Helvetica", colour = "black")
# Fig.1

ggsave("Fig1_meta_analysis.tiff", Fig.1,
       path = here::here("figures"), width = 200, height = 200, units = "mm",
       dpi = 300, device = ragg::agg_tiff, compression = "lzw")

