# Combined biomass + cover model
# Figure: predicted productivity ~ removed_propo by removal treatment type
# Style follows scripts 5.2 and 6.2


# 1. Libraries ---------------------------------------------------------------


library(tidyverse)
library(brms)
library(tidytext)
library(tidybayes)
library(ggdist)
library(patchwork)
library(emmeans)
library(here)

rm(list = ls())

set.seed(321)


# 2. Load model and data -----------------------------------------------------


fit3_inter <- read_rds(here::here("model_output", "fit3_inter_traits_3.rds"))

df_trt_biom  <- read.csv(here::here("data", "df_biomass_trt.csv"), header = TRUE) %>%
  mutate(block = as.character(block), productivity = biomass)
df_trt_cover <- read.csv(here::here("data", "df_cover_trt.csv"),   header = TRUE) %>%
  mutate(block = as.character(block), productivity = cover)

df <- bind_rows(df_trt_biom, df_trt_cover) %>%
  mutate(removal_trt = factor(removal_trt))
# 3. Fitted values with 30-60% CrI (population-level) -----------------------


fitted_df <- cbind(
  fit3_inter$data,
  fitted(fit3_inter, re_formula = NA, scale = "linear", prob = c(0.025, 0.975))
) %>%
  tibble::as_tibble() %>%
  mutate(removal_trt = as.factor(removal_trt)) %>%
  # response_variable not in model formula; join from original data.
  # brms preserves row order and lognormal drops productivity <= 0 rows,
  # so this assumes all productivity values in df_biomass_trt/df_cover_trt are > 0.
  left_join(
    df %>%
      distinct(study_ID, block, plot, time_length_years,
               removed_propo, removal_trt, response_variable) %>%
      mutate(removal_trt = as.character(removal_trt)),
    by = c("study_ID", "block", "plot", "time_length_years",
           "removed_propo", "removal_trt")
  )


# 4. Build plotting data -----------------------------------------------------

cv_sub <- fitted_df %>%
  filter(
    productivity > 0,
    !is.na(removed_propo),
    !is.na(removal_trt),
    !is.na(Estimate),
    !is.na(Q2.5),
    !is.na(Q97.5)
  ) %>%
  mutate(
    ymin_exp = exp(Q2.5),
    ymax_exp = exp(Q97.5)
  ) %>%
  filter(!is.infinite(ymin_exp), !is.infinite(ymax_exp))


# 5. Colors and theme --------------------------------------------------------

col_dom    <- "#882255" 
col_sub    <- "#4477AA" 
col_traits <- "#997700" 

colour_vals <- c(
  dominant         = col_dom,
  subordinate      = col_sub,
  traits = col_traits
)


theme_combined <- theme(
  legend.position  = "none",
  panel.background = element_rect(fill = "white"),
  panel.grid       = element_blank(),
  plot.background  = element_rect(fill = "white", color = NA),
  axis.line        = element_line(colour = "black"),
  axis.text        = element_text(colour = "black"),
  axis.ticks       = element_line(colour = "black"),
  axis.title.x     = element_text(face = "bold"),
  axis.title.y     = element_text(face = "bold"),
  plot.margin      = margin(5.5, 5.5, 10, 5.5),
  text             = element_text(size = 10, family = "Helvetica")
)


# 6. Main scatter + fitted line plot -----------------------------------------


rem_combined <- ggplot(
  cv_sub %>% filter(!is.na(removal_trt)),
  aes(x = removed_propo, colour = removal_trt, fill = removal_trt)
) +
  geom_ribbon(
    aes(ymin = ymin_exp, ymax = ymax_exp),
    alpha  = 0.4,
    colour = NA
  ) +
  geom_point(aes(y = productivity, shape = response_variable), size = 1, alpha = 0.5) +
  geom_line(aes(y = exp(Estimate)), linewidth = 1.2) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values   = colour_vals) +
  scale_shape_manual(values  = c(biomass = 16, cover = 17)) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(
    labels = function(x) ifelse(x == 0, "0%\ncontrol",
                                scales::percent(x, scale = 100))
  ) +
  scale_y_continuous(
    trans  = "log",
    breaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)
  ) +
  labs(
    x = "Proportion of species removed",
    y = "Productivity (log scale)"
  ) +
  theme_combined

rem_combined

# 7. Study-level forest plot -------------------------------------------------


# Extract posterior draws for fixed + study-level random effects
draws_tidy <- fit3_inter %>%
  spread_draws(
    b_removed_propo,
    `b_removed_propo:removal_trttraits`,
    `b_removed_propo:removal_trtsubordinate`,
    r_study_ID[study_ID, term]
  ) %>%
  pivot_wider(names_from = term, values_from = r_study_ID)


# Compute study-level slopes per treatment
# dominant is the reference level; traits and subordinate
# are deviations from it
draws_slopes <- draws_tidy %>%
  mutate(
    slope_dominant = b_removed_propo +
      removed_propo,
    
    slope_traits = b_removed_propo +
      `b_removed_propo:removal_trttraits` +
      removed_propo +
      `removed_propo:removal_trttraits`,
    
    slope_subordinate = b_removed_propo +
      `b_removed_propo:removal_trtsubordinate` +
      removed_propo +
      `removed_propo:removal_trtsubordinate`
  )


# Summarise: mean + 95% CrI per study x treatment
study_slopes <- draws_slopes %>%
  pivot_longer(
    cols      = c(slope_dominant, slope_traits, slope_subordinate),
    names_to  = "removal_trt",
    values_to = "slope"
  ) %>%
  mutate(removal_trt = str_remove(removal_trt, "slope_")) %>%
  group_by(study_ID, removal_trt) %>%
  summarise(
    mean  = mean(slope),
    lower = quantile(slope, 0.025),
    upper = quantile(slope, 0.975),
    .groups = "drop"
  )


# Keep only observed study x treatment combinations
study_trt_combos <- cv_sub %>%
  distinct(study_ID, removal_trt) %>%
  mutate(removal_trt = as.character(removal_trt))


study_slopes <- study_slopes %>%
  inner_join(study_trt_combos, by = c("study_ID", "removal_trt")) %>%
  left_join(df %>% distinct(study_ID, response_variable), by = "study_ID")


# Study labels
study_labels <- cv_sub %>%
  group_by(study_ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste0(study_ID, " (n=", n, ")"))


# Factor order: Subordinate, Traits, Dominant (top to bottom)
study_slopes <- study_slopes %>%
  left_join(study_labels, by = "study_ID") %>%
  mutate(
    removal_trt = case_when(
      removal_trt == "dominant" ~ "Dominant",
      removal_trt == "traits" ~ "Traits",
      removal_trt == "subordinate" ~ "Subordinate",
      TRUE ~ removal_trt
    ),
    removal_trt = factor(removal_trt,
                         levels = c("Subordinate", "Traits", "Dominant"))
  )


# 7b. Overall posterior slopes (population-level fixed effects only) ----------


overall_slopes <- fit3_inter %>%
  spread_draws(
    b_removed_propo,
    `b_removed_propo:removal_trttraits`,
    `b_removed_propo:removal_trtsubordinate`
  ) %>%
  mutate(
    Dominant = b_removed_propo,
    `Traits` = b_removed_propo + `b_removed_propo:removal_trttraits`,
    Subordinate = b_removed_propo + `b_removed_propo:removal_trtsubordinate`
  ) %>%
  pivot_longer(
    cols = c(Dominant, `Traits`, Subordinate),
    names_to  = "removal_trt",
    values_to = "slope"
  ) %>%
  mutate(removal_trt = factor(removal_trt,
                              levels = c("Dominant", "Subordinate", "Traits")))


# 7c. Full posterior draws for halfeye in panel b
draws_slopes_long <- draws_slopes %>%
  pivot_longer(
    cols      = c(slope_dominant, slope_traits, slope_subordinate),
    names_to  = "removal_trt",
    values_to = "slope"
  ) %>%
  mutate(removal_trt = str_remove(removal_trt, "slope_")) %>%
  inner_join(study_trt_combos, by = c("study_ID", "removal_trt")) %>%
  left_join(study_labels, by = "study_ID") %>%
  mutate(
    removal_trt = case_when(
      removal_trt == "dominant"    ~ "Dominant",
      removal_trt == "traits"      ~ "Traits",
      removal_trt == "subordinate" ~ "Subordinate",
      TRUE                         ~ removal_trt
    ),
    removal_trt = factor(removal_trt, levels = c("Subordinate", "Traits", "Dominant"))
  )

draws_slopes_long <- draws_slopes_long %>%
  filter(!is.na(label)) %>%
  group_by(removal_trt, label) %>%
  mutate(med_slope = median(slope, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = as.character(label))

# 7c. Forest plot with halfeye
plot2_combined <-
  ggplot(
    draws_slopes_long,
    aes(x = slope, y = reorder_within(label, med_slope, removal_trt),
        colour = removal_trt, fill = removal_trt)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95),
               slab_size = 0.5, point_size = 0.8) +
  facet_grid(removal_trt ~ ., scales = "free_y", space = "free_y") +
  geom_text(
    data = study_slopes %>% distinct(removal_trt),
    aes(label = removal_trt),
    x = 0.05, y = Inf,
    hjust = 0, vjust = 1.2,
    color = "black", size = 3,
    family = "Helvetica",
    inherit.aes = FALSE
  ) +
  scale_y_reordered() +
  scale_fill_manual(values = c(
    "Dominant"    = col_dom,
    "Traits"      = col_traits,
    "Subordinate" = col_sub
  )) +
  scale_color_manual(values = c(
    "Dominant"    = col_dom,
    "Traits"      = col_traits,
    "Subordinate" = col_sub
  )) +
  scale_x_continuous(
    breaks = c(-5, log(0.05), log(0.35), 0, log(2.7)),
    labels = c("-100%", "-95%", "-65%", "0%", "170%")
  ) +
  coord_cartesian(xlim = c(-5.5, log(2.7) + 0.1), clip = "off") +
  labs(x = "Proportional change in productivity", y = NULL) +
  theme_classic() +
  theme(
    legend.position  = "none",
    strip.text       = element_blank(),
    strip.background = element_blank(),
    axis.title.x     = element_text(face = "bold"),
    text             = element_text(size = 10, family = "Helvetica")
  )
  



  
  
  # 7d. Overall posterior halfeye plot -----------------------------------------


plot_halfeye <- ggplot(
  overall_slopes,
  aes(x = slope, y = removal_trt, fill = removal_trt, colour = removal_trt)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95)) +
  geom_text(
    data = overall_slopes %>% distinct(removal_trt),
    aes(label = removal_trt, colour = removal_trt),
    x = 0.1, y = Inf,
    hjust = 0, vjust = 1.2,
    size = 3.5, family = "Helvetica",
    inherit.aes = FALSE
  ) +
  facet_grid(removal_trt ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    "Dominant"    = col_dom,
    "Traits"      = col_traits,
    "Subordinate" = col_sub
  )) +
  scale_colour_manual(values = c(
    "Dominant"    = col_dom,
    "Traits"      = col_traits,
    "Subordinate" = col_sub
  )) +
  coord_cartesian(xlim = c(-6, 2.5), clip = "off") +
  labs(x = "Overall slope", y = NULL) +
  theme_classic() +
  theme(
    legend.position  = "none",
    strip.text       = element_blank(),
    strip.background = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.title.x     = element_text(face = "bold"),
    text             = element_text(size = 10, family = "Helvetica")
  )


# 7e. Centered legend --------------------------------------------------------


legend_gg <- ggplot(
  data.frame(response_variable = c("biomass", "cover")),
  aes(x = 1, y = 1, shape = response_variable)
) +
  geom_point(alpha = 0) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  guides(shape = guide_legend(
    title         = "Response variable",
    override.aes  = list(alpha = 1, size = 2)
  )) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    text            = element_text(size = 9, family = "Helvetica")
  )


# 8. Combine and save --------------------------------------------------------


final_plot <- (rem_combined | plot2_combined) / wrap_elements(legend_gg) +
  plot_layout(heights = c(20, -0.6)) +
  plot_annotation(tag_levels = list(c("a", "b", "")))

final_plot

# ggsave(
#   filename = "Fig3.pdf",
#   plot     = final_plot,
#   width    = 200,
#   height   = 150,
#   units    = "mm",
#   path     = here::here("figures"),
#   device   = cairo_pdf
# )

ggsave(
  filename = "Supp_Fig_removal_treatment_slopes.pdf",
  plot     = plot_halfeye,
  width    = 100,
  height   = 100,
  units    = "mm",
  path     = here::here("figures"),
  device   = cairo_pdf
)

# 9. Posterior summaries (for reporting) ------------------------------------

draws <- as_draws_df(fit3_inter)

# Posterior probability that each treatment slope is negative
mean(draws$b_removed_propo < 0)                                                       # dominant
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trttraits`) < 0)         # traits
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trtsubordinate`) < 0)    # subordinate

# % productivity change at 50% species removal (removed_propo = 0.5)
draws %>%
  as_tibble() %>%
  transmute(
    pct_dominant    = (exp(b_removed_propo * 0.5) - 1) * 100,
    pct_traits      = (exp((b_removed_propo + `b_removed_propo:removal_trttraits`)      * 0.5) - 1) * 100,
    pct_subordinate = (exp((b_removed_propo + `b_removed_propo:removal_trtsubordinate`) * 0.5) - 1) * 100
  ) %>%
  summarise(across(everything(),
                   list(mean = mean,
                        lower  = ~ quantile(.x, 0.025),
                        upper  = ~ quantile(.x, 0.975))))

# Marginal slopes (emtrends)
emtrends(fit3_inter, ~ removal_trt, var = "removed_propo")
