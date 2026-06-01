# Additional disturbances plot
# Figures: scatter + forest plot of productivity ~ add_dist * removed_propo

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(tidytext)
library(patchwork)
library(here)

rm(list = ls())

# 1. Load model and data ------------------------------------------------------

fit_dist <- read_rds(here::here("model_output", "additional_disturbances_1.rds"))

df_biom  <- read.csv(here::here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here::here("data", "df_cover_brm_2.csv"), header = TRUE)

df_biom$add_dist <- "no"
df_biom$add_dist[df_biom$drought  == "yes"] <- "drought"
df_biom$add_dist[df_biom$nutrient == "yes"] <- "nutrient"
df_biom$add_dist <- factor(df_biom$add_dist, levels = c("drought", "no", "nutrient"))
df_biom$add_dist <- relevel(df_biom$add_dist, ref = "no")

df_cover$add_dist <- "no"
df_cover$add_dist[df_cover$warming  == "yes"] <- "warming"
df_cover$add_dist[df_cover$nutrient == "yes"] <- "nutrient"
df_cover$add_dist <- factor(df_cover$add_dist, levels = c("warming", "no", "nutrient"))
df_cover$add_dist <- relevel(df_cover$add_dist, ref = "no")

df_biom_add <- df_biom %>%
  select(study_ID, block, plot, time_length_years, experiment_duration,
         pre_removal, add_dist, country, latitude, longitude, altitude_m,
         mean_annual_ppt_mm, mean_annual_temperature_Celsius, latitude_abs,
         author, publ_year, removal_method_category, removal_treatment,
         removal_treatment_category, response_variable, n_removed, removed_propo,
         biomass, richness, ricmin, ricmean, ricmax,
         remov_propo_min, remov_propo_mean, remov_propo_max,
         n_remov_min, n_remov_mean, n_remov_max) %>%
  mutate(block = as.character(block), productivity = biomass)

df_cover_add <- df_cover %>%
  select(study_ID, block, plot, time_length_years, experiment_duration,
         pre_removal, add_dist, country, latitude, longitude, altitude_m,
         mean_annual_ppt_mm, mean_annual_temperature_Celsius, latitude_abs,
         author, publ_year, removal_method_category, removal_treatment,
         removal_treatment_category, response_variable, n_removed, removed_propo,
         cover, richness, ricmin, ricmean, ricmax,
         remov_propo_min, remov_propo_mean, remov_propo_max,
         n_remov_min, n_remov_mean, n_remov_max) %>%
  mutate(block = as.character(block), productivity = cover)

df <- bind_rows(df_biom_add, df_cover_add)

# 2. Fitted values (population-level) ----------------------------------------

fitted_df <- cbind(
  fit_dist$data,
  fitted(fit_dist, re_formula = NA, scale = "linear", prob = c(0.025, 0.975))
) %>%
  tibble::as_tibble() %>%
  mutate(
    add_dist          = factor(add_dist, levels = c("no", "warming", "drought", "nutrient")),
    response_variable = df$response_variable
  )

# 3. Build plotting data ------------------------------------------------------

cv_sub <- fitted_df %>%
  filter(productivity > 0, !is.na(removed_propo), !is.na(add_dist),
         !is.na(Estimate), !is.na(Q2.5), !is.na(Q97.5)) %>%
  mutate(ymin_exp = exp(Q2.5), ymax_exp = exp(Q97.5)) %>%
  filter(!is.infinite(ymin_exp), !is.infinite(ymax_exp))

# 4. Colors and theme ---------------------------------------------------------

col_drought <- "#E69F00"
col_warming <- "#D55E00"
col_nutr    <- "#6B5EA8"
col_no      <- "gray60"

colour_vals <- c(no = col_no, warming = col_warming,
                 nutrient = col_nutr, drought = col_drought)

colour_vals_labelled <- c(
  "Species removed"   = col_no,
  "Warming"           = col_warming,
  "Drought"           = col_drought,
  "Nutrient addition" = col_nutr
)

theme_dist <- theme(
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

# 5. Scatter + fitted line plot -----------------------------------------------

plot_scatter <- ggplot(
  cv_sub,
  aes(x = removed_propo, colour = add_dist, fill = add_dist)
) +
  geom_ribbon(aes(ymin = ymin_exp, ymax = ymax_exp), alpha = 0.4, colour = NA) +
  geom_point(aes(y = productivity, shape = response_variable), size = 1, alpha = 0.5) +
  geom_line(aes(y = exp(Estimate)), linewidth = 1.2) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values   = colour_vals) +
  scale_x_continuous(
    labels = function(x) ifelse(x == 0, "0%\ncontrol", scales::percent(x, scale = 100))
  ) +
  scale_y_continuous(
    trans  = "log",
    breaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)
  ) +
  labs(x = "Proportion of species removed", y = "Productivity") +
  theme_dist

# 6. Study-level slopes -------------------------------------------------------

draws_tidy <- fit_dist %>%
  spread_draws(
    b_removed_propo,
    `b_add_distwarming:removed_propo`,
    `b_add_distnutrient:removed_propo`,
    `b_add_distdrought:removed_propo`,
    r_study_ID[study_ID, term]
  ) %>%
  pivot_wider(names_from = term, values_from = r_study_ID)

draws_slopes <- draws_tidy %>%
  mutate(
    slope_no       = b_removed_propo + removed_propo,
    slope_warming  = b_removed_propo + `b_add_distwarming:removed_propo`  + removed_propo,
    slope_drought  = b_removed_propo + `b_add_distdrought:removed_propo`  + removed_propo,
    slope_nutrient = b_removed_propo + `b_add_distnutrient:removed_propo` + removed_propo
  )

study_slopes <- draws_slopes %>%
  pivot_longer(cols = c(slope_no, slope_nutrient, slope_warming, slope_drought),
               names_to = "add_dist", values_to = "slope") %>%
  mutate(add_dist = str_remove(add_dist, "slope_")) %>%
  group_by(study_ID, add_dist) %>%
  summarise(mean  = mean(slope, na.rm = TRUE),
            lower = quantile(slope, 0.025, na.rm = TRUE),
            upper = quantile(slope, 0.975, na.rm = TRUE),
            .groups = "drop")

study_dist_combos <- cv_sub %>%
  distinct(study_ID, add_dist) %>%
  mutate(add_dist = as.character(add_dist))

study_labels <- cv_sub %>%
  group_by(study_ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste0(study_ID, " (n=", n, ")"))

study_slopes <- study_slopes %>%
  inner_join(study_dist_combos, by = c("study_ID", "add_dist")) %>%
  left_join(df %>% distinct(study_ID, response_variable), by = "study_ID") %>%
  left_join(study_labels, by = "study_ID") %>%
  mutate(
    add_dist = case_when(
      add_dist == "no"       ~ "Species removed",
      add_dist == "warming"  ~ "Warming",
      add_dist == "nutrient" ~ "Nutrient addition",
      add_dist == "drought"  ~ "Drought",
      TRUE                   ~ add_dist
    ),
    add_dist = factor(add_dist, levels = c("Warming", "Drought",
                                           "Nutrient addition", "Species removed"))
  )

# 7. Overall posterior slopes -------------------------------------------------

overall_slopes <- fit_dist %>%
  spread_draws(
    b_removed_propo,
    `b_add_distwarming:removed_propo`,
    `b_add_distdrought:removed_propo`,
    `b_add_distnutrient:removed_propo`
  ) %>%
  mutate(
    `Species removed`   = b_removed_propo,
    Warming             = b_removed_propo + `b_add_distwarming:removed_propo`,
    Drought             = b_removed_propo + `b_add_distdrought:removed_propo`,
    `Nutrient addition` = b_removed_propo + `b_add_distnutrient:removed_propo`
  ) %>%
  pivot_longer(cols = c(`Species removed`, Warming, Drought, `Nutrient addition`),
               names_to = "add_dist", values_to = "slope") %>%
  mutate(add_dist = factor(add_dist,
                           levels = c("Warming", "Nutrient addition",
                                      "Drought", "Species removed")))

# 8. Forest plot --------------------------------------------------------------

plot_forest <- ggplot(
  study_slopes,
  aes(x = mean, y = reorder_within(label, mean, add_dist), colour = add_dist)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  geom_pointrange(aes(xmin = lower, xmax = upper, shape = response_variable),
                  linewidth = 0.3, size = 0.6) +
  facet_grid(add_dist ~ ., scales = "free_y", space = "free_y") +
  scale_y_reordered() +
  scale_color_manual(values = colour_vals_labelled) +
  labs(x = "Slope of removed_propo (study-level)", y = NULL) +
  theme_classic() +
  theme(
    legend.position  = "none",
    strip.text       = element_blank(),
    strip.background = element_blank(),
    axis.title.x     = element_text(face = "bold"),
    text             = element_text(size = 10, family = "Helvetica")
  )

# 9. Overall posterior halfeye ------------------------------------------------

plot_halfeye <- ggplot(
  overall_slopes,
  aes(x = slope, y = add_dist, fill = add_dist, colour = add_dist)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95)) +
  geom_text(
    data = overall_slopes %>% distinct(add_dist),
    aes(label = add_dist, colour = add_dist),
    x = -0.1, y = Inf,
    hjust = 1, vjust = 1.2,
    size = 3.5, family = "Helvetica",
    inherit.aes = FALSE
  ) +
  facet_grid(add_dist ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values   = colour_vals_labelled) +
  scale_colour_manual(values = colour_vals_labelled) +
  coord_cartesian(xlim = c(-6, 1), clip = "off") +
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

# 10. Legend ------------------------------------------------------------------

legend_gg <- ggplot(
  data.frame(response_variable = c("biomass", "cover")),
  aes(x = 1, y = 1, shape = response_variable)
) +
  geom_point(alpha = 0) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  guides(shape = guide_legend(title = "Response variable",
                              override.aes = list(alpha = 1, size = 2))) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title    = element_text(face = "bold"),
        text            = element_text(size = 9, family = "Helvetica"))

# 11. Combine and save --------------------------------------------------------

final_plot <- (plot_scatter | plot_forest) / wrap_elements(legend_gg) +
  plot_annotation(tag_levels = list(c("a", "b", ""))) +
  plot_layout(heights = c(20, -0.6))

final_plot

ggsave(filename = "Extended Data Fig. 12 - add_dist.pdf",
       plot = final_plot, width = 200, height = 150, units = "mm",
       path = here::here("figures"), device = cairo_pdf)

ggsave(
  filename = "Extended Data Fig. 13 - add_dist.pdf",
  plot     = plot_halfeye,
  width    = 100,
  height   = 100,
  units    = "mm",
  path     = here::here("figures"),
  device   = cairo_pdf
)

# 12. Posterior probabilities -------------------------------------------------

draws_raw <- as_draws_df(fit_dist)

mean(draws_raw$b_removed_propo < 0)
mean((draws_raw$b_removed_propo + draws_raw$`b_add_distnutrient:removed_propo`) < 0)

draws_raw %>%
  as_tibble() %>%
  transmute(
    pct_no       = (exp(b_removed_propo * 0.5) - 1) * 100,
    pct_nutrient = (exp((b_removed_propo + `b_add_distnutrient:removed_propo`) * 0.5) - 1) * 100
  ) %>%
  summarise(across(everything(),
                   list(median = median,
                        lower  = ~ quantile(.x, 0.025),
                        upper  = ~ quantile(.x, 0.975))))
