# Combined productivity model (biomass + cover)
# Figure: scatter + study-level forest plot
# Follows the same style as Productivity_AdditionalDisturbances.R

# 1. Load Libraries -----------------------------------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(tidytext)
library(patchwork)
library(here)

rm(list = ls())

# 2. Load Data ----------------------------------------------------------------

df_biom  <- read.csv(here::here("data", "df_biom_brm_2.csv"),  header = TRUE)
df_cover <- read.csv(here::here("data", "df_cover_brm_2.csv"), header = TRUE)

df_biom_trt <- df_biom %>%
   filter(!(study_ID == "ID_29" &
             removal_treatment %in% c("abundance_reduction",
                                      "abundance_richness_reduction"))) %>%
  select(study_ID, block, author, publ_year, plot, time_length_years,
         response_variable, removed_propo, biomass) %>%
  mutate(block = as.character(block), productivity = biomass)

df_cover_trt <- df_cover %>%
  select(study_ID, block, author, publ_year, plot, time_length_years,
         response_variable, removed_propo, cover) %>%
  mutate(block = as.character(block), productivity = cover)

df <- bind_rows(df_biom_trt, df_cover_trt)

# 3. Load Model ---------------------------------------------------------------

mod_rem_propo <- read_rds(here::here("model_output", "model_removed_propo_2.rds"))
mod_rem_propo %>% summary()

prod_fix <- as.data.frame(fixef(mod_rem_propo))

# 4. Fitted values (population-level) ----------------------------------------

fitted_df <- cbind(
  mod_rem_propo$data,
  fitted(mod_rem_propo, re_formula = NA, scale = "linear",
         prob = c(0.025, 0.975))
) %>%
  tibble::as_tibble() %>%
  left_join(
    df %>% distinct(study_ID, block, plot, time_length_years,
                    removed_propo, productivity, response_variable),
    by = c("study_ID", "block", "plot", "time_length_years",
           "removed_propo", "productivity")
  ) %>%
  mutate(ymin_exp = exp(Q2.5), ymax_exp = exp(Q97.5)) %>%
  filter(productivity > 0, !is.na(removed_propo), !is.na(Estimate))

# 5. Study-level coefficients for scatter lines ------------------------------

study_rv <- df %>% distinct(study_ID, response_variable)

prod_exp_coef <- coef(mod_rem_propo)

study_coef <- bind_cols(
  prod_exp_coef$study_ID[,, "Intercept"] %>%
    as_tibble() %>%
    mutate(Intercept = Estimate,
           study_ID  = rownames(prod_exp_coef$study_ID[,, "Intercept"])) %>%
    select(study_ID, Intercept),
  prod_exp_coef$study_ID[,, "removed_propo"] %>%
    as_tibble() %>%
    mutate(Slope = Estimate) %>%
    select(Slope)
) %>%
  inner_join(
    df %>%
      group_by(study_ID) %>%
      summarise(xmin = min(removed_propo),
                xmax = max(removed_propo),
                .groups = "drop"),
    by = "study_ID"
  ) %>%
  left_join(study_rv, by = "study_ID")

# 6. Colors and theme ---------------------------------------------------------

col_biomass <- "#D55E00"
col_cover   <- "#009E73"

colour_vals <- c(biomass = col_biomass, cover = col_cover)

# Per-study palettes (brown for biomass, green for cover), ordered by slope
biom_studies <- study_coef %>%
  filter(response_variable == "biomass") %>%
  arrange(Slope) %>%
  pull(study_ID)

cover_studies <- study_coef %>%
  filter(response_variable == "cover") %>%
  arrange(Slope) %>%
  pull(study_ID)

brown_palette <- setNames(
  colorRampPalette(c("#E8956B", "#8A3200"))(length(biom_studies)),
  biom_studies
)
green_palette <- setNames(
  colorRampPalette(c("#5ABFA8", "#004D38"))(length(cover_studies)),
  cover_studies
)

study_palette <- c(brown_palette, green_palette)

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
  text             = element_text(size = 10, family = "Helvetica")
)

# 7. Scatter plot -------------------------------------------------------------
# Study-level lines + overall fitted line/ribbon; points colored by response_variable

plot_scatter <- ggplot(fitted_df, aes(x = removed_propo)) +
  geom_ribbon(aes(ymin = ymin_exp, ymax = ymax_exp),
              alpha = 0.3, fill = "gray50") +
  geom_segment(data = study_coef,
               aes(x      = xmin,
                   xend   = xmax,
                   y      = exp(Intercept + Slope * xmin),
                   yend   = exp(Intercept + Slope * xmax),
                   colour = study_ID),
               linewidth = 0.5, alpha = 0.6) +
  geom_point(aes(y = productivity,
                 colour = study_ID,
                 shape  = response_variable),
             size = 1, alpha = 0.5) +
  geom_line(aes(y = exp(Estimate)), linewidth = 1.2, colour = "black") +
  scale_colour_manual(values = study_palette) +
  scale_shape_manual(values  = c(biomass = 16, cover = 17)) +
  scale_x_continuous(
    labels = function(x) ifelse(x == 0, "0%\ncontrol or\npre removals",
                                scales::percent(x, scale = 100))
  ) +
  scale_y_continuous(
    trans  = "log",
    breaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)
  ) +
  labs(x = "Proportion of species removed", y = "Productivity (log scale)") +
  theme_combined

plot_scatter

# 8. Study-level slopes -------------------------------------------------------

draws_tidy <- mod_rem_propo %>%
  spread_draws(b_removed_propo, r_study_ID[study_ID, term]) %>%
  pivot_wider(names_from = term, values_from = r_study_ID)

draws_slopes <- draws_tidy %>%
  mutate(slope = b_removed_propo + removed_propo)

study_slopes <- draws_slopes %>%
  group_by(study_ID) %>%
  summarise(
    mean  = mean(slope),
    lower = quantile(slope, 0.025),
    upper = quantile(slope, 0.975),
    .groups = "drop"
  )

# Join response_variable and labels
study_labels <- fitted_df %>%
  group_by(study_ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste0(study_ID, " (n=", n, ")"))

study_slopes <- study_slopes %>%
  left_join(study_rv,     by = "study_ID") %>%
  left_join(study_labels, by = "study_ID") %>%
  mutate(response_variable = factor(response_variable,
                                    levels = c("biomass", "cover")))

# Join draws with labels and order by median slope
ordered_labels <- study_slopes %>%
  arrange(mean) %>%
  pull(label)

draws_slopes_plot <- draws_slopes %>%
  left_join(study_rv,     by = "study_ID") %>%
  left_join(study_labels, by = "study_ID") %>%
  mutate(
    label             = factor(label, levels = ordered_labels),
    response_variable = factor(response_variable, levels = c("biomass", "cover"))
  )

# 9. Forest plot (halfeye) ----------------------------------------------------

plot_forest <-
  ggplot(
    draws_slopes_plot,
    aes(x = slope, y = label, colour = response_variable)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = prod_fix["removed_propo", "Estimate"],
             linewidth = 1.2, colour = "black") +
  annotate("rect",
           xmin  = prod_fix["removed_propo", "Q2.5"],
           xmax  = prod_fix["removed_propo", "Q97.5"],
           ymin  = -Inf, ymax = Inf,
           alpha = 0.1, fill = "gray50") +
  stat_halfeye(alpha = 0.7, slab_size = 1.2, point_size = 1) +
  scale_color_manual(values = colour_vals) +
  scale_x_continuous(
    breaks = c(-5, log(0.05), log(0.35), 0, log(2.7)),
    labels = c("-100%", "-95%", "-65%", "0%", "170%")
  ) +
  coord_cartesian(xlim = c(-5.5, log(2.7) + 0.1)) +
  labs(x = "Proportional change in productivity", y = NULL) +
  theme_classic() +
  theme(
    legend.position  = "none",
    axis.title.x     = element_text(face = "bold"),
    text             = element_text(size = 10, family = "Helvetica"),
    plot.margin      = margin(8, 80, 8, 8)
  )

plot_forest

# 10. Legend ------------------------------------------------------------------

legend_gg <- ggplot(
  data.frame(response_variable = c("biomass", "cover")),
  aes(x = 1, y = 1, shape = response_variable, colour = response_variable)
) +
  geom_point(alpha = 0) +
  scale_shape_manual(values  = c(biomass = 16, cover = 17)) +
  scale_colour_manual(values = colour_vals) +
  guides(
    shape  = guide_legend(title = "Response variable",
                          override.aes = list(alpha = 1, size = 2)),
    colour = guide_legend(title = "Response variable",
                          override.aes = list(alpha = 1, size = 2))
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    text            = element_text(size = 9, family = "Helvetica")
  )

# 11. Combine and save --------------------------------------------------------

final_plot <- (plot_scatter | plot_forest) / wrap_elements(legend_gg) +
  plot_annotation(tag_levels = list(c("a", "b", ""))) +
  plot_layout(heights = c(20, -0.6))

final_plot

# ggsave(
#   filename = "Fig2.pdf",
#   plot     = final_plot,
#   width    = 200,
#   height   = 150,
#   units    = "mm",
#   path     = here::here("figures"),
#   device   = cairo_pdf
# )
