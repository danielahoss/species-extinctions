# Supplementary Fig. 13 from the type-moderator additional-disturbances model
# ---------------------------------------------------------------------------
# Path A (measurement type on mean + sigma). A single combined model where the
# proportion-removed effect interacts with the additional disturbance applied
# (no additional disturbance = reference; drought, nutrient, warming as offsets).
#   (a) productivity vs proportion removed, one fitted line per disturbance
#       (marginalised over measurement type, drawn over each disturbance's
#       observed removal range);
#   (b) overall proportion-removed slope per disturbance, summarised at 50%
#       removal (matching Fig 2 / Supplementary Fig. 11).
# Model: model_output/model_dist_typemod.rds (scripts/5.1)

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(patchwork)
library(posterior)
library(here)

rm(list = ls())

m  <- read_rds(here("model_output", "model_dist_typemod.rds"))
df <- m$data %>%
  mutate(add_dist = factor(add_dist, levels = c("no", "drought", "nutrient", "warming")))

lev  <- c("no", "drought", "nutrient", "warming")
labs_dist <- c(no = "No additional\ndisturbance", drought = "Drought",
               nutrient = "Nutrient addition", warming = "Warming")
cv <- c(no = "#555555", drought = "#DDAA33", nutrient = "#228833", warming = "#CC3311")

theme_combined <- theme(
  legend.position = "none",
  panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"), axis.title = element_text(face = "bold"),
  text = element_text(size = 10, family = "Helvetica"))

# ---- posterior pieces --------------------------------------------------------

post <- as_draws_df(m)
mcov <- post$b_measurecover
slope_of <- list(
  no       = post$b_removed_propo,
  drought  = post$b_removed_propo + post$`b_add_distdrought:removed_propo`,
  nutrient = post$b_removed_propo + post$`b_add_distnutrient:removed_propo`,
  warming  = post$b_removed_propo + post$`b_add_distwarming:removed_propo`)
icpt_of <- list(
  no       = post$b_Intercept,
  drought  = post$b_Intercept + post$b_add_distdrought,
  nutrient = post$b_Intercept + post$b_add_distnutrient,
  warming  = post$b_Intercept + post$b_add_distwarming)

# ---- Panel a: fitted lines marginalised over measure, clipped to data range --

xmax_dist <- df %>% group_by(add_dist) %>%
  summarise(xmax = max(removed_propo, na.rm = TRUE), .groups = "drop")

line_df <- map_dfr(lev, function(d) {
  s <- slope_of[[d]]; i <- icpt_of[[d]]
  xg <- seq(0, xmax_dist$xmax[xmax_dist$add_dist == d], length.out = 100)
  map_dfr(xg, function(x) {
    marg <- 0.5 * (exp(i + s * x) + exp(i + s * x + mcov))   # avg over measure
    tibble(add_dist = d, removed_propo = x,
           y = median(marg), ymin = quantile(marg, .025), ymax = quantile(marg, .975))
  })
}) %>% mutate(add_dist = factor(add_dist, levels = lev))

pts <- df %>% transmute(add_dist, removed_propo, productivity, measure)

plot_a <- ggplot() +
  geom_ribbon(data = line_df, aes(removed_propo, ymin = ymin, ymax = ymax,
                                  fill = add_dist), alpha = 0.22) +
  geom_point(data = pts, aes(removed_propo, productivity, colour = add_dist,
                             shape = measure), size = 1, alpha = 0.45) +
  geom_line(data = line_df, aes(removed_propo, y, colour = add_dist), linewidth = 1.2) +
  scale_colour_manual(values = cv) + scale_fill_manual(values = cv) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = function(x) ifelse(x == 0, "0%\ncontrol",
                                                 scales::percent(x, scale = 100))) +
  scale_y_continuous(trans = "log", breaks = c(1,2,4,8,16,32,64,128,256,512,1024,2048)) +
  labs(x = "Proportion of species removed", y = "Productivity (log scale)") +
  theme_combined

# ---- Panel b: overall slope per disturbance, summarised at 50% removal --------

rlev <- 0.5
sl <- imap_dfr(slope_of, ~ tibble(add_dist = .y, slope = .x)) %>%
  mutate(chg = slope * rlev,
         add_dist = factor(add_dist, levels = rev(lev)))

# posterior probability of a negative slope per disturbance (for the caption)
sl %>% group_by(add_dist) %>%
  summarise(pp_negative = round(mean(slope < 0), 3), .groups = "drop") %>% print()

plot_b <- ggplot(sl, aes(x = chg, y = add_dist, colour = add_dist, fill = add_dist)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95), slab_size = 0.5, point_size = 0.9) +
  facet_grid(add_dist ~ ., scales = "free_y", space = "free_y") +
  geom_text(data = distinct(sl, add_dist),
            aes(label = labs_dist[as.character(add_dist)]),
            x = log(0.09), y = Inf, hjust = 0, vjust = 1.3,
            colour = "black", size = 3, family = "Helvetica", inherit.aes = FALSE) +
  scale_colour_manual(values = cv) + scale_fill_manual(values = cv) +
  scale_x_continuous(breaks = log(c(0.10, 0.30, 0.50, 0.75, 1.0, 1.5)),
                     labels = c("-90%", "-70%", "-50%", "-25%", "0%", "+50%")) +
  coord_cartesian(xlim = c(-3.0, 0.7), clip = "off") +
  labs(x = "Proportional change at 50% removal", y = NULL) +
  theme_classic() +
  theme(legend.position = "none", strip.text = element_blank(),
        strip.background = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.x = element_text(face = "bold"),
        text = element_text(size = 10, family = "Helvetica"))

# ---- legend (shape = measure) ------------------------------------------------

legend_gg <- ggplot(tibble(measure = c("biomass", "cover")), aes(1, 1, shape = measure)) +
  geom_point(size = 2, alpha = 0) +
  scale_shape_manual(values = c(biomass = 16, cover = 17), name = "Response variable") +
  guides(shape = guide_legend(override.aes = list(alpha = 1))) +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"),
        text = element_text(size = 9, family = "Helvetica"))

final_plot <- (plot_a | plot_b) / wrap_elements(legend_gg) +
  plot_annotation(tag_levels = list(c("a", "b", ""))) +
  plot_layout(heights = c(20, -0.5))

ggsave(here("figures", "SupplementaryFigure_13_additional_disturbances.pdf"),
       final_plot, width = 200, height = 150, units = "mm", device = cairo_pdf)
message("saved Supplementary Fig. 13")
