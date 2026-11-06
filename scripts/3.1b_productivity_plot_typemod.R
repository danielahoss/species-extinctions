# Figure 2: productivity response to species removal (biomass and cover)
# ---------------------------------------------------------------------------
# (a) population-level curves and study-level lines; (b) study-level effects at
# 50% of species removed.
#
# Model: model_output/model_removed_propo_typemod_3.rds (fit in 3.1a)

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(patchwork)
library(posterior)
library(here)

rm(list = ls())

m <- read_rds(here("model_output", "model_removed_propo_typemod_3.rds"))
d <- m$data
study_measure <- distinct(d, study_ID, measure)

col_biomass <- "#D55E00"   # orange
col_cover   <- "#009E73"   # green
colour_vals <- c(biomass = col_biomass, cover = col_cover)

# 1. Posterior: fixed effects + study-level random effects, per draw -----------

fixdr <- as_draws_df(m) %>% as_tibble() %>%
  transmute(.draw, b_Intercept, b_removed_propo, b_measurecover,
            b_int = `b_removed_propo:measurecover`)

redr <- spread_draws(m, r_study_ID[study_ID, term]) %>%
  pivot_wider(names_from = term, values_from = r_study_ID) %>%   # Intercept, removed_propo
  left_join(fixdr, by = ".draw") %>%
  left_join(study_measure, by = "study_ID") %>%
  mutate(is_cover = measure == "cover",
         icpt  = b_Intercept     + Intercept     + if_else(is_cover, b_measurecover, 0),
         slope = b_removed_propo  + removed_propo + if_else(is_cover, b_int, 0))

# study-level medians
study_sl <- redr %>% group_by(study_ID, measure) %>%
  summarise(icpt = median(icpt), slope = median(slope), .groups = "drop")
xr <- d %>% group_by(study_ID) %>%
  summarise(xmin = min(removed_propo), xmax = max(removed_propo), .groups = "drop")
scoef <- left_join(study_sl, xr, by = "study_ID")

# 2. Population-level curves per measure (re_formula = NA) ---------------------

# each curve is drawn only across the observed removal range of its response type
xmax_m <- tapply(d$removed_propo, d$measure, max)
grid <- bind_rows(lapply(names(xmax_m), function(lv)
  tibble(measure = lv, removed_propo = seq(0, xmax_m[[lv]], length.out = 120)))) %>%
  mutate(measure = factor(measure, levels = c("biomass", "cover")))
overall <- bind_cols(
  grid,
  as_tibble(fitted(m, newdata = grid, re_formula = NA, scale = "linear",
                   prob = c(0.025, 0.975)))) %>%
  transmute(removed_propo, measure = as.character(measure),
            y = exp(Estimate), ymin = exp(Q2.5), ymax = exp(Q97.5))

# 3. Overall slope per measure (forest reference lines) -----------------------

slope_biom <- fixdr$b_removed_propo
slope_cov  <- fixdr$b_removed_propo + fixdr$b_int
fx <- tibble(measure = c("biomass", "cover"),
             est = c(median(slope_biom), median(slope_cov)),
             lo  = c(quantile(slope_biom, .025), quantile(slope_cov, .025)),
             hi  = c(quantile(slope_biom, .975), quantile(slope_cov, .975)))

pts <- d %>% transmute(study_ID, removed_propo, productivity,
                       measure = as.character(measure))

theme_combined <- theme(
  legend.position = "none",
  panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"), axis.title = element_text(face = "bold"),
  text = element_text(size = 10, family = "Helvetica"))

# 4. Panel a: scatter with two population-level curves ------------------------

plot_scatter <- ggplot() +
  geom_ribbon(data = overall,
              aes(removed_propo, ymin = ymin, ymax = ymax, fill = measure), alpha = 0.18) +
  geom_segment(data = scoef,
               aes(x = xmin, xend = xmax,
                   y = exp(icpt + slope * xmin), yend = exp(icpt + slope * xmax),
                   colour = measure), linewidth = 0.4, alpha = 0.35) +
  geom_point(data = pts,
             aes(removed_propo, productivity, colour = measure, shape = measure),
             size = 1, alpha = 0.30) +
  geom_line(data = overall, aes(removed_propo, y, colour = measure), linewidth = 1.4) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  scale_x_continuous(labels = function(x) ifelse(x == 0, "0%", scales::percent(x, scale = 100))) +
  scale_y_continuous(trans = "log", breaks = c(1,2,4,8,16,32,64,128,256,512,1024,2048)) +
  labs(x = "Proportion of species removed", y = "Productivity (log scale)") +
  theme_combined

# 5. Panel b: forest with two overall-effect reference lines ------------------

# change in log(productivity) at 50% removal = slope * 0.5
rlev <- 0.5

labs_n <- pts %>% count(study_ID) %>% mutate(label = paste0(study_ID, " (n=", n, ")"))
sl <- redr %>% transmute(study_ID, measure, chg = slope * rlev) %>%
  left_join(labs_n, by = "study_ID")
ord <- sl %>% group_by(label) %>% summarise(m = mean(chg), .groups = "drop") %>%
  arrange(m) %>% pull(label)
sl <- sl %>% mutate(label = factor(label, levels = ord))
fx50 <- fx %>% mutate(across(c(est, lo, hi), ~ .x * rlev))

plot_forest <- ggplot(sl, aes(x = chg, y = label, colour = measure)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  geom_rect(data = fx50, inherit.aes = FALSE,
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf, fill = measure), alpha = 0.10) +
  geom_vline(data = fx50, aes(xintercept = est, colour = measure), linewidth = 1.1) +
  stat_halfeye(aes(fill = measure), slab_alpha = 0.35, slab_size = 0,
               point_size = 0.9, interval_size = 0.7) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  scale_x_continuous(breaks = log(c(0.10, 0.30, 0.50, 0.75, 1.0, 1.5)),
                     labels = c("-90%", "-70%", "-50%", "-25%", "0%", "+50%")) +
  coord_cartesian(xlim = c(-2.4, 0.55)) +
  labs(x = "Expected change in productivity\nat 50% of species removed", y = "Study") +
  theme_classic() +
  theme(legend.position = "none", axis.title = element_text(face = "bold"),
        text = element_text(size = 10, family = "Helvetica"))

# 6. Shared legend + assembly -------------------------------------------------

legend_gg <- ggplot(tibble(measure = c("biomass", "cover")),
                    aes(1, 1, colour = measure, shape = measure)) +
  geom_point(size = 2, alpha = 0) +   # hide dummy panel points; keys shown via override
  scale_colour_manual(values = colour_vals, name = "Response variable") +
  scale_shape_manual(values = c(biomass = 16, cover = 17), name = "Response variable") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"),
        text = element_text(size = 9, family = "Helvetica"))

final_plot <- (plot_scatter | plot_forest) / wrap_elements(legend_gg) +
  plot_annotation(tag_levels = list(c("a", "b", ""))) +
  plot_layout(heights = c(20, -0.4))

final_plot

fx

ggsave(here("figures", "Fig2.pdf"), final_plot,
       width = 200, height = 150, units = "mm", device = cairo_pdf)


