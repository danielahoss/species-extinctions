# Figure 2 from TWO separate models (biomass + cover), plotted together
# ---------------------------------------------------------------------------
# Round 3 revision (Reviewer #2 Comment 2.2 / Reviewer #3 Comment 3.2).
# Same layout as scripts/3.2_productivity_plot.R, but the single pooled overlay
# is replaced by one biomass overlay + one cover overlay in both panels:
#   (a) scatter with two population-level dose-response curves;
#   (b) forest of per-study slopes with two overall-effect reference lines.
# A single 2-colour scheme (biomass = orange, cover = green) is shared across
# both panels so the study lines/points in (a) match the slopes in (b).
#
# Models: scripts/3.1b_productivity_models_separate.R
#   -> model_output/model_biomass_2.rds, model_output/model_cover_2.rds
# Until those in-repo refits exist, this falls back to the reanalysis fits.

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(patchwork)
library(here)

rm(list = ls())

# 1. Load the two separate models ---------------------------------------------
# Prefer the in-repo refits; fall back to the reanalysis fits if not yet built.

load_model <- function(repo_name, fallback_path) {
  p <- here("model_output", repo_name)
  if (file.exists(p)) read_rds(p) else read_rds(fallback_path)
}
reanalysis <- here("..", "model_output_reanalysis")
mb <- load_model("model_biomass_2.rds", file.path(reanalysis, "model_biomass.rds"))
mc <- load_model("model_cover_2.rds",   file.path(reanalysis, "model_cover.rds"))

col_biomass <- "#D55E00"   # orange
col_cover   <- "#009E73"   # green
colour_vals <- c(biomass = col_biomass, cover = col_cover)

# 2. Extract the per-model plotting pieces ------------------------------------

resp_col <- function(m) if ("biomass" %in% colnames(m$data)) "biomass" else "cover"

pieces <- function(m, lab) {
  d <- m$data
  if (!"productivity" %in% colnames(d)) d$productivity <- d[[resp_col(m)]]

  # smooth population-level dose-response curve + 95% ribbon (re_formula = NA)
  grid <- tibble(removed_propo = seq(0, max(d$removed_propo, na.rm = TRUE),
                                     length.out = 120))
  overall <- bind_cols(
    grid,
    as_tibble(fitted(m, newdata = grid, re_formula = NA, scale = "linear",
                     prob = c(0.025, 0.975)))
  ) %>%
    transmute(removed_propo, response_variable = lab,
              y = exp(Estimate), ymin = exp(Q2.5), ymax = exp(Q97.5))

  # per-study intercept + slope for the scatter segments
  cf <- coef(m)$study_ID
  scoef <- tibble(
    study_ID  = rownames(cf[, , "Intercept"]),
    Intercept = cf[, "Estimate", "Intercept"],
    Slope     = cf[, "Estimate", "removed_propo"]
  ) %>%
    inner_join(d %>% group_by(study_ID) %>%
                 summarise(xmin = min(removed_propo), xmax = max(removed_propo),
                           .groups = "drop"), by = "study_ID") %>%
    mutate(response_variable = lab)

  # per-study slope draws for the forest
  sl <- m %>%
    spread_draws(b_removed_propo, r_study_ID[study_ID, term]) %>%
    filter(term == "removed_propo") %>%
    mutate(slope = b_removed_propo + r_study_ID, response_variable = lab)

  pts <- d %>% transmute(study_ID, removed_propo, productivity,
                         response_variable = lab)

  list(overall = overall, scoef = scoef, sl = sl, pts = pts,
       fx = fixef(m)["removed_propo", ])
}

pb <- pieces(mb, "biomass")
pc <- pieces(mc, "cover")

overall <- bind_rows(pb$overall, pc$overall)
scoef   <- bind_rows(pb$scoef,   pc$scoef)
pts     <- bind_rows(pb$pts,     pc$pts)
sl      <- bind_rows(pb$sl,      pc$sl)
fx      <- tibble(response_variable = c("biomass", "cover"),
                  est = c(pb$fx["Estimate"], pc$fx["Estimate"]),
                  lo  = c(pb$fx["Q2.5"],     pc$fx["Q2.5"]),
                  hi  = c(pb$fx["Q97.5"],    pc$fx["Q97.5"]))

theme_combined <- theme(
  legend.position = "none",
  panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"),
  axis.title = element_text(face = "bold"),
  text = element_text(size = 10, family = "Helvetica"))

# 3. Panel (a): scatter with two overall curves -------------------------------

plot_scatter <- ggplot() +
  geom_ribbon(data = overall,
              aes(x = removed_propo, ymin = ymin, ymax = ymax,
                  fill = response_variable), alpha = 0.18) +
  geom_segment(data = scoef,
               aes(x = xmin, xend = xmax,
                   y = exp(Intercept + Slope * xmin),
                   yend = exp(Intercept + Slope * xmax),
                   colour = response_variable), linewidth = 0.4, alpha = 0.35) +
  geom_point(data = pts,
             aes(x = removed_propo, y = productivity, colour = response_variable,
                 shape = response_variable), size = 1, alpha = 0.30) +
  geom_line(data = overall,
            aes(x = removed_propo, y = y, colour = response_variable),
            linewidth = 1.4) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  scale_x_continuous(labels = function(x) ifelse(
    x == 0, "0%", scales::percent(x, scale = 100))) +
  scale_y_continuous(trans = "log",
                     breaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)) +
  labs(x = "Proportion of species removed", y = "Productivity (log scale)") +
  theme_combined

# 4. Panel (b): forest with two overall-effect reference lines ----------------

rlev <- 0.5   # summarise the forest at 50% removal (matches the main Fig 2)
labs_n <- pts %>% count(study_ID) %>% mutate(label = paste0(study_ID, " (n=", n, ")"))
sl <- sl %>% mutate(chg = slope * rlev) %>% left_join(labs_n, by = "study_ID")
ord <- sl %>% group_by(label) %>% summarise(m = mean(chg), .groups = "drop") %>%
  arrange(m) %>% pull(label)
sl <- sl %>% mutate(label = factor(label, levels = ord))
fx50 <- fx %>% mutate(across(c(est, lo, hi), ~ .x * rlev))

plot_forest <- ggplot(sl, aes(x = chg, y = label, colour = response_variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  geom_rect(data = fx50, inherit.aes = FALSE,
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf,
                fill = response_variable), alpha = 0.10) +
  geom_vline(data = fx50, aes(xintercept = est, colour = response_variable),
             linewidth = 1.1) +
  stat_halfeye(aes(fill = response_variable), slab_alpha = 0.35,
               slab_size = 0, point_size = 0.9, interval_size = 0.7) +
  scale_colour_manual(values = colour_vals) +
  scale_fill_manual(values = colour_vals) +
  scale_x_continuous(breaks = log(c(0.10, 0.30, 0.50, 0.75, 1.0, 1.5)),
                     labels = c("-90%", "-70%", "-50%", "-25%", "0%", "+50%")) +
  coord_cartesian(xlim = c(-2.4, 0.55)) +
  labs(x = "Proportional change at 50% removal", y = NULL) +
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_text(face = "bold"),
        text = element_text(size = 10, family = "Helvetica"))

# 5. Shared legend + assembly -------------------------------------------------

legend_gg <- ggplot(tibble(response_variable = c("biomass", "cover")),
                    aes(1, 1, colour = response_variable, shape = response_variable)) +
  geom_point(size = 2, alpha = 0) +
  scale_colour_manual(values = colour_vals, name = "Response variable") +
  scale_shape_manual(values = c(biomass = 16, cover = 17), name = "Response variable") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         shape  = guide_legend(override.aes = list(alpha = 1))) +
  theme_void() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"),
        text = element_text(size = 9, family = "Helvetica"))

final_plot <- (plot_scatter | plot_forest) / wrap_elements(legend_gg) +
  plot_annotation(tag_levels = list(c("a", "b", ""))) +
  plot_layout(heights = c(20, -0.4))

final_plot

# The two removal slopes (log scale)
fixef(mb)["removed_propo", ]
fixef(mc)["removed_propo", ]

ggsave(here("figures", "SupplementaryFigure_08_separate_models.pdf"),
       final_plot, width = 200, height = 150, units = "mm", device = cairo_pdf)
