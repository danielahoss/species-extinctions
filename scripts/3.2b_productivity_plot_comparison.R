# Supplementary Fig. 8: combined vs separate models, 1:1 comparison
# ---------------------------------------------------------------------------
# A single visual check that combining biomass and cover in one model does not
# distort the estimates. Per-study and average proportion-removed slopes from
# the combined type-moderator model (x) vs the separate biomass/cover models
# (y), with a 1:1 line: points on the line mean pooling with a measurement-type
# term does not shift the estimates. All 24 studies here have raw plot-level data.
# Models: model_removed_propo_typemod ; model_biomass_2 ; model_cover_2
suppressMessages({library(tidyverse); library(brms); library(tidybayes)
                  library(posterior); library(here)})
rm(list = ls())
repo <- here::here()
col <- c(biomass = "#D55E00", cover = "#009E73")

# ---- combined type-moderator model: per-study slope draws --------------------
m  <- read_rds(file.path(repo, "model_output", "model_removed_propo_typemod.rds"))
sm <- distinct(m$data, study_ID, measure)
fx <- as_draws_df(m) %>% as_tibble() %>%
  transmute(.draw, b = b_removed_propo, bint = `b_removed_propo:measurecover`)
comb <- spread_draws(m, r_study_ID[study_ID, term]) %>%
  filter(term == "removed_propo") %>% left_join(fx, by = ".draw") %>%
  left_join(sm, by = "study_ID") %>%
  mutate(slope = b + r_study_ID + if_else(measure == "cover", bint, 0)) %>%
  group_by(study_ID, measure) %>%
  summarise(c_est = median(slope), c_lo = quantile(slope, .025),
            c_hi = quantile(slope, .975), .groups = "drop")

# ---- separate models: per-study slope draws ---------------------------------
sep_slopes <- function(path, lab) {
  mm <- read_rds(file.path(repo, "model_output", path))
  mm %>% spread_draws(b_removed_propo, r_study_ID[study_ID, term]) %>%
    filter(term == "removed_propo") %>%
    mutate(slope = b_removed_propo + r_study_ID, measure = lab) %>%
    group_by(study_ID, measure) %>%
    summarise(s_est = median(slope), s_lo = quantile(slope, .025),
              s_hi = quantile(slope, .975), .groups = "drop")
}
sep <- bind_rows(sep_slopes("model_biomass_2.rds", "biomass"),
                 sep_slopes("model_cover_2.rds",  "cover"))

df <- inner_join(comb, sep, by = c("study_ID", "measure"))

# overall (fixed) slopes, with 95% CIs, from both model sets
ov_comb <- tibble(measure = c("biomass", "cover"),
                  c_est = c(median(fx$b), median(fx$b + fx$bint)),
                  c_lo  = c(quantile(fx$b, .025), quantile(fx$b + fx$bint, .025)),
                  c_hi  = c(quantile(fx$b, .975), quantile(fx$b + fx$bint, .975)))
fxb <- fixef(read_rds(file.path(repo,"model_output","model_biomass_2.rds")))["removed_propo",]
fxc <- fixef(read_rds(file.path(repo,"model_output","model_cover_2.rds")))["removed_propo",]
ov_sep <- tibble(measure = c("biomass", "cover"),
                 s_est = c(fxb["Estimate"], fxc["Estimate"]),
                 s_lo  = c(fxb["Q2.5"],     fxc["Q2.5"]),
                 s_hi  = c(fxb["Q97.5"],    fxc["Q97.5"]))
ov <- inner_join(ov_comb, ov_sep, by = "measure")

# zoom to fit the bulk of the estimates; the widest CIs clip softly at the border
rng <- c(min(c(df$c_est, df$s_est)) - 1.4, max(c(df$c_est, df$s_est)) + 0.7)

p <- ggplot(df, aes(c_est, s_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "gray55") +
  # study-level estimates: faint thin CIs behind, haloed points on top
  geom_linerange(aes(ymin = s_lo, ymax = s_hi, colour = measure), linewidth = 0.3, alpha = 0.15) +
  geom_linerange(aes(xmin = c_lo, xmax = c_hi, colour = measure), linewidth = 0.3, alpha = 0.15) +
  geom_point(aes(fill = measure, shape = "Study-level"), size = 2.4,
             colour = "white", stroke = 0.4, alpha = 0.65) +
  # average (fixed-effect) estimates: coloured diamonds with 95% CI bars
  geom_linerange(data = ov, aes(ymin = s_lo, ymax = s_hi, colour = measure), linewidth = 0.9) +
  geom_linerange(data = ov, aes(xmin = c_lo, xmax = c_hi, colour = measure), linewidth = 0.9) +
  geom_point(data = ov, aes(fill = measure, shape = "Average"), size = 4.8,
             colour = "white", stroke = 0.7) +
  scale_colour_manual(values = col, guide = "none") +
  scale_fill_manual(values = col, name = "Response variable",
                    guide = guide_legend(order = 1,
                      override.aes = list(shape = 21, size = 3.2, colour = NA, alpha = 1))) +
  scale_shape_manual(values = c("Study-level" = 21, "Average" = 23), name = "Estimate",
                     guide = guide_legend(order = 2,
                       override.aes = list(fill = "black", colour = "black", size = 3.2, alpha = 1))) +
  coord_equal(xlim = rng, ylim = rng, expand = FALSE) +
  labs(x = "Removal slope, combined model (log scale)",
       y = "Removal slope, separate models (log scale)") +
  theme_minimal(base_size = 10, base_family = "Helvetica") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(colour = "black"), axis.title = element_text(face = "bold"),
        axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
        legend.position = c(0.015, 0.985), legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
        legend.title = element_text(face = "bold"), legend.margin = margin(3, 5, 3, 5),
        legend.key = element_rect(fill = NA, colour = NA))

ggsave(file.path(repo, "figures", "SupplementaryFigure_08_separate_models.pdf"),
       p, width = 130, height = 130, units = "mm", device = cairo_pdf)
message("saved Supplementary Fig. 8 (combined vs separate slope comparison)")
