# Figure 3 from the type-moderator removal-treatment model (Path A)
# ---------------------------------------------------------------------------
# Same layout as scripts/4.2 (colour = removal treatment, shape = measurement
# type), driven by model_remtrt_typemod (measure on mean + sigma; scripts/4.1c).
#   (a) productivity vs proportion removed, one fitted line per treatment
#       (marginalised over measurement type, and drawn only over each
#       treatment's observed removal range);
#   (b) study-level slopes as a forest, faceted by treatment, summarised at
#       50% removal (matching Fig 2).
# Model: model_output/model_remtrt_typemod.rds

library(tidyverse)
library(brms)
library(tidybayes)
library(ggdist)
library(tidytext)
library(patchwork)
library(posterior)
library(here)

rm(list = ls())

m <- read_rds(here("model_output", "model_remtrt_typemod.rds"))

df <- bind_rows(
  read.csv(here("data", "df_biomass_trt.csv")) %>%
    mutate(block = as.character(block), productivity = biomass, measure = "biomass"),
  read.csv(here("data", "df_cover_trt.csv")) %>%
    mutate(block = as.character(block), productivity = cover, measure = "cover")
) %>% mutate(removal_trt = as.character(removal_trt))

col_dom <- "#882255"; col_sub <- "#4477AA"; col_traits <- "#997700"
cv  <- c(dominant = col_dom, subordinate = col_sub, traits = col_traits)
cvC <- c(Dominant = col_dom, Subordinate = col_sub, Traits = col_traits)

theme_combined <- theme(
  legend.position = "none",
  panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"), axis.title = element_text(face = "bold"),
  text = element_text(size = 10, family = "Helvetica"))

# ---- Panel a: fitted lines marginalised over measure, clipped to data range --

post <- as_draws_df(m)
mcov <- post$b_measurecover
slope_of <- list(
  dominant    = post$b_removed_propo,
  subordinate = post$b_removed_propo + post$`b_removed_propo:removal_trtsubordinate`,
  traits      = post$b_removed_propo + post$`b_removed_propo:removal_trttraits`)
icpt_of <- list(
  dominant    = post$b_removal_trtdominant,
  subordinate = post$b_removal_trtsubordinate,
  traits      = post$b_removal_trttraits)

# per-treatment observed removal range: don't draw the line beyond the data
xmax_trt <- df %>% group_by(removal_trt) %>%
  summarise(xmax = max(removed_propo, na.rm = TRUE), .groups = "drop")

line_df <- map_dfr(names(slope_of), function(trt) {
  s <- slope_of[[trt]]; i <- icpt_of[[trt]]
  xg <- seq(0, xmax_trt$xmax[xmax_trt$removal_trt == trt], length.out = 100)
  map_dfr(xg, function(x) {
    marg <- 0.5 * (exp(i + s * x) + exp(i + s * x + mcov))   # avg over measure
    tibble(removal_trt = trt, removed_propo = x,
           y = median(marg), ymin = quantile(marg, .025), ymax = quantile(marg, .975))
  })
})

pts <- df %>% transmute(removal_trt, removed_propo, productivity, measure)

plot_a <- ggplot() +
  geom_ribbon(data = line_df, aes(removed_propo, ymin = ymin, ymax = ymax,
                                  fill = removal_trt), alpha = 0.25) +
  geom_point(data = pts, aes(removed_propo, productivity, colour = removal_trt,
                             shape = measure), size = 1, alpha = 0.45) +
  geom_line(data = line_df, aes(removed_propo, y, colour = removal_trt), linewidth = 1.2) +
  scale_colour_manual(values = cv) + scale_fill_manual(values = cv) +
  scale_shape_manual(values = c(biomass = 16, cover = 17)) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = function(x) ifelse(x == 0, "0%\ncontrol",
                                                 scales::percent(x, scale = 100))) +
  scale_y_continuous(trans = "log", breaks = c(1,2,4,8,16,32,64,128,256,512,1024,2048)) +
  labs(x = "Proportion of species removed", y = "Productivity (log scale)") +
  theme_combined

# ---- Panel b: study-level slopes, forest at 50% removal ----------------------

rlev <- 0.5
draws_tidy <- m %>%
  spread_draws(b_removed_propo, `b_removed_propo:removal_trttraits`,
               `b_removed_propo:removal_trtsubordinate`,
               r_study_ID[study_ID, term], ndraws = 3000) %>%
  pivot_wider(names_from = term, values_from = r_study_ID)

draws_slopes <- draws_tidy %>%
  mutate(
    dominant    = b_removed_propo + removed_propo,
    traits      = b_removed_propo + `b_removed_propo:removal_trttraits` +
                    removed_propo + `removed_propo:removal_trttraits`,
    subordinate = b_removed_propo + `b_removed_propo:removal_trtsubordinate` +
                    removed_propo + `removed_propo:removal_trtsubordinate`) %>%
  select(.draw, study_ID, dominant, traits, subordinate) %>%
  pivot_longer(c(dominant, traits, subordinate),
               names_to = "removal_trt", values_to = "slope") %>%
  mutate(chg = slope * rlev)

combos <- df %>% distinct(study_ID, removal_trt)
labs_n <- df %>% count(study_ID) %>% mutate(label = paste0(study_ID, " (n=", n, ")"))

draws_plot <- draws_slopes %>%
  inner_join(combos, by = c("study_ID", "removal_trt")) %>%
  left_join(labs_n, by = "study_ID") %>%
  filter(!is.na(label)) %>%
  group_by(removal_trt, label) %>% mutate(med = median(chg)) %>% ungroup() %>%
  mutate(removal_trt = factor(recode(removal_trt, dominant = "Dominant",
                                     traits = "Traits", subordinate = "Subordinate"),
                              levels = c("Subordinate", "Traits", "Dominant")))

plot_b <- ggplot(draws_plot,
                 aes(x = chg, y = reorder_within(label, med, removal_trt),
                     colour = removal_trt, fill = removal_trt)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95), slab_size = 0.5, point_size = 0.8) +
  facet_grid(removal_trt ~ ., scales = "free_y", space = "free_y") +
  geom_text(data = distinct(draws_plot, removal_trt),
            aes(label = removal_trt), x = log(0.11), y = Inf, hjust = 0, vjust = 1.2,
            colour = "black", size = 3, family = "Helvetica", inherit.aes = FALSE) +
  scale_y_reordered() +
  scale_colour_manual(values = cvC) + scale_fill_manual(values = cvC) +
  scale_x_continuous(breaks = log(c(0.10, 0.30, 0.50, 0.75, 1.0, 1.5)),
                     labels = c("-90%", "-70%", "-50%", "-25%", "0%", "+50%")) +
  coord_cartesian(xlim = c(-2.4, 0.55), clip = "off") +
  labs(x = "Proportional change at 50% removal", y = NULL) +
  theme_classic() +
  theme(legend.position = "none", strip.text = element_blank(),
        strip.background = element_blank(), axis.title.x = element_text(face = "bold"),
        text = element_text(size = 10, family = "Helvetica"))

# ---- legend (shape = measure), fixed -----------------------------------------

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

final_plot

ggsave(here("figures", "Fig3_typemod_canonical.png"),
       final_plot, width = 200, height = 150, units = "mm", dpi = 300)

# ggsave(
#   filename = "Fig3.pdf", plot = final_plot,
#   width = 200, height = 150, units = "mm",
#   path = here("figures"), device = cairo_pdf
# )
