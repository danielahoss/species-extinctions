# Experimental species extinctions show that not all losses are equal

Daniela Hoss, Shane A. Blowes, Felícia M. Fischer, Tomas Herben, Emma Ladouceur, Valério D. Pillar, Helge Bruelheide, Jonathan M. Chase

**Code authors:** Daniela Hoss and Shane A. Blowes

## Overview

This repository contains the R scripts used to reproduce all analyses and figures in the manuscript.

Two complementary analyses are run:

1. **Meta-analysis** of study-level effect sizes (log response ratios) across 38 studies — Figure 1 and supplementary moderator analyses.
2. **Synthetic (plot-level) analysis** of raw data from the 24 studies with plot-level data, modelling productivity as a continuous function of the proportion of species removed. In the current version, measurement type (biomass vs. cover) enters the models as a predictor of both the mean and the residual variance ("type-moderator" specification) — Figures 2 and 3.

## Data availability

The study-level effect sizes for all 38 studies used in the meta-analysis, and the aggregated plot-level data for 18 of the 24 studies used in the synthetic analysis, will be openly deposited in a public repository upon acceptance.

All analysis code in `scripts/` is provided without restriction.

## Data

Download the data files and place them in the `data/` folder before running any scripts. Required files:
`effects_brm.csv`, `df_biom_brm_2.csv`, `df_cover_brm_2.csv`, `df_biomass_trt.csv`, `df_cover_trt.csv`, `moderators.csv`.

## Scripts

Scripts are numbered in execution order. `.1` scripts fit Bayesian models (computationally expensive; the fitted model is cached as an `.rds` in `model_output/` and reloaded on subsequent runs). `.2` scripts extract posterior draws and produce figures. 

### Meta-analysis (Figure 1, supplementary)

| Script | Produces |
|--------|----------|
| `1.1_meta_analysis_model.R` | Random-effects meta-analytic model |
| `1.2_meta_analysis_plot.R` | Figure 1 |
| `1.3_supp_fig_meta_residuals.R` | Supplementary Fig. 7 (meta residuals) |
| `2.1_meta_regression_model.R` | Meta-regression models (climate / geography moderators) |
| `2.2_meta_regression_plot.R` | Supplementary Figs. 2–6 (moderators) |

### Synthetic analysis — productivity (Figure 2)

| Script | Produces |
|--------|----------|
| `3.1a_productivity_model_typemod.R` | Type-moderator productivity model (`model_removed_propo_typemod`) |
| `3.1b_productivity_plot_typemod.R` | Figure 2 |
| `3.2a_productivity_models_separate.R` | Separate biomass / cover models (sensitivity; `model_biomass_2`, `model_cover_2`) |
| `3.2b_productivity_plot_comparison.R` | Supplementary Fig. 8 (combined vs. separate, 1:1 slope comparison) |
| `3.3_climate_interaction_model.R` | Proportion-removed × climate interaction test (Supplementary Table 5) |

### Synthetic analysis — removal treatments (Figure 3)

| Script | Produces |
|--------|----------|
| `4.0_data_prep_removal_treatments.R` | Prepare the removal-treatment data subset |
| `4.1a_removal_treatment_model_typemod.R` | Type-moderator removal-treatment model (`model_remtrt_typemod`) |
| `4.1b_removal_treatment_plot_typemod.R` | Figure 3 |

### Additional disturbances (supplementary)

| Script | Produces |
|--------|----------|
| `5.1a_additional_disturbances_model_typemod.R` | Type-moderator additional-disturbances model (`model_dist_typemod`) |
| `5.1b_additional_disturbances_plot_typemod.R` | Supplementary Fig. 12 (additional disturbances) |

### Supplementary figures and diagnostics

| Script | Produces |
|--------|----------|
| `6_supp_fig_map.R` | Supplementary Fig. 1 (study map) |
| `7_supp_figures_typemod.R` | Supplementary residual and overall-slope figures |


## Requirements

All analyses were run in R. Key packages: `brms` (with the `cmdstanr` backend), `tidybayes`, `ggdist`, `DHARMa`, `patchwork`, `ragg` (TIFF output for Fig. 1), `viridis`, `posterior`, `bayesplot`, `tidytext`, `sf`, `rnaturalearth`, `rnaturalearthdata` (Supp. map), `here`, `tidyverse`.

Scripts use `here::here()` for file paths — launch R from the `git_publ/` directory (or open the `.Rproj` file).

## Model outputs

Fitted model objects (`.rds`) live in `model_output/` and are not tracked in this repository due to size. Re-fit by running the model (`.1a`) scripts — note this takes hours of MCMC sampling — or use the cached `.rds` files.
