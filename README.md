# Consistent loss of ecosystem functioning following realistic scenarios of biodiversity loss

Daniela Hoss, Shane A. Blowes, Felícia M. Fischer, Tomas Herben, Emma Ladouceur, Valério D. Pillar, Helge Bruelheide, Jonathan M. Chase

## Overview

This repository contains the R scripts used to reproduce all analyses and figures in the manuscript. Data are archived on Zenodo: **https://doi.org/10.5281/zenodo.14541661**

## Data

Download the data files from Zenodo and place them in the `data/` folder before running any scripts.

## Scripts

Scripts are numbered in execution order. Files ending in `.1` fit Bayesian models (computationally expensive; fitted models are saved as `.rds` files and reloaded on subsequent runs). Files ending in `.2` extract posterior draws and produce figures.

| Script | Description |
|--------|-------------|
| `1.1_meta_analysis_model.R` | Fit random-effects meta-analytic model (Figure 1) |
| `1.2_meta_analysis_plot.R` | Plot meta-analysis results (Figure 1) |
| `2.1_meta_regression_model.R` | Fit meta-regression models with climate/geography moderators |
| `2.2_meta_regression_plot.R` | Plot meta-regression results (Extended Data figures) |
| `3.1_productivity_model.R` | Fit species-level biomass and cover models (Figure 2) |
| `3.2_productivity_plot.R` | Plot species-level results (Figure 2) |
| `4.0_data_prep_removal_treatments.R` | Prepare data subset for removal-treatment comparisons |
| `4.1_removal_treatment_model.R` | Fit removal-treatment models |
| `4.2_removal_treatment_plot.R` | Plot removal-treatment results (Figure 3) |
| `5.1_additional_disturbances_model.R` | Fit model including additional disturbances (drought/nutrients) |
| `5.2_additional_disturbances_plot.R` | Plot additional-disturbances results |
| `6.1_supp_map.R` | Supplementary study location map |
| `_run_meta_diagnostics.R` | Model diagnostics for the meta-analysis (pp_check, trace, DHARMa, funnel plot) |

## Requirements

All analyses were run in R. Key packages: `brms`, `tidybayes`, `ggdist`, `DHARMa`, `patchwork`, `here`, `tidyverse`.

Models use `here::here()` for file paths — R must be launched from the `git_publ/` directory (or with the `.Rproj` file open).

## Model outputs

Fitted model objects (`.rds`) are not tracked in this repository due to file size. Re-fit by running the `.1` scripts, or contact the authors for access.
