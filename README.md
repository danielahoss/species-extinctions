# Species extinctions


Code Authors: Daniela Hoss, Shane Blowes


Daniela Hoss, Shane A. Blowes, Felícia M. Fischer, Tomas Herben, Emma Ladouceur, Valério D. Pillar, Helge Bruelheide, Jonathan M. Chase. Consistent loss of ecosystem functioning following realistic scenarios of biodiversity loss.

Article DOI: [ ](https://)

[Data DOI](https://doi.org/10.5281/zenodo.14541661)



## Data
Species-level data is not provided. Data openly available to reproduce the results is stored in [Zenodo](https://doi.org/10.5281/zenodo.14541661) and includes: the data needed to run the models and reproduce the figures.

**Folder "data" (in Zenodo):** 

- df_cover_brm.csv
- df_biom_brm.csv
- effects_brm.csv

  
**Folder "model_output" (in Zenodo):**

- meta_brm_multi_id.rds
- mod_cover_warmnutr.rds
- model_biom_droughtnutr.rds
- model_cover.rds
- model_biomass.rds
- ma_moderators_ppt.rds
- ma_moderators_lat_ppt_temp.rds
- ma_moderators_response_variable.rds
- ma_moderators_lat.rds
- ma_moderators_time.rds
- ma_moderators_temp.rds

## Folder "scripts": 
R Script file names listed below are numbered in the order they should be used.

**1.1_Fig1-model.R** Random-effects meta-analytic model *meta_brm.rds* and model checks Extended Data

**1.2_Fig1-plot.R** Data extraction and wrangling from *meta_brm.rds* to produce Figure 1

**2.1_meta_moderators-model.R** Meta-regression models with study-level moderators and model checks Extended Data

**2.2_meta_moderators-plot.R** Data extraction and wrangling- with study-level moderators and model checks Extended Data

**3.1_Fig2-biomass-model.R** Species-level biomass data model *model_biomass.rds* and model checks

**3.2_Fig2-biomass-plot.R** Data extraction and wrangling from *model_biomass.rds* to produce Figure 2a 

**4.1_Fig2-cover-model.R** Species-level cover data model *model_cover.rds* and model checks

**4.2_Fig2-cover-plot.R** Data extraction and wrangling from *model_cover.rds* to produce Figure 2b 

**5.1_Fig3-biomass_dist-model.R** Species-level biomass data model *model_biomass_dist.rds* and model checks

**5.2_Fig3-biomass_dist-plot.R** Data extraction and wrangling from *model_biomass_dist.rds* to produce Figure 3a 

**6.1_Fig3-cover_dist-model.R** Species-level cover data model *model_cover_dist.rds* and model checks

**6.2_Fig3-cover_dist-plot.R** Data extraction and wrangling from *model_cover_dist.rds* to produce Figure 3b 

**7_supp_fig_map.R** Extended Data - Map
