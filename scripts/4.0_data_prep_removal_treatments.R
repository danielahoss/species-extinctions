# Data preparation: removal treatment subsets
# Reads raw biomass and cover data, subsets to studies with comparable treatment
# categories (dominant, subordinate, traits), and saves analysis-ready CSVs.
# Outputs: data/df_biomass_trt.csv, data/df_cover_trt.csv
#

library(tidyverse)
library(here)

# 1. Biomass ------------------------------------------------------------------

df_biom <- read.csv(here("data", "df_biom_brm_2.csv"), header = TRUE)

# Drop pre-removal rows that are not controls at year 1 (not all studies have pre-removals)
df_biom <- df_biom %>%
  filter(!(pre_removal == 1 & removal_treatment_category_v2 != "control" & time_length_years == 1))

as.factor(df_biom$removal_treatment_category_v2) %>% levels

# Subordinate
df_sub_biom <- df_biom %>%
  group_by(study_ID) %>%
  filter(all(c("control", "subordinate") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "subordinate")) %>%
  mutate(removal_trt = first(removal_treatment_category_v2[removal_treatment_category_v2 != "control"])) %>%
  ungroup()

# Traits 
df_traits_biom <- df_biom %>%
  group_by(study_ID) %>%
  filter(all(c("control", "trait") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "trait")) %>%
  mutate(removal_trt = "traits") %>%
  ungroup()

# Dominant
df_dom_biom <- df_biom %>%
  group_by(study_ID) %>%
  filter(all(c("control", "dominant") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "dominant")) %>%
  mutate(removal_trt = first(removal_treatment_category_v2[removal_treatment_category_v2 != "control"])) %>%
  ungroup()



df_biomass_trt <- bind_rows(df_sub_biom, df_traits_biom, df_dom_biom) %>%
  select(
    study_ID, block, plot, time_length_years, experiment_duration,
    warming, elevation, nutrient, drought,
    country, latitude, longitude, altitude_m,
    mean_annual_ppt_mm, mean_annual_temperature_Celsius,
    removal_trt, response_variable, n_removed, removed_propo,
    biomass, richness, ricmin, ricmean, ricmax,
    remov_propo_min, remov_propo_mean, remov_propo_max,
    n_remov_min, n_remov_mean, n_remov_max
  ) %>%
  mutate(removal_trt = factor(removal_trt)) %>%
  mutate(block = as.character(block),
         productivity = biomass)

as.factor(df_biomass_trt$removal_trt) %>% levels

# write.csv(df_biomass_trt, row.names = FALSE, here("data", "df_biomass_trt.csv"))


# 2. Cover --------------------------------------------------------------------

df_cover <- read.csv(here("data", "df_cover_brm_2.csv"), header = TRUE)

# Drop pre-removal rows that are not controls at year 1 (not all studies have pre-removals)
df_cover <- df_cover %>%
  filter(!(pre_removal == 1 & removal_treatment_category_v2 != "control" & time_length_years == 1))

# Subordinate
df_sub_cover <- df_cover %>%
  group_by(study_ID) %>%
  filter(all(c("control", "subordinate") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "subordinate")) %>%
  mutate(removal_trt = first(removal_treatment_category_v2[removal_treatment_category_v2 != "control"])) %>%
  ungroup()

# Traits 
df_traits_cover <- df_cover %>%
  group_by(study_ID) %>%
  filter(all(c("control", "trait") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "trait")) %>%
  mutate(removal_trt = "traits") %>%
  ungroup()

# Dominant
df_dom_cover <- df_cover %>%
  group_by(study_ID) %>%
  filter(all(c("control", "dominant") %in% removal_treatment_category_v2)) %>%
  filter(removal_treatment_category_v2 %in% c("control", "dominant")) %>%
  mutate(removal_trt = first(removal_treatment_category_v2[removal_treatment_category_v2 != "control"])) %>%
  ungroup()

df_cover_trt <- bind_rows(df_sub_cover, df_traits_cover, df_dom_cover) %>%
  select(
    study_ID, block, plot, time_length_years, experiment_duration,
    warming, elevation, nutrient, drought,
    country, latitude, longitude, altitude_m,
    mean_annual_ppt_mm, mean_annual_temperature_Celsius,
    removal_trt, response_variable, n_removed, removed_propo,
    cover, richness, ricmin, ricmean, ricmax,
    remov_propo_min, remov_propo_mean, remov_propo_max,
    n_remov_min, n_remov_mean, n_remov_max
  ) %>%
  mutate(removal_trt = factor(removal_trt)) %>%
  mutate(block = as.character(block),
         productivity = cover)

# write.csv(df_cover_trt, row.names = FALSE, here("data", "df_cover_trt.csv"))

