

library(officer)
library(tibble)
library(tidyverse)
library(purrr)
library(tidyr)
library(stringr)
library(flextable)
library(loo)     # for loo() and loo_compare()

# Clear workspace 
rm(list = ls())

mod_meta %>% formula
mod_meta %>% get_prior
mod_meta <- read_rds(here::here("model_output_a", "a_meta_brm_multi_id.rds"))
mod_biom <- read_rds(here::here("model_output_a", "a_model_biomass_doublechecking_new.rds")) 
mod_cover <- read_rds(here::here("model_output_a", "model_cover_doublechecking_new.rds"))
mod_biom_spp <- read_rds(here::here("model_output", "m_biomass_subset_interaction.rds"))
mod_cover_spp <- read_rds(here::here("model_output", "m_cover_subset_interaction.rds"))
mod_biomass_add_dist <- read_rds(here::here("model_output_a", "a_model_biom_droughtnutr.rds")) 
mod_cover_add_dist <- read_rds(here::here("model_output_a", "a_mod_cover_warmnutr.rds")) 


# Put all your model objects in a named list
models <- list(
  mod_meta             = mod_meta,
  mod_biom             = mod_biom,
  mod_cover            = mod_cover,
  mod_biom_spp         = mod_biom_spp,
  mod_cover_spp        = mod_cover_spp,
  mod_biomass_add_dist = mod_biomass_add_dist,
  mod_cover_add_dist   = mod_cover_add_dist
)


# Convert each model summary into a tidy data frame
summary_list <- lapply(names(models), function(m) {
  as.data.frame(summary(models[[m]])$fixed) %>%
    rownames_to_column("term") %>%
    mutate(model = m)
})

# If you prefer the list to keep the model names:
names(summary_list) <- names(models)
  
do.call(c, lapply(summary_list, `[[`, "term"))

#  labels for the "Model" column
model_labels <- c(
  mod_meta  = "Meta-analysis (Fig. 1)",
  mod_biom  = "Biomass (Fig. 2a–b)",
  mod_cover = "Total species cover (Fig. 2c–d)",
  mod_biom_spp = "Removal treatment - Biomass (Fig. 3a–b)",
  mod_cover_spp = "Removal treatment - Total species cover (Fig. 3c–d)",
  mod_biomass_add_dist = "Additional treatment - Biomass (Fig. 4a)",
  mod_cover_add_dist = "Additional treatment - Total species cover (Fig. 4b)"
)


 

#  labels for the "term" column
term_labels <- c(
  add_distdrought  = "Drought",            
  'add_distdrought:removed_propo'  = "% removed : Drought",           
  add_distNPK_addition  = "Nutrient",           
  'add_distNPK_addition:removed_propo'  = "% removed : Nutrient",           
  add_distnutrient  = "Nutrient",            
  'add_distnutrient:removed_propo'  = "% removed : Nutrient",            
  add_distwarming  = "Warming",           
  'add_distwarming:removed_propo'  = "% removed : Warming",            
  Intercept  = "Intercept",           
  removal_trtdominant  = "Dominant",            
  removal_trtfunctional_group  = "Functional group",            
  removal_trtsubordinate  = "Subordinate",            
  removed_propo  = "% removed",           
  'removed_propo:removal_trtfunctional_group'  = "% removed : Functional group",            
  'removed_propo:removal_trtsubordinate'  = "% removed : Subordinate"
)




make_brms_table <- function(summary_list, model_labels, term_labels,
                            digits = 3,
                            family = "Times New Roman",
                            font_size = 11) {
  
  # --- Combine model summaries ---
  combined_summary <- bind_rows(summary_list) %>%
    relocate(model, .before = term) %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(Model = recode(model, !!!model_labels),
           term  = recode(term,  !!!term_labels)) %>%
    rename(`l-95% CI` = `l-95% CI`, `u-95% CI` = `u-95% CI`) %>%
    select(Model, term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)
  
  # --- Build flextable ---
  ft <- flextable(combined_summary) %>%
    set_header_labels(
      Model = "Model",
      term = "Regression coefficient",
      Estimate = "Estimate",
      Est.Error = "Est.Error",
      `l-95% CI` = "l-95% CI",
      `u-95% CI` = "u-95% CI"
    ) %>%
    merge_v(j = "Model") %>%
    valign(j = "Model", valign = "top") %>%
    bold(j = "Model", part = "body", bold = TRUE) %>%
    # padding(i = ~ term != "Intercept", j = "term", padding.left = 12) %>%
    colformat_num(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), digits = digits) %>%
    align(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), align = "right") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
    autofit() %>%
    set_table_properties(layout = "fixed") %>%
    width(j = "Model", width = 1.5) %>%
    width(j = "term",  width = 2.3) %>%
    width(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), width = 0.75) %>%
    padding(part = "all", padding.top = 2, padding.bottom = 2,
                       padding.left = 2, padding.right = 2) %>%
    padding(j = "term", padding.left = 6, part = "body") %>%
    padding(i = ~ term != "Intercept", j = "term", padding.left = 12, part = "body")
 
  
  # --- Format Model column ---
  # Models WITH a dash → three lines
  ft <- compose(
    ft, i = ~ grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_extract(Model, "^[^-]+-")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_trim(str_match(Model, "-\\s*([^\\(]+)\\(Fig")[,2]),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # Models WITHOUT a dash → two lines
  ft <- compose(
    ft, i = ~ !grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_remove(Model, "\\s*\\(Fig\\..*\\)$")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # --- Final styling ---
  ft <- ft %>%
    font(fontname = family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "header", align = "center")
  
  return(ft)
}


ft <- make_brms_table(summary_list, model_labels, term_labels)

doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "SupplementaryTable1_brms_model_summary_subsets_a.docx")



# next session --------------------------------------------------


mod_biom_1   <- read_rds(here::here("model_output", "model_biomass_subset.rds"))
mod_biom_2   <- read_rds(here::here("model_output", "m_biomass_subset_interaction.rds"))
mod_biom_3   <- read_rds(here::here("model_output", "m_biomass_subset_interaction_randomeffects.rds"))

emtrends(mod_biom_2, ~ removal_trt, var = "removed_propo")


# extract posterior draws for the slopes
draws <- as_draws_df(mod_biom_2)

# probability that each slope is negative
mean(draws$b_removed_propo < 0)  # dominant (main effect)
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trtfunctional_group`) < 0)
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trtsubordinate`) < 0)



library(tidyverse)
library(ggplot2)

# create a tidy dataframe of the three slopes
slopes_df <- tibble(
  dominant = draws$b_removed_propo,
  functional_group = draws$b_removed_propo + draws$`b_removed_propo:removal_trtfunctional_group`,
  subordinate = draws$b_removed_propo + draws$`b_removed_propo:removal_trtsubordinate`
) %>%
  pivot_longer(everything(), names_to = "treatment", values_to = "slope")




library(tidybayes)

biomass_remov_trt_posterior_probability <- ggplot(slopes_df, aes(x = slope, y = treatment)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(-8,8
                           )) +
  labs(x = "Slope of removed_propo (log biomass)", y = "Removal treatment") +
  theme_classic()

ggsave(
  filename = "biomass_remov_trt_posterior_probability.pdf",
  plot     = biomass_remov_trt_posterior_probability,
  width    = 200,
  height   = 200,
  units    = "mm",
  path     = "figures",
  device   = cairo_pdf
)



mod_cover_1   <- read_rds(here::here("model_output_a", "model_cover_subset.rds"))
mod_cover_2   <- read_rds(here::here("model_output", "m_cover_subset_interaction.rds"))
mod_cover_3   <- read_rds(here::here("model_output", "m_cover_subset_interaction_randomeffects.rds"))
fit2_df
fit1_df   <- read_rds(here::here("model_output_reanalysis", "fit1_df.rds"))
emtrends(fit2_df, ~ removal_trt, var = "removed_propo")




# extract posterior draws for the slopes
draws <- as_draws_df(fit2_df)

# probability that each slope is negative
mean(draws$b_removed_propo < 0)  # dominant (main effect)
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trtfunctional_group`) < 0)
mean((draws$b_removed_propo + draws$`b_removed_propo:removal_trtsubordinate`) < 0)

# create a tidy dataframe of the three slopes
slopes_df <- tibble(
  dominant = draws$b_removed_propo,
  functional_group = draws$b_removed_propo + draws$`b_removed_propo:removal_trtfunctional_group`,
  subordinate = draws$b_removed_propo + draws$`b_removed_propo:removal_trtsubordinate`
) %>%
  pivot_longer(everything(), names_to = "treatment", values_to = "slope")



library(tidybayes)

productivity_remov_trt_posterior_probability <- ggplot(slopes_df, aes(x = slope, y = treatment)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(-7,7)) +
  labs(x = "Slope of removed_propo (log productivity)", y = "Removal treatment") +
  theme_classic()

ggsave(
  filename = "productivity_remov_trt_posterior_probability.pdf",
  plot     = productivity_remov_trt_posterior_probability,
  width    = 200,
  height   = 200,
  units    = "mm",
  path     = "figures",
  device   = cairo_pdf
)





egg_combined <- ggarrange(
  biomass_remov_trt_posterior_probability, cover_remov_trt_posterior_probability,

  ncol   = 2,
  widths = c(1, 1),
  labels = c("a)", "b)"
             # , "c)", "d)"
  ),
  draw   = FALSE
)


ggsave(
  filename = "removed_propo_&_remov_trt_posterior_probability.pdf",
  plot     = egg_combined,
  width    = 200,
  height   = 200,
  units    = "mm",
  path     = "figures",
  device   = cairo_pdf
)

# removal treatments biomass ------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(flextable)
library(loo)     # for loo() and loo_compare()

comp <- loo_compare( mod_biom_2,
                     mod_biom_1,
                     mod_biom_3)
print(comp, simplify = FALSE)


# Put all your model objects in a named list
models <- list(
   mod_biom_2 = mod_biom_2,
  mod_biom_1 = mod_biom_1,
  mod_biom_3 = mod_biom_3,
  mod_cover_2 = mod_cover_2)


# Convert each model summary into a tidy data frame
summary_list <- lapply(names(models), function(m) {
  as.data.frame(summary(models[[m]])$fixed) %>%
    rownames_to_column("term") %>%
    mutate(model = m)
})

# If you prefer the list to keep the model names:
names(summary_list) <- names(models)



#  labels for the "Model" column
model_labels <- c(
  mod_biom_2  = "Biomass Interactive effects of proportion of species removed and removal treatment, accounting for study duration",
   mod_biom_1  = "Biomass Proportion of species removed",
  mod_biom_3 = "Biomass Interactive effects of proportion of species removed and removal treatment")

# all_terms <- unique(unlist(lapply(summary_list, function(df) df$term)))
# all_terms
                                  
                    
do.call(c, lapply(summary_list, `[[`, "term"))
    
term_labels <- c(
  Intercept = "Intercept",                                        
  removed_propo = "% removed",                                      
  removal_trtdominant = "Dominant",                          
  removal_trtfunctional_group = "Functional group",                 
  removal_trtsubordinate = "Subordinate",                       
  'removed_propo:removal_trtfunctional_group' = "Functional group : % removed", 
  'removed_propo:removal_trtsubordinate'= "Subordinate : % removed" )    

make_brms_table <- function(summary_list, model_labels, term_labels,
                             models,
                            digits = 3,
                            family = "Times New Roman",
                            font_size = 11) {
  # --- LOO + elpd_diff / se_diff per model ---
  loo_list <- lapply(models, function(m) {
    tryCatch(loo(m), error = function(e) NULL)
  })
  loo_list <- loo_list[!vapply(loo_list, is.null, logical(1))]
  
  elpd_df <- if (length(loo_list) >= 2) {
    lc <- loo_compare(loo_list)
    as.data.frame(lc) |>
      tibble::rownames_to_column("model") |>
      select(model, elpd_diff, se_diff)
  } else if (length(loo_list) == 1) {
    tibble::tibble(model = names(loo_list),
                   elpd_diff = 0,
                   se_diff = NA_real_)
  } else {
    tibble::tibble(model = names(models),
                   elpd_diff = NA_real_,
                   se_diff = NA_real_)
  }
  
  # --- Combine model summaries ---
  combined_summary <- bind_rows(summary_list) %>%
    relocate(model, .before = term) %>%
    left_join(elpd_df, by = "model") %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(Model = recode(model, !!!model_labels),
           term  = recode(term,  !!!term_labels)) %>%
    select(Model, elpd_diff, se_diff, term, Estimate, Est.Error, `l-95% CI`, `u-95% CI` )
  
  # --- Build flextable ---
  ft <- flextable(combined_summary) %>%
    set_header_labels(
      Model = "Model",
      elpd_diff = "ELPD diff",
      se_diff = "SE diff",
      term = "Regression coefficient",
      Estimate = "Estimate",
      Est.Error = "Est.Error",
      `l-95% CI` = "l-95% CI",
      `u-95% CI` = "u-95% CI"
    ) %>%
    merge_v(j = "Model") %>%
    merge_v(j = "elpd_diff") %>%
    merge_v(j = "se_diff") %>%
    valign(j = c("Model","elpd_diff","se_diff"), valign = "top") %>%
    bold(j = "Model", part = "body", bold = TRUE) %>%
    colformat_num(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
                  digits = digits) %>%
    align(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
          align = "right") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
    autofit() %>%
    set_table_properties(layout = "fixed") %>%
    width(j = "Model", width = 1.4) %>%
    width(j = "term",  width = 1.5) %>%
    width(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), width = 0.75) %>%
    width(j = "elpd_diff", width = 0.75) %>%
    width(j = "se_diff", width = 0.65) %>%
    padding(part = "all", padding.top = 2, padding.bottom = 2)
  # ,
  #           padding.left = 2, padding.right = 2) 
  # # %>%
  #   padding(j = "term", padding.left = 6, part = "body")
  # %>%
  #   padding(i = ~ term != "Intercept", j = "term", padding.left = 12, part = "body")
  
  # --- Format Model column (your existing two/three-line logic) ---
  ft <- compose(
    ft, i = ~ grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_extract(Model, "^[^-]+-")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_trim(str_match(Model, "-\\s*([^\\(]+)\\(Fig")[,2]),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  ft <- compose(
    ft, i = ~ !grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_remove(Model, "\\s*\\(Fig\\..*\\)$")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # --- Final styling ---
  ft <- ft %>%
    font(fontname = family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "header", align = "center")
  
  return(ft)
}

# call it
ft <- make_brms_table(summary_list, model_labels, term_labels, models)
ft


doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "SupplementaryTable2_brms_model_comparison_biomass_sub.docx")






# removal treatments cover------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(flextable)
library(loo)     # for loo() and loo_compare()


comp <- loo_compare(mod_cover_1,
                    mod_cover_2,
                    mod_cover_3,
                    fit2_df
)
print(comp, simplify = FALSE)

# Put all your model objects in a named list
models <- list(
  mod_cover_2 = mod_cover_2,
  mod_cover_1 = mod_cover_1,
  mod_cover_3 = mod_cover_3,
  fit2_df = fit2_df
)


# Convert each model summary into a tidy data frame
summary_list <- lapply(names(models), function(m) {
  as.data.frame(summary(models[[m]])$fixed) %>%
    rownames_to_column("term") %>%
    mutate(model = m)
})

# If you prefer the list to keep the model names:
names(summary_list) <- names(models)



#  labels for the "Model" column
model_labels <- c(
  mod_cover_2 = "Cover Interactive effects of proportion of species removed and removal treatment, accounting for study duration",
  mod_cover_1 = "Cover Proportion of species removed",
  mod_cover_3 = "Cover Interactive effects of proportion of species removed and removal treatment",
  fit2_df = "Productivity: Interactive effects of proportion of species removed 
  and removal treatment, accounting for study duration"
  
)

all_terms <- unique(unlist(lapply(summary_list, function(df) df$term)))
all_terms



term_labels <- c(
  Intercept = "Intercept",                                        
  removed_propo = "% removed",                                      
  removal_trtdominant = "Dominant",                          
  removal_trtfunctional_group = "Functional group",                 
  removal_trtsubordinate = "Subordinate",                       
 'removed_propo:removal_trtfunctional_group' = "Functional group : % removed",
  'removed_propo:removal_trtsubordinate'= "Subordinate : % removed" 
 )    

make_brms_table <- function(summary_list, model_labels, term_labels,
                            models,
                            digits = 3,
                            family = "Times New Roman",
                            font_size = 11) {
  # --- LOO + elpd_diff / se_diff per model ---
  loo_list <- lapply(models, function(m) {
    tryCatch(loo(m), error = function(e) NULL)
  })
  loo_list <- loo_list[!vapply(loo_list, is.null, logical(1))]
  
  elpd_df <- if (length(loo_list) >= 2) {
    lc <- loo_compare(loo_list)
    as.data.frame(lc) |>
      tibble::rownames_to_column("model") |>
      select(model, elpd_diff, se_diff)
  } else if (length(loo_list) == 1) {
    tibble::tibble(model = names(loo_list),
                   elpd_diff = 0,
                   se_diff = NA_real_)
  } else {
    tibble::tibble(model = names(models),
                   elpd_diff = NA_real_,
                   se_diff = NA_real_)
  }
  
  # --- Combine model summaries ---
  combined_summary <- bind_rows(summary_list) %>%
    relocate(model, .before = term) %>%
    left_join(elpd_df, by = "model") %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(Model = recode(model, !!!model_labels),
           term  = recode(term,  !!!term_labels)) %>%
    select(Model, elpd_diff, se_diff, term, Estimate, Est.Error, `l-95% CI`, `u-95% CI` )
  
  # --- Build flextable ---
  ft <- flextable(combined_summary) %>%
    set_header_labels(
      Model = "Model",
      elpd_diff = "ELPD diff",
      se_diff = "SE diff",
      term = "Regression coefficient",
      Estimate = "Estimate",
      Est.Error = "Est.Error",
      `l-95% CI` = "l-95% CI",
      `u-95% CI` = "u-95% CI"
    ) %>%
    merge_v(j = "Model") %>%
    merge_v(j = "elpd_diff") %>%
    merge_v(j = "se_diff") %>%
    valign(j = c("Model","elpd_diff","se_diff"), valign = "top") %>%
    bold(j = "Model", part = "body", bold = TRUE) %>%
    colformat_num(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
                  digits = digits) %>%
    align(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
          align = "right") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
    autofit() %>%
    set_table_properties(layout = "fixed") %>%
    width(j = "Model", width = 1.4) %>%
    width(j = "term",  width = 1.5) %>%
    width(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), width = 0.75) %>%
    width(j = "elpd_diff", width = 0.75) %>%
    width(j = "se_diff", width = 0.65) %>%
    padding(part = "all", padding.top = 2, padding.bottom = 2)
  # ,
  #           padding.left = 2, padding.right = 2) 
  # # %>%
  #   padding(j = "term", padding.left = 6, part = "body")
  # %>%
  #   padding(i = ~ term != "Intercept", j = "term", padding.left = 12, part = "body")
  
  # --- Format Model column (your existing two/three-line logic) ---
  ft <- compose(
    ft, i = ~ grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_extract(Model, "^[^-]+-")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_trim(str_match(Model, "-\\s*([^\\(]+)\\(Fig")[,2]),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  ft <- compose(
    ft, i = ~ !grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_remove(Model, "\\s*\\(Fig\\..*\\)$")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # --- Final styling ---
  ft <- ft %>%
    font(fontname = family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "header", align = "center")
  
  return(ft)
}

# call it
ft <- make_brms_table(summary_list, model_labels, term_labels, models = models)
ft


doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "SupplementaryTable2_brms_model_comparison_cover_sub.docx")









# removal treatments productivity------------------------------------------------------
require(tidyverse)
library(dplyr)
library(purrr)
library(tidyr)
library(brms)
library(stringr)
library(flextable)
library(loo)     # for loo() and loo_compare()
fit1 <- read_rds(here::here("model_output_reanalysis", "fit1.rds"))
fit2_add <- read_rds(here::here("model_output_reanalysis", "fit2_add_v2_traits.rds"))
fit3_inter <- read_rds(here::here("model_output_reanalysis", "fit3_inter_v2_traits.rds"))

comp <- loo_compare(fit3_inter,
                    fit1,
                    fit2_add
                    )

print(comp, simplify = FALSE)

# Put all your model objects in a named list
models <- list(
  fit3_inter =   fit3_inter,
  fit1 = fit1,
  fit2_add =   fit2_add)


# Convert each model summary into a tidy data frame
summary_list <- lapply(names(models), function(m) {
  as.data.frame(summary(models[[m]])$fixed) %>%
    rownames_to_column("term") %>%
    mutate(model = m)
})

# If you prefer the list to keep the model names:
names(summary_list) <- names(models)



#  labels for the "Model" column
model_labels <- c(
  fit3_inter = "Interactive effects of proportion of species removed and removal treatment on productivity",
  fit1 = "Effects of proportion of species removed on productivity",
  fit2_add = "Additive effects of proportion of species removed and removal treatment on productivity")

all_terms <- unique(unlist(lapply(summary_list, function(df) df$term)))
all_terms



term_labels <- c(
  Intercept = "Intercept",                                        
  removed_propo = "% removed",                                      
  removal_trtdominant = "Dominant",                          
  removal_trttraits = "Traits",                 
  removal_trtsubordinate = "Subordinate",                       
  'removed_propo:removal_trttraits' = "Traits : % removed",
  'removed_propo:removal_trtsubordinate'= "Subordinate : % removed" 
)    

make_brms_table <- function(summary_list, model_labels, term_labels,
                            models,
                            digits = 3,
                            family = "Times New Roman",
                            font_size = 11) {
  # --- LOO + elpd_diff / se_diff per model ---
  loo_list <- lapply(models, function(m) {
    tryCatch(loo(m), error = function(e) NULL)
  })
  loo_list <- loo_list[!vapply(loo_list, is.null, logical(1))]
  
  elpd_df <- if (length(loo_list) >= 2) {
    lc <- loo_compare(loo_list)
    as.data.frame(lc) |>
      tibble::rownames_to_column("model") |>
      select(model, elpd_diff, se_diff)
  } else if (length(loo_list) == 1) {
    tibble::tibble(model = names(loo_list),
                   elpd_diff = 0,
                   se_diff = NA_real_)
  } else {
    tibble::tibble(model = names(models),
                   elpd_diff = NA_real_,
                   se_diff = NA_real_)
  }
  
  # --- Combine model summaries ---
  combined_summary <- bind_rows(summary_list) %>%
    relocate(model, .before = term) %>%
    left_join(elpd_df, by = "model") %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(Model = recode(model, !!!model_labels),
           term  = recode(term,  !!!term_labels)) %>%
    select(Model, elpd_diff, se_diff, term, Estimate, Est.Error, `l-95% CI`, `u-95% CI` )
  
  # --- Build flextable ---
  ft <- flextable(combined_summary) %>%
    set_header_labels(
      Model = "Model",
      elpd_diff = "ELPD diff",
      se_diff = "SE diff",
      term = "Regression coefficient",
      Estimate = "Estimate",
      Est.Error = "Est.Error",
      `l-95% CI` = "l-95% CI",
      `u-95% CI` = "u-95% CI"
    ) %>%
    merge_v(j = "Model") %>%
    merge_v(j = "elpd_diff") %>%
    merge_v(j = "se_diff") %>%
    valign(j = c("Model","elpd_diff","se_diff"), valign = "top") %>%
    bold(j = "Model", part = "body", bold = TRUE) %>%
    colformat_num(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
                  digits = digits) %>%
    align(j = c("Estimate","Est.Error","l-95% CI","u-95% CI","elpd_diff","se_diff"),
          align = "right") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
    autofit() %>%
    set_table_properties(layout = "fixed") %>%
    width(j = "Model", width = 1.4) %>%
    width(j = "term",  width = 1.5) %>%
    width(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), width = 0.75) %>%
    width(j = "elpd_diff", width = 0.75) %>%
    width(j = "se_diff", width = 0.65) %>%
    padding(part = "all", padding.top = 2, padding.bottom = 2)
  # ,
  #           padding.left = 2, padding.right = 2) 
  # # %>%
  #   padding(j = "term", padding.left = 6, part = "body")
  # %>%
  #   padding(i = ~ term != "Intercept", j = "term", padding.left = 12, part = "body")
  
  # --- Format Model column (your existing two/three-line logic) ---
  ft <- compose(
    ft, i = ~ grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_extract(Model, "^[^-]+-")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_trim(str_match(Model, "-\\s*([^\\(]+)\\(Fig")[,2]),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  ft <- compose(
    ft, i = ~ !grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_remove(Model, "\\s*\\(Fig\\..*\\)$")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # --- Final styling ---
  ft <- ft %>%
    font(fontname = family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "header", align = "center")
  
  return(ft)
}

# call it
ft <- make_brms_table(summary_list, model_labels, term_labels, models = models)
ft


doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "SupplementaryTable3_brms_model_comparison_productivity_sub.docx")









# moderators ----------------------------------------------------

mod_ppt <- read_rds(here::here("model_output", "ma_moderators_ppt.rds")) 
mod_temp <- read_rds(here::here("model_output", "ma_moderators_temp.rds")) 
mod_time <- read_rds(here::here("model_output", "ma_moderators_time.rds")) 
mod_lat <- read_rds(here::here("model_output", "ma_moderators_lat.rds")) 
mod_resp <- read_rds(here::here("model_output", "ma_moderators_response_variable.rds")) 
mod_covs <- read_rds(here::here("model_output", "ma_moderators_lat_ppt_temp.rds"))
mod_rem_meth <- read_rds(here::here("model_output_b", "ma_moderators_removal_method.rds"))





# Put all your model objects in a named list
models <- list(
  mod_ppt = mod_ppt,
  mod_temp = mod_temp,
  mod_time = mod_time,
  mod_lat = mod_lat,
  mod_resp = mod_resp,
  mod_covs = mod_covs,
  mod_rem_meth = mod_rem_meth
)


# Convert each model summary into a tidy data frame
summary_list <- lapply(names(models), function(m) {
  as.data.frame(summary(models[[m]])$fixed) %>%
    rownames_to_column("term") %>%
    mutate(model = m)
})

# If you prefer the list to keep the model names:
names(summary_list) <- names(models)




# Nice labels for the "Model" column
model_labels <- c(
  mod_ppt = "Mean annual precipitation (mm)",
  mod_temp = "Mean annual temperature (ºC)",
  mod_time = "Experiment duration",
  mod_lat = "Latitude",
  mod_resp =  "Response variable",
  mod_covs = "Covariates",
  mod_rem_meth = "Removal method"
)


term_labels <- c(
  ppt_pad = "Mean annual precipitation (mm)",
  temp_pad = "Mean annual temperature (ºC)",
  time_pad = "Experiment duration",
  latitude_abs = "|Latitude|",
  lat_pad = "|Latitude|",
  response_variablebiomass =  "Response variable - biomass",
  response_variablecover = "Response variable - cover",
  removal_method_categorychemical          = "Chemical",           
  removal_method_categorychemicalDclipping = "Chemical / clipping",  
  removal_method_categoryclipping          = "Clipping",           
  removal_method_categorypulledout         = "Pulled out",          
  removal_method_categorypulledoutDchemical= "Pulled out / chemical"
)




make_brms_table <- function(summary_list, model_labels, term_labels,
                            digits = 3,
                            family = "Times New Roman",
                            font_size = 11) {
  
  # --- Combine model summaries ---
  combined_summary <- bind_rows(summary_list) %>%
    relocate(model, .before = term) %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(Model = recode(model, !!!model_labels),
           term  = recode(term,  !!!term_labels)) %>%
    rename(`l-95% CI` = `l-95% CI`, `u-95% CI` = `u-95% CI`) %>%
    select(Model, term, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)
  
  # --- Build flextable ---
  ft <- flextable(combined_summary) %>%
    set_header_labels(
      Model = "Model",
      term = "Regression coefficient",
      Estimate = "Estimate",
      Est.Error = "Est.Error",
      `l-95% CI` = "l-95% CI",
      `u-95% CI` = "u-95% CI"
    ) %>%
    merge_v(j = "Model") %>%
    valign(j = "Model", valign = "top") %>%
    bold(j = "Model", part = "body", bold = TRUE) %>%
    # padding(i = ~ term != "Intercept", j = "term", padding.left = 12) %>%
    colformat_num(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), digits = digits) %>%
    align(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), align = "right") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1)) %>%
    autofit() %>%
    set_table_properties(layout = "fixed") %>%
    width(j = "Model", width = 1.3) %>%
    width(j = "term",  width = 2.3) %>%
    width(j = c("Estimate","Est.Error","l-95% CI","u-95% CI"), width = 0.75) %>%
    padding(part = "all", padding.top = 2, padding.bottom = 2,
            padding.left = 2, padding.right = 2) %>%
    padding(j = "term", padding.left = 6, part = "body") %>%
    padding(i = ~ term != "Intercept", j = "term", padding.left = 12, part = "body")
  
  
  # --- Format Model column ---
  # Models WITH a dash → three lines
  ft <- compose(
    ft, i = ~ grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_extract(Model, "^[^-]+-")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_trim(str_match(Model, "-\\s*([^\\(]+)\\(Fig")[,2]),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # Models WITHOUT a dash → two lines
  ft <- compose(
    ft, i = ~ !grepl(" - ", Model), j = "Model",
    value = as_paragraph(
      as_chunk(str_trim(str_remove(Model, "\\s*\\(Fig\\..*\\)$")),
               props = fp_text(bold = TRUE, font.family = family)),
      as_chunk("\n"),
      as_chunk(str_extract(Model, "\\(Fig.*\\)$"),
               props = fp_text(italic = TRUE, font.family = family))
    )
  )
  
  # --- Final styling ---
  ft <- ft %>%
    font(fontname = family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bold(part = "header", bold = TRUE) %>%
    align(part = "header", align = "center")
  
  return(ft)
}


ft <- make_brms_table(summary_list, model_labels, term_labels)

doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "SupplementaryTable2_brms_model_summary.docx")








