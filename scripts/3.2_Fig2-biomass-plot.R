# Community-level biomass 
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

require(tidyverse)
require(DHARMa)
require(brms)
require(colorspace)

rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_biom <- read.csv(here::here( "data", "df_biom_brm.csv"), header = T) 
df_biom %>% head



# 3. Upload model and plot figures ----------------------------------------

mod_biom <- read_rds(here::here("model_output", "model_biomass.rds")) 
mod_biom %>% summary



biom_fitted <- cbind(mod_biom$data, fitted(mod_biom, re_formula = NA, scale = 'linear')) %>% 
  as_tibble() %>% 
  left_join(df_biom, by = c("study_ID", "block",
                            # "plot",
                            "time_pad", 
                             "biomass", 
                             "removed_propo"))


biom_exp_coef <- coef(mod_biom)



biom_exp_coef2 <- bind_cols(biom_exp_coef$study_ID[,,'Intercept'] %>%
                              as_tibble() %>%
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     study_ID = rownames(biom_exp_coef$study_ID[,,'Intercept'])) %>%
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            biom_exp_coef$study_ID[,,'removed_propo'] %>%
                              as_tibble() %>%
                              mutate(Slope = Estimate,
                                     Slope_lower = Q2.5,
                                     Slope_upper = Q97.5) %>%
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5))   %>%
  inner_join(df_biom %>%
               group_by(study_ID) %>%
               summarise(xmin = min(removed_propo),
                         xmax = max(removed_propo),
                         cxmin = min(removed_propo),
                         cxmax = max(removed_propo)),
             by = 'study_ID') %>% 
  inner_join(biom_fitted %>%
               dplyr::distinct( study_ID, author, publ_year, .keep_all= FALSE) %>% 
               unite("ref", author:publ_year, sep = " ", remove = T),
             by = 'study_ID') 




biom_fix <- as.data.frame(fixef(mod_biom))



biom_fitted <- biom_fitted %>% 
  left_join(biom_exp_coef2, by = c("study_ID"))




# create a brown palette
create_hex_brown_palette <- function(n) {
  hex_colors <- colorRampPalette(c("#d9bf9d","#3F3326" ))(n)
  return(hex_colors)
}

num_colors <- 13
my_hex_brown_palette <- create_hex_brown_palette(num_colors)

# Plot the palette
barplot(rep(1, num_colors),
        col = my_hex_brown_palette)

  

# Figure 2a -----------------------------------------------------


Fig.2a <-
  ggplot() +
  geom_point(data = biom_fitted,
             aes(x = removed_propo, y = biomass,
                 colour = reorder(study_ID, Slope)),
             size = .5, alpha = 0.7) +
  
  geom_segment(data = biom_exp_coef2,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + Slope * cxmin),
                   yend = exp(Intercept + Slope * cxmax),
                   group = study_ID,
                   colour = reorder(study_ID, Slope)),
               linewidth = .7) +
  
  #  fixed effect
  geom_ribbon(data = biom_fitted,
              aes(x = removed_propo,
                  ymin = exp(Q2.5),
                  ymax = exp(Q97.5)),
              alpha = 0.4,
              fill = "#996633"
              ) +

  geom_line(data = biom_fitted,
            aes(x = removed_propo, y = exp(Estimate)),
            linewidth = 1,
            colour = "#996633") + 
   
  coord_cartesian(xlim = c(0, 1)) +
  # Scale axis
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  
  scale_y_continuous(trans = 'log', breaks = c(0, 2, 4, 8, 16, 
                                               32, 64, 128, 
                                               256, 512, 1024, 2048)) +
  labs(x = 'Proportion of species removed',
       y = expression(bold(Biomass~(g/m^bold("2"))))) +
  
  scale_color_manual(values = my_hex_brown_palette) +
  
  theme(legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
       legend.position = "none",
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white",color = NA),
        text = element_text(size = 7, family = "Helvetica"))


  Fig.2a

  
# ggsave("Fig.2a.pdf",  path = "figures", width = 179, height = 89, units = 'mm')

  

# show study-level effects
Fig.2a_study_level_effects  <-
  ggplot() + 
  geom_point(data = biom_exp_coef2, aes(x = reorder(study_ID, Slope), y = Slope,
                                     colour = reorder(study_ID, Slope), fill = reorder(study_ID, Slope)), size = 2) +
  geom_errorbar(data = biom_exp_coef2, aes(x = study_ID,
                                        ymin = Slope_lower,
                                        ymax = Slope_upper,
                                        colour = study_ID),
                width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = my_hex_brown_palette) +
  
#  fixed effect
geom_hline(data =  biom_fix,
           aes(yintercept = Estimate[2]), linewidth = 1.2) +
  
  geom_rect(data = biom_fix,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.2) +
  
  labs(x = 'Study', y = "Study-level effects") +
  
  scale_x_discrete(limits = rev(levels(biom_fitted$study_ID))) + 
  
  coord_flip() + 
  
  theme(
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    legend.position = "none",
    
    axis.title = element_text(face="bold"),
    axis.ticks = element_line(colour = "black"),
    
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(size = 7, family = "Helvetica"))

Fig.2a_study_level_effects

# ggsave("ExtendedData_Fig.2a_study_level_effects.pdf", path = "figures", width = 179, height = 89, units = 'mm')



library(patchwork)
Fig.2a + Fig.2a_study_level_effects # + plot_annotation(tag_levels = 'c') 
# ggsave("Fig.2a_plus_study_level_effects.pdf", path = "figures", width = 190, height = 100, units = 'mm')
