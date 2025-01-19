# Community-level cover & additional disturbances
# Figures

# 1. Load Libraries and Set Up Environment --------------------------------

library(tidyverse)   
library(gridExtra)
library(brms)        
library(DHARMa)      

# Clear workspace 
rm(list = ls())

# 2. Load and Preprocess Data ---------------------------------------------

df_cover<- read.csv(here::here("data", "df_cover_brm.csv"), header = T) 

df_cover %>% names

as.factor(df_cover$warming) %>% levels                        
as.factor(df_cover$mowing) %>% levels 
as.factor(df_cover$NPK_addition) %>% levels  
# as.factor(df_cover$NSP) %>% levels                            
# as.factor(df_cover$N_addition) %>% levels                     
# as.factor(df_cover$drought) %>% levels                        


# removal_disturbance -----------------------------------------------------
df_cover$disturbance <- "no"

# do not use warming
# "warming",
df_cover$disturbance[apply(df_cover[,c(
  "warming", "NPK_addition")] == "yes", 1, any)] <- "yes"

as.factor(df_cover$disturbance) %>% levels


df_cover$add_dist <- "no"

df_cover$add_dist[df_cover$warming == "yes"] <- "warming"
df_cover$add_dist[df_cover$NPK_addition == "yes"] <- "NPK_addition"


as.factor(df_cover$add_dist) %>% levels


df_cover$removed_propo[which(df_cover$disturbance == "yes" )] %>% max
df_cover$removed_propo[which(df_cover$disturbance == "yes" )] %>% min

df_cover$removed_propo[which(df_cover$disturbance == "no" )] %>% max
df_cover$removed_propo[which(df_cover$disturbance == "no" )] %>% min

df_cover_dist <- df_cover %>% filter(removed_propo <= 0.35)
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "yes" )] %>% max
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "yes" )] %>% min
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "no" )] %>% max
df_cover_dist$removed_propo[which(df_cover_dist$disturbance == "no" )] %>% min



# 3. Upload model and plot figures ----------------------------------------


mod_cover_add_dist <- read_rds(here::here("model_output", "mod_cover_add_dist.rds")) 
mod_cover_add_dist %>% summary


## plot disturbance

cover_fitted <- cbind(mod_cover_add_dist$data, fitted(mod_cover_add_dist, re_formula = NA)) %>% 
  as_tibble() %>% 
  inner_join(df_cover_dist %>% 
               distinct(study_ID,
                        block, 
                        plot,
                        time_pad, 
                        removed_propo,
                        richness, 
                        cover, 
                        disturbance, 
                        add_dist),
             relationship = "many-to-many",
             by = c('study_ID', 'block', 'plot', 
                    'removed_propo', 'disturbance',
                    "cover"))
cover_fitted %>% head


as.factor(cover_fitted$disturbance) %>% levels
as.factor(cover_fitted$add_dist) %>% levels

cover_exp_coef <- coef(mod_cover_add_dist)
cover_exp_coef2 <- bind_cols(cover_exp_coef$study_ID[,,'Intercept'] %>%
                               as_tibble() %>%
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      study_ID = rownames(cover_exp_coef$study_ID[,,'Intercept'])) %>%
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             cover_exp_coef$study_ID[,,'disturbanceyes'] %>%
                               as_tibble() %>%
                               mutate(Slope = Estimate,
                                      Slope_lower = Q2.5,
                                      Slope_upper = Q97.5) %>%
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5))   %>%
  inner_join(df_cover_dist %>%
               group_by(study_ID) %>%
               summarise(xmin = min(removed_propo),
                         xmax = max(removed_propo),
                         cxmin = min(removed_propo),
                         cxmax = max(removed_propo)),
             by = 'study_ID')

## plot disturbance



Fig.3b_cover_add_dist <- 
  df_cover_dist %>% 
  
  ggplot(aes(x = removed_propo, fill = disturbance,  
             color = disturbance, shape = disturbance)) +
  
  geom_jitter(aes(y = cover, 
                  color = disturbance),
              width = 0.01, height = 0, size = 1,  alpha = 0.7) +
  
  geom_smooth(data = cover_fitted,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity") +
  
  # Scale axis
  scale_x_continuous(
    labels = scales::percent_format(scale = 100),
    limits = c(0, 0.4), 
    oob = scales::squish) +
  
  scale_y_continuous(
    trans = 'log', 
    breaks = c(0, 2, 4, 8, 16,32, 64, 128,
               256, 512, 1024, 2048)) +
  
  scale_colour_manual(
    name = "",
    labels = c("species removal",
               "species removal +\nadd disturbance"),
    values = c("#81B024","#253601")) +
  
  scale_fill_manual(
    name = "",
    labels = c("species removal",
               "species removal +\nadd disturbance"),
    values = c("#81B024","#253601")) +
  
  scale_shape_manual(
    name = "",
    labels = c("species removal",
               "species removal +\nadd disturbance"),
    values = c(19, 19)) +
  
  labs(x = 'Proportion of species removed',
       y = expression(bold(cover~(g/m^bold("2"))))) +
  
  coord_cartesian(xlim = c(0, .4), clip = "off") + 
  
  theme(
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.position = c(.8, .12),
    legend.margin = margin(1, 1, 1, 1),
    
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    axis.title = element_text(face="bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(colour = "black"),
    
    text = element_text(size = 7, family = "Helvetica", colour = "black"))

Fig.3b_cover_add_dist

#ggsave("ExtendedData_Fig.3b_cover-add_dist_with_drought.pdf", 
# path = "figures", width = 90, height = 100, units = 'mm')



ggsave("Fig.3b_cover.png",  path = "figures",
       width = 89, height = 89, units = 'mm')



Fig.3adr + Fig.3b + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'),
        text = element_text(size = 7, family = "Helvetica"))

ggsave("Fig.3ab.png",  path = "figures",
       width = 170, height = 89, units = 'mm')

ggsave("Fig.3ab_present.png",  path = "figures",
       width = 170, height = 89, units = 'mm')

