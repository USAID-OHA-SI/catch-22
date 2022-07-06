# PURPOSE: Munge and Analysis of Dinosaurs
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-07-06
# NOTES: Data Viz training summer series 2022

# LOCALS & SETUP ============================================================================

  # Libraries
    library(datasauRus)
    library(glitr)
    library(glamr)
    library(tidyverse)
    library(glue)
    library(extrafont)
  
    
  # Functions
  # Pull distinct values
    pull_val <- function(x){
      df %>% 
        distinct({{x}}) %>% 
        pull()
    }
    

# LOAD & MUNGE DATA ============================================================================  

 df <- 
  datasaurus_dozen %>% 
  filter(dataset %ni% c("star", "v_lines", "wide_lines", "x_shape")) %>% 
  group_by(dataset) %>% 
  mutate(mean_x = round(mean(x), 1),
         mean_y = round(mean(y), 1),
         sd_x = round(sd(x), 1),
         sd_y = round(sd(y), 1), 
         corr = round(cor(x, y), 2)) %>% 
      ungroup()
    
  mean_x <- pull_val(mean_x)
  mean_y <- pull_val(mean_y)
  sd_x <- pull_val(sd_x)
  sd_y <- pull_val(sd_y)

# VIZ ============================================================================
  
    
  df %>%  
  ggplot(aes(x = x, y = y)) +
    geom_point(color = scooter, size = 3, alpha = 0.5) +
    theme(legend.position = "none") +
    facet_wrap(~dataset, ncol = 3, scale = "free") +
    scale_x_continuous(limits = c(0, 100)) + 
    scale_y_continuous(limits = c(0, 100)) +
    si_style_xyline(facet_space = 0.5) + 
    geom_smooth(method = "lm", color = golden_sand) +
    labs(subtitle = glue("Mean x = {mean_x}  y = {mean_y} | Standard Deviation x = {sd_x}  y = {sd_y} | Correlation = -0.06"),
         caption = "Source: datasaurus dozen package") +
    theme(legend.position = "none")
  
  si_save("Images/dino_distribution.png", scale = 1.25)











  
  




