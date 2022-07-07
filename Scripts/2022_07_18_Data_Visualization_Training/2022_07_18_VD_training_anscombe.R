# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Data Visualization Training Summer Series | ANSCOMBE'S QUARTET
# REF ID:   8523f10d 
# LICENSE:  MIT
# DATE:     2022-07-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)


# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "8523f10d"

# ANSCOMBE'S QUARTET ------------------------------------------------------
  
  anscombe %>% 
    pivot_longer(everything(),
                 names_to = c(".value", "set"),
                 names_pattern = "(.)(.)") %>% 
    mutate(set = glue("Dataset {set}")) %>% 
    ggplot(aes(x, y)) +
    geom_smooth(method = "lm", se = FALSE, color = golden_sand) + 
    geom_point(color = scooter, size = 4, alpha = .8) +
    facet_wrap(~set) +
    labs(x = NULL, y = NULL, caption = glue("Source: Tufte - Ancombe's Quartet | Ref ID: {ref_id}")) +
    si_style() +
    theme(plot.background = element_blank())
  
  si_save("Images/anscombe.png", heigh = 5, width = 8.5)
  
  #for table
  anscombe %>% 
    pivot_longer(everything(),
                 names_to = c(".value", "set"),
                 names_pattern = "(.)(.)") %>% 
    group_by(set) %>% 
    summarise(across(c(x, y), 
                     list(mean = mean, 
                          #median = median, 
                          variance = var,
                          sd = sd),
                     .names = "{fn}_{col}"))
  