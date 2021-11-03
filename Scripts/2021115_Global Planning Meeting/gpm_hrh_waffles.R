# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | T.Essam | USAID
# PURPOSE:  HRH Waffle Making Time
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:
# NOTE: 

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
  library(waffle)

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  plot_title <- ""
  

# IMPORT ------------------------------------------------------------------
  # Create data
  
  hrh_fp <- data.frame(
    name = c("USAID", "PEPFAR"),
    value = c(117, 113)
  )
  
  waffle(hrh_fp, rows = 10, flip = T, colors = c("#5bb5d5", "#d67288")) 
  
  si_save("AI/HRH_waffle.svg")
  
  hrh_bgt <- data.frame(
    name = c("USAID", "PEPFAR"),
    value = c(39, 57)
  )
  
  waffle(hrh_bgt, rows = 10, flip = T, colors = c("#5bb5d5", "#d67288")) 
  