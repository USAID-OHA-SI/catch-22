# PROJECT: Create plot showing data is not the first item of memorablity
# PURPOSE: Munge and Analysis of beyond memorablity chart - remake
# AUTHOR: Tim Essam | SI
# REF ID:   aac25d65
# LICENSE: MIT
# DATE: 2023-05-15
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(googlesheets4)
      
    
  # SI specific paths/functions  
    load_secrets()
  
  # REF ID for plots
    ref_id <- "aac25d65"
    
  # location  
  # url <- https://docs.google.com/spreadsheets/d/1R952PQbqHZaiJ5NoqoiJwQ8uTW-D6lSsgyxcl_2ZjgE/edit#gid=0
  # search: beyond_memorabilty  
    gs_id <- "1R952PQbqHZaiJ5NoqoiJwQ8uTW-D6lSsgyxcl_2ZjgE"

# LOAD DATA ============================================================================  

  df <- read_sheet(gs_id)

# MUNGE ============================================================================
  
  df <- df %>% 
      mutate(fill_clr = case_when(
        part == "data" ~ "#6768ab",
        part %in% c("title", "label", "paragraph") ~ grey20k,
        TRUE ~ grey20k), 
        text_clr = case_when(
          part %in% c( "data") ~ "white",
          TRUE ~ grey90k),
        part_fct = fct_reorder(part, mentions))
  
# VIZ ============================================================================

  df %>% 
      ggplot(aes(x = mentions, y = part_fct, fill = fill_clr)) +
      geom_col(width = 0.75) +
      geom_text(aes(label = comma(mentions), color = text_clr),
                family = "Source Sans Pro SemiBold",
                hjust = 1.15) +
      geom_vline(xintercept = seq(250, 1000, 250), color ="white")+
      scale_fill_identity() +
      scale_color_identity() +
      coord_cartesian(expand = F) +
      si_style_yline() +
      scale_x_continuous(position = "top") +
      labs(x = NULL, y = "",
           title = "TEXTUAL ELEMENTS ARE MENTIONED MOST OFTEN IN DESCRIPTIONS OF GRAPHICS",
           subtitle = "Total number of mentions of different graphical elements from Borkin et al., 2015",
           caption = "Original graphic from Bork et al., 2015, Beyond Memorability: Visual Recognition and Recall -- Inspired by Jonathan Burn-Murdoch's 2023 Outlier Presentation")
    
    si_save("Graphics/Borkin_et_al_remake.svg")
   

# SPINDOWN ============================================================================

