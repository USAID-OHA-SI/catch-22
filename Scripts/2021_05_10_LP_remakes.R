# Purpose: LP remakes of graphics
# Author: Tim Essam | SI, 
# Date: 2020-05-10
# Notes: Ad hoc request from LP team

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
  

# LOAD DATA ============================================================================  
  
  source("Data/lp_adhoc_data_2021_05.R")

# MUNGE ============================================================================
  
  # Remake Graphic 3 - Funding
    
    lp_type %>% 
      mutate(fill_color = ifelse(category == "Local", old_rose, trolley_grey_light),
             text_color = ifelse(category == "Local", "white", grey70k),
             combo = paste(type, category, "\n"),
             increase = ifelse(!is.na(change), paste(percent(change), "\n", "Increase"), NA_character_)) %>% 
      ggplot(aes(x = COP, y = value, group = category, fill = fill_color)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = value, color = text_color), size = 12/.pt, vjust = 1.25, fontface = "bold") +
      geom_text(aes(label = increase), vjust = -0.5, size = 12/.pt)+
      facet_wrap(~combo, nrow = 1) +
      scale_fill_identity() +
      scale_color_identity() +
      si_style_xline() +
      coord_cartesian(expand = F) +
      theme(axis.text = element_blank(), 
            plot.title = element_markdown(family = "Source Sans Pro Regular")) +
      labs(x = NULL, y = NULL, 
           title = "INCREASE IN <span style = 'color:#c43d4d;'>LOCAL AWARDS</span> AND <span style = 'color:#c43d4d;'>LOCAL PARTNERS</span>")
      
      
    ggsave(file.path(images, "LP_ad_hoc_lp3.png"), width = 10, height = 4.56, 
           scale = 1.25, dpi = "retina")
    
    
  # Remake 2 - LP_2 COP TRENDS
    
      lp_fund %>% 
        ggplot(aes(x = COP, y = funding)) +
        geom_col(aes(y = 1000), fill = grey10k) +
        geom_col(fill = old_rose) +
        geom_hline(yintercept = 1000, color = grey50k, linetype = "dotted") +
        geom_text(aes(label = percent(pct, 1)), vjust = -0.5, size = 14/.pt,
                  fontface = "bold") +
        geom_text(aes(label = paste0("$", funding, "M")), vjust = 1.55, color = "white",
                  size = 14/.pt, fontface = "bold") +
        annotate("text", x = 4.3, y = 1025, label = "Goal: 70%") +
        si_style_xline()+
        coord_cartesian(expand = F, clip = "off" ) +
        theme(axis.text = element_blank(), 
              plot.title = element_markdown(family = "Source Sans Pro Regular"),
              plot.caption=element_text(hjust = 0)) +
        scale_y_continuous(limits = c(0, 1050)) +
        labs(x = NULL, y = NULL, title = "USAID/PEPFAR <span style = 'color:#c43d4d;'>LOCAL PARTNER FUNDING </span> (AS OF MAY 2021)\n",
             caption = "Includes LTS OUs only. Percentages exclude GHSC-PSM/RTK and M&O.\nSource: SBU Local Partner Strategy Datasheet derived from Local Partners Workplans.")
        
      ggsave(file.path(images, "LP_ad_hoc_lp2.png"), width = 6.22, height = 4.45, 
             scale = 1.25, dpi = "retina")
      
    # Remake cluttered table
      cop_proj %>% 
        mutate(ou_order = fct_reorder(OU, COP_order)) %>% 
        ggplot(aes(x = factor(COP), y = lp_share, group = ou_order)) +
        geom_area(fill = grey10k, alpha = 0.75) +
        geom_line() +
        geom_point(size = 3, shape = 21, aes(fill = lp_share), color = "white")+
        geom_text(aes(label = ifelse(COP == "COP21\nProj.", percent(lp_share, 1), NA_character_)), hjust = -1/3,
                  size = 8/.pt, color = grey80k) +
        geom_text(aes(label = ifelse(COP == "COP18\n", percent(lp_share, 1), NA_character_)), hjust = 1.25,
                  size = 8/.pt, color = grey80k) +
        scale_fill_si(palette = "old_roses", discrete = F) +
        facet_wrap(~ou_order) +
        si_style_ygrid()+
        scale_y_continuous(labels = percent, limits = c(0, 1), breaks = c(.5, 1)) +
        coord_cartesian(expand = T, clip = "off") +
        labs(x = NULL, y = NULL, title = "PEPFAR LOCAL PARTNER TRANSITION ACTUALS & PROJECTIONS (COP20 & 21)",
             caption = "COP21 FAST tools still being finalized for most countries. Numbers are subject to change\n(may be drammatic shifts in some countries in both numberator and denominator.") +
        theme(legend.position = "none",
              panel.spacing = unit(0.5, "lines"),
              plot.caption=element_text(hjust = 0))
        
      ggsave(file.path(images, "LP_ad_hoc_heatmap.png"), width = 10, height = 4.55, 
             scale = 1.25, dpi = "retina")
      
      
      # Remake lp trend
      lp_trend %>% 
        ggplot(aes(COP, value)) +
        geom_col(fill = old_rose) +
        geom_hline(yintercept = c(10, 20, 30), color = "white", linetype = "dotted") +
        geom_text(aes(label = value_label), vjust = -0.5, size = 12/.pt, 
                  fontface = "bold") +
        si_style_xline() +
        coord_cartesian(expand = F) +
        scale_y_continuous(position = "right", label = dollar_format(suffix = "M"), limits = c(0, 31)) +
        labs(x = NULL, y = NULL, title = "PEPFAR OBLIGATED G2G FUNDING",
             subtitle = "Steady increase in funding and mechanisms for G2G across 6 countries and 23 mechanisms between\nCOP18-COP21: Zambia, Mozambique, South Africa, Malawi, Uganda, Namibia")
        
      ggsave(file.path(images, "LP_ad_hoc_trends.png"), width = 10, height = 4.55, 
             scale = 1.25, dpi = "retina")
      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

