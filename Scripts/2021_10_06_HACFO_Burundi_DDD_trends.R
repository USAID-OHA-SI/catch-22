# PURPOSE: Munge and Analysis of ADHOC DATA
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-10-06
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
    library(googlesheets4)
    
  
  # Set paths
    load_secrets()
    gdrive_id <- "1cqDGqlnVN0ZQn01x-WVCqAaMC03asgps1qwAfMYBpF4"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df <- googlesheets4::read_sheet(gdrive_id) %>% 
      rename(a_target = target, results = cumulative) %>% 
      mutate(ach = results/a_target,
             month = as.Date(month)) %>% 
      pivot_longer(results:a_target,
                    names_to = "type",
                    values_to = "value")

  df %>% str()  
    
# VIZ ============================================================================

  df %>% 
    ggplot(aes(x = month, group = type, y = value, fill = type)) +
    annotate(geom = "rect", 
             xmin = as.Date("2021-01-01"),
             xmax = as.Date("2021-02-01"),
             ymin = 0, ymax = Inf, fill = trolley_grey_light, alpha = 0.55) +
    geom_col(position = "dodge") +
    si_style_ygrid() +
    scale_y_continuous(labels = comma, position = "right") +
    coord_cartesian(expand = F) +
    geom_hline(yintercept = seq(2e3, 6e3, 2e3), color = "white") +
    scale_fill_manual(values = c("a_target" = burnt_sienna_light, "results" = burnt_sienna)) +
    geom_text(data = . %>% filter(type == "results"), aes(label = percent(ach, 1)), 
              family = "Source Sans Pro", 
              vjust = -1, hjust = 1) +
    labs(x = NULL, y = NULL, title = "DDD CUMULATIVE NUMBER OF PODI BENEFICIARES", 
         caption = "Source: EpiC Burundi January 2020 - March 2021 Report", fill = "") +
    scale_x_date(date_breaks = "1 month", date_labels ="%b %y") +
    theme(legend.key.height = unit(1, 'cm'))
    
  si_save("Graphics/BDI_DDD_growth.svg")
    

# SPINDOWN ============================================================================

