# PROJECT: Request for Graphic on PMTCT from FO
# PURPOSE: Munge and Analysis of UNAIDS data
# AUTHOR:  Tim Esssam | SI
# REF ID:  4a112e3d
# LICENSE: MIT
# DATE:   2023-07-19
# NOTES:   

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
  
      
  # SI specific paths/functions  
    load_secrets()
    data_path <- "Data_public/Elimination of vertical transmission_Global.xlsx"
      
  # Grab metadata
    data_source <- "UNAIDS epidemiological estimates 2023"
  
    plot_title <- "Since 2010, new HIV infections among children have declined by 58%, from an estimated 310,000 in 2010 to 130,000 in 2022."
    
  # REF ID for plots
    ref_id <- "4a112e3d"
  

# LOAD DATA ============================================================================  

  df <- readxl::read_excel(data_path, sheet = 2, skip = 1) %>% 
      janitor::clean_names() %>% 
      mutate(value = str_remove_all(children_0_14_estimate, " ") %>% as.numeric(),
             year = as.numeric(year))
    
  df %>% str()
    
  names(df)

# VIZ ==================================================================================
  
  # Check % change
  df %>% filter(year %in% c(2000, 2022)) 
  df %>% filter(year %in% c(2010, 2022))  

  (130 - 530)/530
  (130 - 310) / 310
  
  df %>% 
    ggplot(aes(x = year, y = value)) +
    geom_col(fill = genoa, width = 0.75) +
    scale_y_continuous(labels = scales::label_number_si(), limits = c(0, 5.75e5)) +
    scale_x_continuous(breaks = seq(1990, 2022, 1)) +
    coord_cartesian(expand = F) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = str_to_upper(plot_title), 
         caption = glue("{data_source} | ref id: {ref_id}")) 
  
  si_save("Graphics/UNAIDS_new_infections_peds.svg", scale = 1.25)
    
  

