# PROJECT:  
# PURPOSE:  
# AUTHOR:   A.Chafetz | USAID
# REF ID:   caeb7159 
# LICENSE:  MIT
# DATE:     2024-06-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(googlesheets4)

  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets("email")  

  gs_id <- as_sheets_id("1O4-c1lXeL21PzmaiuLqHxPapnoktsIykmln6cs1P70E") 
  
  
# IMPORT ------------------------------------------------------------------
  
  df_rfd <- read_sheet(gs_id,col_types = "c")
  

# MUNGE -------------------------------------------------------------------

  df_long <- df_rfd %>% 
    pivot_longer(-starts_with("rf"),
                names_to = "status",
                values_to = "count") %>% 
    group_by(rf_num) %>% 
    mutate(count = as.double(count),
           count_rf = sum(count) %>% as.character(),
           count_lab = ifelse(rf_num == "Objective 1", glue("Indicators = {count_rf}"), count_rf)) %>% 
    ungroup() %>% 
    mutate(rf_name = case_match(rf_name,
                                "Priority Health Areas Improved" ~ "Priority Health Areas\nImproved",
                                "Access to Services through Health Systems Enhanced" ~ "Access to Services\nthrough Health\nSystems Enhanced",
                                "Resilient Ecosystems Sustained" ~ "Resilient Ecosystems\nSustained",
                                "Knowledge and Innovative Solutions" ~ "Knowledge and\nInnovative Solutions"
           )) %>% 
    mutate(rf_desc = glue("{rf_num}:\n{rf_name}\n{count_lab}")) %>% 
    # unite(rf_desc, c(rf_num, rf_name), sep = ":\n") %>% 
    mutate(status = status %>% 
             str_replace(" - | ", "\n") %>% 
             as_factor(),
           rf_desc= as_factor(rf_desc) %>% fct_rev) %>% 
    filter(count > 0)

  df_points <- df_long %>% 
    uncount(count)
  

# VIZ ---------------------------------------------------------------------
  
  
  df_points %>% 
    ggplot(aes(status, rf_desc)) + 
    geom_point(aes(color = fct_rev(rf_ind_cat)),
               position = position_jitter(width = .3, height = .3, seed = 42),
               size = 4, alpha = .6) +
    scale_x_discrete(position = "top") +
    scale_color_manual(values = c(hw_orchid_bloom, hw_electric_indigo, hw_slate)) +
    labs(x = NULL, y = NULL, color = NULL,
         # title = "Mapping Indicators to GH Results Framework Domains"
         ) +
    si_style_nolines() +
    # si_style_nolines(font_title = "Gill Sans MT",
    #                  font_subtitle = "Gill Sans MT",
    #                  font_plot = "Gill Sans MT",
    #                  font_caption = "Gill Sans MT") 
    theme(legend.position = "none")

  si_preview()
  si_save("../Downloads/gh_rf_domains.svg",
          height = 4.75)
  