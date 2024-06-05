# PROJECT:  catch-22
# PURPOSE:  provide a sense of where indicators sit on maturity matrix
# AUTHOR:   A.Chafetz | USAID
# REF ID:   caeb7159 
# LICENSE:  MIT
# DATE:     2024-06-04
# UPDATED:  2024-06-05

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

  ref_id <- "caeb7159"
    
  load_secrets("email")  

  gs_id <- as_sheets_id("1O4-c1lXeL21PzmaiuLqHxPapnoktsIykmln6cs1P70E") 
  
  gs_sht <- "Data Revised"
  
# IMPORT ------------------------------------------------------------------
  
  df_rfd <- read_sheet(gs_id, gs_sht, col_types = "c")
  

# MUNGE -------------------------------------------------------------------

  df_rfd_lim <- df_rfd %>% 
    select(-contains("Institutionalized "), -Total) %>% glimpse()
  
  df_long <- df_rfd_lim %>% 
    pivot_longer(-starts_with("rf"),
                 names_to = "status",
                 values_to = "count") %>% 
    mutate(is_approved = str_detect(status, "Not Approved", negate = TRUE),
           status_alt = str_remove(status, " -.*")) %>% 
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
    mutate(rf_desc = glue("{rf_num}:\n{rf_name}\n{count_lab}"),
           border_color = case_when(rf_ind_cat == "PPR" ~ hw_electric_indigo,
                                    rf_ind_cat == "SDG" ~ hw_orchid_bloom,
                                    TRUE ~ hw_slate),
           fill_color = ifelse(is_approved == FALSE, NA_character_, border_color)) %>% 
    mutate(status = status %>% 
             str_replace(" - | ", "\n") %>% 
             as_factor(),
           status_alt = as_factor(status_alt),
           rf_desc= as_factor(rf_desc) %>% fct_rev) %>% 
    filter(count > 0,
           !is.na(rf_name))

  df_points <- df_long %>% 
    uncount(count)
  

  df_long %>% 
    count(status, wt = count)
  
# VIZ ---------------------------------------------------------------------
  
  df_points %>% 
    ggplot(aes(status_alt, rf_desc)) + 
    geom_point(aes(fill = fill_color, 
                   color = border_color),
               shape = 21,
               position = position_jitter(width = .3, height = .3, seed = 42),
               size = 4, alpha = .6) +
    scale_x_discrete(position = "top") +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL, fill = NULL, color = NULL,
         # title = "Mapping Indicators to GH Results Framework Domains"
         caption = glue("USAID/GH | {Sys.Date()} | Ref id: {ref_id}")
    ) +
    si_style_nolines() +
    # si_style_nolines(font_title = "Gill Sans MT",
    #                  font_subtitle = "Gill Sans MT",
    #                  font_plot = "Gill Sans MT",
    #                  font_caption = "Gill Sans MT") 
    theme(legend.position = "none")

  si_preview()
  # si_save("Graphics/gh_rf_domains.svg", height = 4.75)
  si_save("Graphics/gh_rf_domains.svg",
          height = 4, width = 8.5)
  
  
  
 
  