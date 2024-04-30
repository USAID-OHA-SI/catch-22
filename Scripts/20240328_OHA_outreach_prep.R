# PROJECT: catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  recreate prep initations for OHA outreach slides
# REF ID:   a6bb0f0c 
# LICENSE:  MIT
# DATE:     2024-03-28
# UPDATED:  adatped from 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------

  filepath <-  si_path() %>% 
  return_latest("OU_IM_FY22")
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    metadata <- get_metadata(filepath) 
  
  ref_id <- "a6bb0f0c"
  
  #clean number
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }

# IMPORT ------------------------------------------------------------------
  
  df <- read_psd(filepath)
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_psd()

# MUNGE -------------------------------------------------------------------
  
  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
      bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator")
  
  #curr fy prep (for viz title)
  prep_viz <- df_prep %>% 
    # filter(fiscal_year == metadata$curr_fy) %>% 
    count(fiscal_year,indicator, funding_agency, wt = cumulative) %>% 
    filter(fiscal_year %ni% c(2016, 2024))
  
  
  #curr fy prep (for viz title)
  prep_cum <- df_prep %>% 
    filter(fiscal_year == metadata$curr_fy -1) %>% 
    count(wt = cumulative) %>% 
    pull()
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative != 0) %>% 
    distinct(fiscal_year, country) %>% 
    count(fiscal_year, name = "n_countries")
  

  
  
  
  
  # CREATE FULL LIST OF PERIODS ---------------------------------------------
  
  #propagate list of periods not in prep to add to df
  full_fys <- prep_viz %>% select(fiscal_year) %>%  pull(fiscal_year)
  
  extra_pds <- full_pds %>% 
    filter(!period %in% unique(df_prep$period))
  
  
  # VIZ ---------------------------------------------------------------------
  
  fy_start <-  full_pds %>% 
    filter(str_detect(period, "Q1")) %>% 
    pull()
  
  pd_breaks <- full_pds %>% 
    # filter(str_detect(period, "Q(1|3)")) %>% 
    pull()
  
  df_viz <- df_prep %>% 
    bind_rows(extra_pds) %>% 
    arrange(period)
  
  v <- prep_viz %>% 
    ggplot(aes(fiscal_year, n, group = funding_agency)) + 
    geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
    # geom_vline(xintercept = fiscal_year, color = "white", 
    #            linewidth = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale()), position = "right", expand = c(.01, .01)) +
    scale_x_continuous(breaks = full_fys, labels = full_fys) +
    geom_text(aes(label = clean_number(n)), vjust = -1, 
              size = 11 /.pt, family = "Source Sans Pro") +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, 
         title = glue("USAID has initiated {(clean_number(prep_cum, 0))} \\
                      onto PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      countries in 2017") %>% toupper,
         subtitle = "Pre-Exposure Prophylaxis (PrEP) Cumulative Annual Results",
         caption = glue("Source: {metadata$source}
                        SI analytics | {ref_id}
                     US Agency for International Development")) +
    si_style_ygrid()
  
  
  si_save("Images/20240328_OHA_Outreach_prep.png")

