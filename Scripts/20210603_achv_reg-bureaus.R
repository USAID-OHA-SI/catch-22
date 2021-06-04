# PROJECT:  catch22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  viz for regional bureaus
# LICENSE:  MIT
# DATE:     2021-06-03
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(countrycode)
  # library(fontawesome)
  library(emojifont)
  

# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()

  sel_ind <- c("TX_CURR", "KP_PREV", "OVC_SERV")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() 
  
  df_nat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds() 
  
  #https://drive.google.com/drive/u/0/folders/1CD3Asd5Uror4YEGUpcXzv-ah0_UV3dkU
  df_dp <- read_csv("../../../Downloads/Datapack_Master_05_25_v1.csv")

  df_meta <- get_outable(datim_user(), datim_pwd()) %>%
    select(countryname, countryname_iso)
  
# MUNGE -------------------------------------------------------------------

  df_achv <- df %>%
    filter(fundingagency == "USAID",
           indicator %in% sel_ind,
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in% c(2020,2021)) %>%
    group_by(countryname, fiscal_year, indicator) %>%
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(across(c(targets, cumulative), ~ na_if(., 0)),
           achievement = cumulative/targets)
  
  df_plhiv <- df_nat %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in%  c(2019:2021)) %>%
    group_by(countryname, plhiv_fy = fiscal_year) %>% 
    summarise(plhiv = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(countryname) %>%
    filter(plhiv_fy == max(plhiv_fy)) %>%
    ungroup()  
  
  df_meta <- df_meta %>%
    mutate(wb_region = countrycode(df_meta$countryname_iso, "iso3c", "region"),
           usaid_region = case_when(countryname == "Ukraine" ~ "Europe",
                                    wb_region == "Sub-Saharan Africa" ~ "Africa",
                                    wb_region == "Latin America & Caribbean" ~ "LAC",
                                    TRUE ~ "Asia")) %>% 
    select(-c(wb_region, countryname_iso))
  
  df_dp_targets <- df_dp %>%
    filter(fundingagency == "USAID",
           (indicator %in% sel_ind & disagg == "Age/Sex" | indicator == "KP_PREV" & disagg == "KeyPop"),
           mech_code != "00000") %>%
    group_by(countryname, fiscal_year, indicator) %>%
    summarise(targets = sum(targets, na.rm = TRUE)) %>%
    ungroup()
  

# JOIN --------------------------------------------------------------------

  df_full <- df_achv %>%
    bind_rows(df_dp_targets) %>%
    left_join(df_plhiv, by = "countryname") %>%
    left_join(df_meta, by = "countryname")  

  write_csv(df_full, "Dataout/achv.csv", na = "")
  
# VIZ ---------------------------------------------------------------------

  df_full <- read_csv("Dataout/achv.csv", na = "")
  
  df_viz <- df_full %>%
    mutate(indicator = factor(indicator, sel_ind)) %>% 
    arrange(indicator, fiscal_year) %>% 
    mutate(achv_viz = ifelse(achievement > 1, 1, achievement),
           plhiv = ifelse(is.na(plhiv), 0, plhiv),
           countryname = case_when(countryname == "Democratic Republic of the Congo" ~ "DRC",
                                   countryname == "Dominican Republic" ~ "DR",
                                   countryname == "Papua New Guinea" ~ "PNG",
                                   TRUE ~ countryname),
           ctry_lab = glue("{countryname}<br><span style = 'color:#909090;font-size:5pt'>{comma(plhiv)} ({plhiv_fy})</span>"),
           ctry_lab = fct_reorder(ctry_lab, plhiv, max),
           ind_yr_grp = glue("{indicator} {fiscal_year}") %>% fct_inorder,
           flag_achv = case_when(fiscal_year == 2021 & indicator == "TX_CURR" & achievement < .8 ~ "low",
                                 fiscal_year == 2021 & achievement < .4 ~ "low",
                                 TRUE ~ "ok")) 
  
  df_viz <- df_viz %>% 
    group_by(countryname, indicator) %>% 
    mutate(increase = targets > lag(targets, 1, order_by = fiscal_year),
           delta_arrow = ifelse(increase == TRUE, 
                                emojifont::fontawesome('fa-arrow-circle-up'), 
                                emojifont::fontawesome('fa-arrow-circle-down'))) %>% 
    ungroup() 

  df_viz %>% 
    filter(usaid_region == "Africa") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    ggplot(aes(achv_viz, ctry_lab, fill = ind_yr_grp)) +
    geom_linerange(aes(xmin = 0, xmax = 1), color = "#909090") +
    geom_text(aes(x = .25, label = comma(targets, 1)), na.rm = TRUE, nudge_y = -.3,
            hjust = 1,size = 6/.pt, family = "Source Sans Pro", color = "#909090") +
    geom_text(aes(x = .3, label = delta_arrow), nudge_y = -.25,
                  alpha = .4, size = 5/.pt, family = 'fontawesome-webfont') +
    geom_point(shape = 21, size = 5, alpha = .9, color = trolley_grey_light,
               na.rm = TRUE) +
    geom_text(aes(label = round(achievement*100)), na.rm = TRUE,
              size = 5/.pt, family = "Source Sans Pro") +
    facet_grid(~ind_yr_grp) +
    expand_limits(x = c(0, 1.1)) +
    scale_x_continuous(label = percent) +
    scale_fill_manual(values = c(si_palettes$scooters[4], si_palettes$scooters[6], si_palettes$scooters[8],
                                 si_palettes$moody_blues[4], si_palettes$moody_blues[6], si_palettes$moody_blues[8],
                                 si_palettes$genoas[4], si_palettes$genoas[6], si_palettes$genoas[8])) +
    labs(x = NULL, y = NULL,
         title = "USAID/PEPFAR PERFORMANCE AND TARGET TRENDS IN AFRICA",
         caption = "Source: PEPFAR FY21Q2i MSD + NAT_SUBNAT, COP21 Data Packs,
         SI Analytics: Aaron Chafetz/Tim Essam/Cody Adelson
         US Agency for International Development") +
    si_style_nolines() +
    theme(panel.spacing = unit(.2, "lines"),
          axis.text.x = element_blank(),
          axis.text.y = element_markdown(),
          strip.text.x = element_text(size = 9),
          legend.position = "none")
  

  si_save("Images/achv_reg-bureas-AFR.png")
  si_save("Graphics/achv_reg-bureas-AFR.svg")
  
  
  df_viz %>% 
    filter(usaid_region == "Asia") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    ggplot(aes(achv_viz, ctry_lab, fill = ind_yr_grp)) +
    geom_linerange(aes(xmin = 0, xmax = 1), color = "#909090") +
    geom_text(aes(x = .25, label = comma(targets, 1)), na.rm = TRUE, nudge_y = -.3,
              hjust = 1,size = 6/.pt, family = "Source Sans Pro", color = "#909090") +
    geom_text(aes(x = .3, label = delta_arrow), nudge_y = -.25,
                  alpha = .4, size = 5/.pt, family = 'fontawesome-webfont') +
    geom_point(shape = 21, size = 7, alpha = .9, color = trolley_grey_light,
               na.rm = TRUE) +
    geom_text(aes(label = round(achievement*100)), na.rm = TRUE,
              size = 7/.pt, family = "Source Sans Pro") +
    facet_grid(~ind_yr_grp) +
    expand_limits(x = c(0, 1.1)) +
    scale_x_continuous(label = percent) +
    scale_fill_manual(values = c(si_palettes$scooters[4], si_palettes$scooters[6], si_palettes$scooters[8],
                                 si_palettes$moody_blues[4], si_palettes$moody_blues[6], si_palettes$moody_blues[8],
                                 si_palettes$genoas[4], si_palettes$genoas[6], si_palettes$genoas[8])) +
    labs(x = NULL, y = NULL,
         title = "USAID/PEPFAR PERFORMANCE AND TARGET TRENDS IN ASIA",
         caption = "Source: PEPFAR FY21Q2i MSD + NAT_SUBNAT, COP21 Data Packs,
         SI Analytics: Aaron Chafetz/Tim Essam/Cody Adelson
         US Agency for International Development") +
    si_style_nolines() +
    theme(panel.spacing = unit(.2, "lines"),
          axis.text.x = element_blank(),
          axis.text.y = element_markdown(),
          strip.text.x = element_text(size = 9),
          legend.position = "none")
  
  
  si_save("Graphics/achv_reg-bureas-ASIA.svg")
  
  
  
  df_viz %>% 
    filter(usaid_region == "LAC") %>%
    mutate(fy = as.character(fiscal_year)) %>% 
    ggplot(aes(achv_viz, ctry_lab, fill = ind_yr_grp)) +
    geom_linerange(aes(xmin = 0, xmax = 1), color = "#909090") +
    geom_text(aes(x = .25, label = comma(targets, 1)), na.rm = TRUE, nudge_y = -.3,
              hjust = 1,size = 6/.pt, family = "Source Sans Pro", color = "#909090") +
    geom_text(aes(x = .3, label = delta_arrow), nudge_y = -.25,
                  alpha = .4, size = 5/.pt, family = 'fontawesome-webfont') +
    geom_point(shape = 21, size = 7, alpha = .9, color = trolley_grey_light,
               na.rm = TRUE) +
    geom_text(aes(label = round(achievement*100)), na.rm = TRUE,
              size = 7/.pt, family = "Source Sans Pro") +
    facet_grid(~ind_yr_grp) +
    expand_limits(x = c(0, 1.1)) +
    scale_x_continuous(label = percent) +
    scale_fill_manual(values = c(si_palettes$scooters[4], si_palettes$scooters[6], si_palettes$scooters[8],
                                 si_palettes$moody_blues[4], si_palettes$moody_blues[6], si_palettes$moody_blues[8],
                                 si_palettes$genoas[4], si_palettes$genoas[6], si_palettes$genoas[8])) +
    labs(x = NULL, y = NULL,
         title = "USAID/PEPFAR PERFORMANCE AND TARGET TRENDS IN LAC",
         caption = "Source: PEPFAR FY21Q2i MSD + NAT_SUBNAT, COP21 Data Packs,
         SI Analytics: Aaron Chafetz/Tim Essam/Cody Adelson
         US Agency for International Development") +
    si_style_nolines() +
    theme(panel.spacing = unit(.2, "lines"),
          axis.text.x = element_blank(),
          axis.text.y = element_markdown(),
          strip.text.x = element_text(size = 9),
          legend.position = "none")
  
  
  si_save("Graphics/achv_reg-bureas-LAC.svg")
  