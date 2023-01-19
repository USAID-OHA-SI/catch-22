# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Data Visualization Training Summer Series | data viz remake
# REF ID:   a8405a4a 
# LICENSE:  MIT
# DATE:     2022-07-07
# UPDATED:  2023-01-19

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
  
  ref_id <- "a8405a4a"

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   


# MUNGE -------------------------------------------------------------------

  df_zam_pos_share <- df_msd %>% 
    filter(operatingunit == "Zambia",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           age_2019 != "Unknown Age",
           modality != "Malnutrition",
           fiscal_year == 2022) %>% 
    mutate(modality = str_replace(modality, "Mod", " (Comm.)"),
           modality = ifelse(modality == "OtherPITC", "Other PITC", modality)) %>% 
    count(age_2019, modality, wt = qtr2, name = "value") %>% 
    mutate(age_2019 = fct_lump(age_2019, n = 5, w = value),
           modality = fct_lump(modality, n = 9, w = value)) %>% 
    count(age_2019, modality, wt = value, name = "value") %>% 
    group_by(modality) %>% 
    mutate(share = value / sum(value),
           modality = glue("{modality} [{comma(sum(value))}]")) %>% 
    ungroup() %>% 
    mutate(modality = fct_reorder(modality, value, sum),
           fill_color = ifelse(share >= .2, "#BE3455", suva_grey),
           fill_alpha = ifelse(share >= .2, .8, .4)
           )
  
  mod_other <- df_zam_pos_share %>% 
    distinct(modality) %>%
    pull() %>% 
    str_subset("Other \\[")
  
  df_zam_pos_share <- df_zam_pos_share %>% 
    mutate(modality = fct_relevel(modality, mod_other))
  

# VIZ ---------------------------------------------------------------------

  df_zam_pos_share %>% 
    ggplot(aes(share, modality, fill = fill_color, alpha = fill_alpha)) +
    # geom_col(alpha = .8) +
    geom_col() +
    geom_vline(xintercept = seq(0, .3, .1), color = "white") +
    facet_grid(~age_2019) +
    scale_x_continuous(expand = c(.005, .005), label = percent,
                       breaks = c(0, .2)) +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         title = "MOST 25-34 YEAR OLDS IDENTIFIED IN THE COMMUNITY",
         subtitle = "Bar represent age band's share of HTS_TST_POS by modality \nHighlighted where over 20%",
         caption = glue("Source: {source_info()} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "picas"))
  
  si_save("Images/zam_mods_alt.png", heigh = 3.3, width = 5, scale = 1.2)
  
# VIZ MAKING PROCESSS -----------------------------------------------------
  
  v1 <- df_msd %>% 
    filter(operatingunit == "Zambia",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           age_2019 != "Unknown Age",
           modality != "Malnutrition",
           fiscal_year == 2022) %>% 
    count(age_2019, modality, wt = qtr2, name = "value") %>% 
    group_by(modality) %>% 
    mutate(share = value / sum(value)) %>% 
    ggplot(aes(modality, share, fill = age_2019)) +
    geom_col()
  
  v2 <- df_zam_pos_share %>% 
    mutate(modality = word(modality)) %>% 
    ggplot(aes(modality, share, fill = age_2019)) +
    geom_col(alpha = .8)
  
  v3 <- df_zam_pos_share %>% 
    mutate(modality = word(modality)) %>% 
    ggplot(aes(share, modality, fill = age_2019)) +
    geom_col(alpha = .8) +
    facet_grid(~age_2019) +
    theme(panel.spacing = unit(.5, "picas"))
  
  v4 <- df_zam_pos_share %>% 
    mutate(modality = word(modality)) %>% 
    ggplot(aes(share, modality)) +
    geom_col(alpha = .8) +
    facet_grid(~age_2019) +
    scale_x_continuous(expand = c(.005, .005), label = percent,
                       breaks = c(0, .2)) +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "picas"))
  
  
  v5 <- df_zam_pos_share %>% 
    mutate(modality = word(modality)) %>% 
    ggplot(aes(share, modality, fill = fill_color)) +
    geom_col(alpha = .8) +
    facet_grid(~age_2019) +
    scale_x_continuous(expand = c(.005, .005), label = percent,
                       breaks = c(0, .2)) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         subtitle = "Bar represent age band's share of HTS_TST_POS by modality \nHighlighted where over 20%") +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "picas"))
  
  v6 <- df_zam_pos_share %>% 
    ggplot(aes(share, modality, fill = fill_color)) +
    geom_col(alpha = .8) +
    geom_vline(xintercept = seq(0, .3, .1), color = "white") +
    facet_grid(~age_2019) +
    scale_x_continuous(expand = c(.005, .005), label = percent,
                       breaks = c(0, .2)) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = "MOST 25-34 YEAR OLDS IDENTIFIED IN THE COMMUNITY",
         subtitle = "Bar represent age band's share of HTS_TST_POS by modality \nHighlighted where over 20%",
         caption = glue("Source: {source_info()} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(panel.spacing = unit(.5, "picas"),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(size = 9))
  
  
  (v1 + v2 + v3) / (v4 + v5 + v6)
  
  si_save("Images/zam_mods_process_alt.png", 
          width = 9.83, height = 4.39,
          scale = 1.4)
  