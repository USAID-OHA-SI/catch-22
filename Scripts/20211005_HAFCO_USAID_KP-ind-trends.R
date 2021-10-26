# PROJECT:  catch22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  KP Trends
# LICENSE:  MIT
# DATE:     2021-10-05
# UPDATED: 

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
  
  ind_sel <- c("PrEP_NEW","HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS")

  msd_source <- source_info()
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  

# MUNGE -------------------------------------------------------------------

  df_kp <- df %>% 
    filter(fiscal_year %in% c(2020:2021),
           fundingagency == "USAID",
           indicator %in% ind_sel,
           !(indicator == "TX_PVLS" & standardizeddisaggregate == "KeyPop/HIVStatus"),
           str_detect(standardizeddisaggregate, "KeyPop")) %>%
    clean_indicator() %>% 
    group_by(fundingagency, indicator, fiscal_year) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd()

  df_viz <- df_kp %>% 
    mutate(indicator = factor(indicator, ind_sel),
           r_grp = ifelse(indicator %in% ind_sel[1:3], "top", "bottom"),
           value = na_if(value, 0),
           ind_lab = case_when(indicator == "PrEP_NEW" ~ "KPs newly enrolled on antiretroviral pre-exposure prophylaxis",
                               indicator == "HTS_TST_POS" ~ "KPs receiving HIV testing services and positive results",
                               indicator == "TX_NEW" ~ "KPs newly enrolled on antiretroviral therapy",
                               indicator == "TX_CURR"~ "KPs currently receiving antiretroviral therapy",
                               indicator == "TX_PVLS_D" ~ "KPs ART patients with Viral Load result record (last 12mo)",
                               indicator == "TX_PVLS" ~ "KPs ART patients with supressed Viral Load results (last 12mo)"
                               )) %>%
    arrange(indicator) %>% 
    mutate(ind_lab = fct_inorder(ind_lab)) %>% 
    group_by(r_grp) %>% 
    mutate(max = max(value, na.rm = TRUE)) %>% 
    ungroup()
  
  df_viz %>% 
    ggplot(aes(period, value, group = ind_lab, fill = ind_lab, color = ind_lab)) +
    geom_blank(aes(y = max)) +
    geom_area(alpha = .2, size = 1, na.rm = TRUE) +
    geom_point(shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
    facet_wrap(~ind_lab, nrow = 2, scales = "free_y") +
    scale_y_continuous(label = scales::label_number_si()) +
    scale_fill_manual(values = c(si_palettes$genoa, scooter_light), aesthetics = c("color", "fill")) +
    labs(x = NULL, y = NULL,
         caption = glue("Source: {msd_source}")) +
    si_style_ygrid(facet_space = .2) +
    theme(legend.position = "none")
  
  si_save("Graphics/20211005_HFCAO_kp-trends.svg")
  