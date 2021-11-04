# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  peds viral load coverage
# LICENSE:  MIT
# DATE:     2021-10-29
# UPDATED:  2021-11-04

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

  msd_source <- source_info()

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  
  df_vl_peds <- df %>% 
    filter(fundingagency == "USAID",
           # fiscal_year == 2021,
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age Aggregated/Sex/Indication/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus"
                                           ),
           trendscoarse == "<15") %>% 
    clean_indicator() 
  
  df_vl_peds %>% 
    group_by(fiscal_year, fundingagency, indicator, trendscoarse) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(vlc = tx_pvls_d/lag(tx_curr, n = 2, by = period),
           vls = tx_pvls/tx_pvls_d,
           vls_adj = tx_pvls/lag(tx_curr, n = 2, by = period)) %>% 
    filter(period == max(period))
  
  
  df_viz <- df_vl_peds %>% 
    group_by(fiscal_year, fundingagency, indicator, ageasentered) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(ageasentered) %>% 
    mutate(vlc = tx_pvls_d/lag(tx_curr, n = 2, by = period),
           vls = tx_pvls/tx_pvls_d,
           vls_adj = tx_pvls/lag(tx_curr, n = 2, by = period)) %>%
    ungroup() %>% 
    filter(period == max(period),
           ageasentered != "<15") %>% 
    mutate(age_lab = case_when(ageasentered == "10-14" ~ glue("{ageasentered}\nTX_CURR FY21Q3 = {number(tx_curr, 1, scale = 1e-3, suffix = 'k')}"),
                               TRUE ~ glue("{ageasentered}\n{number(tx_curr, 1, scale = 1e-3, suffix = 'k')}")))

  df_viz %>% 
    ggplot(aes(vlc, ageasentered)) +
    geom_vline(xintercept = 0, color = "#505050") +
    geom_vline(xintercept = .9, linetype = "dashed", color = "#505050") +
    geom_segment(aes(x = 0, xend = vlc, yend = ageasentered), 
                 size = 2, color = moody_blue) +
    geom_point(size = 14, shape = 21, stroke = 2, 
               fill = "white", color = moody_blue) +
    geom_text(aes(label = percent(vlc, 1)),
              family = "Source Sans Pro", color = moody_blue) +
    scale_x_continuous(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         caption = glue("Source: {msd_source}",
                      "USAID SI Analytics",
                      "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    coord_cartesian(clip = "off") +
    si_style_nolines() +
    theme(axis.text.x = element_blank())

  si_save("Graphics/gpm_usaid_vlc-peds-by-age.svg",
          height = 3.33, width = 5.5)  
  