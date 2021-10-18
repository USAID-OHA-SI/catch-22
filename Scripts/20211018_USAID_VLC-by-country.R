# PROJECT:  catch-22
# AUTHOR:   A.Chafetz, T.Essam, K.Srikanth | USAID
# PURPOSE:  Explore VLS aross USAID mechanisms
# LICENSE:  MIT
# DATE:     2021-10-18
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
  

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   


# MUNGE -------------------------------------------------------------------

  #filter for necessary indicators
  df_vls <- df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator()  
  
  #aggregate to country x mech x fy x ind level
  df_vls <- df_vls %>% 
    group_by(countryname, mech_code, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 

  #reshape long by period, then wide by indicator for calculations
  df_vls <- df_vls %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")

  #create alternate denom for VLC
  df_vls <- df_vls %>% 
    group_by(countryname, mech_code) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = "period")) %>% 
    ungroup()

  #limit to most recent period & TX_PVLS_D > 0
  df_vls <- df_vls %>% 
    filter(period == max(period),
           tx_pvls_d > 0)
  
  #calc VLC/S
  df_vls <- df_vls %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2)
  