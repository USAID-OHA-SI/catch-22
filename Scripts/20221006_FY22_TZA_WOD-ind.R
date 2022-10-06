# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TZA WOD Data Request
# REF ID:   826b8313 
# LICENSE:  MIT
# DATE:     2022-10-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(gt)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "826b8313"

  curr_fy <- source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd() %>% 
    

# MUNGE -------------------------------------------------------------------

  #bind datasets
  df_tza <- bind_rows(df %>% filter(operatingunit == "Tanzania"),
                      df_arch %>% filter(operatingunit == "Tanzania")
                      )

  #limit timeframe for last 5 years
  df_tza <- df_tza %>% 
    filter(between(fiscal_year, curr_fy-5, curr_fy))
    

# CUMULATIVE STATS --------------------------------------------------------

  df_cum <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "HTS_SELF", "AGYW_PREV", 
                            "OVC_SERV_UNDER_18", "VMMC_CIRC",
                            "PrEP_NEW","HRH_PRE", "TB_PREV")) %>% 
    pluck_totals()
    
  df_cum_pds <- df_cum %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("q"), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value != 0) %>% 
    group_by(indicator) %>% 
    summarise(period_min = min(period, na.rm = TRUE),
              period_max = max(period, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(reporting_period = glue("{str_sub(period_min, end = 4)}-{period_max}")) %>% 
    select(indicator, reporting_period)
  
  df_cum <- df_cum %>% 
    count(indicator, wt = cumulative, name = "cumulative") %>% 
    left_join(df_cum_pds, by = "indicator")


# TX STATS ----------------------------------------------------------------

  df_tx <- df_tza %>% 
    filter(indicator == "TX_CURR",
           (standardizeddisaggregate == "Total Numerator" |
            otherdisaggregate == "ARV Dispensing Quantity - 6 or more months"),
           fiscal_year == curr_fy) %>% 
    mutate(indicator = ifelse(standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus", 
                              "TX_MMD.6mo", indicator)) %>% 
    count(indicator, wt = cumulative, name = "cumulative") %>% 
    mutate(reporting_period = curr_pd)
  
  

# BABIES HIV FREE STATS ---------------------------------------------------

  df_hiv_free <- df_tza %>% 
    filter(indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_EID_POS"),
           standardizeddisaggregate == "Total Numerator") 
  
  df_hiv_free_pds <- df_hiv_free %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("q"), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value != 0) %>% 
    summarise(period_min = min(period, na.rm = TRUE),
              period_max = max(period, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(indicator = "BORN_HIV_FREE",
           reporting_period = glue("{str_sub(period_min, end = 4)}-{period_max}")) %>% 
    select(indicator, reporting_period) 
    
  df_hiv_free <- df_hiv_free %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator, 
                values_from = cumulative) %>%
    rowwise() %>% 
    mutate(hiv_free = PMTCT_EID - sum(PMTCT_HEI_POS, PMTCT_EID_POS, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(indicator = "BORN_HIV_FREE") %>% 
    count(indicator, wt = hiv_free, name = "cumulative") %>% 
    left_join(df_hiv_free_pds, by = "indicator")



# BIND TABLE --------------------------------------------------------------

  df_totals <- bind_rows(df_cum, df_tx, df_hiv_free) %>% 
      arrange(indicator)

  (gt_totals <- df_totals %>% 
    gt() %>% 
    tab_header(
      title = "PEPFAR PROGRESS IN THE LAST FIVE YEARS",
      subtitle = "Tanzania"
    ) %>% 
    tab_source_note(
      source_note = glue("Source: {source_info()} | Ref Id: {ref_id}")
      ) %>% 
    fmt_number(
      columns = cumulative,
      decimals = 0,
      suffixing = FALSE
    ) %>% 
    cols_align(
      align = "right",
      columns = c(reporting_period)
    )  %>% 
    cols_label(
      indicator = "Indicator",
      cumulative = "Cumulative",
      reporting_period = "Reporting Period"
    ) %>% 
    tab_options(
      table.font.names = "Source Sans Pro"
    )
  )
  gtsave(gt_totals, "Images/FY22_TZA_WOD-ind.png")
  