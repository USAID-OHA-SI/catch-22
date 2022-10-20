# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TZA WOD Data Request
# REF ID:   826b8313 
# LICENSE:  MIT
# DATE:     2022-10-06
# UPDATED:  2022-10-20

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)  # remotes::install_github("USAID-OHA-SI/gagglr")
  library(glue)
  library(gt)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "826b8313"
  
  get_metadata()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()
    

# MUNGE -------------------------------------------------------------------

  #bind datasets
  df_tza <- bind_rows(df %>% filter(operatingunit == "Tanzania"),
                      df_arch %>% filter(operatingunit == "Tanzania")
                      )

  #limit timeframe for last 5 years
  df_tza <- df_tza %>% 
    filter(between(fiscal_year, metadata$curr_fy-5, metadata$curr_fy))
    
  rm(df, df_arch)
  
# CUMULATIVE STATS --------------------------------------------------------

  df_cum <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "HTS_SELF", "VMMC_CIRC",
                            "PrEP_NEW","HRH_PRE", "TB_PREV")) %>% 
    pluck_totals()
  
  df_fo <- df_tza %>% 
    filter(indicator == "PMTCT_FO",
           otherdisaggregate == "HIV-uninfected") %>% 
    mutate(indicator = "PMTCT_FO.Uninfected")
    
  df_cum_pds <- df_cum %>% 
    bind_rows(df_fo) %>% 
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
    bind_rows(df_fo) %>% 
    count(indicator, wt = cumulative, name = "cumulative") %>% 
    left_join(df_cum_pds, by = "indicator")


# SNAPSHOT STATS ----------------------------------------------------------

  df_snap <- df_tza %>% 
    clean_indicator() %>% 
    filter(indicator %in% c("AGYW_PREV", "OVC_SERV_UNDER_18","TX_CURR"),
           (standardizeddisaggregate == "Total Numerator" |
              otherdisaggregate == "ARV Dispensing Quantity - 6 or more months")) %>% 
    mutate(indicator = case_when(standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus" ~ "TX_MMD.6mo", 
                                 standardizeddisaggregate == "Outcome" ~ "PMTCT_FO.Uninfected",
                                 TRUE ~ indicator)) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("q"), sum, na.rm = TRUE), 
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    filter(value != 0) %>% 
    group_by(indicator) %>% 
    slice_max(order_by = period, n = 1) %>% 
    ungroup() %>% 
    rename(cumulative = value,
           reporting_period = period)

# BIND TABLE --------------------------------------------------------------

  df_totals <- bind_rows(df_cum, df_snap, #df_hiv_free
                         ) %>% 
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
  