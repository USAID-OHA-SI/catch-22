# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-10-26
# UPDATED:  
# NOTE:     derived from agitprop/04a_long_term_tx_trends.R

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
  library(janitor)
  library(lubridate)

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")
  

# IMPORT ------------------------------------------------------------------
  
  #Source: PEPFAR Spotlight (public)
  df_hist <- read_csv("../agitprop/Data/Country and Regional Targets_Results 2004-2016.csv",
                      na = c("", "NA", "null"),
                      col_types = c(Year = "i",
                                    `Measure Value` = "d",
                                    .default = "c")) %>% 
    clean_names()
  
  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------

  #source info
  msd_source <- source_info()
  
  df_tx <- df %>% 
    bind_rows(df_arch) %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup()
  
  df_tx <- df_tx %>% 
    reshape_msd() %>% 
    arrange(indicator, period) %>% 
    select(-period_type) %>% 
    mutate(source = "MSD")

  df_hist_clean <- df_hist %>% 
    filter(indicator_short_name %in% c("Patients Currently Receiving ART",
                                       "Patients Newly Receiving ART"),
           measure_name == "Results",
           country_region != "Global",
           dsd_ta == "DSD+TA") %>% 
    group_by(period = year, indicator = indicator_short_name) %>% 
    summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
    ungroup()

  df_hist_clean <- df_hist_clean %>% 
    mutate(indicator = recode(indicator,
                              "Patients Currently Receiving ART" = "TX_CURR",
                              "Patients Newly Receiving ART" = "TX_NEW"),
           period = glue("FY{str_sub(period, 3)}Q4"),
           source = "Spotlight") %>% 
    filter(!period %in% unique(df_tx$period))
  
  df_tx <- bind_rows(df_hist_clean, df_tx)

  df_tx <- df_tx %>% 
    mutate(period_date = period %>% 
             str_remove("FY") %>% 
             yq() %m-% days(1))

  df_tx <- df_tx %>% 
    mutate(eoy_or_maxq = str_detect(period, "Q4") | max(period) == period,
           bar_alpha = case_when(str_detect(period, "Q4") ~ 1,
                                 period == max(period) ~ .6),
           year = glue("20{str_sub(period, 3, 4)}") %>% as.integer)
  
  
  df_tx <- df_tx %>%
    pivot_wider(names_from = indicator) %>% 
    arrange(period) %>% 
    group_by(year) %>% 
    mutate(TX_NEW_cum = cumsum(TX_NEW)) %>% 
    ungroup() 
  
  df_tx <- df_tx %>% 
    pivot_longer(c("TX_CURR", "TX_NEW", "TX_NEW_cum"), 
                 names_to = "indicator") %>% 
    mutate(ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving antiretroviral therapy",
                                 TRUE ~ "Newly enrolled on antiretroviral therapy"))
  
  
  df_tx %>% 
    filter(eoy_or_maxq == TRUE,
           indicator %in% c("TX_CURR", "TX_NEW_cum")) %>% 
    ggplot(aes(year, value)) +
    geom_col(aes(alpha = bar_alpha, fill = ind_label),
             position = position_dodge(width = 0), width = 1.8) +
    geom_hline(yintercept = seq(3e6, 18e6, 3e6), color = "white") +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks =  seq(3e6, 18e6, 3e6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005))+
    scale_fill_manual(values = c(moody_blue_light, moody_blue)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "PEPFAR HAS VASTLY SCALED UP LIFE SAVING ART IN THE LAST 15+ YEARS",
         caption = glue("Source: Spotlight FY04-14, {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines()

  
  #data point for context
  df_tx %>% 
    filter(period == max(period),
           indicator == "TX_CURR") %>%
    select(indicator, value) %>% 
    mutate(value = clean_number(value, 1))
  
  si_save("Graphics/gpm_pepfar_historic_tx-trends.svg",
          height = 4.25)
  