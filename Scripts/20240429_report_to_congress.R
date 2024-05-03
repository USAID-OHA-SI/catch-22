# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  HIV blurb for HIV/TB/Malaria Report to Congress
# REF ID:   614cf6d6 
# LICENSE:  MIT
# DATE:     2024-04-29
# UPDATED:  202-05-03 (added visual)

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

# DATIM GENIE REPORT PARAMETERS -------------------------------------------

# PSNU By IM
# DATIM data as of: 3/15/2024, 19:44:38 UTC
# Genie report updated: 3/16/2024, 05:41:15 UTC
# Current period(s): 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2022 Target, 2023 Q1, 2023 Q2, 2023 Q3, 2023 Q4, 2023 Target, 2024 Q1, 2024 Target

# Daily/Frozen: Frozen
# Indicator: TX_CURR, TX_NEW
# Standardized Disaggregate: Total Numerator
# Fiscal Years: 2020-2022
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # get files
  filepath_msd <- si_path() %>% return_latest("OU_IM_FY22")
  filepath_arch_msd <- si_path() %>% return_latest("OU_IM_FY15")
  filepath_hrh <- si_path() %>% return_latest("HRH")
  filepath_fsd <- si_path() %>% return_latest("Financial")
  
  genie_path <- si_path() %>% 
    return_latest("Genie")

  # Grab metadata
    metadata <- get_metadata(filepath_msd) 
  
  ref_id <- "614cf6d6"

# IMPORT ------------------------------------------------------------------
  
  #tx curr method 1
  df_msd <- read_psd(filepath_msd)
  df_arch <- read_psd(filepath_arch_msd)
  df_hrh <- read_psd(filepath_hrh)
  df_fsd <- read_psd(filepath_fsd)
  
  #tx curr method 2
  df_genie <- read_psd(genie_path)  

# TX_CURR -------------------------------------------------------------------
  
  # two methods
    # 1) normal PEPFAR TX_CURR totals - exclude UKR
    # 2) last year, we excluded all Military SNUs to match spotlight
  

  #method 1  
 df_msd_total <- df_msd %>% 
    rbind(df_arch)
  
  df_method1 <- df_msd_total %>% 
    filter(fiscal_year %in% c(2020, 2021, 2022),
           operatingunit != "Ukraine",
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
    mutate(tx_diff = (cumulative - lag(cumulative))) 
  
  df_method1 %>% 
   # filter(fiscal_year == 2022) %>% 
    gt() %>% 
    fmt_number(
      columns = c(3,4),
      decimals = 2,
      suffixing = TRUE
    ) %>% 
    tab_header(
      title = glue("Method 1: TX_CURR" %>% toupper()),
      subtitle = "Normal PEPFAR TX_CURR totals - exclude UKR")
  
  
  df_msd_total %>% 
    filter(fiscal_year %in% c(2022),
           #operatingunit != "Ukraine",
           indicator %in% c("TX_CURR")) %>% 
    group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop")
    
  #method 2
  #filter out Military SNUs
  df_agg <- df_genie %>% 
   # filter(str_detect(snu1, "_Mil", negate = TRUE)) %>%
    count(fiscal_year, indicator, wt = cumulative) 
  
  #verify TX numbers
  df_agg %>% 
   # pivot_wider(names_from = fiscal_year, values_from = n) %>%
    filter(indicator == "TX_CURR") %>% 
    mutate(tx_diff = (n - lag(n))) %>% 
    #filter(fiscal_year == 2022) %>% 
    gt() %>% 
    fmt_number(
      columns = c(3,4),
      decimals = 2,
      suffixing = TRUE
    ) %>% 
    tab_header(
      title = glue("Method 2: TX_CURR" %>% toupper()),
      subtitle = "Excludes Military SNUs and Ukraine")

# HRH -------------------------------------------------------------------
  
  df_hrh %>% 
    filter(operating_unit != "Ukraine",
           program == "C&T") %>% 
    group_by(fiscal_year, program) %>% 
    summarise( individual_count  = sum( individual_count , na.rm = TRUE), .groups = "drop") %>% 
    gt() %>% 
    fmt_number(
      columns = c(3),
      decimals = 0
    ) %>% 
    tab_header(
      title = glue("HRH Individual Count by Program Area" %>% toupper()))
  
# BUDGET/ER ----------------------------------------------------------------
  
  #ended up not using FSD values and instead used Pano Dossiers to calculate

    # df_fsd %>% 
    # remove_mo() %>% 
    # filter(program == "C&T",
    #       # operatingunit != "Ukraine",
    #  # sub_program == "C&T: HIV Drugs",
    #        fiscal_year == 2021) %>% 
    # group_by(fiscal_year, planning_cycle, program, sub_program) %>% 
    # summarise(cop_budget_total  = sum(cop_budget_total , na.rm = TRUE), .groups = "drop") %>% 
    # gt() %>% 
    # fmt_currency(
    #   columns = c(5),
    #   decimals = 0
    # ) %>% 
    # tab_header(
    #   title = glue("HIV Procurements" %>% toupper()),
    #   subtitle = "Filtered to sub_program C&T: HIV Drugs")
    
  
  
# TX_CURR and NEW viz ----------------------------------------------------
  
df_viz <- df_msd_total %>% 
    filter(fiscal_year <= 2022,
           operatingunit != "Ukraine",
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
  mutate(ind_label = case_when(indicator == "TX_CURR" ~ 
                                 "Currently receiving ART treatment",
                               TRUE ~ "Newly enrolled on ART treatment"))


#shorten numbers to K/M
label_scales <- scales::label_number(accuracy=0.1, 
                                     scale_cut=scales::cut_short_scale())

#add title info
title_info <- df_viz %>% 
  filter(indicator == "TX_CURR",
         fiscal_year %in% c(min(fiscal_year), max(fiscal_year))) %>% 
  select(fiscal_year, cumulative) %>% 
  mutate(added = (cumulative - lag(cumulative)) %>% label_scales(),
         yrs = fiscal_year - lag(fiscal_year)) %>% 
  filter(fiscal_year == max(fiscal_year))

#pull out current data point for context
val_2022 <- df_viz %>% 
  filter(fiscal_year == 2022, indicator == "TX_CURR") %>%
  select(indicator, cumulative) %>% 
  pull(cumulative) %>% 
  label_scales()

df_viz %>% 
  mutate(viz_alpha = ifelse(fiscal_year == max(fiscal_year), 0.8, 0.6)) %>% 
  ggplot(aes(fiscal_year, cumulative, alpha = viz_alpha)) +
  geom_col(aes(fill = ind_label), position = "identity") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_viz$fiscal_year) %>% length()) +
  geom_text(aes(label = label_scales(cumulative), vjust = -0.3,
                family = "Source Sans Pro")) +
  expand_limits(y = c(0,22000000)) +
  scale_fill_manual(values = c(denim_light, "#5378A5")) +
  scale_alpha_identity()+
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("AS OF 2021, PEPFAR AGENCIES HAVE PROVIDED TREATMENT FOR {val_2022} PATIENTS"),
       #subtitle = glue("OVER THE PAST {title_info$yrs} YEARS, USAID HAS ADDED {title_info$added} PATIENTS ONTO TREATMENT"),
       caption = glue("Note: Ukraine removed from PEPFAR totals
                      Source: {metadata$source} | US Agency for International Development")) +
  si_style_ygrid()

si_save("Graphics/20240429_txcurr_congress_report.svg")
    