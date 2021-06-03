# PROJECT:  catch22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-06-01
# UPDATED:  2021-06-03
# NOTE:     adapted from USAID-OHA-SI/agitprop/04b_usaid_tx_trends

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
  library(janitor)
  library(lubridate)
  library(ggrepel)
  library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan", "Ramona Godbole")
  
  load_secrets()
  
  #GDrive ID [pulled by V.Desir 2021-06-02]
  gdrive_copmatrix <- "1vstol1MxcsB6dHncSZJEie2s3HJqgP6TQKMWevaxMZ8"
  
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
  
  #budget data from COP Matrix (NextGen)
  df_copmatrix <- read_sheet(as_sheets_id(gdrive_copmatrix),
                             skip = 4) %>% 
    clean_names()


# MUNGE BUDGET ------------------------------------------------------------

  #remove footnote line
  df_copmatrix <- df_copmatrix %>% 
    filter(planning_cycle != "Office of U.S. Foreign Assistance Resources")
  
  #identify supply chain mechanism to remove
  df_sch_mechs <- df_copmatrix %>% 
    filter(str_detect(mechanism_name, "GHSC|Supply Chain")) %>% 
    distinct(mechanism_id, mechanism_name) %>% 
    filter(str_detect(mechanism_name, "Kenya Supply Chain System Strengthening|TA|QA", negate = TRUE))
  
  #filter out M&O & supply chain
  df_copmatrix <- df_copmatrix %>%
    filter(record_type != "Management and Operations",
           !mechanism_id %in% unique(df_sch_mechs$mechanism_id))
  
  #convert to values
  df_copmatrix <- df_copmatrix %>% 
    mutate(total_planned_funding = total_planned_funding %>% 
             str_remove_all("\\$|,") %>% 
             as.numeric,
           fiscal_year = str_sub(implementation_year, -4) %>% as.integer)
  
  #annual planning budget
  df_budget <- df_copmatrix %>% 
    bind_rows(df_copmatrix %>% mutate(funding_agency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency = funding_agency) %>% 
    summarise(total_planned_funding = sum(total_new_funding_sources, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fundingagency %in% c("PEPFAR", "USAID"))
  
  
# MUNGE -------------------------------------------------------------------
  
  #current period
  curr_fy <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
  
  #treatment dataset
  df_tx <- df %>% 
    bind_rows(df_arch) %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") 
  
  df_tx <- df_tx %>%
    bind_rows(df_tx %>% mutate(fundingagency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(type = ifelse(fiscal_year == curr_fy & curr_qtr != 4 & cumulative < targets, "targets", "results"),
           value = ifelse(type == "targets", targets, cumulative)) %>%
    select(-c(targets, cumulative)) %>% 
    filter(fundingagency %in% c("PEPFAR", "USAID")) %>% 
    mutate(source = "MSD")

  df_hist_clean <- df_hist %>% 
    filter(indicator_short_name %in% c("Patients Currently Receiving ART"),
           measure_name == "Results",
           country_region != "Global",
           dsd_ta == "DSD+TA") %>% 
    mutate(fundingagency = "PEPFAR") %>% 
    group_by(fiscal_year = year, fundingagency) %>% 
    summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(type = "results",
           source = "Spotlight") %>% 
    filter(!fiscal_year %in% unique(df_tx$fiscal_year))
  
  df_tx <- bind_rows(df_hist_clean, df_tx) %>% 
    rename(tx_curr = value)


# MUNGE BUDGET ------------------------------------------------------------

  # df_budget <- df_budget %>%
  #   pivot_longer(cols = starts_with("planned"),
  #                names_to = "fiscal_year",
  #                names_prefix = "planned_fy",
  #                values_drop_na = TRUE) %>% 
  #   mutate(value = value %>% str_remove_all("\\$|,") %>% as.numeric) %>% 
  #   filter(program != "Total")
  # 
  # df_budget_ct <- df_budget %>% 
  #   # filter(program == "C&T") %>% 
  #   group_by(fiscal_year, program) %>% 
  #   summarise(value = sum(value, na.rm = TRUE)) %>% 
  #   ungroup()
   
  

# COMBINE DATASETS --------------------------------------------------------

  df_viz <- df_tx %>% 
      tidylog::full_join(df_budget) %>% 
      filter(fiscal_year >= 2005,
             fiscal_year <= curr_fy) %>% 
      arrange(fundingagency, fiscal_year) %>%
      mutate(type_alpha = ifelse(type == "targets", .6, 1),
             agency_alpha = ifelse(fundingagency == "USAID", 1, .6),
             label_tx = glue("patients receiving\n antiretroviral therapy"),
             label_budget = "annual planned funding")
     
# VIZ ---------------------------------------------------------------------


  v1 <- df_viz %>% 
    ggplot(aes(fiscal_year, tx_curr)) +
    geom_col(aes(alpha = agency_alpha), fill = denim, position = "identity", na.rm = TRUE) +
    geom_hline(yintercept = 0, color = "gray40") +
    geom_hline(yintercept = seq(3e6, 18e6, 3e6), color = "white") +
    facet_grid(label_tx~., switch = "y") +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks =  seq(3e6, 18e6, 3e6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005))+
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          strip.text.y = element_markdown(family = "Source Sans Pro SemiBold", hjust = .5, 
                                          color = denim))


  v2 <- df_viz %>% 
    ggplot(aes(fiscal_year, total_planned_funding)) +
    geom_col(aes(alpha = agency_alpha), fill = scooter, position = "identity") +
    geom_hline(yintercept = 0, color = "gray40") +
    geom_hline(yintercept = seq(1e9, 3e9, 1e9), color = "white") +
    facet_grid(label_budget~., switch = "y") +
    scale_x_continuous(expand = c(.005, .005))+
    scale_y_continuous(labels = unit_format(1, unit = "B", prefix = "$", scale = 1e-9),
                       breaks = seq(1e9, 3e9, 1e9),
                       position = "right", expand = c(.005, .005)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          strip.text.y = element_markdown(family = "Source Sans Pro SemiBold", hjust = .5,
                                          color = scooter))
  

  v1 / v2 +
    plot_annotation(
      title = glue("PEPFAR HAS VASTLY SCALED UP 
                   <span style='color:{denim}'>LIFE SAVING ART</span> IN THE LAST 15+ <br>YEARS 
                   WITHOUT A PROPORTIONATE INCREASE IN 
                   <span style='color:{scooter}'>FUNDING</span>"),
      caption = glue("Planned funding exclude Management and Operations as well as supply chain
                     Source: PEPFAR MSD FY21Q2 + Spotlight FY04-14, PEPFAR COP Matrix [2021-06-02]
                     US Agency for International Development | SI analytics: {paste(authors, collapse = '/')}")) & 
    si_style_nolines() & 
    theme(plot.title = element_markdown(size = 23),
          strip.placement = "outside",
          strip.text.y = element_markdown(family = "Source Sans Pro SemiBold", hjust = .5))
  
  
  si_save("Graphics/power_tx_budget.svg")

  
  
  
  combo <- df_tx %>%
    filter(eoy_or_maxq == TRUE) %>% 
    select(year, tx_curr = value) %>% 
    full_join(df_budget_ct %>% 
                select(year = fiscal_year, planned_funding_ct = value) %>% 
                mutate(year = as.integer(year))) %>% 
    mutate(ue = planned_funding_ct/tx_curr)
  
  
  combo %>% 
    mutate(yr_lab = case_when(year %% 5 ==0 ~ year)) %>% 
    ggplot(aes(tx_curr, planned_funding_ct, label = yr_lab)) +
    geom_point(size = 4) +
    geom_path() +
    geom_text_repel(family = "Source Sans Pro", color = "#505050", force = 15, segment.colour =  NA) +
    scale_x_continuous(labels = unit_format(1, unit = "M", scale = 1e-6)) +
    scale_y_continuous(labels = unit_format(.1, unit = "B", scale = 1e-9)) +
    labs(y = "PEPFAR Budget (planned, billions USD)",
         x = "Current on Treatment") +
    si_style()
  
  combo %>% 
    mutate(yr_lab = case_when(year %% 5 ==0 ~ year)) %>% 
    ggplot(aes(tx_curr, planned_funding_ct, label = yr_lab)) +
    geom_point(size = 4) +
    geom_path() +
    geom_text_repel(family = "Source Sans Pro", color = "#505050", force = 15, segment.colour =  NA) +
    # scale_x_log10(labels = unit_format(1, unit = "M", scale = 1e-6)) +
    # scale_y_log10(labels = unit_format(.1, unit = "B", scale = 1e-9)) +
    scale_x_continuous(labels = unit_format(1, unit = "M", scale = 1e-6)) +
    scale_y_continuous(labels = unit_format(.1, unit = "B", scale = 1e-9)) +
    labs(y = "PEPFAR Budget (planned, billions USD)",
         x = "Current on Treatment") +
    si_style()
  
  
  combo %>% 
    ggplot(aes(year, ue)) +
    geom_point(size = 3) +
    geom_segment(aes(y = 0, yend = ue, xend = year), size = 1) +
    scale_y_continuous(labels = dollar) +
    labs(x = NULL, y = NULL, subtitle =  "PEPFAR Care and Treatment Budget per Patient on Treatment") +
    si_style_ygrid()
    
  