# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COVID Stringency Index + MER Trends
# LICENSE:  MIT
# DATE:     2021-10-27
# UPDATED:  
# NOTES:    derived from agitprop/17b_stringency_mer.R

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
  library(lubridate)
  library(COVIDutilities)

# GLOBAL VARIABLES --------------------------------------------------------

  msd_source <- source_info()
  curr_qtr_end <- source_info(return = "period") %>% convert_qtr_to_date() %>% as.Date() %m+% months(3)
  
  authors <- c("Aaron Chafetz", "Tim Essam")
  
  load_secrets()
  
  #quarter starts (for viz)
  qtrs <- seq.Date(as.Date("2019-01-01"), as.Date(curr_qtr_end), by = "3 months")

  
# IMPORT ------------------------------------------------------------------

  #MER data
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() 
  
  #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
  df_stringency <- pull_stringency_index(date_end = curr_qtr_end)
  
  #COVID cases (JHU)
  df_covid <- pull_jhu_covid()

  #10th case
  df_tenthcase_date <- df_covid %>% 
    filter(tenth_case == 1) %>% 
    group_by(iso) %>% 
    filter(date == min(date)) %>% 
    ungroup() %>% 
    select(iso, date, tenth_case)   

  

# MUNGE MER ---------------------------------------------------------------

  #select indicator and reshape long
  df_mer <- df %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS","TX_NEW", "TX_CURR", "TX_PVLS", "VMMC_CIRC"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator", "Age/Sex/ARVDispense/HIVStatus"),
           otherdisaggregate %in% c(NA, "ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months"),
           fiscal_year >= 2019,
           mech_code != "16772") %>% 
    mutate(indicator = ifelse(standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus", "TX_MMD_o3mo", indicator)) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, indicator, countryname) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    filter(!(str_detect(period, "FY22") & value == 0))

  #adjust quarters to dates for working with COVID data
  df_mer <- df_mer %>% 
    mutate(date = period %>% 
             str_remove("FY") %>% 
             yq(), .after = period)
  

# MERGE -------------------------------------------------------------------

  df_stringency_viz <- df_stringency %>% 
    left_join(df_tenthcase_date) %>% 
    select(date, countryname, stringency, bins, color, tenth_case)

  
  df_early <- expand.grid(countryname = unique(df_mer$countryname),
                          date = seq.Date(min(df_mer$date), (min(df_stringency_viz$date, na.rm = TRUE)- days(1)), by = "day"),
                          color = "#D9CDC3") %>%
    as_tibble() %>%
    mutate(across(c(countryname, color), as.character))
  
  
  df_viz <- df_stringency_viz %>% 
    bind_rows(df_early) %>%
    tidylog::full_join(df_mer %>%
                         pivot_wider(names_from = indicator)) %>% 
    mutate(countryname = recode(countryname, "Democratic Republic of the Congo" = "DRC"))
    
  df_viz <- df_viz %>% 
    mutate(VLS = TX_PVLS/TX_PVLS_D,
           TX_MMD_o3mo_share = TX_MMD_o3mo/TX_CURR)
    
  df_viz <- df_viz %>% 
    pivot_longer(-date:-period,
                 names_to = "indicator")

  df_dates <- df_mer %>% 
    distinct(period, date) %>% 
    arrange(date) %>% 
    mutate(fy = str_sub(period, end = -3)) %>% 
    filter(str_detect(period, "Q1"))
  
  lst_mmd_order <- df_viz %>% 
    filter(date == max(date, na.rm = TRUE),
           indicator == "TX_CURR",
           !countryname %in% c("South Africa", "Namibia")) %>% 
    arrange(desc(value)) %>% 
    pull(countryname)
  
  plot_mer_stringency <- function(ind_sel, n_countries = 16, save = FALSE){
    
    if(str_detect({ind_sel}, "MMD")){
      df_viz <-  filter(df_viz, date >= "2020-01-01")
      df_dates <- filter(df_dates, date >= "2020-01-01")
    }
    
    df_viz <- filter(df_viz, indicator == {ind_sel})
    
      
    #latest value for ordering
    lst_lrg <- df_viz %>% 
      filter(date == max(date, na.rm = TRUE)) %>% 
      slice_max(order_by = value, n = n_countries) %>% 
      pull(countryname)
    
    if(str_detect({ind_sel}, "MMD"))
      lst_lrg <- lst_mmd_order[1:n_countries]
    
    df_viz <- df_viz %>% 
      filter(countryname %in% lst_lrg) %>% 
      mutate(countryname = factor(countryname, lst_lrg)) 
    
    v <- df_viz %>% 
      ggplot(aes(date, value), na.rm = TRUE) +
      geom_area(alpha = .4, color = genoa, fill = genoa_light, na.rm = TRUE) +
      geom_blank(aes(y = 1.2e6)) +
      geom_vline(data = filter(df_viz, tenth_case ==1, countryname %in% lst_lrg),
                 aes(xintercept = date), color = "#909090", linetype = "dotted", na.rm = TRUE) +
      geom_vline(xintercept = df_dates$date, color = "white") +
      geom_rug(aes(color = color), sides="b", na.rm = TRUE) +
      facet_wrap(~countryname, scales = "free_y", nrow = 2) +
      # scale_y_continuous(labels = number_format(accuracy = y_accuracy, scale = y_scale, suffix = y_suffix, big.mark = ",")) +
      scale_y_continuous(label = label_number_si()) +
      # scale_y_dynamic() +
      scale_x_date(breaks = as.Date(df_dates$date), labels = df_dates$fy) +
      expand_limits(y = 1) +
      scale_color_identity() +
      labs(x = NULL, y = NULL,
           title = "PEPFAR's work has remained resilient in the face of government policies to curb COVID" %>% toupper,
           subtitle = glue("{ind_sel} in the largest {n_countries} countries | {min(df_dates$fy)}-{max(df_dates$fy)}"),
           caption = glue("Sources: PEPFAR {msd_source}, JHU COVID-19 feed, Stringency Index from Blavatnik School of Government at Oxford University
         SI analytics: SI analytics: {paste(authors, collapse = '/')}
         US Agency for International Development")) +
      si_style_ygrid() +
      theme(panel.spacing.x = unit(.5, "lines"),
            panel.spacing.y = unit(.5, "lines"))
    
    if(save == TRUE){
      file_out <- glue("Graphics/gpm_country_stringency_{ind_sel}.svg")
      usethis::ui_info("saving to {usethis::ui_field(file_out)}")
      si_save(file_out, plot = v, height = 4.25)
    }
    
    return(v)
  }
  
  plot_mer_stringency("TX_CURR", n_countries = 8, save = FALSE)
