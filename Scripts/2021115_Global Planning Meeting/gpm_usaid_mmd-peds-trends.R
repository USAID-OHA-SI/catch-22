# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  MMD
# LICENSE:  MIT
# DATE:     2021-10-29
# UPDATED:  
# NOTE:     Derived from gpm_usaid_mmd-peds-trends.R

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
  
  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   


# MUNGE MMD ---------------------------------------------------------------

  #keep just TX_CURR/MMD and reshape
  df_mmd <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           trendscoarse == "<15",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")) %>% 
    group_by(fiscal_year, countryname, indicator, trendscoarse, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Dominican Republic" = "DR")) %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(total = sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
           #unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
           #unknown = ifelse(unknown < 0, 0, unknown),
           o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
           ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, countryname, indicator, trendscoarse, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) 
  

# MMD FOR AGENCY ROLL UP --------------------------------------------------

  #aggregate up to agency level
  df_mmd_agency <- df_mmd %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate,
                                      "o3mmd" = "MMD - 3 months or more",
                                      "o6mmd" = "MMD - 6 months or more")) %>% 
    group_by(period, otherdisaggregate) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(share = tx_mmd / tx_curr)
  
  #adjust for viz
  df_mmd_agency <- df_mmd_agency %>% 
    mutate(bar_color = ifelse(otherdisaggregate == "MMD - 3 months or more", scooter, genoa),
           otherdisaggregate_md = glue("<span style='color:{bar_color}'>{otherdisaggregate}</span>"))

  
# VIZ ---------------------------------------------------------------------
  
  msd_source <- source_info()
  
  df_mmd_agency %>% 
    filter(otherdisaggregate == "MMD - 3 months or more") %>% 
    ggplot(aes(period, tx_mmd)) + 
    geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5) +
    geom_col(aes(fill = bar_color)) +
    geom_text(aes(label = percent(share, 1)), vjust = -1,
                  family = "Source Sans Pro SemiBold", size = 13/.pt, color = trolley_grey) +
    geom_errorbar(aes(ymax = tx_curr, ymin = tx_curr), color = trolley_grey) +
    facet_wrap(~otherdisaggregate) +
    scale_fill_identity() +
    scale_y_continuous(labels = label_number_si(),
                       position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "MORE THAN HALF OF USAID'S PEDIATRIC PATIENTS ARE ON 3 OR MORE MONTHS OF MULTI MONTH DISPENSING (MMD)",
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more",  
                        "Source: {msd_source}",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13))
  

  si_save("Graphics/gpm_usaid_mmd_peds_trends.svg", height = 4.25)  
  
  
  