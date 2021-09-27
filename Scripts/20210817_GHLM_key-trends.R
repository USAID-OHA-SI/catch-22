# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  visual for GHLM depicting performace in key areas
# LICENSE:  MIT
# DATE:     2021-08-17
# UPDATED:  201-08-23


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
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()
  
  authors <- c("Aaron Chafetz")
  
  msd_source <- source_info()
    
  #path to TX_NET_NEW adjustments
  nn_path <- "C:/Users/achafetz/Documents/GitHub/right_size/Dataout/TX_CURR_NN_Calcs.csv"

# IMPORT ------------------------------------------------------------------
  
  df_nn <- vroom(nn_path)
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_rds()
  

  # MUNGE MMD ---------------------------------------------------------------
  
  #keep just TX_CURR/MMD and reshape
  df_mmd <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    )) %>%
    group_by(fiscal_year, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd()
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate,
                                      "o3mmd" = "MMD - 3 months or more",
                                      "o6mmd" = "MMD - 6 months or more"),
           share = tx_mmd / tx_curr,
           indicator = "TX_MMD") %>% 
    rename(value = tx_mmd)

# MUNGE OTHER -------------------------------------------------------------

  df_nn_agg <- df_nn %>% 
    filter(str_detect(period, "FY2"),
           flag_loneobs %in% c(FALSE, NA),
           fundingagency %in% c("USAID", "NLV6dy7BE2O")) %>% 
    mutate(indicator = "TX_NET_NEW - Adjusted") %>% 
    group_by(period, indicator) %>% 
    summarise(value = sum(tx_net_new_adj_plus, na.rm = TRUE)) %>% 
    ungroup()
    
  df_prep_agg <- df %>% 
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           fiscal_year >= 2020
           ) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    mutate(value = na_if(value, 0))


# BIND DATA ---------------------------------------------------------------

  df_viz <- bind_rows(df_nn_agg, df_prep_agg, df_mmd)
  
  df_viz <- df_viz %>% 
    mutate(bar_color = case_when(otherdisaggregate == "MMD - 3 months or more" ~ scooter, 
                                 otherdisaggregate == "MMD - 6 months or more" ~ genoa,
                                 indicator == "PrEP_NEW" ~ moody_blue,
                                 TRUE ~ burnt_sienna),
           facet_lab = ifelse(indicator == "TX_MMD",
                                 glue("TX_{otherdisaggregate}"),
                                 glue("{indicator}")),
           facet_lab_md = ifelse(indicator == "TX_MMD",
                                 glue("<span style='color:{bar_color}'>TX_{otherdisaggregate}</span>"),
                                 glue("<span style='color:{bar_color}'>{indicator}</span>")),
           order = case_when(otherdisaggregate == "MMD - 3 months or more" ~ 1, 
                             otherdisaggregate == "MMD - 6 months or more" ~ 2,
                             indicator == "PrEP_NEW" ~ 4,
                             TRUE ~ 3),
           facet_lab = fct_reorder(facet_lab, order, max),
           facet_lab_md = fct_reorder(facet_lab_md, order, max))
  
    
  df_viz %>% 
    filter(indicator %in% c("TX_NET_NEW - Adjusted", "PrEP_NEW")) %>% 
    ggplot(aes(period, value)) + 
    geom_blank(aes(y = 2.5e5)) + 
    # geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5, na.rm = TRUE) +
    geom_col(aes(fill = bar_color), na.rm = TRUE) +
    geom_text(aes(label = percent(share, 1)), vjust = -1, na.rm = TRUE,
              family = "Source Sans Pro", color = trolley_grey) +
    geom_errorbar(aes(ymax = tx_curr, ymin = tx_curr), color = trolley_grey, na.rm = TRUE) +
    facet_wrap(~facet_lab) +
    # facet_wrap(~facet_lab, scales = "free_y") +
    scale_fill_identity() +
    scale_y_continuous(label = label_number_si(),
                       expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         caption = glue("Adjusted TX_NET_NEW accounts for partner site transitions and removes 'lone obserations'
                        Source: {msd_source} + TX_NET_NEW Adjustments
                        SI analytics: {paste(authors, collapse = '/')} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          # strip.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 13),
          strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/20210823_GHLM_trends.svg",
          width = 9.5,
          height = 4.25)
  
  
  
  
  df_ag_map <- datim_dim_items("Funding Agency") %>% 
    rename(fundingagency = id,
           agency = item)
  
  df_nn_agg <- df_nn %>% 
    left_join(df_ag_map) %>% 
    filter(flag_loneobs %in% c(NA, FALSE),
           agency %in% c("USAID", "HHS/CDC"),
           str_detect(period, "FY2")) %>% 
    group_by(period, agency) %>% 
    summarise(across(c(tx_net_new_adj_plus, tx_curr), sum, na.rm = TRUE), .groups = "drop") %>% 
    rename(fundingagency = agency) %>% 
    clean_agency() %>% 
    pivot_longer(starts_with("tx"), names_to = "indicator") %>% 
    mutate(indicator = toupper(indicator),
           indicator = recode(indicator, "TX_NET_NEW_ADJ_PLUS" = "TX_NET_NEW - Adjusted")) %>% 
    arrange(fundingagency, indicator, period)
  
  
  df_nn_agg %>% 
    filter(indicator != "TX_CURR") %>% 
    mutate(fy = str_sub(period, end = 4)) %>% 
    count(fundingagency, indicator, fy, wt = value) %>% 
    spread(fy, n)
  
  df_nn_agg %>% 
    mutate(fy = str_sub(period, end = 4)) %>% 
    filter(indicator != "TX_CURR",
           period != "FY20Q1") %>% 
    count(fundingagency, wt = value)
  
  df_viz2 <- df_nn_agg %>% 
    mutate(fundingagency = fct_rev(fundingagency),
           fill_alpha = ifelse(period == max(period), 1, .75),
           fill_color = case_when(indicator == "TX_CURR" ~ trolley_grey,
                                  fundingagency == "USAID" ~ denim,
                                  TRUE ~ scooter))
  # fill_color = ifelse(fundingagency == "USAID", denim, scooter))
  
  df_viz2 %>% 
    ggplot(aes(period, value, fill = fill_color, alpha = fill_alpha)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "#202020") +
    facet_grid(indicator~fundingagency, scales = "free_y", switch = "y") +
    scale_y_continuous(label = label_number_si()) +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, 
         caption = glue("Adjusted TX_NET_NEW accounts for partner site transitions and removes 'lone obserations'
                        Source: TX_NET_NEW Adjustments calculated from DATIM extracted [2021-08-23]
                        SI analytics: {paste(authors, collapse = '/')} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(strip.placement = "outside",
          strip.text.x = element_text(family = "Source Sans Pro SemiBold"),
          strip.text.y = element_text(family = "Source Sans Pro SemiBold", hjust = .5),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/20210824_GHLM_NN_trends.svg",
          width = 9.5,
          height = 4.25)  
  