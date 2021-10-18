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
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  msd_source <- source_info()
  pd <- source_info(return = "period")

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
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
    bind_rows(df_vls %>% mutate(mech_code = "National")) %>% 
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
  
  #identify where 80% of TX_CURR is for viz facet
  lst_lrg_ctry <- df_vls %>% 
    filter(mech_code == "National") %>% 
    count(countryname, wt = tx_curr) %>% 
    arrange(desc(n)) %>% 
    mutate(cumsum = cumsum(n)) %>% 
    mutate(share = cumsum/sum(n)) %>% 
    filter(share <= .83) %>% 
    pull(countryname)
  
  #viz dataframe
  df_viz <- df_vls %>% 
    mutate(vls_mech = case_when(mech_code != "National" ~ vls),
           vls_nat = case_when(mech_code == "National" ~ vls),
           fill_color = ifelse(vls_mech > .9, scooter, moody_blue),
           lrg_ctry = ifelse(countryname %in% lst_lrg_ctry, "80% of USAID Treatment Portfolio", "Remaining 20%"),
           countryname = recode(countryname, 
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR"),
           mech_lab = case_when(vls_mech < .9 ~ mech_code))

  

  
# VIZ ---------------------------------------------------------------------

 
  df_viz %>% 
    ggplot(aes(vls_mech, fct_reorder(countryname, vls_nat, na.rm = TRUE))) +
    geom_blank() +
    annotate("rect",
             xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
             fill = trolley_grey_light, alpha = .4) +
    geom_vline(xintercept = .9, linetype = "dashed") +
    geom_point(aes(size = tx_curr, color = fill_color), alpha = .6,
               position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
    geom_text_repel(aes(label = mech_lab), na.rm = TRUE,
                    family = "Source Sans Pro", color = "#505050", size = 9/.pt) +
    geom_errorbar(aes(xmin = vls_nat, xmax = vls_nat), size = 1.1, color = grey60k) +
    scale_x_continuous(label = percent_format(1)) +
    facet_grid(lrg_ctry ~ ., scale = "free_y", space = "free") +
    scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M")) +
    scale_color_identity() +
    expand_limits(x = .75) +
    labs(y = NULL, x = "Viral Load Supression Rate (TX_PVLS/TX_PVLS_D)",
         size = glue("Current on Treatment ({pd})"),
         caption = glue("Source: {msd_source}
                        SI Analytics: {paste0(authors, collapse = '/')}
                        US Agency for International Development")) +
    si_style()
  