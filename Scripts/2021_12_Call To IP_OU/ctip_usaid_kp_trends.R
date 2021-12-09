# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  KP successes to highlight
# LICENSE:  MIT
# DATE:     2021-10-28
# UPDATED:  2021-12-09
# NOTES:    updated from agitprop/gpm_usaid_kp_trends.R

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
  library(googlesheets4)
  library(janitor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  gs_id <- as_sheets_id("1-9HJdTTwKTKEOeSMkwo3hifrjEdvsA820INZ4fKdTRg")

  msd_source <- source_info()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  df_twbx <- read_sheet(gs_id, sheet = "FY21Q4",
                        .name_repair = ~ make_clean_names(.)) 
    

# MUNGE -------------------------------------------------------------------

  df_twbx <- df_twbx %>% 
    rename(value = results,
           period = time_period,
           proxy_link = linkage) %>% 
    mutate(period = str_remove(period, " "))
  
  df_viz <- df_twbx %>%
    arrange(indicator, period) %>% 
    mutate(start_pt = case_when(period == min(period) ~ value),
           end_pt = case_when(period == max(period) ~ value),
           var_lab = case_when(period == min(period) ~ indicator),
           fill_color = ifelse(indicator == "HTS_TST_POS", genoa, burnt_sienna),
           lab_pos = case_when(!is.na(proxy_link) ~ 1e3))
  
  v1 <- df_viz %>% 
    ggplot(aes(period, value, group = indicator,
               fill = fill_color, color = fill_color)) +
    geom_line(size = 1) +
    # geom_area(data = df_viz %>% filter(indicator != "HTS_TST_POS"),
    #           alpha = .4) +
    geom_point(aes(y = start_pt), na.rm = TRUE, shape = 21, stroke = 1.1, fill = "white") +
    geom_point(aes(y = end_pt), na.rm = TRUE, shape = 21, stroke = 1.1) +
    geom_text(aes(label = var_lab), na.rm = TRUE, family = "Source Sans Pro SemiBold") +
    geom_label(aes(y= lab_pos, label = percent(proxy_link, 1)),
               color = "white", family = "Source Sans Pro SemiBold") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_y_continuous(label = label_number_si()) +
    expand_limits(y = 0) +
    labs(x = NULL, y = NULL) +
    si_style()
    
  
  df_prep <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "KeyPopAbr")) %>% 
    group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>% 
    reshape_msd() %>% 
    mutate(standardizeddisaggregate = recode(standardizeddisaggregate, 
           "KeyPopAbr" = "keypop",
           "Total Numerator" = "total"),
           value = na_if(value, 0)) %>% 
    pivot_wider(names_from = standardizeddisaggregate) %>% 
    mutate(share = keypop/total)

  
  df_prep <- df_prep %>% 
    mutate(grp = case_when(period %in% c("FY20Q1", "FY20Q2") ~ "FY20S1",
                           period %in% c("FY20Q3", "FY20Q4") ~ "FY20S2",
                           TRUE ~ period)) %>% 
    group_by(grp) %>% 
    mutate(kp_qtr_avg = mean(keypop, na.rm = TRUE)/2) %>% 
    ungroup() %>% 
    mutate(kp_qtr_avg = case_when(str_detect(period, "20") ~ kp_qtr_avg,
                            period == "FY21Q1" ~ keypop))
  
  
  df_tx <- df %>% 
    filter(fundingagency == "USAID",
           fiscal_year >= 2020,
           indicator %in% c("TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "KeyPop/HIVStatus") %>% 
    group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE),.groups = "drop") %>% 
    reshape_msd() %>% 
    mutate(fill_color = ifelse(indicator == "TX_CURR", moody_blue_light, moody_blue))
  
  df_tx %>%
    select(-fill_color) %>% 
    pivot_wider(names_from= "indicator") %>% 
    mutate(ret = TX_CURR / (lag(TX_CURR) + TX_NEW))
  
  # v2 <- df_prep %>%
  #   ggplot(aes(period, keypop, group = "x")) +
  #   # geom_area(aes(y = total), alpha = .4) +
  #   geom_area(fill = scooter, color = scooter, size = 1, alpha = .4) +
  #   geom_line(aes(y = kp_qtr_avg), size = .7, color = scooter, linetype = "dashed") +
  #   geom_vline(xintercept = c("FY20Q1", "FY21Q1"), color = "white", linetype = "dashed") +
  #   geom_point(shape =21, color = "white", fill= scooter, size = 4) +
  #   geom_point(aes(y = kp_qtr_avg), #shape =21, color = "white",
  #              color = scooter, size = 2) +
  #   scale_y_continuous(label = label_number_si()) +
  #   labs(x = NULL, y = NULL) +
  #   si_style_ygrid()

  
  v2 <- df_prep %>% 
      ggplot(aes(period, keypop)) +
      geom_col(fill = scooter,  alpha = .8) +
      scale_y_continuous(label = label_number_si()) +
      labs(x = NULL, y = NULL) +
      si_style_ygrid()
    

  v3 <- df_tx %>% 
    ggplot(aes(period, value, fill = fill_color)) +
    geom_col(position = "identity") +
    scale_y_continuous(label = label_number_si()) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_ygrid()
  
  
  v1 + v2 + (v3 / plot_spacer()) +
    plot_annotation(caption = glue("Source: {msd_source} + FY21Q3 Key Population Dashboard",
                                   "USAID SI Analytics",
                                   "Global Planning Meeting 2021-11-15", .sep = " | "),
                    theme = si_style_ygrid()) #&
    # theme(axis.text.y = element_markdown(),
    #       panel.spacing.x = unit(20, "pt"),
    #       panel.spacing.y = unit(0, "pt"),
    #       plot.title = element_markdown())
  
  
  si_save("Graphics/ctip_usaid_kp_trends.svg",
          height = 4.25)
  