# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  DREAM/AGYW plots for pre-planning meeting
# LICENSE:  MIT
# DATE:     2021-11-01
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  msd_source <- source_info()

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  

# MUNGE -------------------------------------------------------------------

  df_agyw <- df %>% 
    filter(indicator == "AGYW_PREV",
           fiscal_year == 2021,
           str_detect(standardizeddisaggregate, "Age/Sex/Tim"),
           ageasentered %in% c("10-14", "15-19", "20-24"),
           numeratordenom == "D")
  
  df_agyw <- df_agyw %>% 
    mutate(time = ifelse(otherdisaggregate_sub == "<6 Months in DREAMS", "u6mo", "o6mo"))

  df_agyw <- df_agyw %>% 
    bind_rows(df_agyw %>% mutate(operatingunit = "All PEPFAR")) %>% 
    group_by(fiscal_year, operatingunit, time) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = time, values_from = cumulative) %>% 
    mutate(total = o6mo + u6mo,
           share_u6mo = u6mo/total)
  
  df_viz <- df_agyw %>% 
    mutate(clean_num = case_when(operatingunit == "All PEPFAR" ~ number(total, .1, scale = 1e-6, suffix = "M"),
                              TRUE ~ number(total, 1, scale = 1e-3, suffix = "K")),
           ou_lab = case_when(share_u6mo == max(share_u6mo) ~ glue("{operatingunit} (Total DREAMS = {clean_num})"),
                              TRUE ~ glue("{operatingunit} ({clean_num})")),
           alpha_fill = ifelse(operatingunit == "All PEPFAR", .95, .7))

  pepfar_total <- df_viz %>% 
    filter(operatingunit == "All PEPFAR") %>% 
    pull(total) %>% 
    number(.1, scale = 1e-6, suffix = "M")
  
  # df_viz %>% 
  #   ggplot(aes(share_u6mo, fct_reorder(ou_lab, share_u6mo), alpha = alpha_fill)) +
  #   geom_col(fill = moody_blue) +
  #   scale_x_continuous(label = percent, expand = c(.005, .005)) +
  #   scale_alpha_identity() +
  #   labs(x = NULL, y = NULL,
  #        title = glue("Across PEPFAR, nearly 75% of the {pepfar_total} DREAMS beneficiaries have been enrolled in the last 6 months") %>% toupper(),
  #        subtitle = "AGYW between 10-25 years old in FY21Q2",
  #        caption = glue("Source: {msd_source}",
  #                       "USAID SI Analytics",
  #                       "Global Planning Meeting 2021-11-15", .sep = " | ")) +
  #   si_style_xgrid()
  
  
  
  
  df_comp <- df %>% 
    filter(indicator == "AGYW_PREV",
           fiscal_year == 2021,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))
  
  
  df_comp <- df_comp %>% 
    bind_rows(df_comp %>% mutate(operatingunit = "All PEPFAR")) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, operatingunit, indicator) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = indicator, 
                names_glue = "{tolower(indicator)}",
                values_from = cumulative) %>% 
    mutate(share_comp = agyw_prev/agyw_prev_d)
  
  df_viz_comp <- df_comp %>% 
    mutate(clean_num = case_when(operatingunit == "All PEPFAR" ~ number(agyw_prev_d, .1, scale = 1e-6, suffix = "M"),
                                 TRUE ~ number(agyw_prev_d, 1, scale = 1e-3, suffix = "K")),
           ou_lab = case_when(share_comp == max(share_comp) ~ glue("{operatingunit} (Total DREAMS = {clean_num})"),
                              TRUE ~ glue("{operatingunit} ({clean_num})")),
           alpha_fill = ifelse(operatingunit == "All PEPFAR", .95, .7))
  
  pepfar_total_comp <- df_viz_comp %>% 
    filter(operatingunit == "All PEPFAR") %>% 
    pull(agyw_prev_d) %>% 
    number(.1, scale = 1e-6, suffix = "M")
  
  # df_viz_comp %>% 
  #   ggplot(aes(share_comp, fct_reorder(ou_lab, share_comp), alpha = alpha_fill)) +
  #   geom_col(fill = burnt_sienna) +
  #   expand_limits(x = 1) +
  #   scale_x_continuous(label = percent, expand = c(.005, .005)) +
  #   scale_alpha_identity() +
  #   labs(x = NULL, y = NULL,
  #        title = glue("Across PEPFAR, less than 40% of the {pepfar_total} DREAMS beneficiaries have completed the primary package of services") %>% toupper(),
  #        subtitle = "DREAMS AGYW beneficiaries in FY21Q2",
  #        caption = glue("Source: {msd_source}",
  #                       "USAID SI Analytics",
  #                       "Global Planning Meeting 2021-11-15", .sep = " | ")) +
  #   si_style_xgrid()
  # 
  
  df_viz_overall <- df_viz_comp %>% 
    select(operatingunit, ou_lab, agyw_prev, agyw_prev_d, share_comp) %>% 
    left_join(df_viz %>% 
                select(operatingunit, share_u6mo)) %>% 
    pivot_longer(starts_with("share_"),
                 names_to = "type", 
                 names_prefix = "share_") %>% 
    mutate(fill_color = ifelse(type == "comp", burnt_sienna, moody_blue),
           alpha_fill = ifelse(operatingunit == "All PEPFAR", .95, .7),
           order = case_when(type == "comp" ~ value),
           type = ifelse(type == "comp", "Primary Package Completion Rate",
                         "Share of beneficiaries (10-24yo) in DREAMS <6 months"))  

  
  
  df_viz_overall %>% 
    ggplot(aes(value, fct_reorder(ou_lab, order, sum, na.rm = TRUE), 
               alpha = alpha_fill, fill = fill_color)) +
    geom_col() +
    geom_vline(xintercept = 0) +
    # geom_vline(xintercept = .9) +
    facet_wrap(~ type) +
    scale_x_continuous(label = percent, expand = c(.005, .005)) +
    scale_alpha_identity() +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = glue("A high share of new beneficiaries may be contributing to less than 40% of the {pepfar_total} DREAMS beneficiaries having completed the primary package of services") %>% toupper(),
         subtitle = "DREAMS AGYW beneficiaries in FY21Q2",
         caption = glue("Source: {msd_source}",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    si_style_xgrid()
  
  
  si_save("Graphics/gpm_pepfar_dreams-completion.svg",
          height = 4.25)
  
  
  
  # df_viz_comp %>% 
  #   select(operatingunit, ou_lab, agyw_prev_d, share_comp) %>% 
  #   left_join(df_viz %>% 
  #               select(operatingunit, share_u6mo)) %>% 
  #   filter(operatingunit != "All PEPFAR") %>% 
  #   ggplot(aes(share_comp, 
  #              share_u6mo)) +
  #   geom_point(aes(size = agyw_prev_d)) +
  #   ggrepel::geom_text_repel(aes(label = operatingunit)) +
  #   si_style()
  