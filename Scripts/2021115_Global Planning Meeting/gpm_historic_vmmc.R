# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  VMMC trends
# LICENSE:  MIT
# DATE:     2021-11-09
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

  curr_fy <- source_info(return = "fiscal_year")
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   

# MUNGE -------------------------------------------------------------------

  df_vmmc <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "VMMC_CIRC",
           standardizeddisaggregate == "Total Numerator")

  df_vmmc <- df_vmmc %>% 
    group_by(fiscal_year, fundingagency, indicator) %>% 
    summarise(across(c(starts_with("qtr"), targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(qtr4 = ifelse(fiscal_year == curr_fy, -1, qtr4)) %>% 
    reshape_msd("quarters") %>%
    adorn_achievement() %>% 
    mutate(across(c(results, results_cumulative, achievement_qtrly, achv_label, achv_color), 
                  ~ ifelse(results < 0, NA_real_, .))) 
  
  df_vmmc <- df_vmmc %>% 
    mutate(qtr = str_sub(period, -1) %>% as.numeric(),
           targets_qtrly = targets * (qtr/4),
           targets_qtrly_min = targets * ((qtr/4) - .1),
           targets_qtrly_max = targets * ((qtr/4) + .1),
           achv_lab_position = -1e5)
  
  # df_vmmc %>% 
  #   ggplot(aes(period, results, fill = achv_color)) +
  #   geom_col() +
  #   geom_errorbar(aes(ymin = targets_qtrly, ymax = targets_qtrly)) 
  #   facet_grid(. ~ fiscal_year, scales = "free_x", space = "free_x") +
  #   scale_fill_identity() +
  #   si_style_ygrid()
  
  df_vmmc %>% 
    ggplot(aes(period, results_cumulative, fill = achv_color)) +
    geom_col(fill = burnt_sienna, alpha = .8) +
    geom_errorbar(aes(ymin = targets_qtrly, ymax = targets_qtrly), na.rm = TRUE) +
    geom_label(aes(y = achv_lab_position, label = percent(achievement_qtrly, 1)),
               na.rm = TRUE, label.size = 0, family = "Source Sans Pro") + 
    facet_grid(. ~ fiscal_year, scales = "free_x", space = "free_x") +
    labs(x = NULL, y = NULL,
         title = "USAID VMMC SERVICES SLOWED DOWN DURING COVID BUT FY21 TARGETs ARE WITHIN REACH AS SERVICES CONTINUE TO COME BACK ONLINE",
         subtitle = "Cumulative VMMC results against quarterly target benchmarks",
         caption = glue("Source: {msd_source}",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    scale_y_continuous(label = number_format(.1, scale = 1e-6, suffix = "M")) +
    scale_fill_identity() +
    si_style_ygrid()
  
  
  si_save("Graphics/<.svg",
          height = 4.25)  
  
  # 
  # df_vmmc %>% 
  #   ggplot(aes(period, results_cumulative, fill = achv_color)) +
  #   geom_col() +
  #   geom_errorbar(aes(ymin = targets_qtrly_min, ymax = targets_qtrly_max), 
  #                  # size = 2, 
  #                 na.rm = TRUE) +
  #   facet_grid(. ~ fiscal_year, scales = "free_x", space = "free_x") +
  #   scale_fill_identity() +
  #   si_style_ygrid()
  # 
  # 
  # df_vmmc %>% 
  #   # ggplot(aes(period, results_cumulative, fill = achv_color)) +
  #   # geom_col() +
  #   ggplot(aes(period)) +
  #   geom_ribbon(aes(ymin = targets_qtrly_min, ymax = targets_qtrly_max), 
  #                 # size = 2, 
  #               fill = "grey70",
  #                 na.rm = TRUE) +
  #   facet_grid(. ~ fiscal_year, scales = "free_x", space = "free_x") +
  #   # scale_fill_identity() +
  #   si_style_ygrid()
  # 
  # 
  # df_vmmc %>% 
  #   ggplot(aes(period, results_cumulative, fill = achv_color, group = fiscal_year)) +
  #   geom_errorbar(aes(ymin = targets_qtrly_min, ymax = targets_qtrly_max), na.rm = TRUE) +
  #   geom_line() +
  #   geom_point() +
  #   facet_grid(. ~ fiscal_year, scales = "free_x", space = "free_x") +
  #   scale_fill_identity() +
  #   si_style_ygrid()
  