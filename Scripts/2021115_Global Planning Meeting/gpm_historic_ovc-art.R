# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  OVC ART coverage
# LICENSE:  MIT
# DATE:     2021-11-08
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
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_rds()   

# MUNGE -------------------------------------------------------------------

  df_ovc <- df %>% 
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "OVC_HIVSTAT",
           fiscal_year >= 2018,
           ((fiscal_year <=2018 & standardizeddisaggregate == "StatusPosART") |
              fiscal_year > 2018 & standardizeddisaggregate == "ReportedStatus" & statushiv == "Positive")
              ) %>% 
    mutate(type = ifelse(otherdisaggregate == "Receiving ART", "on_art", "no_art")) %>% 
    group_by(fiscal_year, type) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value > 0) %>% 
    pivot_wider(names_from = "type") %>% 
    mutate(total = on_art + no_art,
           art_share = on_art/total)
    
  
  v_pos <- df_ovc %>% 
    ggplot(aes(period, total)) +
    geom_col(fill = denim) +
    geom_text(data = . %>% filter(period != max(period)),
              aes(label = number(total, 1, scale = 1e-3,  suffix = "k")),
              vjust = 1.3, family = "Source Sans Pro SemiBold",
              color = "white", size = 12/.pt) +
    geom_text(data = . %>% filter(period == max(period)),
              aes(label = number(total, 1, scale = 1e-3,  suffix = "k")),
              vjust = -1, family = "Source Sans Pro SemiBold",
              color = denim, size = 12/.pt) +
    coord_cartesian(clip = "off", expand = TRUE) +
    labs(x = NULL, y = NULL) +
    si_style_ygrid() +
    theme(axis.text.y = element_blank())
  
  v_share <- df_ovc %>% 
    ggplot(aes(period, art_share)) +
    geom_point(size = 12, color = scooter) +
    geom_text(aes(label = percent(art_share, 1)),
              hjust = .5, vjust = .5,
              family = "Source Sans Pro SemiBold",
              color = "white", size = 10/.pt) +
    coord_cartesian(clip = "off", expand = TRUE) +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())
  
  v_share /v_pos  + 
    plot_layout(heights = c(.5, 8)) + 
    plot_annotation(title = "Since FY20, USAID has been able to ensure that all HIV+ OVC are on treatment" %>% toupper,
                    caption = glue("Source: {msd_source}",
                                   "USAID SI Analytics",
                                   "Global Planning Meeting 2021-11-15", .sep = " | "),
                    theme = si_style_ygrid()) 

  si_save("Graphics/gpm_historic_ovc-art.svg",
          height = 3.33, width = 5.5)  
  