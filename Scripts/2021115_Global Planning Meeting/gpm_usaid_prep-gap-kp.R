# PROJECT:  cath-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  prep gap
# LICENSE:  MIT
# DATE:     2021-11-04
# UPDATED:  2021-11-08

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
  library(colorspace)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  msd_source <- source_info()


# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   
  
  
  df_gap <- df %>% 
    filter(fundingagency == "USAID",
           fiscal_year >= 2021,
           indicator %in% c("PrEP_NEW", "HTS_TST_NEG"),
           standardizeddisaggregate %in% c("KeyPopAbr","KeyPop/Result")
           ) %>% 
    group_by(fiscal_year, operatingunit, psnu, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")

  df_gap <- df_gap %>% 
    reshape_msd()

  df_gap_lim <- df_gap %>% 
    pivot_wider(names_from = "indicator") %>% 
    filter(HTS_TST_NEG > 0, 
           PrEP_NEW > 0) %>% 
    pivot_longer(c(HTS_TST_NEG, PrEP_NEW), names_to = "indicator")
  
  df_gap_lim <- df_gap_lim %>% 
    count(period, indicator, wt = value, name = "value")
      
  
  df_gap_lim %>% 
    ggplot(aes(period, value, fill = indicator, 
               color = indicator, alpha = indicator)) +
    geom_col(width = 0.8, position = position_dodge(width = .2)) +
    geom_label(aes(label = number(value, 1, scale = 1e-3, suffix = "K")),
               show.legend = FALSE, fill= "white", family = "Source Sans Pro", 
               position = position_dodge(width = .2), alpha = 1) +
    scale_alpha_manual(values = c("HTS_TST_NEG" = .7,
                                  "PrEP_NEW" = .9)) +
    scale_fill_manual(values = c("HTS_TST_NEG" = scooter,
                                 "PrEP_NEW" = denim),
                      aesthetics = c("color", "fill")) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Only a fraction of Key Populations with negative HIV tests are using PrEP" %>% toupper,
         caption = glue("Note: Limited to only PSNUs reporting on both KP PrEP_NEW and HTS_TST_NEG
                        Source: {msd_source}",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    si_style_ygrid() +
    theme(axis.text.y = element_blank(),
          legend.position = "none")
  
  si_save("Graphics/gpm_usaid_prep-gap-kp.svg", height = 4.25) 
  