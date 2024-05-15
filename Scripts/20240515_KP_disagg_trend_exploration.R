# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  KP disagg exploration
# REF ID:   602a37f6 
# LICENSE:  MIT
# DATE:     2024-05-15
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    metadata <- get_metadata(filepath) 
  
  ref_id <- "602a37f6"

# IMPORT ------------------------------------------------------------------
  
  filepath <- si_path() %>% return_latest("OU_IM_FY22")
  
  df_msd <- read_psd(filepath)
  

# MUNGE -------------------------------------------------------------------
  
 df_kp <- df_msd %>% 
    filter(country %in% c("Malawi", "Ghana", "Kenya", "Tanzania"),
           funding_agency == "USAID",
           #fiscal_year %in% c(2022, 2023, 2024),
           indicator %in% c("TX_CURR", "TX_NEW", "PrEP_NEW", "HTS_TST"),
           str_detect(standardizeddisaggregate, "KeyPop")) %>% 
    group_by(country, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    mutate(val_label = case_when(period == min(period)~comma(value),
                                 period == max(period)~comma(value),
                                 TRUE~NA))
 
  
  
# VIZ -------------------------------------------------------------------
  
  my_labels <- df_kp %>% distinct(period) %>% pull()
  cust_labels <- nrsmisc::every_nth(my_labels, 4, inverse = T)
  # Add this to your ggplot
  
 df_kp %>% 
   ggplot(aes(x = period, y = value, group = period_type,
              color = indicator, fill = indicator)) + 
   geom_area(alpha = .2, size = 1, na.rm = TRUE) +
   geom_point(size = 2) +
   facet_grid(indicator~country, scales = "free_y", switch = "y") +
   si_style_nolines() +
   scale_x_discrete(labels = cust_labels) +
  # expand_limits(y = 30000) +
   geom_text(aes(label = val_label, hjust = 0.5,
                 vjust = -0.5, family = "Source Sans Pro")) +
   scale_fill_manual(values = c("HTS_TST" = hw_hunter, "PrEP_NEW" = hw_midnight_blue,
                                "TX_CURR" = hw_orchid_bloom, "TX_NEW" = hw_sun_kissed)) +
   scale_color_manual(values = c("HTS_TST" = hw_hunter, "PrEP_NEW" = hw_midnight_blue,
                                "TX_CURR" = hw_orchid_bloom, "TX_NEW" = hw_sun_kissed)) +
   labs(x = NULL, y = NULL,
        title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
        subtitle = "Countries of interest: Ghana, Kenya, Malawi, Tanzania",
        caption = glue("Source: {metadata$caption}
                       Note: Uganda does not report on Key Population disaggregates")) + 
   theme(axis.text.y = element_blank(),
         legend.position = "none",
         strip.text.y.left = element_text(angle=360))
 
 si_save("Images/KP_disagg.png")
  
