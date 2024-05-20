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
    ref_id <- "602a37f6"

  # Grab metadata

    filepath <- si_path() %>% return_latest("OU_IM_FY22")
  
    metadata <- get_metadata(filepath) 
    
    clean_number <- function(x, digits = 0){
      dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                       x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                       x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                       TRUE ~ glue("{x}"))
    }
  

# IMPORT ------------------------------------------------------------------
  
  
  df_msd <- read_psd(filepath)
  

  

# MUNGE -------------------------------------------------------------------
  
 df_kp <- df_msd %>% 
    filter(country %in% c("Malawi", "Ghana", "Kenya", "Tanzania"),
           #funding_agency == "USAID",
           fiscal_year %in% c(2023, 2024),
           indicator %in% c("TX_CURR", "TX_NEW", "PrEP_NEW", "HTS_TST"),
           str_detect(standardizeddisaggregate, "KeyPop"),
           otherdisaggregate %in% c("MSM", "TG")) %>% 
    group_by(country, fiscal_year, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    group_by(country, indicator, otherdisaggregate) %>% 
    mutate(lag_val = lag(value, 1, order_by = period),
           pct_change = (value - lag_val) / lag_val) %>% 
    ungroup() %>% 
    mutate(pd = stringr::str_remove(period, "FY"),
           date = lubridate::yq(pd)-months(3)) %>% 
    select(-pd, period_type) %>% 
    mutate(bar_color = ifelse(pct_change <0 & !is.na(pct_change), old_rose, trolley_grey_light)) %>%
    #mutate(pct_change = ifelse(is.na(pct_change), " ", pct_change)) %>% 
    mutate(full_lab = glue("{clean_number(value)}\n ({percent(pct_change, 1)})")) 
  
# VIZ FUNCTIONS -------------------------------------------------------------------
    
  
viz_kp_bar <- function(df, cntry, kp, save = F) {
  
  viz <- df  %>% 
    filter(country == cntry,
           otherdisaggregate == kp) %>% 
    ggplot(aes(x = date, y = value, fill = bar_color)) +
    geom_col(width = 50) +
    facet_grid(indicator~otherdisaggregate, scales = "free_y", switch = "y") +
    geom_text(aes(label = full_lab, family = "Source Sans Pro")) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    scale_fill_identity() +
    si_style_xline() + 
    labs(x = NULL, y = NULL,
         title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
         subtitle = glue("Country of Interest: {cntry}"),
         caption = glue("Source: {metadata$caption}
                        Note: Uganda does not report on Key Population disaggregates")) +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          strip.text.y.left = element_text(angle=360))
  
  
  if (save == T) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/KP legislation/{cntry}_{kp}_fy24.svg"))
  }
  
  return(viz)
}  


viz_kp_line <- function(df, cntry, kp, save = F) {
  
viz <- df  %>%
  filter(country == cntry,
         otherdisaggregate == kp) %>%
  ggplot(aes(x = date, y = value, color = bar_color, group = period_type)) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(indicator~otherdisaggregate, scales = "free_y", switch = "y") +
 geom_text(aes(label = full_lab, family = "Source Sans Pro", color = "black")) +
  #scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
  scale_color_identity() +
  si_style_xline() +
  labs(x = NULL, y = NULL,
       title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
       subtitle = glue("Country of Interest: {cntry}"),
       caption = glue("Source: {metadata$caption}
                        Note: Uganda does not report on Key Population disaggregates")) +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        strip.text.y.left = element_text(angle=360))

if (save == T) {
  glitr::si_save(
    plot = viz,
    filename = glue::glue("./Graphics/KP legislation/{cntry}_{kp}_line.svg"))
}

return(viz)

}


# VIZ ITERATE -------------------------------------------------------------------


cntry_list <- c("Ghana", "Kenya" ,"Malawi", "Tanzania",
                "Ghana", "Kenya" ,"Malawi", "Tanzania")

kp_list <- c("MSM","MSM", "MSM", "MSM",
             "TG","TG", "TG", "TG")

map2_dfr(cntry_list, kp_list,
         ~viz_kp_bar(df_kp, cntry = .x, kp = .y, save = T))

#use this one for the KP deck
map2_dfr(cntry_list, kp_list,
         ~viz_kp_line(df_kp, cntry = .x, kp = .y, save = T))
  
  
# VIZ -------------------------------------------------------------------
  
  
  
# old code
  
 #  # get qtr results for each country for KeyPop disaggs
 #  df_kp <- df_msd %>% 
 #    filter(country %in% c("Malawi", "Ghana", "Kenya", "Tanzania"),
 #           funding_agency == "USAID",
 #           #fiscal_year %in% c(2022, 2023, 2024),
 #           indicator %in% c("TX_CURR", "TX_NEW", "PrEP_NEW", "HTS_TST"),
 #           str_detect(standardizeddisaggregate, "KeyPop")) %>% 
 #    group_by(country, fiscal_year, indicator) %>% 
 #    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
 #    reshape_msd()
 #  
 #  my_labels <- df_kp %>% distinct(period) %>% pull()
 #  cust_labels <- nrsmisc::every_nth(my_labels, 4, inverse = T)
 #  # Add this to your ggplot
 #  
 # df_kp %>% 
 #   ggplot(aes(x = period, y = value, group = period_type,
 #              color = indicator, fill = indicator)) + 
 #   geom_area(alpha = .2, size = 1, na.rm = TRUE) +
 #   geom_point(size = 2) +
 #   facet_grid(indicator~country, scales = "free_y", switch = "y") +
 #   si_style_nolines() +
 #   scale_x_discrete(labels = cust_labels) +
 #  # expand_limits(y = 30000) +
 #   geom_text(aes(label = val_label, hjust = 0.5,
 #                 vjust = -0.5, family = "Source Sans Pro")) +
 #   scale_fill_manual(values = c("HTS_TST" = hw_hunter, "PrEP_NEW" = hw_midnight_blue,
 #                                "TX_CURR" = hw_orchid_bloom, "TX_NEW" = hw_sun_kissed)) +
 #   scale_color_manual(values = c("HTS_TST" = hw_hunter, "PrEP_NEW" = hw_midnight_blue,
 #                                "TX_CURR" = hw_orchid_bloom, "TX_NEW" = hw_sun_kissed)) +
 #   labs(x = NULL, y = NULL,
 #        title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
 #        subtitle = "Countries of interest: Ghana, Kenya, Malawi, Tanzania",
 #        caption = glue("Source: {metadata$caption}
 #                       Note: Uganda does not report on Key Population disaggregates")) + 
 #   theme(axis.text.y = element_blank(),
 #         legend.position = "none",
 #         strip.text.y.left = element_text(angle=360))
 # 
 # si_save("Images/KP_disagg.png")
  
