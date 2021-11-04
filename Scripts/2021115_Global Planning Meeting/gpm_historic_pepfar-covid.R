# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COVID cases in  PEPFAR countries
# LICENSE:  MIT
# DATE:     2021-10-27
# UPDATED:  2021-11-04

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
  library(lubridate)
  library(COVIDutilities)
  library(zoo)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")
  
  #quarter starts
  qtrs <- seq.Date(as.Date("2019-10-01"), today(), by = "3 months")
  
  #current quarter
  curr_qtr <- source_info()
  curr_qtr_start <- source_info(return = "period") %>% convert_qtr_to_date() %>% as.Date()
  curr_qtr_end <- source_info(return = "period") %>% convert_qtr_to_date() %>% as.Date() %m+% months(3)

  dt_breaks <- seq.Date("2020-01-01", )

# IMPORT ------------------------------------------------------------------
  
  #COVID cases (JHU)
    df_covid <- pull_jhu_covid()   
  
  #limit to just PEPFAR countries
  df_covid <- df_covid %>% 
    filter(iso %in% pepfar_country_list$countryname_iso)
  
  #clean up Kazakhstan misentries
  df_covid <- df_covid %>% 
    mutate(daily_cases = ifelse(daily_cases <=0, NA, daily_cases),
           daily_cases = ifelse(countryname == "Kazakhstan" & date == "2021-07-23", NA, daily_cases))
  
  
  #aggregate to all of pepfar
  df_covid_agg <- df_covid %>% 
    mutate(countryname = "All PEPFAR") %>% 
    group_by(countryname, date) %>% 
    summarise(daily_cases = sum(daily_cases, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(rollingavg_7day = rollmean(daily_cases, 7, fill = NA, align = c("right")))


# VIZ ---------------------------------------------------------------------

  v1 <- df_covid_agg %>% 
    ggplot(aes(date, daily_cases)) +
    geom_col(fill = burnt_sienna, alpha = .8, na.rm = TRUE) +
    geom_hline(aes(yintercept = 0), size = 0.5, color = grey20k) +
    geom_line(aes(y = rollingavg_7day), color = si_palettes$burnt_siennas[7], #size = 1,
              na.rm = TRUE) +
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%b '%y",
                 breaks = qtrs) +
                 # breaks = c(as.Date("2020-03-01"), today())) +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         # title = "COVID HAS INTRODUCED PROGRAM AND REPORTING HURDLES FOR PEPFAR",
         subtitle = "Aggregated daily COVID Cases",
         # caption = glue("Source: Source: JHU COVID-19 feed [{today()}]",
         #                "USAID SI Analytics",
         #                "Global Planning Meeting 2021-11-15", .sep = " | ")
         ) +
    si_style_ygrid()
# 
#   si_save("Graphics/gpm_historic_pepfar-covid.svg",
#           height = 4.25)  

  df_covid_agg %>% 
    filter(date == max(date, na.rm = TRUE))

  df_covid_agg %>% 
    filter(daily_cases == max(daily_cases))
  
  
  
  df_reg_agg <- df_covid %>% 
    left_join(pepfar_country_xwalk %>% 
                select(countryname, region)) %>% 
    group_by(region, date) %>% 
    summarise(daily_cases = sum(daily_cases, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(rollingavg_7day = rollmean(daily_cases, 7, fill = NA, align = c("right")))
  
  df_reg_agg <- df_reg_agg %>% 
    group_by(date) %>% 
    mutate(share = daily_cases/sum(daily_cases, na.rm = TRUE)) %>% 
    ungroup()
  
 
  
  v2 <- df_reg_agg %>% 
    ggplot(aes(date,  share, fill = region)) +
    geom_col() +
    scale_y_continuous(label = percent, position = "right") +
    scale_x_date(date_labels = "%b '%y",
                breaks = qtrs) +
    scale_fill_si(palette = "scooter", alpha = .8, discrete = TRUE) +
    facet_grid(fct_reorder(region, daily_cases, max, na.rm = TRUE, .desc = TRUE) ~ .,
               switch = "y") +
    labs(x = NULL, y = NULL, 
         subtitle = "Regional share of daily COVID Cases") +
    si_style_ygrid() +
    theme(panel.spacing.y = unit(.5, "lines"),
          legend.position = "none")

  v1 + v2 + plot_layout(widths = c(2, 1)) +
    plot_annotation(title = "COVID HAS INTRODUCED PROGRAM AND REPORTING HURDLES FOR PEPFAR",
                    caption = glue("Source: Source: JHU COVID-19 feed [{today()}]",
                                   "USAID SI Analytics",
                                   "Global Planning Meeting 2021-11-15", .sep = " | "),
                    theme = si_style_ygrid())
  
  
  si_save("Graphics/gpm_historic_pepfar-covid-regional-share.svg",
          height = 4.25)  
  