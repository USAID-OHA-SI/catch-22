# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Trend in NEW HIV infections
# REF ID:   f197b743 
# LICENSE:  MIT
# DATE:     2022-06-09
# UPDATED:  2022-07-21

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
  library(mindthegap)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "f197b743"
  
# IMPORT ------------------------------------------------------------------
  

  df_un <- pull_unaids("HIV Estimates - Integer")


# MUNGE -------------------------------------------------------------------

  df_tbl <- df_un %>% 
    bind_rows(df_un %>% 
                filter(pepfar == "PEPFAR") %>% 
                mutate(country = "PEPFAR")) %>% 
    filter(country %in% c("Global","PEPFAR"),
           indicator == "New HIV Infections",
           stat == "est",
           sex == "all",
           age != "0-14",
           year %in% c(2010, 2020)) %>% 
    count(year, country, age, wt = value, name = "value") %>% 
    pivot_wider(names_from = year) %>% 
    mutate(delta = (`2020` - `2010` )/ `2010`) 

  df_trend <- df_un %>% 
    bind_rows(df_un %>% 
                filter(pepfar == "PEPFAR") %>% 
                mutate(country = "PEPFAR")) %>% 
    filter(country %in% c("Global","PEPFAR"),
           indicator == "New HIV Infections",
           # stat == "est",
           sex == "all",
           age == "15+") %>% 
    count(year, country, age, stat, wt = value, name = "value") %>% 
    pivot_wider(names_from = stat)

# TABLE OUTPUT ------------------------------------------------------------

  df_tbl%>% 
    gt(rowname_col = "age",
       groupname_col = "country") %>% 
    fmt_integer(c(`2010`, `2020`)) %>% 
    fmt_percent(delta, decimals = 0) %>% 
    tab_header(title = "New HIV Infections") %>% 
    tab_source_note(glue("{source_note}"))





# VIZ ---------------------------------------------------------------------

  df_trend %>% 
    ggplot(aes(year, est, group = country)) +
    geom_vline(xintercept = c(2004, 2020), linetype = "dashed") +
    geom_ribbon(data = . %>%  filter(year <= 2004),
                aes(ymin = low, ymax = high), alpha = .2,) +
    geom_ribbon(data = . %>%  filter(year >= 2004),
                aes(ymin = low, ymax = high), alpha = .2,
                fill = scooter_med) +
    geom_path(color = trolley_grey, size = 1.1) +
    geom_path(data = . %>%  filter(year >= 2004), color = scooter, size = 1.1) +
    geom_point(data = . %>% filter(year == max(year)), size = 4, 
               shape = 21, color = scooter,  fill = "white", stroke = 1.1) +
    geom_point(data = . %>% filter(year == min(year)), size = 3, 
               color = trolley_grey, stroke = 1.1) +
    geom_label(data = . %>% filter(year == 2004),
               aes(label = label_number_si(.1)(est)),
               family = "Source Sans Pro",
               vjust = -.8) +
    geom_label(data = . %>% filter(year == max(year)),
               aes(label = label_number_si(.1)(est)),
               family = "Source Sans Pro",
               vjust = 1.8) +
    scale_y_continuous(label = label_number_si()) +
    expand_limits(y = 0) +
    coord_cartesian(expand = T, clip = "off") +
    facet_wrap(~fct_rev(country)) +
    labs(x = NULL, y = NULL,
         title = "SIGNIFICANT DECLINE IN NEW INFECTIONS SINCE PEPFAR'S INCEPTION IN 2004",
         subtitle = "New HIV Infections 15+ Trends (estimates and error bounds)",
         caption = glue("{source_note} | Ref Id: {ref_id}")) +
    si_style()

  si_save("Images/new_infections.png")
  si_save("Graphics/new_infections.svg")
