# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Trend in NEW HIV infections
# REF ID:   f197b743 
# LICENSE:  MIT
# DATE:     2022-06-09
# UPDATED:  2022-08-29

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
  library(gt)
  library(mindthegap)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "f197b743"
  
# IMPORT ------------------------------------------------------------------
  

  df_un <- pull_unaids("HIV Estimates", pepfar_only = FALSE)


# MUNGE -------------------------------------------------------------------

  df_tbl <- df_un %>% 
    bind_rows(df_un %>% 
                filter(pepfar == TRUE) %>% 
                mutate(country = "PEPFAR")) %>% 
    filter(country %in% c("Global","PEPFAR"),
           indicator == "Number New HIV Infections",
           sex == "All",
           age != "0-14",
           year %in% c(2010, 2021)) %>% 
    count(year, country, age, wt = estimate, name = "value") %>% 
    pivot_wider(names_from = year) %>% 
    mutate(delta = (`2021` - `2010` )/ `2010`) 

  df_trend <- df_un %>% 
    bind_rows(df_un %>% 
                filter(pepfar == TRUE) %>% 
                mutate(country = "PEPFAR")) %>% 
    filter(country %in% c("Global","PEPFAR"),
           indicator == "Number New HIV Infections",
           sex == "All",
           age == "15+") %>% 
    group_by(year, country, age) %>% 
    summarise(across(c(estimate, lower_bound, upper_bound), sum, na.rm = TRUE), 
              .groups = "drop")

# TABLE OUTPUT ------------------------------------------------------------

  df_tbl%>% 
    gt(rowname_col = "age",
       groupname_col = "country") %>% 
    fmt_integer(c(`2010`, `2021`)) %>% 
    fmt_percent(delta, decimals = 0) %>% 
    tab_header(title = "New HIV Infections") %>% 
    tab_source_note(glue("{source_note}"))


# VIZ ---------------------------------------------------------------------

  df_trend %>% 
    ggplot(aes(year, estimate, group = country)) +
    geom_vline(xintercept = c(2004, 2021), linetype = "dashed") +
    geom_ribbon(data = . %>%  filter(year <= 2004),
                aes(ymin = lower_bound, ymax = upper_bound), alpha = .2,) +
    geom_ribbon(data = . %>%  filter(year >= 2004),
                aes(ymin = lower_bound, ymax = upper_bound), alpha = .2,
                fill = scooter_med) +
    geom_path(color = trolley_grey, size = 1.1) +
    geom_path(data = . %>%  filter(year >= 2004), color = scooter, size = 1.1) +
    geom_point(data = . %>% filter(year == max(year)), size = 4, 
               shape = 21, color = scooter,  fill = "white", stroke = 1.1) +
    geom_point(data = . %>% filter(year == min(year)), size = 3, 
               color = trolley_grey, stroke = 1.1) +
    geom_label(data = . %>% filter(year == 2004),
               aes(label = label_number(.1, scale_cut = cut_short_scale())(estimate)),
               family = "Source Sans Pro",
               vjust = -.8) +
    geom_label(data = . %>% filter(year == max(year)),
               aes(label = label_number(.1, scale_cut = cut_short_scale())(estimate)),
               family = "Source Sans Pro",
               vjust = 1.8) +
    scale_y_continuous(label = label_number(.1, scale_cut = cut_short_scale())) +
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
