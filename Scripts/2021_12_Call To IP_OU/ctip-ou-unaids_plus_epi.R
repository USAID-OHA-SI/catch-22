# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  compare 90s and epi control
# LICENSE:  MIT
# DATE:     2021-12-07
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
  library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  #UNAID GOAL - 90 or 95
  goal <- 90

  #non regional country list
  lts <- pepfar_country_list %>% 
    filter(operatingunit == countryname) %>% 
    pull(countryname)
  
# IMPORT ------------------------------------------------------------------
  
  #HIV estimates
  df_est <- pull_unaids("HIV Estimates - Integer", TRUE)
  
  #Test and Treat percent estimates
  df_tt <- pull_unaids("Test & Treat - Percent", TRUE)
  


# MUNGE HIV ESTIMATES -----------------------------------------------------

  #limit HIV estimates data
  df_est <- df_est %>% 
    filter(indicator %in% c("PLHIV", "AIDS Related Deaths", "New HIV Infections"),
           age == "all",
           sex == "all",
           stat == "est") %>% 
    select(year, country, indicator, value)
  
  #reshape wide to align with T&T
  df_est <- df_est %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{indicator %>% str_extract_all('Deaths|Infections|PLHIV') %>% tolower}")
  
  #identify if epi control or not
  df_est <- df_est %>%
    arrange(country, year) %>% 
    group_by(country) %>% 
    mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>% 
    ungroup() %>% 
    mutate(infections_below_deaths = infections < deaths,
           ratio = infections / deaths,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE)
  
  #structure for alignment with T&T
  df_est_lim <- df_est %>% 
    filter(year == max(year),
           country != "Burma") %>% 
    mutate(indicator = "Epi\nControl",
           value = round(ratio, 1),
           achv = epi_control,
           direction_arrow = ifelse(declining_deaths == TRUE, "\u25B2", "\u25BC"),
           lab_epi = case_when(!is.na(ratio) ~ glue("{label_number_si()(infections)} | {label_number_si()(deaths)}")), 
           lab_epi2 = case_when(!is.na(ratio) ~ glue("{label_number_si()(infections)} | {label_number_si()(deaths)} {direction_arrow}"))) %>% 
    select(year, country, indicator, value, lab_epi, lab_epi2, infections_below_deaths, achv)
  
# MUNGE DATA --------------------------------------------------------------

  #limit Test and Treat data
  df_tt <- df_tt %>% 
    filter(year == max(year),
           indicator %in% c("PLHIV_ON_ART", "KNOWN_STATUS", "VLS"),
           age == "all",
           sex == "all",
           stat == "est") %>% 
    select(year, country, indicator, value)
  
 

# MERGE DATA --------------------------------------------------------------

  df_viz <- df_tt %>% 
    filter(country != "Burma") %>% 
    mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                              "PLHIV_ON_ART" = "On\nART"),
           achv = case_when(indicator == "Known\nStatus" ~ value >= goal,
                                   indicator == "On\nART" ~ value >= (goal/100)^2*100, 
                                   indicator == "VLS" ~ value >= (goal/100)^3*100)) %>% 
    group_by(country) %>% 
    mutate(grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved",
                                value == min(value, na.rm = TRUE) ~ indicator), 
           gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
                           value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                           TRUE ~ 0)) %>%
    ungroup()
  
  #merge
  df_viz <- df_viz %>% 
    bind_rows(df_est_lim) %>%
    left_join(df_est %>% select(year, country, plhiv)) %>% 
    arrange(country) %>% 
    group_by(country) %>% 
    fill(grouping, .direction = "downup") %>% 
    ungroup() 
  
  
  df_viz <- df_viz %>% 
    mutate(country = recode(country,
                            "Democratic Republic of the Congo" = "DRC",
                            "Trinidad and Tobago" = "T&T",
                            "Papua New Guinea" = "PNG",
                            "Dominican Republic" = "DR"),
           country_plhiv = glue("{country} <span style='color:{matterhorn}'>({label_number_si()(plhiv)})</span>"),
           indicator = fct_inorder(indicator),
           fill_color = case_when(achv == TRUE & indicator == "Epi\nControl" ~ denim,
                                  achv == TRUE ~ scooter,
                                  TRUE ~ "white"),
           border_color = ifelse(indicator == "Epi\nControl", denim, scooter),
           shp = ifelse(indicator == "Epi\nControl", 21, 22),
           arrow = ifelse(infections_below_deaths == TRUE, 25, 24)
           # country = reorder_within(country, gap, grouping, max, na.rm = TRUE)
           )
    

# VIZ ---------------------------------------------------------------------

  df_viz %>%
    filter(country %in% lts,
           !is.na(value)) %>% 
    ggplot(aes(indicator, fct_reorder(country, plhiv), 
               fill = fill_color, color = border_color, shape = shp)) +
    geom_point(size = 6.5) +
    # geom_point(data = . %>%  filter(indicator == "Epi\nControl"), 
    #            aes(shape = arrow), fill = suva_grey, size = 2, na.rm = TRUE) +
    geom_vline(xintercept = 3.5) +
    geom_text(data = . %>% filter(achv != TRUE & indicator != "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), family = "Source Sans Pro SemiBold", size = 3) +
    geom_text(data = . %>% filter(achv == TRUE & indicator != "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 3) +
    geom_text(data = . %>% filter(achv != TRUE & indicator == "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), family = "Source Sans Pro SemiBold", size = 2.5) +
    geom_text(data = . %>% filter(achv == TRUE & indicator == "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 2.5) +
    geom_text(size = 3,
              nudge_x = 1,
              aes(label = lab_epi), color = matterhorn, family = "Source Sans Pro") +
    # facet_wrap(~ grouping, scales = "free_y", nrow = 1) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_shape_identity() +
    scale_x_discrete(position = "top") +
    # scale_y_reordered() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown())
  
  si_save("Images/ctip-ou-unaids_plus_epi.png", width = 3.5)
    
  