# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  compare 90s and epi control
# LICENSE:  MIT
# DATE:     2021-12-07
# UPDATED:  2021-12-15

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
    filter(operatingunit == country) %>% 
    pull(country)
  
  #rows of countries per column in viz
  row_max <- 17
  
# IMPORT ------------------------------------------------------------------
  
  #HIV estimates
  df_est <- pull_unaids("HIV Estimates - Integer", TRUE)
  
  #Test and Treat percent estimates
  df_tt <- pull_unaids("Test & Treat - Percent", TRUE)
  

  df_deaths <- readxl::read_excel(file.path(si_path("path_downloads"), "Reshma request 7Jun2022.xlsx"),
                                  na = c("", "..."))
    
# MUNGE ALL DEATHS --------------------------------------------------------
  
  df_deaths_lim <- df_deaths %>% 
    rename("indicator" = ...1,
           "country" = ...3,
           "iso" = ISO3) %>% 
    filter(iso %in% pepfar_country_list$country_iso) %>%
    pivot_longer(-c(iso, indicator, country),
                 names_to = "year") %>% 
    mutate(indicator = "Non-AIDS Related Deaths to HIV Population",
           year = as.double(year)) %>%
    select(year, country, #indicator, 
           deaths_non_aids = value)


# MUNGE HIV ESTIMATES -----------------------------------------------------

  #limit HIV estimates data
  df_est_lim <- df_est %>% 
    filter(indicator %in% c("PLHIV", "AIDS Related Deaths", "New HIV Infections"),
           # age == "all",
           age == "15+",
           sex == "all",
           stat == "est") %>% 
    select(year, country, indicator, value)
  
  #reshape wide to align with T&T
  df_est_lim <- df_est_lim %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{indicator %>% str_extract_all('Deaths|Infections|PLHIV') %>% tolower}")
  
  #join with total deaths
  df_est_lim <- df_est_lim %>% 
    tidylog::left_join(df_deaths_lim) %>% 
    rowwise() %>% 
    mutate(deaths_all = sum(deaths, deaths_non_aids)) %>% 
    ungroup()
  
  #plhiv for plot
  df_plhiv <- df_est_lim %>%
    filter(year == max(year)) %>% 
    select(year, country, plhiv)
  
  #identify if epi control or not
  df_est_lim <- df_est_lim %>%
    arrange(country, year) %>% 
    group_by(country) %>% 
    mutate(declining_deaths = deaths_all - lag(deaths_all, order_by = year) <= 0) %>% 
    ungroup() %>% 
    mutate(infections_below_deaths = infections < deaths_all,
           ratio = infections / deaths_all,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE)
  
  #structure for alignment with T&T
  df_est_lim <- df_est_lim %>% 
    filter(year == max(year),
           !is.na(ratio)) %>% 
    mutate(indicator = "Epi\nControl",
           value = round(ratio, 1),
           achv = epi_control,
           direction_arrow = ifelse(declining_deaths == TRUE, "\u25B2", "\u25BC"),
           lab_epi = case_when(!is.na(ratio) ~ glue("{label_number_si()(infections)} | {label_number_si()(deaths)}")), 
           lab_epi2 = case_when(!is.na(ratio) ~ glue("{label_number_si()(infections)} | {label_number_si()(deaths)} {direction_arrow}"))) %>% 
    select(year, country, indicator, value, lab_epi, lab_epi2, declining_deaths, achv)
  
# MUNGE DATA --------------------------------------------------------------

  #limit Test and Treat data
  df_tt_lim <- df_tt %>% 
    filter(year == max(year),
           indicator %in% c("KNOWN_STATUS", "PLHIV_ON_ART", "VLS"),
           age == "all",
           sex == "all",
           stat == "est") %>% 
    select(year, country, indicator, value)
  
  df_tt_lim <- df_tt_lim %>% 
    filter(!is.na(value)) %>% 
    mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                              "PLHIV_ON_ART" = "On\nART"),
           set = recode(indicator, "Known\nStatus" = 1,
                        "On\nART" = 2,
                        "VLS" = 3),
           goal_rate = round((goal/100)^set*100),
           achv = value >= goal_rate) %>% 
    group_by(country) %>% 
    mutate(gap = goal_rate - value,
           grouping = case_when(country %in% c("Guatemala", "Tajikistan") ~ "On ART",
                                max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
                                gap == max(gap, na.rm = TRUE) ~ str_replace(indicator, "\\n", " "),
                                TRUE ~ NA_character_), 
           gap = max(gap)) %>%
    ungroup() 
  
# MERGE DATA --------------------------------------------------------------

  #merge
  df_viz <- df_tt_lim %>% 
    bind_rows(df_est_lim) %>%
    left_join(df_plhiv, by = c("year", "country")) %>% 
    arrange(country) %>% 
    group_by(country) %>% 
    fill(grouping, .direction = "downup") %>% 
    ungroup() %>% 
    filter(!is.na(grouping))
  
  
  df_viz <- df_viz %>% 
    mutate(country = recode(country,
                            "Democratic Republic of the Congo" = "DRC",
                            "Trinidad and Tobago" = "T&T",
                            "Papua New Guinea" = "PNG",
                            "Dominican Republic" = "DR"),
           country_plhiv = glue("{country} <span style='color:{matterhorn}'>({label_number_si()(plhiv)})</span>"),
           indicator = factor(indicator, c("Known\nStatus", "On\nART", "VLS", "Epi\nControl")),
           fill_color = case_when(achv == TRUE & indicator == "Epi\nControl" ~ denim,
                                  achv == TRUE ~ scooter,
                                  TRUE ~ "white"),
           border_color = ifelse(indicator == "Epi\nControl", denim, scooter),
           shp = ifelse(indicator == "Epi\nControl", 21, 22),
           arrow = ifelse(declining_deaths == TRUE, 25, 24),
           fill_color_arrow = ifelse(declining_deaths == TRUE, suva_grey, burnt_sienna))
  
  df_viz <- df_viz %>% 
    group_by(country) %>% 
    mutate(gap = max(gap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(country_grp = reorder_within(country, -gap, grouping, max, na.rm = TRUE)) %>% 
    arrange(desc(country_grp))

  df_viz <- df_viz %>% 
    left_join(df_viz %>% 
                distinct(country, country_grp) %>% 
                mutate(column_order = ceiling(row_number()/row_max)))

  
# VIZ ---------------------------------------------------------------------

  df_viz %>%
    ggplot(aes(indicator, country_grp,
               fill = fill_color, color = border_color, shape = shp)) +
    geom_point(size = 6.5) +
    geom_point(data = . %>%  filter(indicator == "Epi\nControl"),
               aes(shape = arrow, fill = fill_color_arrow, color = fill_color_arrow), size = 2, na.rm = TRUE) +
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
    geom_text(size = 3, nudge_x = 1, na.rm = TRUE,
              aes(label = lab_epi), color = matterhorn, family = "Source Sans Pro") +
    facet_grid(grouping~., scales = "free_y", space = "free_y") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_shape_identity() +
    scale_x_discrete(position = "top", expand = c(.05, .05)) +
    scale_y_reordered() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          strip.text.y = element_blank(),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/ctip-ou-unaids_plus_epi.svg", 
          width = 3.2, height = 12)
  