# PROJECT:  catch-22
# AUTHOR:   A.Chafetz & K.Srikanth | USAID
# PURPOSE:  compare 90s and epi control
# REF ID:   e505d102 
# LICENSE:  MIT
# DATE:     2021-12-07
# UPDATED:  2022-09-19

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
  library(googledrive)
  library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "e505d102"
  
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
  df_est <- pull_unaids("HIV Estimates", TRUE)
  
  #Test and Treat percent estimates
  df_tt <- pull_unaids("HIV Test & Treat", TRUE)
  
  #pull Total PLHIV death data
  g_id <- "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"
  
  df_deaths <- range_speedread(ss = g_id, sheet = "UNAIDS_epi_control") %>% 
    filter(indicator == "Number Total Deaths HIV Pop")
  

# MUNGE HIV ESTIMATES -----------------------------------------------------

  #limit HIV estimates data
  df_est_lim <- df_est %>% 
    filter(indicator %in% c("Number PLHIV", "Number AIDS Related Deaths", "Number New HIV Infections"),
           age == "All",
           sex == "All",
         #  stat == "est"
           ) %>% 
    select(year, country, indicator, estimate)
  
  #reshape wide to align with T&T
  df_est_lim <- df_est_lim %>% 
    pivot_wider(names_from = indicator,
                values_from = "estimate",
                names_glue = "{indicator %>% str_extract_all('Deaths|Infections|PLHIV') %>% tolower}")
  
  #plhiv for plot
  df_plhiv <- df_est_lim %>%
    filter(year == max(year)) %>% 
    select(year, country, plhiv)
  
  #grab total deaths
  total_deaths <- df_deaths %>% 
    #select(-c(iso2, geo_level)) %>% 
    filter(age == "all",
           sex == "all") %>% 
    select(c(country, year, indicator, estimate)) %>% 
    spread(indicator, estimate) %>% 
    janitor::clean_names() %>% 
    rename(total_deaths = number_total_deaths_hiv_pop)
  
  #identify if epi control or not
  df_est_lim <- df_est_lim %>%
    arrange(country, year) %>% 
    left_join(total_deaths, by = c("year", "country")) %>% 
    group_by(country) %>% 
    mutate(declining_deaths = total_deaths - lag(total_deaths, order_by = year) <= 0) %>% 
    ungroup() %>% 
    mutate(infections_below_deaths = infections < total_deaths,
           ratio = infections / total_deaths,
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
           lab_epi = case_when(!is.na(ratio) ~ glue("{label_number(accuracy = 1, scale_cut = cut_short_scale())(infections)} | {label_number(accuracy = 1, scale_cut = cut_short_scale())(total_deaths)}")), 
           lab_epi2 = case_when(!is.na(ratio) ~ glue("{label_number(accuracy = 1, scale_cut = cut_short_scale())(infections)} | {label_number(accuracy = 1, scale_cut = cut_short_scale())(total_deaths)} {direction_arrow}"))) %>% 
    select(year, country, indicator, value, lab_epi, lab_epi2, declining_deaths, achv)
  
# MUNGE DATA --------------------------------------------------------------

  munge_cascade <- function(df, denom) {
    
    if (denom == "PLHIV") {
      indic <- c("Percent Known Status of PLHIV",
                 "Percent on ART of PLHIV",
                 "Percent VLS of PLHIV")
    } else if (denom == "Relative") {
      indic <- c("Percent Known Status of PLHIV",
                 "Percent on ART with Known Status",
                 "Percent VLS on ART")
    }
    
    df_tt_lim <- df %>% 
      filter(year == max(year),
             indicator %in% indic,
             age == "All",
             sex == "All",
             # stat == "est"
      ) %>% 
      select(year, country, indicator, estimate) %>% 
      rename(value = estimate)
    
    df_tt_lim <- df_tt_lim %>% 
      filter(!is.na(value)) %>% 
      mutate(indicator = case_when(indicator == indic[1] ~ "Known\nStatus",
                                   indicator == indic[2] ~ "On\nART",
                                   indicator == indic[3] ~ "VLS"),
             set = recode(indicator, "Known\nStatus" = 1,
                          "On\nART" = 2,
                          "VLS" = 3),
             goal_rate = round((goal/100)^set*100),
             achv = value > goal_rate) %>% 
      group_by(country) %>% 
      mutate(gap = goal_rate - value,
             grouping = case_when(country %in% c("Guatemala", "Tajikistan") ~ "On ART",
                                  max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
                                  gap == max(gap, na.rm = TRUE) ~ str_replace(indicator, "\\n", " "),
                                  TRUE ~ NA_character_), 
             gap = max(gap)) %>%
      ungroup() 
    
    return(df_tt_lim)
    
  }
  
# MERGE DATA --------------------------------------------------------------

  #merge
  df_viz <- 
    munge_cascade(df_tt, "PLHIV") %>% #decide what base
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
    labs(x = NULL, y = NULL,
         caption = glue("Source: {source_note} | Ref ID: {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          strip.text.y = element_blank(),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/ctip-ou-unaids_plus_epi-2022.svg", 
          width = 3.2, height = 12)
  