# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  95s plots for WAR 2023 Draft Estimates
# REF ID:   111307ac 
# LICENSE:  MIT
# DATE:     2023-05-15
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
  
  ref_id <- "111307ac"
  
  data_folder <- "Data"
  
  goal <- 0.95

# IMPORT ------------------------------------------------------------------
  
 df <- data_folder %>% 
    return_latest("Pivot tables for 3-95s") %>% 
    read_excel()

# MUNGE CASCADE ------------------------------------------------------------

  #num PLHIV
  df_est <- df %>% 
    filter(
           population == "Adults") %>% 
    select(Source, country, population, PLHIV)
  
  
 df_viz <- df %>% 
    filter(population == "Adults") %>% 
    select(Source, country, population, PLHIV,`PLHIV 1st 95`, `Dependent 2nd 95`, `Dependent 3rd 95`) %>% 
    pivot_longer(cols = `PLHIV 1st 95`:`Dependent 3rd 95`, names_to = "indicator") %>% 
    mutate(indicator = case_when(indicator == "Dependent 2nd 95" ~ "On Treatment",
                          indicator == "PLHIV 1st 95" ~ "Known Status",
                          indicator == "Dependent 3rd 95" ~ "Virally Suppressed",
                          TRUE ~ indicator),
           PLHIV = ifelse(is.na(PLHIV), 0, PLHIV)) %>%
    mutate(base = "Relative Base")
   
  #BURUNDI AND MALAWI - issues because 1st 95 and 3 95 are the same
  df_viz <- df_viz %>%
    #rename(value = estimate) %>% 
    group_by(country) %>% 
    mutate(value = round(value, 2),
           grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
           grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                                #country == "Eswatini" ~ "Z_Achieved",
                                #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                                TRUE ~ grouping),
           gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
                           value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                           TRUE ~ 0),
           achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
           # gap = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", 0, gap),
           # achv = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", NA, achv),
           # grouping = ifelse(iso %in% c("BDI", "MWI"), "Known Status", grouping),
           dot_color = case_when(grouping == "Known Status" ~ "#009ee3",
                                 grouping == "On Treatment" ~ "#009ee3",
                                 grouping == "Virally Suppressed" ~ "#009ee3",
                                 grouping == "Achieved" ~ "#dd052a",
                                 # grouping == "Z_Achieved" ~ genoa,
                                 TRUE ~ "#a8e5ff")) %>% 
    fill(grouping, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(gap_bar = case_when(value < goal ~ value),
           country = reorder_within(country, gap, grouping, max, na.rm = TRUE))
  
  # CASCADE PLOT ----------------------------------------------------------------
  
  epi_ctrl_cnt <- df_viz %>% 
    filter(grouping == "Achieved") %>% 
    distinct(country) %>% 
    nrow()
  
  df_viz %>% 
    ggplot(aes(value, country, color = dot_color)) +
    geom_vline(xintercept = goal, linetype = "dashed") + 
    geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                   size = 2.5, na.rm = TRUE) +
    geom_point(size = 4, na.rm = TRUE) +
    scale_y_reordered(limits = rev) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_identity() +
    facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
    labs(x = NULL, y = NULL, color = NULL,
         title = glue("BASED ON 2023 DRAFT ESTIMATES, {epi_ctrl_cnt} WEST AFRICA COUNTRIES HAVE ACHIEVED THE 95-95-95 GOALS"),
         caption = glue("Source: Draft 2023 HIV Estimates
                      SI analytics: US Agency for International Development")) +
    si_style_xgrid() +
    theme(strip.text.y = element_blank(),
          panel.spacing = unit(.5, "lines"))
  
  
  si_save("Graphics/01_WAR_cascade_plot.svg")
  si_save("Images/01_WAR_cascade_plot.png")
  
  
  # SCORECARD PLOT -----------------------------------------------------------
  
  munge_cascade <- function(df, denom, pop = "Adults") {
    
    if (denom == "PLHIV") {
      indic <- c("PLHIV 1st 95",
                 "PLHIV 2nd 95",
                 "PLHIV 3rd 95")
    } else if (denom == "Relative") {
      indic <- c("PLHIV 1st 95",
                 "Dependent 2nd 95",
                 "Dependent 3rd 95")
    }
    
    df_tt_lim <- df %>% 
      filter(population == pop) %>% 
      select(any_of(indic), Source, country, population, PLHIV) %>% 
      pivot_longer(cols = indic, names_to = "indicator")

    df_tt_lim <- df_tt_lim %>%
      filter(!is.na(value)) %>%
      mutate(indicator = case_when(indicator == indic[1] ~ "Known\nStatus",
                                   indicator == indic[2] ~ "On\nART",
                                   indicator == indic[3] ~ "VLS"))


    if (denom == "PLHIV") {
      df_tt_lim <- df_tt_lim %>%
        mutate(set = recode(indicator, "Known\nStatus" = 1,
                            "On\nART" = 2,
                            "VLS" = 3),
               goal_rate = (goal^set),
               achv = value > goal_rate)
    } else if (denom == "Relative") {
      df_tt_lim <- df_tt_lim %>%
        mutate(value = round(value, 2),
               goal_rate = goal,
               achv = value > goal_rate)
    }
    
   df_tt_lim <- df_tt_lim %>%
      group_by(country) %>%
      mutate(gap = goal_rate - value,
             grouping = case_when(country %in% c("Guatemala", "Tajikistan") ~ "On ART",
                                  max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
                                  gap == max(gap, na.rm = TRUE) ~ str_replace(indicator, "\\n", " "),
                                  TRUE ~ NA_character_)
              ,
              gap = max(gap)
             ) %>%
      ungroup()

  df_viz2 <- df_tt_lim %>%
      arrange(country) %>%
      group_by(country) %>%
      fill(grouping, .direction = "downup") %>%
      ungroup() %>%
      filter(!is.na(grouping)) %>%
      mutate(fill_color = case_when(achv == TRUE & indicator == "Epi\nControl" ~ denim,
                                    achv == TRUE ~ scooter,
                                    TRUE ~ "white")) %>%
      mutate(country_grp = reorder_within(country, -gap, grouping, max, na.rm = TRUE)) %>%
      arrange(desc(country_grp))

    row_max <- 17

    df_viz2 <- df_viz2 %>%
      left_join(df_viz2 %>%
                  distinct(country, country_grp) %>%
                  mutate(column_order = ceiling(row_number()/row_max))) %>%
      mutate(value = round(value, 2)) %>%
      mutate(value = value*100)

    return(df_viz2)
    
  }
  
  
  plot_cascade <- function(df) {
    
   title_pop <-  df %>% 
      distinct(population) %>% 
      pull()
   
    df %>% 
    ggplot(aes(indicator, country_grp,
               fill = fill_color, shape = 22, color = scooter)) +
      geom_point(size = 6.5) +
      # geom_point(data = . %>%  filter(indicator == "Epi\nControl"),
      #            aes(shape = arrow, fill = fill_color_arrow, color = fill_color_arrow), size = 2, na.rm = TRUE) +
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
      # geom_text(size = 3, nudge_x = 1, na.rm = TRUE,
      #           aes(label = lab_epi), color = matterhorn, family = "Source Sans Pro") +
      facet_grid(grouping~., scales = "free_y", space = "free_y") +
      scale_fill_identity(aesthetics = c("fill", "color")) +
      scale_shape_identity() +
      scale_x_discrete(position = "top", expand = c(.05, .05)) +
      scale_y_reordered() +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL,
           title = glue("{title_pop}"),
           caption = glue("Source: Draft 2023 HIV Estimates| Ref ID: {ref_id}")) +
      si_style_nolines() +
      theme(axis.text.y = element_markdown(),
            strip.text.y = element_blank(),
            panel.spacing.y = unit(.5, "lines"))
  }

  
v_adults <-  munge_cascade(df, denom = "PLHIV", "Adults") %>%
    plot_cascade()
  
v_peds <-  munge_cascade(df, denom = "PLHIV", "Children") %>%
   plot_cascade()
 
 v_f <- munge_cascade(df, denom = "PLHIV", "Women") %>%
   plot_cascade()
  
v_m <-  munge_cascade(df, denom = "PLHIV", "Men") %>%
   plot_cascade()


(v_adults + v_peds) / (v_f + v_m)
si_save("Graphics/02_WAR_scorecard_all_pop.svg")
si_save("Images/02_WAR_scorecard_all_pop.png")


v_adults
si_save("Graphics/03_WAR_scorecard_adult.svg")
si_save("Images/03_WAR_scorecard_adult.png")


#REL BASE 

v_adults <-  munge_cascade(df, denom = "Relative", "Adults") %>%
  plot_cascade()

v_peds <-  munge_cascade(df, denom = "Relative", "Children") %>%
  plot_cascade()

v_f <- munge_cascade(df, denom = "Relative", "Women") %>%
  plot_cascade()

v_m <-  munge_cascade(df, denom = "Relative", "Men") %>%
  plot_cascade()



(v_adults + v_peds) / (v_f + v_m)
si_save("Graphics/02_WAR_scorecard_all_pop_rel.svg")
si_save("Images/02_WAR_scorecard_all_pop_rel.png")


v_adults
si_save("Graphics/03_WAR_scorecard_adult_rel.svg")
si_save("Images/03_WAR_scorecard_adult_rel.png")

 