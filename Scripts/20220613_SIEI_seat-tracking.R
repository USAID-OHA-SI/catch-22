# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  SIEI seat tracking
# LICENSE:  MIT
# DATE:     2022-06-13
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
  library(googlesheets4)
  library(janitor)
  library(lubridate)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  #authenication for GDrive
  load_secrets("email")

  #google sheet id for seat tracker
  seat_tracker <- as_sheets_id("1JM-EDxMlpvN8GklOf0bd3GArIEMJlIGxhaCejKuju_k")
  
  #identify max week of data (to filter out entry errors for future weeks)
  max_week <- floor_date(Sys.Date() -7, "week", getOption("lubridate.week.start", 1))

# IMPORT ------------------------------------------------------------------
  
  #read in weekly response data
  df_tracker <- read_sheet(seat_tracker,
                           .name_repair = make_clean_names)
  
  #read in full list of team members
  df_team <- read_sheet(seat_tracker, sheet = "Team Details",
                           .name_repair = make_clean_names)


# MUNGE -------------------------------------------------------------------

  #full list of team and roles, apply False to make it easier to work with cols
  df_team <- df_team %>% 
    mutate(across(c(chief, pa, remote), ~ ifelse(is.na(.), FALSE, .)))
  
  #expanded team by reporting week to have a full denom each week
  df_team_fullpds <- expand_grid(email = df_team$email, 
           reporting_week = unique(df_tracker$reporting_week)) %>% 
    left_join(df_team, by = "email")
  
  #join full team with tracker responses
  df_tracker_expnd <- left_join(df_team_fullpds, df_tracker,
                                by = c("reporting_week", "email" = "email_address"))
  
  #reshape responses long to tidy
  df_tracker_expnd <- df_tracker_expnd %>% 
    pivot_longer(starts_with("where"),
                 names_to = "weekday",
                 names_prefix = "where_did_you_work_each_day_",
                 names_transform = str_to_sentence,
                 values_to = "location") 
  
  #munge column data for plotting
  df_tracker_expnd <- df_tracker_expnd %>% 
    mutate(email = str_remove(email, "@usaid.gov"),
           lastname = str_extract(name, " .*"),
           shortname = case_when(chief == TRUE | pa == TRUE ~ glue("{word(name)}*"), 
                                 str_detect(name, "Jessica") ~ str_extract(name, "^.* [:alpha:]{1}"),
                                 remote == TRUE ~ glue("{word(name)}^"),
                                 TRUE ~ word(name)),
           branch = ifelse(chief == TRUE | pa == TRUE, "SIEI", branch),
           weekday_num = recode(weekday, 
                                "Monday" = 0,
                                "Tuesday" = 1,
                                "Wednesday" = 2,
                                "Thursday" = 3,
                                "Friday" = 4),
           weekday_abbr = ifelse(str_detect(weekday, "^T"),
                                 str_sub(weekday, end = 2),
                                 str_sub(weekday, end = 1)),
           reporting_week = mdy(glue("{reporting_week}, 2022")),
           date = reporting_week + weekday_num,
           location = recode(location, 
                             "In office, Scheduled" = "In office",
                             "In office, Unscheduled" = "In office",
                             "Telework/Remote/TDY" = "Telework/Remote",
                             "Leave/Holiday" = "Leave/Holiday/TDY",
                             "TDY" = "Leave/Holiday/TDY")) 
  
  #identify number of seats used each day
  df_seats <- df_tracker_expnd %>% 
    mutate(seats_filled = str_detect(location, "In office"),
           seats_filled = ifelse(is.na(seats_filled), FALSE, seats_filled)) %>%
    count(reporting_week, weekday_abbr, weekday_num, date, seats_filled, name = "seats") %>% 
    filter(seats_filled == TRUE) %>% 
    mutate(shortname = "SIEI SEATS TAKEN",
           shortname_days = glue("{shortname} ({sum(seats_filled)})"),
           location = "In office",
           branch = "AAA") %>% 
    select(-seats_filled)
  
  #bind division seat usage onto team data
  df_viz <- bind_rows(df_tracker_expnd, df_seats)
  
  #arrange names by time in office
  df_viz <- df_viz %>% 
    group_by(name) %>% 
    mutate(time_in_office = sum(location == "In office", na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(shortname_days = ifelse(branch == "AAA", shortname,
                                   glue("{shortname} ({comma(time_in_office, 1)})"))) %>% 
    arrange(time_in_office, desc(lastname)) %>% 
    mutate(shortname_days = fct_inorder(shortname_days))
    
  #remove future dates and identify branch's scheduled day in office
  df_viz <- df_viz %>% 
    filter(reporting_week <= max_week) %>% 
    mutate(branch_sch_day = case_when(chief == TRUE | pa == TRUE ~ NA,
                                      branch == "HI" & weekday == "Tuesday" ~ TRUE,
                                      branch == "SI" & weekday == "Wednesday" ~ TRUE,
                                      branch %in% c("EA", "Eval") & weekday == "Thursday" ~ TRUE
                                      ),
           border_color = ifelse(is.na(location),"#909090", "white"))
  

# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(fct_reorder(weekday_abbr, weekday_num, max), shortname_days)) +
    geom_tile(aes(fill = location), color = "white", alpha = .4) +
    geom_tile(data = . %>% filter(branch_sch_day == TRUE),
              aes(fill = location), color = "white") +
    geom_text(aes(label = seats), na.rm = TRUE,
              family = "Source Sans Pro", size = 7/.pt, color = matterhorn) + 
    facet_grid(branch~fct_reorder(format(reporting_week, "Week of %B %d"), reporting_week, max), scales = "free_y", space = "free", switch = "y") +
    scale_x_discrete(position = "top") +
    scale_color_identity(na.value = NA) +
    scale_fill_manual(values = c("In office" = scooter,
                                 "Telework/Remote" = "#808080",
                                 "Leave/Holiday/TDY" = moody_blue),
                      na.value = "white") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "How are desks being used by SIEI in the USAID Annex?" %>% toupper,
         subtitle = "Brighter color designates branch day in office",
         caption = glue("^ = remote worker | * = PA and Branch/Division Chiefs who have their own desks
                        Updated {Sys.Date()}")) +
    si_style_nolines() +
    theme(panel.spacing = unit(.4, "picas"),
          strip.placement = "outside",
          strip.text.y = element_blank(),
          axis.text = element_text(size = 8))
  
  #export
  si_save(glue("Images/SIEI_seating_{Sys.Date()}.png"),
               height = 10, width = 5.625)
  
  
  