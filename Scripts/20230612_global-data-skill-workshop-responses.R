# PROJECT:  
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# REF ID:   a03385c3 
# LICENSE:  MIT
# DATE:     2023-06-12
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googlesheets4)
  library(janitor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "a03385c3" #id for adorning to plots, making it easier to find on GH
  
  gs_id <- as_sheets_id("1E_kIXc0ITI9H6Tdc4w4uX59JKlXewooBdm5RDfFgHgw")

# IMPORT ------------------------------------------------------------------
  
  df_srvy <- read_sheet(gs_id,
                        .name_repair = make_clean_names)

# MUNGE -------------------------------------------------------------------

  df_srvy <- df_srvy %>% 
    select(email_address,
           matches("comfortable|software"),
           -contains("x7"))

  #rename columns
  colnames(df_srvy) <- c("email",
                         "munge_comfort",
                         "munge_software",
                         "aggregate_comfort",
                         "aggregate_software",
                         "viz_comfort",
                         "viz_software",
                         "track")   
  #pivot
  df_srvy <- df_srvy %>% 
    pivot_longer(-c(email, track),
                 names_to = c("type", ".value"),
                 names_sep = "_")
  
  df_srvy <- df_srvy %>% 
    mutate(type = factor(type, c("munge", "aggregate", "viz")),
           track = factor(track, c("Excel", "Tableau", "R"))) %>% 
    arrange(track, email, type) %>% 
    mutate(software_highest = case_when(str_detect(software, "R") ~ "R",
                                        str_detect(software, "Tableau") ~ "Tableau",
                                        str_detect(software, "(STATA|Stata|SAS|MS Access)") ~ "Other",
                                        str_detect(software, "Excel") ~ "Excel"
                                        )) %>% 
    mutate(is_comfortable = str_detect(comfort,"(Agree|Strongly agree)")) 
  

  
  df_srvy <- df_srvy %>% 
    mutate(fill_color = case_when(is_comfortable & software_highest == "R" ~ burnt_sienna_light,
                                  is_comfortable & software_highest == "Tableau" ~ scooter_light,
                                  is_comfortable & software_highest == "Excel" ~ genoa_light,
                                  is_comfortable & software_highest == "Other" ~ trolley_grey_light,
                                  )) %>% 
    group_by(track, email) %>% 
    mutate(n = cur_group_id()) %>% 
    ungroup()


# Viz ---------------------------------------------------------------------

  df_srvy %>% 
    ggplot(aes(type, fct_rev(email), color = fill_color)) + 
    geom_point(size = 4) +
    facet_grid(track ~ ., scales = "free_y") +
    scale_color_identity() +
    si_style_nolines()
  