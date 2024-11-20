# AUTHOR:   K. Srikanth | USAID
# PUROJECT: catch-22
# PURPOSE:  remake ICAP cohort visuals
# REF ID:   97c7b907 
# LICENSE:  MIT
# DATE:     2024-11-20
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

  # Grab metadata
   g_id <- "1VbvulypR2vVwJMV7kw7edx0iZknbe_fiRPR2FQOlBPI" 
  
  ref_id <- "97c7b907"

# IMPORT ------------------------------------------------------------------
  
 df <- read_sheet(g_id)
  
# MUNGE -------------------------------------------------------------------
  
  #race -----
  df %>% 
    filter(indicator == "race") %>% 
    mutate(group = str_replace(group, "/", "/\n    ")) %>% 
    ggplot(aes(x = fct_reorder(group, value), y = value, fill = hw_viking)) +
    geom_col(width = 0.8) +
    coord_flip() +
    si_style_nolines() +
    scale_fill_identity() +
    scale_y_continuous(labels = percent, limits = c(0, .5))+
    geom_text(aes(label = percent(value, 1)),
              hjust = -0.2, family = "Source Sans Pro", size = 4) +
    labs(x = NULL, y = NULL,
        # title = '45% of ',
         title = "ICAP Fellows by Race/Ethnicity" %>% toupper(),
        subtitle = "1997-2024",
         caption = "Source: USAID ICAP Data Oct 2024")
    
  si_save("Images/20241120_icap_race2.png")
  si_save("Graphics/20241120_icap_race.svg")
  
  
  #gender ----
 df_gender <- df %>% 
    filter(indicator == "gender") %>% 
   pivot_wider(names_from = "group")
  
  
  # FUNCTION FOR WAFFLE CHART
  make_waffle <- function(n, clr = grey20k){
    c(n) %>% 
      as.table() %>% 
      waffle::waffle(color = clr, flip = T,
                     reverse = T, size = 0.5) +
      theme(legend.position = "none")
  }
  
  #function to create tile breakdown
  gen_tile_fill <- function(prop){
    x <- round(prop, 2) * 100
    y <- 100 - (round(prop, 2) * 100)
    return(c(x, y))
  }
  
  waffle_prop <- df_gender %>% pull(Women)
  waffle_ban <- df_gender %>% pull(Women)
  
  gen_tile_fill(waffle_prop)
  
  x_pos = 3
  y_pos = 9.5
  fontfam = "Source Sans Pro"

  #"#588253"
  "#80b578"
  
  "#90AC8C"
  
  
 make_waffle(gen_tile_fill(waffle_prop), clr = c("#B98ABF", grey10k)) +si_style_nolines() +
    labs(title = "ICAP Fellows by Gender" %>% toupper(),
         subtitle = "1997-2024",
         caption = "Source: USAID ICAP Data Oct 2024",
         x = NULL, y = NULL) +
    annotate("text", x = x_pos, y = y_pos, label = percent(waffle_ban, accuracy = 1), color= "white", size =40/.pt, family = fontfam, fontface = 2) +
   theme(legend.position = "none",
         axis.text.x = element_blank(),
         axis.text.y = element_blank())
 
 si_save("Images/20241120_icap_gender_grey.png")
 si_save("Graphics/20241120_icap_race.svg")
 
 
# sector ------
 
 df %>% 
   filter(indicator == "sector") %>% 
   mutate(group = ifelse(group == "Independent consultants, media, entertainment, transition, retired, deceased", "Other", group)) %>% 
   mutate(group = str_replace(group, ",", ",\n    ")) %>% 
   ggplot(aes(x = fct_reorder(group, value), y = value, fill = hw_midnight_blue)) +
   geom_col(width = 0.8) +
   coord_flip() +
   si_style_nolines() +
   scale_fill_identity() +
   scale_y_continuous(labels = percent, limits = c(0, .7))+
   geom_text(aes(label = percent(value, 1)),
             hjust = -0.2, family = "Source Sans Pro", size = 4) +
   labs(x = NULL, y = NULL,
        # title = '45% of ',
        title = "ICAP Fellows by Sector" %>% toupper(),
        subtitle = "1997-2024",
        caption = "Source: USAID ICAP Data Oct 2024
        Note: Other includes independent consultants, media, entertainment, transition, retired, deceased")
 
 si_save("Images/20241120_icap_sector2.png")
 si_save("Graphics/20241120_icap_race.svg")
 
  
  
