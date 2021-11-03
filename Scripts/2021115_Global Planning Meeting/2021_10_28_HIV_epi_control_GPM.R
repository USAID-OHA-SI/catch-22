# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | USAID
# PURPOSE:  Global epidemic control curves 
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:
# NOTE:     Created based on request from FO "2 visuals for AHOP deck" -- 2021-07-16
# TODO: Rewrite to use the miindthegap pacakge to pull the data

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(sf)
  library(rnaturalearth)
  library(gisr)
  library(rmapshaper)
  library(gt)
  library(googlesheets4)

  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")
  
  plot_title <- "STEADY DECLINE IN THE NUMBER OF <span style= 'color:#2057a7;'> NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> AIDS-RELATED DEATHS </span> SINCE THE EARLY 2000s"
  

# IMPORT ------------------------------------------------------------------
  # Grab data from google drive
  
  epi_df <- range_read("15pUjqV1STbE2XPfqX2ZIMu8KKBsSXSNu_GOVthUuMBA", 
                       sheet = "epi_transition_metrics")
  
  epi_df_wide <- 
    epi_df %>% 
    pivot_wider(names_from = metric,
                names_glue = "{metric}_{.value}",
                values_from = c(value, lower, upper)
                ) %>% 
    rename(year = Year)

  
# MUNGE -------------------------------------------------------------------

  #source info
  msd_source <- "UNAIDS, https://aidsinfo.unaids.org/" 
  date_pulled <- "2021-07-19"
    
  plot_title <- "STEADY DECLINE IN THE NUMBER OF NEW HIV INFECTIONS AND AIDS-RELATED DEATHS SINCE THE EARLY 2000s"
  
    note <- str_wrap("HIV epidemic control is the point at which the number number of new HIV infections falls below the number of AIDS-related deaths", width = 40)
  
  
## VIZ ---------------------------------------------------------
  # continents

  epi_df_wide %>% 
      ggplot(aes(x = year)) +
      geom_ribbon(aes(ymax = new_hiv_infections_lower, ymin = new_hiv_infections_upper), 
                  fill = grey10k, alpha = 0.3) +
      geom_ribbon(aes(ymax = aids_related_deaths_lower, ymin = aids_related_deaths_upper), 
                  fill = grey10k, alpha = 0.3) +
      geom_line(aes(y = new_hiv_infections_value), color = denim) +
      geom_line(aes(y = aids_related_deaths_value), color = old_rose) +
      scale_y_continuous(labels = label_number_si()) +
      scale_x_continuous(breaks = seq(1990, 2020, 5)) +
      si_style_ygrid() +
      annotate(geom = "text", x = 2013, y = 2.5e6, label = note, hjust = 0,
               family = "Source Sans Pro Light") +
      annotate(geom = "text", x = 1994, y = 3.1e6, label = c("New HIV Infections"), hjust = 0,
               family = "Source Sans Pro Light", color = denim) +
      annotate(geom = "text", x = 1994, y = 1.5e6, label = c("AIDS-related Deaths"), hjust = 0,
             family = "Source Sans Pro Light", color = old_rose) +
      labs(x = NULL, y = NULL,
           title = plot_title,
           caption =  glue("{source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))+
      theme(plot.title = element_markdown())
    
  ggsave("Graphics/24_HIV_epi_control_trends.pdf", scale = 1.2, width = 10, height = 7, useDingbats = F)
  ggsave("Images/24_HIV_epi_control_trends.png", scale = 1.2, width = 10, height = 7)
  
  
  # Try the inverted method
  epi_df_wide %>% 
    mutate(epi_gap = new_hiv_infections_value - aids_related_deaths_value) %>% 
    ggplot(aes(x = year)) +
    geom_vline(xintercept = 2003, size = 0.5, color = trolley_grey_light) +
    geom_area(aes(y = new_hiv_infections_value), fill = denim, alpha = .25) +
    geom_area(aes(y = -aids_related_deaths_value), fill = old_rose, alpha = .25) +
    geom_line(aes(y = new_hiv_infections_value), color = denim) +
    geom_line(aes(y = -aids_related_deaths_value), color = old_rose) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = new_hiv_infections_value, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = -aids_related_deaths_value, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = new_hiv_infections_value, color = denim, 
                  label = paste0(round(new_hiv_infections_value/1000000, digits = 3), "M")),
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = -aids_related_deaths_value, color = old_rose, 
                  label = paste0(abs(aids_related_deaths_value/1000) %>% comma(1.0), "K")),
              hjust = -0.3,
              family = "Source Sans Pro Light") +  
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = label_number_si()) +
    scale_x_continuous(breaks = seq(1990, 2020, 5)) +
    geom_hline(yintercept = 0, color = grey80k) +
    si_style_ygrid() +
    annotate(geom = "text", x = 2013, y = 2.5e6, label = note, hjust = 0,
             family = "Source Sans Pro Light") +
    annotate(geom = "text", x = 1994, y = 3.1e6, label = c("New HIV Infections"), hjust = 0,
             family = "Source Sans Pro Light", color = denim) +
    annotate(geom = "text", x = 1994, y = -1.5e6, label = c("AIDS-related Deaths"), hjust = 0,
             family = "Source Sans Pro Light", color = old_rose) +
    labs(x = NULL, y = NULL,
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more",  
                        "Source: {msd_source}",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | "))+
    # labs(x = NULL, y = NULL,
    #      title = plot_title,
    #      caption =  glue("{source} [{date_pulled}]
    #                     SI analytics: {paste(authors, collapse = '/')}
    #                  US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  epi_df_wide %>% 
    filter(year %in% c(2003, 2020))
  
  
  ggsave("Graphics/24_HIV_epi_control_trends_ftstyle.pdf", scale = 1.5, width = 10, height = 4.25, useDingbats = F)
  
  