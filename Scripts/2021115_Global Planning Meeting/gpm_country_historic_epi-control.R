# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth USAID
# PURPOSE:  Epi control trend graphs
# LICENSE:  MIT
# DATE:     2021-10-26
# UPDATED:  2021-11-04
# NOTE:     derived from agitprop/24b_HIV_epi_control_country.R

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
  library(gisr)
  library(googlesheets4)
  library(readxl)
  library(stringi)
  # remotes::install_github("https://github.com/USAID-OHA-SI/mindthegap.git", ref = "unaids-data")
  library(mindthegap)


# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  #source info & definition of epidemic control
  source <- "UNAIDS 2021 estimates" 
  date_pulled <- "July 2021"
  
  note <- str_wrap("HIV epidemic control is the point at which the number number of new HIV infections falls below the number of AIDS-related deaths", width = 40)
  
  # epi_control <- str_wrap("PEPFAR defines national HIV epidemic control as the point at which the total number of new HIV infections falls below the total number of deaths from all causes among HIV-infected individuals, with both new infections and deaths among HIV-infected individual slowing and declining.", width = 100)
  epi_control <- "Four PEPFAR countries have reached epidemic control, where new infections falls below deaths and deaths are declining. "
  
  plot_title <- "STEADY DECLINE IN THE NUMBER OF <span style= 'color:#2057a7;'> NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> AIDS-RELATED DEATHS </span> SINCE THE EARLY 2000s"
  
  #focal countries
  sel_cntry <- c("Uganda", "Kenya", "Namibia", "Eswatini")

# IMPORT ------------------------------------------------------------------

  df_epi <- munge_unaids("HIV Estimates")


# MUNGE -------------------------------------------------------------------

  df_epi <- df_epi$`Integer Indicators`
  
  
  df_epi_pepfar <- df_epi %>% 
    filter(stat == "est",
           age == "all",
           indicator %in% c("AIDS Related Deaths", "New HIV Infections")) %>%
    semi_join(pepfar_country_list, by = c("iso" = "countryname_iso")) %>% 
    select(year, country, indicator, value) %>%
    arrange(country, indicator, year)  

  df_epi_pepfar <- df_epi_pepfar %>% 
    mutate(indicator = word(indicator, -1) %>% tolower) %>%
    pivot_wider(names_from = "indicator") %>%
    group_by(country) %>% 
    mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>% 
    ungroup() %>% 
    mutate(infections_below_deaths = infections < deaths,
           ratio = infections / deaths,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) 
  
  
  df_epi_pepfar <- df_epi_pepfar %>% 
    pivot_longer(c(infections, deaths), names_to = "indicator") %>% 
    arrange(country, indicator, year) %>% 
    mutate(value_mod = ifelse(indicator == "deaths", -value, value),
           fill_color = ifelse(indicator == "deaths", old_rose, denim))
  
  epi_cntry <- df_epi_pepfar %>% 
    filter(year == max(year),
           indicator == "infections",
           # epi_control == TRUE) %>%
           country %in% sel_cntry) %>% 
    arrange(desc(value)) %>% 
    pull(country)
  
  
  df_viz_pepfar <- df_epi_pepfar %>% 
    mutate(country = "All PEPFPAR") %>% 
    group_by(country, year, indicator, fill_color) %>% 
    summarise(across(c(value, value_mod), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
           max_plot_pt = max(value),
           lab_pt = case_when(year == max(year) ~ value_mod))
  
  df_viz_cntry <- df_epi_pepfar %>% 
    filter(country %in% epi_cntry) %>% 
    mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
           max_plot_pt = max(value),
           lab_pt = case_when(year == max(year) ~ value_mod),
           country = factor(country, epi_cntry)) 
    
  

# VIZ ---------------------------------------------------------------------

  
  v_p <- df_viz_pepfar %>% 
    ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = max_plot_pt)) +
    geom_blank(aes(y = -max_plot_pt)) +
    geom_area(alpha = .25) +
    geom_line() +
    geom_point(aes(y = lab_pt), na.rm = TRUE,
               shape = 21, color = "white", size = 3) +
    geom_hline(yintercept = 0, color = grey80k) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    facet_wrap(~country) +
    scale_y_continuous(labels = label_number_si()) +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = T, clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_markdown())
  
  v_c <- df_viz_cntry %>% 
    ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = max_plot_pt)) +
    geom_blank(aes(y = -max_plot_pt)) +
    geom_area(alpha = .25) +
    geom_line() +
    geom_point(aes(y = lab_pt), na.rm = TRUE,
               shape = 21, color = "white", size = 3) +
    geom_hline(yintercept = 0, color = grey80k) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    facet_wrap(~country) +
    scale_y_continuous(labels = label_number_si()) +
    scale_x_continuous(breaks = seq(1990, 2025, 10)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = T, clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_markdown(),
          panel.spacing.x = unit(20, "pt"),
          panel.spacing.y = unit(0, "pt"))
  
  
  v_p + v_c +  
    plot_annotation(title = plot_title,
                    # subtitle = epi_control,
                    caption = glue("Source: {source} [{date_pulled}]",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | "),
                    theme = si_style_ygrid()) &
    theme(axis.text.y = element_markdown(),
          panel.spacing.x = unit(20, "pt"),
          panel.spacing.y = unit(0, "pt"),
          plot.title = element_markdown())
          # plot.subtitle = element_markdown())
          # plot.title = element_textbox_simple(margin = margin(5.5, 0, 0, 5.5)),
          # plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light",
          #                                        margin = margin(5.5, 5.5, 5.5, 5.5)))
  
  si_save("Graphics/gpm_historic_epi-control.svg",
          height = 4.25)
  
  
 

  
    
    
