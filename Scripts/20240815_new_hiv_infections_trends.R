# PROJECT: Epi Control Progress
# PURPOSE: Munge and Analysis of UNAIDS Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  eb55cc65
# LICENSE: MIT
# DATE:   2024-08-15
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(systemfonts)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(googlesheets4)
  library(mindthegap)

    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "eb55cc65"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    #Recent Infections
    df_epi <- 
      pull_unaids(data_type = "HIV Estimates", pepfar_only = FALSE)
    
    #PEPFAR only
    df_pepfar <- 
      pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)

# MUNGE ============================================================================
  
    #Requirements:
    #Age/Sex: "All", country: "Global"
    #indicators: "Number of New HIV Infections"
    #country: Global/ESA vs 4 OU's mentioned 
    
    epi_glob <- df_epi %>% 
      filter(age == "All", sex == "All",
             country == "Global",
             indicator %in% c("Number New HIV Infections")) %>% 
      #select(-c(estimate_flag)) %>% 
      select(year, country, indicator, estimate) %>% 
      spread(indicator, estimate) %>% 
      janitor::clean_names() 
    
    epi_region <- df_epi %>% 
      filter(age == "All", sex == "All",
             country %in% c("Eastern and southern Africa", "Global"),
             indicator %in% c("Number New HIV Infections")) %>% 
      #select(-c(estimate_flag)) %>% 
      select(year, country, indicator, estimate) %>% 
      spread(indicator, estimate) %>% 
      janitor::clean_names() 
    
    epi_limit <- df_epi %>% 
      filter(age == "All", sex == "All",
             country %in% c("Kenya", "Malawi", "Nepal", "Zimbabwe"),
             indicator %in% c("Number New HIV Infections")) %>% 
      #select(-c(estimate_flag)) %>% 
      select(year, country, indicator, estimate) %>% 
      spread(indicator, estimate) %>% 
      janitor::clean_names() 
    
    epi_multi <- df_epi %>% 
      filter(age == "All", sex == "All",
             country != region, 
             indicator %in% c("Number New HIV Infections"),
             !is.na(estimate)) %>% 
      #select(-c(estimate_flag)) %>% 
      select(year, region, country, indicator, estimate) %>% 
      spread(indicator, estimate) %>% 
      janitor::clean_names() 
  
# VIZ ============================================================================

  #Recent Infections - time series graphs 

# Global ------------------------------------------------------------------

    
      #1) Btwn 2010 - 2023: Global (39% drop) vs ESA (59%) 
    
  
    #reshape to calculate percentage change 
    epi_calc <- epi_glob %>%
      filter(year %in% c(2010:2023)) %>% 
      pivot_wider(names_from = year,
                  values_from = number_new_hiv_infections,
                  names_prefix = "year_") %>%
      mutate(
        perc_change = (year_2023 - year_2010)/(year_2010)) 
    
    epi_calc_2 <- epi_region %>% 
      filter(year %in% c(2010:2023)) %>% 
      pivot_wider(names_from = year,
                  values_from = number_new_hiv_infections,
                  names_prefix = "year_") %>%
      mutate(
        perc_change = (year_2023 - year_2010)/(year_2010)) 
    
    #get reference line value for 2010
    #epi_ref <- epi_glob %>% 
     # filter(year == 2010) %>% 
      #pull(number_new_hiv_infections)
    
    #prep data 
     epi_plot <- epi_calc %>% 
       mutate(base_value = year_2010) %>% 
       pivot_longer(cols = starts_with("year_"),
                    names_to = "year",
                    values_to = "number_new_hiv_infections") %>% 
       mutate(year = as.numeric(sub("year_","", year))) %>% 
       #filter(year == 2023) %>% 
       mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
     
     epi_plot_2 <- epi_calc_2 %>% 
       mutate(base_value = year_2010) %>% 
       pivot_longer(cols = starts_with("year_"),
                    names_to = "year",
                    values_to = "number_new_hiv_infections") %>% 
       mutate(year = as.numeric(sub("year_","", year))) %>% 
       #filter(year == 2023) %>% 
       mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
      
      
    #plot data
     ggplot(epi_plot, 
            aes(x = year, y = perc_change, group = country)) + 
      geom_line(color = glitr::denim, size = 1) + 
      geom_point(data = filter(epi_plot, year == 2023),
     color = denim, size = 3) + 
      geom_hline(yintercept = 0, color = grey80k, size = 1) + 
       geom_area(fill = denim_light, alpha = 0.3) + 
       #geom_text(data = filter(epi_plot, year == 2023),
        #         aes(label = percent(perc_change, accuracy = 0.1)), size = 11/.pt,
         #        family = "Source Sans Pro Semibold", vjust = -3.25) + 
       si_style_ygrid()+ 
       scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
       scale_y_continuous(labels = scales::percent, limits = c(-.4,.1)) +
       theme(plot.title = element_markdown()) + 
     labs(x = NULL, y = NULL ,
          title = glue("GLOBALLY <span style= 'color:#2057a7;'>NEW HIV INFECTIONS</span> HAVE FALLEN BY
                       <span style= 'color:#2057a7;'>39%</span> SINCE 2010"),
          caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)")) 
    
      
     si_save("Images/FY24 GH Portfolio Review/2023_global_new_infections_drop.png")
     
     #faceted version
     epi_plot_2 <- epi_plot_2 %>% 
       mutate(country = fct_relevel(country, "Global", "Eastern and southern Africa")) 
     
     ggplot(epi_plot_2,
            aes(x = year, y = perc_change, group = country)) + 
       geom_line(color = glitr::denim, size = 1) + 
       geom_point(data = filter(epi_plot_2, year == 2023),
                  color = denim, size = 3) + 
       geom_hline(yintercept = 0, color = grey80k, size = 1) + 
       geom_area(fill = denim_light, alpha = 0.3) + 
       #geom_text(data = filter(epi_plot, year == 2023),
       #         aes(label = percent(perc_change, accuracy = 0.1)), size = 11/.pt,
       #        family = "Source Sans Pro Semibold", vjust = -3.25) + 
       facet_wrap(~country, #scales = "free_y", 
                  labeller = as_labeller(c(
                    "Global" = "Global",
                    "Eastern and southern Africa" = "Eastern and Southern Africa"
       ))) + 
       si_style_ygrid()+ 
       scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
       scale_y_continuous(labels = scales::percent#, limits = c(-.4,.1)
                          ) +
       #coord_cartesian(ylim = c(-.6,0))+
       theme(plot.title = element_markdown()) + 
       labs(x = NULL, y = NULL ,
            title = glue("GLOBALLY <span style= 'color:#2057a7;'>NEW HIV INFECTIONS</span> HAVE FALLEN BY
                       <span style= 'color:#2057a7;'>39%</span> SINCE 2010, AND BY <span style= 'color:#2057a7;'>59%</span> IN ESA ONLY"),
            #subtitle = "In 2010, there were 2.1 M new hiv infections globally and 1.1 M in Eastern and Southern Africa alone",
            caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)"))
     
     si_save("Images/FY24 GH Portfolio Review/2023_global_new_infections_facet.png")
      
      

# 4 OUs  ------------------------------------------------------------------
    #2) 4 OUs: 2022 - 2023 (expect to see 75% drop)
      
     #reshape to calculate percent change
     epi_calc <- epi_limit %>%
       filter(year %in% c(2010:2023)) %>% 
       pivot_wider(names_from = year,
                   values_from = number_new_hiv_infections,
                   names_prefix = "year_") %>%
       mutate(
         perc_change = (year_2023 - year_2010)/(year_2010)) 
     
     #prep data 
     epi_plot <- epi_calc %>% 
       mutate(base_value = year_2010) %>% 
       pivot_longer(cols = starts_with("year_"),
                    names_to = "year",
                    values_to = "number_new_hiv_infections") %>% 
       mutate(year = as.numeric(sub("year_","", year))) %>% 
       #filter(year == 2023) %>% 
       mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
     
     
     
     ggplot(epi_plot,
            aes(x = year, y = perc_change, group = country)) + 
       geom_line(color = glitr::denim, size = 1) + 
       geom_point(data = filter(epi_plot, year == 2023),
                  color = denim, size = 3) + 
       geom_hline(yintercept = 0, color = grey80k, size = 1) + 
       geom_area(fill = denim_light, alpha = 0.3) + 
       #geom_text(data = filter(epi_plot, year == 2023),
       #         aes(label = percent(perc_change, accuracy = 0.1)), size = 11/.pt,
       #        family = "Source Sans Pro Semibold", vjust = -3.25) + 
       facet_wrap(~country) + 
       si_style_ygrid()+ 
       scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
       scale_y_continuous(labels = scales::percent#, limits = c(-.8,.1)
       ) +
       theme(plot.title = element_markdown()) + 
       labs(x = NULL, y = NULL ,
            title = glue("SINCE 2010, FOUR COUNTRIES REDUCED THEIR ANNUAL <span style= 'color:#2057a7;'>NEW HIV INFECTIONS</span> BY MORE THAN <span style= 'color:#2057a7;'>75%</span>"),
            #subtitle = "In 2010, there were 2.1 M new hiv infections globally and 1.1 M in Eastern and Southern Africa alone",
            caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)"))
     
 
     si_save("Images/FY24 GH Portfolio Review/2023_global_new_infections_4OUs.png")
     
     

# 18 OUs ------------------------------------------------------------------

     #3) 18 OUS reduced new hiv infections by > 60% since 2010
     
     #reshape to calculate percent change
     epi_flag <- epi_multi %>%
       filter(year %in% c(2010:2023)) %>% 
       pivot_wider(names_from = year,
                   values_from = number_new_hiv_infections,
                   names_prefix = "year_") %>%
       mutate(
         perc_change = (year_2023 - year_2010)/(year_2010),
         decrease_flag = perc_change >= -0.70 & perc_change <= -.59) %>% 
       arrange((perc_change)) 
     
     #prep data 
     epi_prep <- epi_flag %>% 
       filter(decrease_flag == TRUE) %>% 
       mutate(base_value = year_2010) %>% 
       pivot_longer(cols = starts_with("year_"),
                    names_to = "year",
                    values_to = "number_new_hiv_infections") %>% 
       mutate(year = as.numeric(sub("year_","", year))) %>% 
       #filter(year == 2023) %>% 
       mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value))
      
      
     #plot + facet: 18 OUs total 
      #PEPFAR Only - 8 OUs (Burkina Faso, Burundi, Cote d'Ivoire, Liberia, Togo, Uganda, Vietnam, Zambia)
     epi_prep <- epi_prep %>% 
       filter(country %in% c("Burkina Faso", "Burundi", "Cote d'Ivoire", "Liberia", "Togo", "Uganda", "Vietnam", "Zambia"))
       
     ggplot(epi_prep,
            aes(x = year, y = perc_change, group = country)) + 
       geom_line(color = glitr::denim, size = 1) + 
       geom_point(data = filter(epi_prep, year == 2023),
                  color = denim, size = 3) + 
       geom_hline(yintercept = 0, color = grey80k, size = 1) + 
       geom_area(fill = denim_light, alpha = 0.3) + 
       #geom_text(data = filter(epi_plot, year == 2023),
       #         aes(label = percent(perc_change, accuracy = 0.1)), size = 11/.pt,
       #        family = "Source Sans Pro Semibold", vjust = -3.25) + 
       facet_wrap(~country, scales = "free_y"
                  ) + 
       si_style_ygrid()+ 
       scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
       scale_y_continuous(labels = scales::percent#, limits = c(-.6,.1)
       ) +
       theme(plot.title = element_markdown()) + 
       labs(x = NULL, y = NULL ,
            title = glue("8 PEPFAR COUNTRIES REDUCED THEIR ANNUAL <span style= 'color:#2057a7;'>NEW HIV INFECTIONS</span> BY MORE THAN <span style= 'color:#2057a7;'>60%</span>"),
            #subtitle = "This include 8 PEPFAR countries: Burkina Faso, Burundi,  Cote d'Ivoire, Liberia, Togo, Uganda, Vietnam, and Zambia",
            caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)"))
     

     si_save("Images/FY24 GH Portfolio Review/2023_global_new_infections_18OUs.png")
     
     si_save("Images/FY24 GH Portfolio Review/2023_global_new_infections_pepfar_8OUs.png")
     

# PEPFAR OUs rising -------------------------------------------------------

     #4) How many PEPFAR OUs experienced an increase in new infections? 
        #11 OUs: Brazil, Guatemala, Colombia, Kyrgyzstan, Panama, Laos, Nicaragua, Senegal, Peru, PNG, Philippines
     
     #reshape to calculate percent change
     epi_flag <- epi_multi %>%
       filter(year %in% c(2010:2023)) %>% 
       pivot_wider(names_from = year,
                   values_from = number_new_hiv_infections,
                   names_prefix = "year_") %>%
       mutate(
         perc_change = (year_2023 - year_2010)/(year_2010),
         increase_flag = perc_change >= 0.01) %>% 
       arrange((perc_change)) 
     
     #prep data 
     epi_prep <- epi_flag %>% 
       filter(increase_flag == TRUE) %>% 
       mutate(base_value = year_2010) %>% 
       pivot_longer(cols = starts_with("year_"),
                    names_to = "year",
                    values_to = "number_new_hiv_infections") %>% 
       mutate(year = as.numeric(sub("year_","", year))) %>% 
       #filter(year == 2023) %>% 
       mutate(perc_change = (number_new_hiv_infections - base_value)/(base_value)) #%>% distinct(country)
     
     
     #plot rising infections 
     ggplot(epi_prep,
            aes(x = year, y = perc_change, group = country)) + 
       geom_line(color = glitr::denim, size = 1) + 
       geom_point(data = filter(epi_prep, year == 2023),
                  color = denim, size = 3) + 
       geom_hline(yintercept = 0, color = grey80k, size = 1) + 
       geom_area(fill = denim_light, alpha = 0.3) + 
       #geom_text(data = filter(epi_plot, year == 2023),
       #         aes(label = percent(perc_change, accuracy = 0.1)), size = 11/.pt,
       #        family = "Source Sans Pro Semibold", vjust = -3.25) + 
       facet_wrap(~country, scales = "free_y"
       ) + 
       si_style_ygrid()+ 
       scale_x_continuous(breaks= seq(2010, 2025, 5),limits = c(2010, 2025)) + 
       scale_y_continuous(labels = scales::percent#, limits = c(-.6,.1)
       ) +
       theme(plot.title = element_markdown()) + 
       labs(x = NULL, y = NULL ,
            title = glue("11 PEPFAR COUNTRIES EXPERIENCED AN INCREASE IN <span style= 'color:#2057a7;'>NEW HIV INFECTIONS</span> SINCE 2010"),
            #subtitle = "This include 8 PEPFAR countries: Burkina Faso, Burundi,  Cote d'Ivoire, Liberia, Togo, Uganda, Vietnam, and Zambia",
            caption = glue("{source_note} 
                         Calculation: Relative change from baseline (2010) = Year Estimate / Baseline Estimate (2010)"))
     
     si_save("Images/FY24 GH Portfolio Review/2023_global_rise_infections_pepfar_11OUs.png")
     

# 2020 Relative Base ------------------------------------------------------
     df_rel <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = FALSE) %>% 
       dplyr::filter(indic_type == "Percent",
                     country == "Global")  
     
     df_rel_lim <- df_rel %>% 
       filter(year == 2020,
              indicator %in% c("Percent Known Status of PLHIV",
                               "Percent on ART with Known Status",
                               "Percent VLS on ART"), #using rel base indicators
              age == "All",
              sex == "All") %>% #names()
       select(year, country, indicator, estimate)
     
     
     df_rel_lim %>% 
       mutate(share = estimate/100) %>% 
       slice_max(share, n = 3) %>% 
       mutate(benchmark = .90) %>% 
       ggplot(aes(x = indicator, y = share))+ 
       geom_col(aes(y = benchmark), fill = grey20k, alpha = 0.5) + 
       geom_col(aes(fill = indicator)) + 
       geom_hline(yintercept = .9, color = grey40k, linetype = "dotted") + 
       geom_text(aes(label = percent(share,1)),
                 vjust = -.25
       ) + 
       facet_wrap(~indicator, ncol = 3) + 
       scale_fill_si(palette = "old_rose", discrete = T) + 
       si_style_ygrid(facet_space = 0.5) + 
       scale_y_continuous(labels = percent, limits = c(0,1)) + 
       theme(axis.text.x = element_blank(),
             legend.position = "none") + 
       labs(x = NULL, y = NULL,
            title = str_to_upper("Globally, countries fell short of the 90-90-90 targets in 2020"),
            caption = glue("{source_note}"))
     
     si_save("Images/FY24 GH Portfolio Review/2020_global_relbase_performance.png")
     
   

# SPINDOWN ============================================================================

