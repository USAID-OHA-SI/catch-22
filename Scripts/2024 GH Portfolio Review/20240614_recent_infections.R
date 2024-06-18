# PROJECT: Portfolio Review - Recent Infections
# PURPOSE: Munge and Analysis of UNAIDS Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  26ee292d
# LICENSE: MIT
# DATE:   2024-06-14
# NOTES:   https://github.com/USAID-OHA-SI/agitprop/blob/main/Scripts/02_epi_ann_unaids-global-epi-control.R

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

# Grab metadata
metadata <- get_metadata(file_path)

# REF ID for plots
ref_id <- "26ee292d"

# Functions  


# LOAD DATA ============================================================================  
#Recent Infections
df_epi <- 
  pull_unaids(data_type = "HIV Estimates", pepfar_only = FALSE) #%>% 
#filter(country == "Global", str_detect(indicator, "New HIV Infections"))

# MUNGE ============================================================================

#Requirements:
#Age/Sex: "All", country: "Global"
#indicators: "Number of New HIV Infections" but not "Total deaths to HIV Population" 
#incorporate the lower_bound and upper_bound for each estimate  

epi_sub <- df_epi %>% 
  filter(age == "All", sex == "All",
         country == "Global",
         indicator %in% c("Number New HIV Infections",
                          #"Total Deaths to HIV Population"
         )) %>% 
  #select(-c(estimate_flag)) %>% 
  select(year, country, indicator, estimate, lower_bound, upper_bound) %>% 
  spread(indicator, estimate) %>% 
  janitor::clean_names()

#Notes
plot_title <- "STEADY DECLINE IN THE GLOBAL NUMBER OF <span style= 'color:#2057a7;'> 
  NEW HIV INFECTIONS</span> SINCE EARLY 2000s"

note_df <- tibble(
  label = c("**HIV epidemic control is the point at which<br> the number of </span><span style= 'color:#2057a7;'>new HIV infections</span>
    falls<br> below the number of <span style= 'color:#c43d4d;'>total deaths to PLHIV</span>**"),
  year = c(2010), 
  value = c(0.4e6))

# VIZ ============================================================================

#Global recent infections through 2022

epi_sub %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = number_new_hiv_infections), color = denim, size = 1) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),linetype = 2, alpha = 0.1) + 
  #geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
  
  #geom_line(aes(y = -total_deaths_to_hiv_population), color = old_rose, linewidth = 1) +
  #geom_area(aes(y = -total_deaths_to_hiv_population), fill = "#F1CED2",  alpha = 0.95) +
  #geom_line(aes(y = epi_gap), color = "white", size = 0.25) +
  
  geom_point(data = . %>% filter(year == max(year)), 
             aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
  #geom_point(data = . %>% filter(year == max(year)), 
  #         aes(y = -total_deaths_to_hiv_population, fill = old_rose), shape = 21, color = "white", size = 3) + 
  
  geom_text(data = . %>% filter(year == max(year)), 
            aes(y = number_new_hiv_infections, color = denim, 
                label = paste0(round(number_new_hiv_infections/1000000, digits = 3), "M")),
            hjust = -0.3, size = 12/.pt,
            family = "Source Sans Pro SemiBold") +
  #geom_text(data = . %>% filter(year == max(year)), 
  #         aes(y = -total_deaths_to_hiv_population, color = old_rose, 
  #            label = paste0(abs(total_deaths_to_hiv_population/1000) %>% comma(1.0), "K")),
  #       hjust = -0.3, size = 12/.pt,
  #      family = "Source Sans Pro SemiBold") +
  scale_fill_identity() +
  scale_color_identity() +
  #scale_y_continuous(label = ~ label_number_si()(abs(.))) +
  scale_y_continuous(labels = ~((scales::label_number(scale_cut = scales::cut_short_scale()))(abs(.))), 
                     expand = c(0, 0)) + 
  scale_x_continuous(breaks = seq(1990, 2024, 5)) +
  geom_hline(yintercept = 0, color = grey80k) +
  si_style_ygrid(text_scale = 1.15) +
  labs(x = NULL, y = NULL,
       title = plot_title,
       caption =  glue("\n{metadata$caption}
                     SI analytics | US Agency for International Development")) + #{paste(authors, collapse = '/')}
  coord_cartesian(expand = T, clip = "off") +
  theme(plot.title = element_markdown())

ggsave("Images/02_epi_ann_global_epi_control_v1.png", scale = 1.2, width = 10, height = 7)


#Additional annotation (optional)
viz +
  geom_richtext(data = note_df, aes(x = year, y = value, label = label),
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"), hjust = 0, vjust = 0.7,
                size = 12/.pt, color = grey90k) +
  #annotate("curve",
  #        x = 2017, y = epi_gap_end + 0.5e6, xend = 2020, yend = epi_gap_end,
  #       arrow = arrow(length = unit(0.03, "inches"),
  #                    type = "closed"),
  #     curvature = -.4,
  #    color = suva_grey) +
  annotate(geom = "text", x = 2014, y = epi_gap_end + 0.5e6, label = c("Epidemic control gap"),
           family = "Source Sans Pro", color = suva_grey, size = 14/.pt) +
  annotate(geom = "text", x = 1995, y = 2.7e6, label = c("New HIV Infections"), hjust = 0,
           family = "Source Sans Pro SemiBold", color = denim, size = 14/.pt) +
  annotate(geom = "text", x = 1997, y = -1.5e6, label = c("Total Deaths to PLHIV"), hjust = 0,
           vjust = -1, family = "Source Sans Pro SemiBold", color = old_rose, size = 14/.pt) +
  labs(x = NULL, y = NULL,
       title = plot_title,
       caption = glue("Source: {source_note} [{date_pulled}]
                        SI analytics | Ref id: {ref_id}")) +
  coord_cartesian(expand = T, clip = "off") +
  theme(plot.title = element_markdown())

ggsave("Images/02_epi_ann_global_epi_control_v1.png", scale = 1.2, width = 10, height = 7)

# SPINDOWN ============================================================================
