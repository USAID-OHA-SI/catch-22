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
#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

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
         indicator %in% c("Number New HIV Infections")) %>% 
  #select(-c(estimate_flag)) %>% 
  select(year, country, indicator, estimate, lower_bound, upper_bound) %>% 
  spread(indicator, estimate) %>% 
  janitor::clean_names() %>% 
  mutate(goal_infections = 200000)

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

curve_viz <-epi_sub %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = number_new_hiv_infections), color = denim, size = 1) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),linetype = 2, alpha = 0.1) + 
  geom_point(data = . %>% filter(year == max(year)), 
             aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
  geom_text(data = . %>% filter(year == max(year)), 
            aes(y = number_new_hiv_infections, color = denim, 
                label = paste0(round(number_new_hiv_infections/1000000, digits = 3), "M")),
            hjust = -0.3, size = 12/.pt,
            family = "Source Sans Pro SemiBold") +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = ~((scales::label_number(scale_cut = scales::cut_short_scale()))(abs(.))), 
                     expand = c(0, 0)) + 
    scale_x_continuous(limits = c(1990, 2030), breaks = seq(1990, 2030, 5)) +
  geom_hline(yintercept = 0, color = grey80k) +
  geom_hline(yintercept = 200000, color = orchid_bloom, linetype = "dashed") +
  si_style_ygrid(text_scale = 1.15) +
  labs(x = NULL, y = NULL,
       title = plot_title) + #{paste(authors, collapse = '/')}
  coord_cartesian(expand = T, clip = "off") +
  theme(plot.title = element_markdown())

ggsave("Images/02_epi_ann_global_epi_control_v1.png", scale = 1.2, width = 10, height = 7)



epi_bar <- epi_sub %>% 
  filter(year == 2022) %>% 
  pivot_longer(cols = c(number_new_hiv_infections:goal_infections), names_to = "indicator") %>% 
  mutate(year = case_when(indicator == "goal_infections"~ 2030,
                          TRUE ~ year)) %>% 
  mutate(fill_color = ifelse(year == 2022, denim, orchid_bloom))

bar_viz <- epi_bar %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = fct_reorder(year, value), y = value, fill = fill_color)) +
  geom_col(width = 0.6) +
  # geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
  #               linetype = "dashed", width = 0.1, na.rm = TRUE) +
  si_style_xgrid()  +
  scale_y_continuous(labels = ~((scales::label_number(scale_cut = scales::cut_short_scale()))(abs(.))), 
                     expand = c(0, 0), limits = c(0, 1500000)) + 
  coord_flip() +
  scale_fill_identity() +
  geom_text(aes(label = clean_number(value, 1)),
            family = "Source Sans Pro",
            hjust = -0.3) + 
  labs(x = NULL, y = NULL,
       subtitle = "However, this reduction in new infections is not sufficient to reach our goal of <span style= 'color:#E14BA1;'>200k new infections in 2025</span>",
       caption =  glue("\nSource: {source_note}
                     Ref id: {ref_id}")) +
  theme(plot.subtitle = element_markdown())


curve_viz / bar_viz +
  plot_layout(heights = c(2,1))
  
si_save("Graphics/20240618_recent_inf_curve.svg")

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
