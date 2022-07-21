# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  SIC - remake HIV Resource alignment & ARPA
# REF ID:   f18bc5bd 
# LICENSE:  MIT
# DATE:     2022-07-11
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "f18bc5bd"
  
  gs_id <- as_sheets_id("1eD5PNKUkCSsPkJcK_JgxLd5gl2-Rhq7bEeUd83QCoWY")

# IMPORT ------------------------------------------------------------------
  
  df_share <- read_sheet(gs_id, sheet = "resource_alignment") %>% 
    pivot_longer(-country, 
                 names_to = "source",
                 values_drop_na = TRUE)
  
  df_share_areas <- read_sheet(gs_id, sheet = "resource_alignment_areas") %>% 
    pivot_longer(-c(country, area),
                 names_to = "source",
                 values_drop_na = TRUE)
  
  df_arpa <-  read_sheet(gs_id, sheet = "arpa")
  
  df_budget_share <- read_sheet(gs_id, sheet = "budget_allocation")
  

# VIZ ---------------------------------------------------------------------


  df_viz <- df_share %>% 
    mutate(value = value/100,
           pepfar_share = case_when(source == "PEPFAR" ~ value),
           source = factor(source, c("PEPFAR", "Host Country", "Global Fund", "Other")))
  
  df_viz %>% 
    ggplot(aes(value, fct_reorder(country, pepfar_share, na.rm = TRUE), fill = source)) +
    geom_col() +
    facet_grid(~source) +
    scale_x_continuous(labels = percent, position = "top") +
    scale_fill_manual(values = c(scooter, denim_light, moody_blue_light, trolley_grey_light)) +
    labs(x = NULL, y = NULL,
         title = "PEPFAR maintains significant funding for the HIV response" %>% toupper,
         subtitle = "creating another challenge to long-term sustainability",
         caption = glue("USAID OHA only/ NOT FOR DISTRIBUTION 
         Source: HIV Resource Alignment [May 2022] | Ref Id: {ref_id}")) +
    si_style_xgrid() +
    theme(strip.placement = "outside",
          legend.position = "none")
  
  si_save("Images/resource_alignment.png")
  
  df_viz_area <- df_share_areas %>% 
    mutate(value = value/100,
           country = factor(country, c("Lesotho", "Kenya", "Bostwana")),
           source = factor(source, c("PEPFAR", "Host Country", "Global Fund", "Other")))
  
  df_viz_area %>% 
    ggplot(aes(value, area, fill = source)) +
    geom_col() +
    facet_grid(country~source, switch = "y") +
    scale_x_continuous(labels = percent, position = "top") +
    scale_fill_manual(values = c(scooter, denim_light, moody_blue_light, trolley_grey_light)) +
    labs(x = NULL, y = NULL,
         caption = glue("USAID OHA only/ NOT FOR DISTRIBUTION 
         Note: Eswatini + Uganda removed due to lack of Host Country data
         Source: HIV Resource Alignment [May 2022] | Ref Id: {ref_id}")) +
    si_style_xgrid() +
    theme(strip.placement = "outside",
          legend.position = "none",
          strip.text.y = element_text(hjust = .5),
          panel.spacing = unit(.5, "picas"))
  
  si_save("Images/resource_alignment_area.png")
  
  
  
  df_arpa %>% 
    ggplot(aes(value, fct_reorder(country, value))) +
    geom_col(fill = denim, alpha = .8) +
    geom_text(aes(label = label_number_si(.1, prefix = "$")(value)),
               hjust = -.1,
               color = matterhorn, family = "Source Sans Pro") +
    scale_x_continuous(expand = c(.005, .005)) +
    expand_limits(x = 75e6) +
    labs(x = NULL, y = NULL,
         title = glue("Sustained Impact countries receieved a combined {sum(df_arpa$value) %>% comma(accuracy = 1, scale = 1e-6, suffix = 'M', prefix = '$')} in ARPA funding") %>% toupper, 
         caption = glue("Source: USAID COVID Factsheets | Ref Id: {ref_id}")) +
    coord_cartesian(clip = "off", expand = TRUE) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank())

  si_save("Images/arpa.png")  
  
  
  
  df_budget_share %>% 
    pivot_longer(-type, names_to = "country") %>% 
    mutate(type = factor(type, c("SD", "NSD - Site-level", "NSD - Above-site", "PM")),
           fill_color = ifelse(type == "SD", old_rose, trolley_grey),
           sd_share = case_when(type == "SD" ~ value),
           fill_alpha = ifelse(type == "SD", .9, .6)) %>% 
    ggplot(aes(value, fct_reorder(country, sd_share, na.rm = TRUE), 
               fill = fill_color, alpha = fill_alpha)) +
    geom_col(color = "white") +
    scale_x_continuous(expand = c(.005, .005), labels = percent, position = "top") +
    scale_fill_identity() +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         title = glue("<span style='color:{old_rose}'>**SERVICE DELIERVEY**</span> REPRESENTS THE LARGEST PROPROTION OF FY23 BUDGETS IN MOST SIC"),
         subtitle = "Across budget areas: SD, NSD/Site-level, NSD/Above-site, PM",
         caption = glue("Note: Included commodities, excludes M&O
                        Source: FY23 COP Dataset, OHA EA Branch | Ref Id: {ref_id}")) +
    si_style_nolines() +
    theme(plot.title = element_markdown())
  
  
  si_save("Images/budget_allocation.png",
          width = 9.56, height = 3.91)
  