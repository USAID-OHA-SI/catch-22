# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  10-10-10 progress
# LICENSE:  MIT
# DATE:     2021-11-09
# UPDATED:  
# SOURCE:   https://hivpolicylab.org/data

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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  gs_id <- as_sheets_id("1LadXX9g9D6MCp6M3WD27Js85YE8MHrf_ulJQmRVduCU")

  ind_sel <- c(paste0("S", 1:6), "S9")
  

# IMPORT ------------------------------------------------------------------
  
  #read in HIV Policy Lab data export
  df <- read_sheet(gs_id, "Policy adoption data", skip = 6,
                   col_types = "c",
                   .name_repair = make_clean_names )

   
  #limit to just relevant indicators & rename
  df_struct <- df %>% 
    rename(indicator = indicator_subindicator_name) %>% 
    filter(indicator %in% ind_sel) %>% 
    mutate(indicator_name = recode(indicator,
                                   "S1" = "Same-sex sex non-criminalization",
                                   "S2" = "Sex work non-criminalization",
                                   "S3" = "Drug use non-criminalization",
                                   "S4" = "HIV exposure non-criminalization",
                                   "S5" = "Non-discrimination protections",
                                   "S6" = "National human rights institutions",
                                   "S9" = "Gender based violence"))

  #rename countries and limit to just PEPFAR
  df_struct <- df_struct %>% 
    mutate(country = ifelse(str_detect(country, "Ivoire"), "Cote d'Ivoire", country),
           country = recode(country,
                            "Myanmar" = "Burma",
                            "Lao People's Democratic Republic" = "Laos",
                            "Tanzania (United Republic of)" = "Tanzania",
                            "Viet Nam" = "Vietnam"
                            )) %>% 
    filter(country %in% pepfar_country_list$countryname)
  
  #filter to most recent year
  df_struct <- df_struct %>% 
    filter(year == "Most recent")

  #aggregate adoption across PEPFAR countries
  df_viz <- df_struct %>% 
    count(adoption_level, indicator_name) %>% 
    filter(!is.na(adoption_level))
  
  df_viz <- df_viz %>% 
    mutate(adoption_level = factor(adoption_level, c("Adopted", "Partial", "Not adopted")),
           indicator_order = case_when(adoption_level == "Not adopted" ~ n),
           fill_color = case_when(adoption_level == "Not adopted" & indicator_name %in% c("Drug use non-criminalization", "Sex work non-criminalization") ~ old_rose,
                                  adoption_level == "Not adopted" ~ old_rose_light,
                                  adoption_level == "Partial" ~ burnt_sienna_light,
                                  adoption_level == "Adopted" ~ scooter_med))
   
  
  df_viz %>% 
    ggplot(aes(n, fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
    geom_col(aes(fill = fill_color)) +
    geom_vline(xintercept = 0) +
    facet_wrap(~fct_rev(adoption_level)) +
    scale_fill_identity() +
    scale_x_continuous(position = "top") +
    labs(x = NULL, y = NULL,
         title = "THE LARGEST GAP IN THE 10-10-10 GOALS ACROSS PEPFAR COUNTRIES IS IN ADOPTING DRUG USE AND SEX WORK NON-CRIMINALIZATION LAWS/POLICIES",
         subtitle = "Number of PEPFAR countries adopting structural laws/policies towards UNAIDS' 10-10-10 goals",
         caption = glue("Source: HIV Policy Lab [2021-11-09]",
                        "USAID SI Analytics",
                        "Global Planning Meeting 2021-11-15", .sep = " | ")) +
    si_style_xgrid() +
    theme(strip.placement = "outside")

  si_save("Graphics/gpm_pepfar_10s-gap.svg", height = 4.25)  
               