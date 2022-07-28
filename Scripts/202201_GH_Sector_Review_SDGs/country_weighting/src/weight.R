# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-28

# dependencies -----------------------------------------------------------------

  library(glamr)
  library(glitr)
  library(gophr)
  library(tidyverse)
  library(janitor)
  library(assertthat)
  library(ggplot2)
  library(readr)
  library(glue)
  library(lubridate)
  library(openintro)
  library(forcats)
  library(stringr)
  library(readxl)
  library(assertthat)
  library(ggtext)
  library(showtext)
  library(sysfonts)
  library(svglite)
  library(officer)
  library(Cairo)
  library(rvg)
  library(gridExtra)
  

  font_add_google("Source Sans Pro")
  showtext::showtext_auto()

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"

# set inputs and outputs -------------------------------------------------------
  
folder_path <- "catch-22/Dataout/"
  
inputs <- list(
  country_names = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/hand/country_crossmap.csv", 
  template_hsc_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/template_hsccov_usaid.pptx", 
  template_lexp_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/template_lifeexp_usaid.pptx", 
  template_hsc_pepfar = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/template_hsccov_pepfar.pptx",  
  template_lexp_pepfar = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/template_lifeexp_pepfar.pptx" )

outputs <- list(
  select_pop_data = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/GH_scorecard_indicators_weights_2022-07-21.csv"
)

# munge ------------------------------------------------------------------------

# read in country names from all sources for consistency
names <- read_csv(inputs$country_names,
  show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:country_worldpop, ~ as.character(.)))

# unweighted data

selected_data <-  folder_path %>%
  return_latest("GH_scorecard_indicators_2022-07-26.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  clean_names() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  # keep only life expectancy and UHC indicators, low income group
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_both_sexes_years",
    "uhc_subindex1_capacity_access",
    "uhc_subindex2_ncd",
    "uhc_subindex3_mchn",
    "uhc_subindex4_id"), 
   income_group == "Low Income Country (World Bank Classification)") %>%
  # join with GH country list names to make sure names are the same across data sources
  left_join(., names,
    by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = idea_region:indicator, ~ as.character(.)),
    across(.cols = year:value, ~ as.numeric(.)),
    pepfar = as_factor(pepfar),
    # fix missing usaid_region for Togo
    usaid_region = as.character(if_else(
      iso == "TGO", "Sub-Saharan Africa", usaid_region)),
    # fix usaid_support for Togo since it is on the list provided by 
    # Karishma 
    usaid_supported = as.character(if_else(iso == "TGO", "Yes", usaid_supported)), 
      # fix missing usaid support issue 
     usaid_supported = as.character(replace_na(usaid_supported,"No")),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  distinct()

# check for no missing values in usaid_supported, should not have any
assertthat::validate_that(noNA(selected_data$usaid_supported) == TRUE, 
                         msg = "Warning, usaid_supported has missing values.")

# bring in population data from World Bank
pop_data <- world_pop %>%
  pivot_longer(
    cols = year_1960:year_2020,
    names_to = "year",
    values_to = "population") %>%
  select(country, year, population) %>%
  # join with GH country list names
  left_join(., names,
    by = c("country" = "country_worldpop")) %>%
  mutate(
    year = as.numeric(str_remove(year, "year_")),
    population = as.numeric(population),
    # replace country names from world pop with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(country, year, population) %>%
  distinct()

# join data --------------------------------------------------------------------

select_pop_data <- selected_data %>%
  left_join(., pop_data, by = c(
    "country","year")) %>%
  # filter data by years between 1960-2020
  # since world_pop only goes back that far
  filter(as.numeric(year) >= 1960 & as.numeric(year) <= 2020, 
  # filter out Eritrea because it does not have population ests.
  # for 2015, 2017, and 2019 for all uch indicators 
  # nor for 2012-2020 for life expectancy indicators
         iso != "ERI") %>%
  distinct() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator, year, value, goal,
         population, date_data_pulled, ref_link) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population))

# About the data ---------------------------------------------------------------

# What percentage of the data includes countries supported by USAID or PEPFAR?
country_by_groupings <- select_pop_data %>%
  ungroup() %>%
  select(country, usaid_supported, pepfar) %>%
  group_by(country, usaid_supported, pepfar) %>%
  distinct()

# How many are usaid supported?
countries_by_usaid <- tabyl(country_by_groupings, usaid_supported)
# among low income countries: 2 non-USAID supported, 24 USAID supported

# Which are those 2 non-USAID supported?
notsupported <- country_by_groupings %>%
  filter(usaid_supported == "No")
# DPRK and Guinea-Bissau

# How many are PEPFAR supported?
countries_by_pepfar_income <- tabyl(country_by_groupings, pepfar)
# 13 PEPFAR and 13 non-PEPFAR countries

# Example calculation for one indicator, year, and pepfar category -------------
# "UHC Service Coverage Index, 2005, PEPFAR supported countries

uhc_yespepfar_2005 <- select_pop_data %>%
  filter(indicator =="uhc_service_coverage_index", 
         year == 2005, 
         pepfar == "PEPFAR") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# sum the values times the populations
sum_wtvals <- sum(uhc_yespepfar_2005$value*uhc_yespepfar_2005$population)

# sum the populations
sum_pops <- sum(uhc_yespepfar_2005$population)

# divide the summed weighted values by the summed population
weighted_avg = sum_wtvals/sum_pops

# show unweighted avg for comparison
unweighted_avg = sum(uhc_yespepfar_2005$value)/13

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in PEPFAR vs non-PEPFAR countries over time?
# filter by indicator
uhc_pepfar <- 
  ggplot(uhc_pepfar <- select_pop_data %>%
           filter(indicator =="uhc_service_coverage_index") %>%
           group_by(year, pepfar) %>%
           mutate(
             weighted_avg = weighted.mean(value, population)), 
  aes(
  x = year,
  y = weighted_avg,
  group = pepfar,
  color = pepfar)) +
  geom_smooth() +
  geom_vline(xintercept = 2003,
             color = usaid_red,
             linetype = "longdash") +
 annotate("text",
           x = 2004.8, y = 100,
           label = "  PEPFAR first authorized",
           size = 3,
           color = usaid_red) +
  si_style_ygrid() +
  # hsc is an index from 0-100
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_blue,
      "Non-PEPFAR" = usaid_darkgrey),
    labels = NULL) +
  theme(axis.text = element_text(family = "Source Sans Pro",
                                  size = 10,
                                  color = "#505050"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL)


# What has been the change in HSC index in USAID vs non-USAID countries over time?
# filter by indicator and grouping var, 2 non-USAID countries
uhc_usaid  <- 
  ggplot(uhc_usaid <- select_pop_data %>%
           filter(indicator =="uhc_service_coverage_index") %>%
           group_by(year, usaid_supported) %>%
           mutate(
             weighted_avg = weighted.mean(value, population))
                , aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_smooth() +
  geom_vline(xintercept = 2003,
             color = usaid_red,
             linetype = "longdash") +
  annotate("text",
           x = 2004.8, y = 100,
           label = "  PEPFAR first authorized",
           size = 3,
           color = usaid_red) +
  si_style_ygrid() +
  # hsc is an index from 0-100
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_color_manual(
    values = c(
      "Yes" = usaid_blue,
      "No" = usaid_darkgrey),
    labels = NULL) +
  theme(axis.text = element_text(family = "Source Sans Pro",
                                 size = 10,
                                 color = "#505050"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL)

# What has been the change in Life Expectancy at Birth in PEPFAR vs non-PEPFAR countries over time?
lexp_pepfar_fig <- 
  ggplot(lexp_pepfar<- select_pop_data %>%
      filter(indicator =="life_expectancy_at_birth_both_sexes_years") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population))
       , aes(
  x = year,
  y = weighted_avg,
  group = pepfar,
  color = pepfar)) +
  geom_smooth() +
  geom_vline(xintercept = 2003,
             color = usaid_red,
             linetype = "longdash") +
  annotate("text",
           x = 2008, y = 65,
           label = "  PEPFAR first authorized",
           size = 3,
           color = usaid_red) +
  si_style_ygrid() +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_x_continuous(
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_blue,
      "Non-PEPFAR" = usaid_darkgrey),
    labels = NULL) +
  theme( axis.text = element_text(family = "Source Sans Pro",
                                  size = 10,
                                  color = "#505050"),
         legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL)

# What has been the change in Life Expectancy at Birth in USAID vs non-USAID countries over time?
lexp_usaid_fig <- 
  ggplot(lexp_usaid <- select_pop_data %>%
           filter(indicator =="life_expectancy_at_birth_both_sexes_years") %>%
           group_by(year, usaid_supported) %>%
           mutate(
             weighted_avg = weighted.mean(value, population))
       , aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_smooth() +
  geom_vline(xintercept = 2003,
             color = usaid_red,
             linetype = "longdash") +
  annotate("text",
           x = 2008, y = 65,
           label = "  PEPFAR first authorized",
           size = 3,
           color = usaid_red) +
  si_style_ygrid() +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_x_continuous(
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_manual(
    values = c(
      "Yes" = usaid_blue,
      "No" = usaid_darkgrey),
    labels = NULL) +
  theme( axis.text = element_text(family = "Source Sans Pro",
                                  size = 10,
                                  color = "#505050"),
         legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL)

# prepare slides ---------------------------------------------------------------

# HSC index

# PEPFAR
uhc_pepfar_dml <- dml(grid.arrange(uhc_pepfar,
                              nrow = 1, widths = 12.89))
uhc_pepfar_template <- read_pptx(path = inputs$template_hsc_pepfar)
uhc_pepfar_complete <- ph_with(uhc_pepfar_template, uhc_pepfar_dml,
                          location = ph_location(
                            left = 0.5,
                            top = 2.0,
                            width = 12.89,
                            height = 5.9,
                            newlabel = ""))

print(uhc_pepfar_complete,
      target = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsccov_pepfar.pptx")

lifexp_pepfar_dml <- dml(grid.arrange(lexp_pepfar_fig,nrow = 1, widths = 12.89))
lifexp_pepfar_template <- read_pptx(path = inputs$template_lexp_pepfar)
lifexp_pepfar_complete <- ph_with(lifexp_pepfar_template, lifexp_pepfar_dml,
                               location = ph_location(
                                 left = 0.5,
                                 top = 2.0,
                                 width = 12.89,
                                 height = 5.9,
                                 newlabel = ""))

print(lifexp_pepfar_complete,
      target = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_pepfar.pptx")


# USAID
uhc_usaid_dml <- dml(grid.arrange(uhc_usaid,nrow = 1, widths = 12.89))
uhc_usaid_template <- read_pptx(path = inputs$template_hsc_usaid)
uhc_usaid_complete <- ph_with(uhc_usaid_template, uhc_usaid_dml,
                               location = ph_location(
                                 left = 0.5,
                                 top = 2.0,
                                 width = 12.89,
                                 height = 5.9,
                                 newlabel = ""))

print(uhc_usaid_complete,
      target = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsccov_usaid.pptx")

lifexp_usaid_dml <- dml(grid.arrange(lexp_usaid_fig,nrow = 1, widths = 12.89))
lifexp_usaid_template <- read_pptx(path = inputs$template_lexp_usaid)
lifexp_usaid_complete <- ph_with(lifexp_usaid_template, lifexp_usaid_dml,
                                  location = ph_location(
                                    left = 0.5,
                                    top = 2.0,
                                    width = 12.89,
                                    height = 5.9,
                                    newlabel = ""))

print(lifexp_usaid_complete,
      target = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_usaid.pptx")

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(select_pop_data, outputs$select_pop_data)

# end