# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To prepare data for adjusting estimates by population
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-08-12

# dependencies -----------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(janitor)
  library(glue)
  library(lubridate)
  library(openintro)
  library(assertr)
  library(googlesheets4)

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"
load_secrets()

# munge ------------------------------------------------------------------------

# read in country names from all sources for consistency
names <- read_sheet("1tQNqPsgekfty--oA5va4PQCFMrZBVro4fGjFk8zaa70") %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:country_worldpop, ~ as.character(.)))

# unweighted data

unweighted_data <-  read_sheet("17oCZviSJ8EIJ3gqP6Z9H2P4ToUORNykn8EwbLrxWp6I") %>%
  clean_names() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, income_group, indicator, year, value, goal, date_data_pulled, 
         ref_link) %>%
  # keep only life expectancy and UHC indicators,
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_both_sexes_years",
    "uhc_subindex1_capacity_access",
    "uhc_subindex4_id"), 
  # keep only LI and LMI countries
    income_group %in% c("Low Income Country (World Bank Classification)", 
                        "Lower Middle Income Country (World Bank Classification)"), 
  # keep only data from 1990 onward
    year >= 1990) %>%
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
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, income_group, indicator,year, value, goal, date_data_pulled, 
         ref_link) %>%
  distinct()

# bring in population data from World Bank
pop_data <- world_pop %>%
  pivot_longer(
    # if we want to go back to 1980, Palestine does not have pop data from 
    # 1980 - 1989
    cols = year_1990:year_2020,
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

figure_data <- unweighted_data %>%
  left_join(., pop_data, by = c(
    "country","year")) %>%
  # filter data by years between 1960-2020
  # since world_pop only goes back that far
  filter(as.numeric(year) >= 1990 & as.numeric(year) <= 2020, 
  # filter out Eritrea because it does not have population ests.
  # for 2015, 2017, and 2019 for all uch indicators 
  # nor for 2012-2020 for life expectancy indicators
         iso != "ERI",
  year >= 1990) %>%
  distinct() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, income_group, indicator, year, value, goal,
         population, date_data_pulled, ref_link) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population)) %>%
  # validate that no cols we need later on are missing
  verify(., is.na(population) == FALSE) %>%
  verify(., is.na(value) == FALSE) %>%
  verify(., is.na(pepfar) == FALSE) %>%
  verify(., is.na(country) == FALSE) %>%
  verify(., is.na(usaid_region) == FALSE) %>%
  # validate that years are between expected ranges
  verify(between(year, 1990, 2020) == TRUE)

# About the data ---------------------------------------------------------------

# What percentage of the data includes countries supported by PEPFAR?
country_by_groupings <- figure_data %>%
  ungroup() %>%
  select(country, income_group, pepfar) %>%
  group_by(country, income_group, pepfar) %>%
  distinct()

# How many are PEPFAR supported?
countries_by_pepfar_income <- tabyl(country_by_groupings, pepfar, income_group)
# among low income countries: 13 PEPFAR and 13 non-PEPFAR countries
# among LMI countries: 27 PEPFAR and 28 non-PEPFAR countries

# Example calculation for one indicator, year, and pepfar category -------------

# "UHC Service Coverage Index, 2005, PEPFAR supported li countries

uhc_yespepfar_2005_li <- figure_data %>%
  filter(indicator =="uhc_service_coverage_index",
         income_group == "Low Income Country (World Bank Classification)",
         year == 2005, 
         pepfar == "PEPFAR") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# sum the values times the populations
sum_wtvals_li <- sum(uhc_yespepfar_2005_li$value*uhc_yespepfar_2005_li$population)

# sum the populations
sum_pops_li <- sum(uhc_yespepfar_2005_li$population)

# divide the summed weighted values by the summed population
weighted_avg_li = sum_wtvals_li/sum_pops_li

# show unweighted avg for comparison
unweighted_avg_li = sum(uhc_yespepfar_2005_li$value)/nrow(uhc_yespepfar_2005_li)

# "UHC Service Coverage Index, 2005, PEPFAR supported li and lmi countries

uhc_yespepfar_2005_comb <- figure_data %>%
  filter(indicator =="uhc_service_coverage_index",
         year == 2005, 
         pepfar == "PEPFAR") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# sum the values times the populations
sum_wtvals_comb <- sum(uhc_yespepfar_2005_comb$value*uhc_yespepfar_2005_comb$population)

# sum the populations
sum_pops_comb <- sum(uhc_yespepfar_2005_comb$population)

# divide the summed weighted values by the summed population
weighted_avg_comb = sum_wtvals_comb/sum_pops_comb

# show unweighted avg for comparison
unweighted_avg_comb = sum(uhc_yespepfar_2005_comb$value)/nrow(uhc_yespepfar_2005_comb)

# data to use in figures in weightviz.R, GH_scorecard_indicators_figure_data ---
write_sheet(figure_data, "17ZBOzPux0lgkmN48BtE3yzKpeEzDLVmx9B4Pei9nGbw", 
            sheet = "figure_data")

# end