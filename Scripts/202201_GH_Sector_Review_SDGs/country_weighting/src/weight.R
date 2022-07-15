# PROJECT:  catch-22
# AUTHOR:   J. Hoehner | USAID
# PURPOSE:  GH weighting "value" by most recent population
# LICENSE:  MIT
# DATE:     2022-07-15
# UPDATED:  2022-07-15

# load libraries ---------------------------------------------------------------

pacman::p_load("tidyverse","glue", "lubridate",
               "here", "janitor", "readr", "ggplot2",
               "openintro", "forcats", "stringr", "readxl")

# set inputs and outputs -------------------------------------------------------

inputs <- list(
  unweighted_data = here("USAID_PHI/USAID/country_weighting/input/GH_scorecard_indicators_2022-06-23.csv"),
  country_names = here("USAID_PHI/USAID/country_weighting/hand/country_crossmap.csv"))

outputs <- list(

  weighted_data = here(""),
  hsc_fig = here(""),
  lifeexp_fig = here(""))

# munge ------------------------------------------------------------------------
# do we use Burma or Myanmar?

# read in country names from all sources for consistency
names <- read_csv(inputs$country_names,
                  `show_col_types = FALSE`) %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:country_worldpop, ~ as_factor(.)))

# unweighted data

selected_data <- read_csv(inputs$unweighted_data,
                          show_col_types = FALSE) %>%
  clean_names() %>%
  select(country, iso, indicator, usaid_supported, year, value) %>%
  filter(indicator %in% c("uhc_service_coverage_index",
                          "life_expectancy_at_birth_all_sexes")) %>%
  # filter rows with no usaid_supported data
  drop_na(usaid_supported) %>%
  # join with GH country list names
  left_join(., names,
            by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = country:usaid_supported, ~ as.character(.)),
    value = as.numeric(value),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as_factor(if_else(
      is.na(country_ghlist) == FALSE,
      country_ghlist,
      country))) %>%
  # is GH list of countries Ground Truth as far as names to use?
  # Assuming yes
  select(country, iso, indicator, usaid_supported, year, value)

# What percentage of the data includes countries supported by USAID?

countries_by_support <- selected_data %>%
  select(country, usaid_supported) %>%
  group_by(usaid_supported, country) %>%
  distinct()

tabyl(countries_by_support$usaid_supported)

# ~72% (0.72) of countries in this dataset (108) are supported by USAID
# and 2 are not. It seems like we have class imbalance here. Is this an
# issue when only describing the data?

# weight "value" by most recent population from each country -------------------
# only contains data from 1960 onward, need previous decade

pop_data <- world_pop %>%
  pivot_longer(
    cols = year_1960:year_2020,
    names_to = "year",
    values_to = "population") %>%
  select(country, year, population) %>%
  mutate(year = str_remove(year, "year_"),
         year = as.double(year))

pop_country <- pop_data %>%
  select(country) %>%
  distinct()

write_csv(pop_country, here(
  "USAID_PHI/USAID/country_weighting/hand/world_pop_countries.csv"))

# are the lists of country names different?

differences <- as.data.frame(
  setdiff(selected_data$country, pop_data$country))

data_wpop <- full_join(selected_data, pop_data,
          by = c("country", "year")) %>%
  drop_na(indicator) %>%
  distinct() %>%
  arrange(year)

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in USAID vs non-USAID countries over time?



# What has been the change in Life Expectancy at Birth in USAID vs non-USAID countries over time?