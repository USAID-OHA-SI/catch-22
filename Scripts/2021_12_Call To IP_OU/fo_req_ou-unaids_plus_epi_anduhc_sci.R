# PROJECT:  catch-22
# AUTHOR:   J. Hoehner | USAID
# PURPOSE:  compare 90s and epi control with UHC SCI for FO request
# REF ID:   f13e85b5
# LICENSE:  MIT
# DATE:     2023-03-30
# NOTES: lovingly adapted from catch-22/Scripts/2021_12_Call To IP_OU/ctip-ou-unaids_plus_epi.R
# and catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/clean/src/clean.R

# DEPENDENCIES -----------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(scales)
library(tidytext)
library(glue)
library(mindthegap)
library(googlesheets4)
library(janitor)
library(assertr)

# GLOBAL VARIABLES -------------------------------------------------------------
ref_id <- "f13e85b5"

load_secrets()

# UNAID GOAL - 90 or 95
goal <- 95

# IMPORT -----------------------------------------------------------------------

# 95s data

# HIV estimates
df_est <- pull_unaids("HIV Estimates", TRUE)

# Test and Treat percent estimates
df_tt <- pull_unaids("HIV Test & Treat", TRUE)

# pull Total PLHIV death data
g_id <- "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"

df_deaths <- range_speedread(ss = g_id, sheet = "UNAIDS_epi_control") %>%
  filter(indicator == "Number Total Deaths HIV Pop")

# UHC SCI data

# read in country names from all sources for consistency
names <- read_sheet("1tQNqPsgekfty--oA5va4PQCFMrZBVro4fGjFk8zaa70") %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:country_worldpop, ~ as.character(.)))

# unweighted UHC SCI data

unweighted_sci <- read_sheet("17oCZviSJ8EIJ3gqP6Z9H2P4ToUORNykn8EwbLrxWp6I")

# MUNGE HIV ESTIMATES ----------------------------------------------------------

# limit HIV estimates data
df_est_lim <- df_est %>%
  filter(
    indicator %in% c("Number PLHIV", "Number AIDS Related Deaths", 
                     "Number New HIV Infections"),
    age == "All",
    sex == "All",
    # filtering for 2019 since this is the most current year we have
    # UHC SCI data for
    year == 2019) %>%
  select(year, country, indicator, estimate) %>%
  # reshape wide to align with T&T
  pivot_wider(
    names_from = indicator,
    values_from = "estimate",
    names_glue = 
      "{indicator %>% str_extract_all('Deaths|Infections|PLHIV') %>% tolower}")

#plhiv for plot
df_plhiv <- df_est_lim %>%
  # filtering for 2019 since this is the most current year we have
  # UHC SCI data for
  filter(year == 2019) %>% 
  select(year, country, plhiv)

# reshape wide to align with T&T
# grab total deaths
total_deaths <- df_deaths %>%
  filter(
    age == "all",
    sex == "all") %>%
  select(c(country, year, indicator, estimate)) %>%
  spread(indicator, estimate) %>%
  clean_names() %>%
  rename(total_deaths = number_total_deaths_hiv_pop)

# identify if epi control or not
df_est_lim_final <- df_est_lim %>%
  arrange(country, year) %>%
  left_join(total_deaths, by = c("year", "country")) %>%
  group_by(country) %>%
  mutate(declining_deaths = total_deaths - 
           lag(total_deaths, order_by = year) <= 0) %>%
  ungroup() %>%
  mutate(
    infections_below_deaths = infections < total_deaths,
    ratio = infections / total_deaths,
    epi_control = declining_deaths == TRUE & 
      infections_below_deaths == TRUE) %>%
  filter(!is.na(ratio)) %>%
  mutate(
    indicator = "Epi\nControl",
    value = round(ratio, 1)) %>%
  select(year, country, indicator, value)

# MUNGE DATA -------------------------------------------------------------------

# limit Test and Treat data
df_tt_lim <- df_tt %>%
  # filtering for 2019 since this is the most current year we have
  # UHC SCI data for
  filter(
    year == 2019,
    indicator %in% c(
      "Percent Known Status of PLHIV",
      "Percent on ART of PLHIV",
      "Percent VLS of PLHIV"),
    age == "All",
    sex == "All",) %>%
  select(year, country, indicator, estimate) %>%
  rename(value = estimate) %>%
  filter(!is.na(value)) %>%
  mutate(
    indicator = recode(indicator,
      "Percent Known Status of PLHIV" = "Known\nStatus",
      "Percent on ART of PLHIV" = "On\nART",
      "Percent VLS of PLHIV" = "VLS"),
    set = recode(indicator,
      "Known\nStatus" = 1,
      "On\nART" = 2,
      "VLS" = 3),
    goal_rate = round((goal / 100)^set * 100)) %>%
  group_by(country) %>%
  mutate(
    gap = goal_rate - value,
    grouping = case_when(
      country %in% c("Guatemala", "Tajikistan") ~ "On ART",
      max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
      gap == max(gap, na.rm = TRUE) ~ str_replace(indicator, "\\n", " "),
      TRUE ~ NA_character_),
    gap = max(gap)) %>%
  ungroup()

# MUNGE UHC SCI data -----------------------------------------------------------

sci_filt <- unweighted_sci %>%
  clean_names() %>%
  select(country, pepfar, indicator, year, value) %>%
  # keep only specific UHC indicators
  filter(
    indicator %in% c(
      "uhc_service_coverage_index",
      "uhc_subindex1_capacity_access",
      "uhc_subindex4_id"),
    # keep only PEPFAR supported countries
    pepfar == "PEPFAR",
    # filtering for 2019 since this is the most current year we have
    # UHC SCI data for
    year == 2019) %>%
  # join with GH country list names to make sure names are the same across data sources
  left_join(., names,
    by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = country:indicator, ~ as.character(.)),
    across(.cols = year:value, ~ as.numeric(.)),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(country, indicator, year, value) %>%
  pivot_wider(
    names_from = indicator,
    values_from = "value") %>%
  clean_names()

# MERGE DATA -------------------------------------------------------------------

df_final <- df_tt_lim %>%
  bind_rows(df_est_lim_final) %>%
  arrange(country) %>%
  group_by(country) %>%
  fill(grouping, .direction = "downup") %>%
  left_join(df_plhiv %>%
    distinct(country)) %>%
  select(country, indicator, value, year) %>%
  pivot_wider(
    names_from = indicator,
    values_from = "value") %>%
  clean_names() %>%
  select(country, known_status, on_art, vls, epi_control, year) %>%
  full_join(sci_filt, by = c("country", "year")) %>%
  select(
    country, known_status, on_art, vls, epi_control,
    uhc_service_coverage_index, uhc_subindex1_capacity_access,
    uhc_subindex4_id, year)

# Metrics : epi score card and SCI index and sub-indices infectious and capacity
# write_sheet("1-nzn8jqmyeW74Oh5vNugiSeLoRi2jGQpJ0M3Ar23nKQ", "Sheet1")
