# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-21

# dependencies -----------------------------------------------------------------

  library(glamr)
  library(glitr)
  library(gophr)
  library(tidyverse)
  library(janitor)
  library(assertthat)
  library(ggplot2)
  library(readr)
  library(extrafont)
  library(here)
  library(glue)
  library(lubridate)
  library(openintro)
  library(forcats)
  library(stringr)
  library(readxl)
  library(assertthat)
  library(ggtext)

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"

# set inputs and outputs -------------------------------------------------------

i_am("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/src/weight.R")

inputs <- list(
  unweighted_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/GH_scorecard_indicators_2022-07-21.csv"),
  country_names = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/hand/country_crossmap.csv"))

outputs <- list(
  full_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/GH_scorecard_indicators_weights_2022-07-21.csv"),
  hsc_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgfig.svg"))

# munge ------------------------------------------------------------------------

# read in country names from all sources for consistency
names <- read_csv(inputs$country_names,
  show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:country_worldpop, ~ as.character(.)))

# unweighted data

selected_data <- read_csv(inputs$unweighted_data,
  show_col_types = FALSE) %>%
  clean_names() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  # keep only life expectancy and UHC indicators
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_all_sexes", # no longer in the data
    "uhc_subindex1_capacity_access",
    "uhc_subindex2_ncd",
    "uhc_subindex3_mchn",
    "uhc_subindex4_id")) %>%
  # join with GH country list names
  left_join(., names,
    by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = idea_region:indicator, ~ as.character(.)),
    across(.cols = year:value, ~ as.numeric(.)),
    pepfar = as_factor(pepfar),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  distinct()

# What percentage of the data includes countries supported by PEPFAR?
country_by_support <- selected_data %>%
  select(country, pepfar, income_group) %>%
  group_by(country, pepfar, income_group) %>%
  distinct()

countries_by_support <- tabyl(country_by_support$pepfar)

# ~26% (0.26) of countries in this dataset (51) are supported by PEPFAR
# and 143 are not. It seems like we have class imbalance here too

# of the countries in each income group, how many are PEPFAR supported?
countries_by_support_income <- tabyl(country_by_support, pepfar, income_group)

# Among Low income and LMI countries,
# we have comparable numbers of PEPFAR and non-PEPFAR

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

# weight "value" by most recent population from each country -------------------
# only contains data from 1960 onward, need previous decade
# data grouped by country, indicator, income group, and year so that
# different countries, indicators, income groups, and years are not affecting weights

full_data <- selected_data %>%
  left_join(., pop_data, by = c(
    "country",
    "year")) %>%
  # remove rows without population data for now
  # (pre 1960, will work back in later)
  drop_na(population) %>%
  distinct() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator, year, value, goal,
         population, date_data_pulled, ref_link) %>%
  group_by(country, indicator, income_group, year) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population),
    weight = as.numeric(sum(value*population))) %>%
  distinct()

# HSC index in PEPFAR vs non-PEPFAR countries
uhc_low_support <- full_data %>%
  filter(indicator == "uhc_service_coverage_index",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(pepfar, year) %>%
  summarize(pepfar = pepfar,
            year = year,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2)) %>%
  distinct()

# Life Expectancy at Birth in PEPFAR vs non-PEPFAR



# visualize --------------------------------------------------------------------

# What has been the change in HSC index in USAID vs non-USAID countries over time?
# filter by lower income
ggplot(uhc_low_support, aes(
  x = year,
  y = weighted_avg,
  group = pepfar,
  color = pepfar)) +
  geom_line() +
  si_style_ygrid() +
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
  theme( axis.text = element_text(family = "Source Sans Pro",
                                  size = 10,
                                  color = "#505050"),
        plot.title = element_markdown(family = "Source Sans Pro",
                                      size = 14,
                                      color = "#202020"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "GAINS IN <b>POPULATION WEIGHTED HEALTH SERVICE COVERAGE INDEX</b> SINCE 2000
            <span style='color: #002a6c;'>PEPFAR</span> AND <span style='color: #6c6463;'>NON-PEPFAR</span> LOWER-INCOME COUNTRIES")

si_save(outputs$hsc_fig)

# What has been the change in Life Expectancy at Birth in USAID vs non-USAID countries over time?
# filter by lower income

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(full_data, outputs$full_data)

# end