# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-19

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
  unweighted_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/GH_scorecard_indicators_2022-06-23.csv"),
  country_names = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/hand/country_crossmap.csv"))

outputs <- list(
  hsc_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgdata.csv"),
  hsc_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgfig.svg"),
  hsc_fig2 = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgdomainfig.svg"),
  lifeexp_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavgdata.csv"),
  lifeexp_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavgfig.svg"))

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
  select(idea_region, usaid_region, country, iso,
         usaid_supported, income_group, indicator,
         year, value, date_data_pulled, ref_link, cntry_group) %>%
  # keep only life expectancy and UHC indicators
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_all_sexes",
    "uhc_subindex1_capacity_access",
    "uhc_subindex2_ncd",
    "uhc_subindex3_mchn",
    "uhc_subindex4_id")) %>%
  # filter rows with no usaid_supported data
  drop_na(usaid_supported) %>%
  # join with GH country list names
  left_join(., names,
    by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = idea_region:indicator, ~ as.character(.)),
    across(.cols = year:value, ~ as.numeric(.)),
    usaid_supported = as_factor(usaid_supported),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(idea_region, usaid_region, country, iso,
         usaid_supported, income_group, indicator,
         year, value, date_data_pulled, ref_link, cntry_group) %>%
  distinct()

# What percentage of the data includes countries supported by USAID?
country_by_support <- selected_data %>%
  select(country, usaid_supported, income_group) %>%
  group_by(country, usaid_supported, income_group) %>%
  distinct()

countries_by_support <- tabyl(country_by_support$usaid_supported)

# ~72% (0.72) of countries in this dataset (108) are supported by USAID
# and 42 are not. It seems like we have class imbalance here. Is this an
# issue when only describing the data?

# of the countries in each income group, how many are USAID supported?
countries_by_support_income <- tabyl(country_by_support, usaid_supported, income_group)

# among high income countries, 6 are USAID supported (19 are not)
# among UMI countries 35 are USAID supported (13 are not)
# among low income countries 23 are USAID supported and 2 are not
# among LMI countries 43 are USAID supported, 8 are not

# These are very imbalanced...

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

full_data <- selected_data %>%
  left_join(., pop_data, by = c(
    "country",
    "year")) %>%
  # remove rows without population data for now
  # (pre 1960, will work back in later)
  drop_na(population) %>%
  distinct() %>%
  select(idea_region, usaid_region, country, iso,
         usaid_supported, income_group, indicator,
         year, value, population, date_data_pulled,
         ref_link, cntry_group) %>%
  group_by(idea_region, usaid_region, country, iso,
           usaid_supported, income_group,
           indicator, year) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population),
    weight = as.numeric(sum(value*population))) %>%
  distinct()

# HSC index in USAID vs non-USAID countries
# annual population weighted by USAID support
uhc_low_support <- full_data %>%
  filter(indicator == "uhc_service_coverage_index",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(usaid_supported, year) %>%
  summarize(idea_region = idea_region,
            usaid_region = usaid_region,
            country = country,
            iso = iso,
            usaid_supported = usaid_supported,
            income_group = income_group,
            indicator = indicator,
            year = year,
            value = value,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2),
            population = population,
            date_data_pulled = date_data_pulled,
            ref_link = ref_link,
            cntry_group = cntry_group) %>%
  distinct()

# Life Expectancy at Birth in USAID vs non-USAID countries
# annual population weighted by USAID support
lifeexp_low_support <- full_data %>%
  filter(indicator == "life_expectancy_at_birth_all_sexes",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(usaid_supported, year) %>%
  summarize(idea_region = idea_region,
            usaid_region = usaid_region,
            country = country,
            iso = iso,
            usaid_supported = usaid_supported,
            income_group = income_group,
            indicator = indicator,
            year = year,
            value = value,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2),
            population = population,
            date_data_pulled = date_data_pulled,
            ref_link = ref_link,
            cntry_group = cntry_group) %>%
  distinct()

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in USAID vs non-USAID countries over time?
# filter by lower income
ggplot(uhc_low_support, aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
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
    title = "GAINS IN <b>POPULATION WEIGHTED HEALTH SERVICE COVERAGE INDEX</b> SINCE 2020
            <span style='color: #002a6c;'>USAID</span> AND <span style='color: #6c6463;'>NON-USAID</span> LOWER-INCOME COUNTRIES")

si_save(outputs$hsc_fig)

# What has been the change in Life Expectancy at Birth in USAID vs non-USAID countries over time?
# filter by lower income
ggplot(lifeexp_low_support, aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_line() +
  si_style_ygrid() +
  scale_x_continuous(
    limits = c(1960, 2020),
    breaks = c(1960, 1970, 1980,1990, 2000, 2010,2020)) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    minor_breaks = c(15, 25, 35, 45, 55, 65, 75)) +
  scale_color_manual(
    values = c(
      "Yes" = usaid_blue,
      "No" = usaid_darkgrey),
    labels = NULL) +
  theme(axis.text = element_text(family = "Source Sans Pro",
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
    title = "INCREASES IN <b>POPULATION WEIGHTED LIFE EXPECTANCY AT BIRTH</b> SINCE 1960
             <span style='color: #002a6c;'>USAID</span> AND <span style='color: #6c6463;'>NON-USAID</span> LOWER-INCOME COUNTRIES")

si_save(outputs$lifeexp_fig)

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(hsc_popsup, outputs$hsc_data)
write_excel_csv(life_exp_popsup, outputs$lifeexp_data)

# end