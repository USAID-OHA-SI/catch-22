# PROJECT:  Population weighting for GH Sector Review SDGs
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-19

# dependencies -----------------------------------------------------------------

pacman::p_load(
  "tidyverse", "glue", "lubridate",
  "here", "janitor", "readr", "ggplot2",
  "openintro", "forcats", "stringr", "readxl",
  "assertthat", "glamr", "glitr", "ggtext")

extrafont::loadfonts(device = "win")

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"

# set inputs and outputs -------------------------------------------------------

i_am("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/src/weight.R")

inputs <- list(
  unweighted_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/input/GH_scorecard_indicators_2022-06-23.csv"),
  country_names = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/hand/country_crossmap.csv"))

outputs <- list(
  hsc_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgdata.csv"),
  hsc_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgfig.png"),
  lifeexp_data = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavgdata.csv"),
  lifeexp_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavgfig.png"))

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
  select(country, iso, indicator, usaid_supported, year, value) %>%
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_all_sexes")) %>%
  # filter rows with no usaid_supported data
  drop_na(usaid_supported) %>%
  # join with GH country list names
  left_join(., names,
    by = c("country" = "country_unweighted")) %>%
  mutate(
    across(.cols = country:indicator, ~ as.character(.)),
    across(.cols = year:value, ~ as.numeric(.)),
    usaid_supported = as_factor(usaid_supported),
    # replace country names from unweighted data with names from GH list
    # provided by Karishma, otherwise keep the country name
    country = as.character(if_else(is.na(country_ghlist) == FALSE,
      country_ghlist, country))) %>%
  select(country, iso, indicator, usaid_supported, year, value) %>%
  distinct()

# What percentage of the data includes countries supported by USAID?

countries_by_support <- selected_data %>%
  select(country, usaid_supported) %>%
  group_by(usaid_supported, country) %>%
  distinct()

tabyl(countries_by_support$usaid_supported)

# ~97% (0.97) of countries in this dataset (66) are supported by USAID
# and 2 are not. It seems like we have class imbalance here. Is this an
# issue when only describing the data?

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
  group_by(country, indicator, usaid_supported, year) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population),
    weight = as.numeric(sum(value * population)))

# HSC index in USAID vs non-USAID countries

hsc <- full_data %>%
  filter(indicator == "uhc_service_coverage_index") %>%
  ungroup() %>%
  select(usaid_supported, year, value, weight) %>%
  group_by(usaid_supported, year) %>%
  summarize(value = value,
            weight = weight,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2))

# Life Expectancy at Birth in USAID vs non-USAID countries

life_exp <- full_data %>%
  filter(indicator == "life_expectancy_at_birth_all_sexes") %>%
  ungroup() %>%
  select(usaid_supported, year, value, weight) %>%
  group_by(usaid_supported, year) %>%
  summarize(value = value,
            weight = weight,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2))

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in USAID vs non-USAID countries over time?

ggplot(hsc, aes(
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
      "No" = "#cfcdc9"), # Light Grey
    labels = NULL) +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 10,
                                   color = "#505050"),
        axis.text.y = element_text(size = 10,
                                   color = "#505050"),
        plot.title = element_markdown(size = 14,
                                      color = "#202020"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "GAINS IN <b>POPULATION WEIGHTED HSC INDEX</b> IN <br>
            <span style='color: #002a6c;'>USAID</span> AND
            <span style='color: #cfcdc9;'>NON-USAID</span> LOWER-INCOME COUNTRIES")

si_save(outputs$hsc_fig)

# What has been the change in Life Expectancy at Birth in USAID vs non-USAID countries over time?

ggplot(life_exp, aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_line() +
  si_style_ygrid() +
  scale_x_continuous(
    limits = c(1960, 2020),
    breaks = c(
      1960, 1962, 1964, 1966, 1968,
      1970, 1972, 1974, 1976, 1978,
      1980, 1982, 1984, 1986, 1988,
      1990, 1992, 1994, 1996, 1998,
      2000, 2002, 2004, 2006, 2008,
      2010, 2012, 2014, 2016, 2018, 2020)) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    minor_breaks = c(15, 25, 35, 45, 55, 65, 75)) +
  scale_color_manual(
    values = c(
      "Yes" = usaid_blue,
      "No" = "#cfcdc9"), # Light Grey
    labels = NULL) +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 10,
                                   color = "#505050"),
        axis.text.y = element_text(size = 10,
                                   color = "#505050"),
        plot.title = element_markdown(size = 14,
                                      color = "#202020"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "INCREASES IN <b>POPULATION WEIGHTED LIFE EXPECTANCY AT BIRTH</b> IN <br>
            <span style='color: #002a6c;'>USAID</span> AND
            <span style='color: #cfcdc9;'>NON-USAID</span> LOWER-INCOME COUNTRIES")

si_save(outputs$lifeexp_fig)

# save data --------------------------------------------------------------------

write_excel_csv(hsc, outputs$hsc_data)
write_excel_csv(life_exp, outputs$lifeexp_data)

# end