# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-25

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
  hsc_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavgfig.svg"),
  lifeexp_fig = here("catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavgfig.svg")
  )

# munge ------------------------------------------------------------------------

# read in country names from all sources for consistency
names <- read_csv(inputs$country_names,
  show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    across(.cols = country_ghlist:usaid_supported_list, ~ as.character(.)))

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
    "life_expectancy_at_birth_both_sexes_years",
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
      country_ghlist, country)),
    # replace countries with missing usaid_supported value with "No"
    # since I cross-checked them with the list in names and all were not
    # USAID supported
    usaid_supported = as.character(
      if_else(is.na(usaid_supported) == TRUE,
              "No", usaid_supported)),
    # Togo and Eritrea are supported by USAID and are incorrectly listed as not
    # USAID supported
    # Sources:
    # Eritrea: https://pdf.usaid.gov/pdf_docs/PNADA279.pdf
    # Togo: it is included in the USAID GH countries .xlsx list provided by
    #       Karishma for this analysis
    usaid_supported = as.character(
      if_else(iso %in% c("ERI", "TGO") == TRUE,
              "Yes", usaid_supported)),
    # American Samoa is listed both as "Upper Middle Income Country (World Bank Classification)"
    # and "Lower Middle Income Country (World Bank Classification)"
    # so I'm keeping the correct grouping as of this site: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
    income_group = as.character(
      if_else(iso == "ASM" & income_group == "Lower Middle Income Country (World Bank Classification)",
              "Upper Middle Income Country (World Bank Classification)", income_group)),
    ) %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  # Anguilla, Bonaire, Sint Eustatius and Saba,Cook Islands,
  # French Guiana, Guadeloupe, Guernsey, Falkland Islands (Malvinas),
  # Holy See, Jersey, Kosovo (under UNSC res. 1244),
  # Martinique, Mayotte, Montserrat, Niue, Réunion, Saint Helena,
  # Saint Pierre and Miquelon, Tokelau, Venezuela, Wallis and Futuna Islands,
  # Western Sahara are missing income_group, dropping from analysis as these are
  # territories and so have no WHO income classification
  # with the exception of Venezuela which has no WHO income classification
  # as of July 2021 due to pending release of offical statistics
  # https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
  drop_na(income_group) %>%
  distinct()

# What percentage of the data includes countries supported by PEPFAR?
country_by_support <- selected_data %>%
  select(country, pepfar, usaid_supported, income_group) %>%
  group_by(country, pepfar, usaid_supported, income_group) %>%
  distinct()

countries_by_pepfar <- tabyl(country_by_support$pepfar)

# ~24% (0.235) of countries in this dataset (51) are supported by PEPFAR
# and 166 are not. It seems like we have class imbalance here too

# of the countries in each income group, how many are PEPFAR supported?
countries_by_support_income <- tabyl(country_by_support, pepfar, income_group)

# Among Low income and LMI countries,
# we have comparable numbers of PEPFAR and non-PEPFAR

# How about USAID support?

countries_by_usaid <- tabyl(country_by_support$usaid_supported)

# ~51% (0.506) of countries in this dataset (110) are supported by USAID
# and 107 are not.

# of the countries in each income group, how many are USAID supported?
countries_by_usaid_income <- tabyl(country_by_support, usaid_supported, income_group)

# Among Low income countries,
# 2 countries (Democratic People's Republic of Korea,
#              Guinea-Bissau ) are not USAID supported and 25 are
# We definitely have imbalance here and it would be prudent to show the data points
# in the visual

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
  group_by(country, year) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population),
    weight = as.numeric(sum(value*population))) %>%
  distinct()

# HSC index in PEPFAR vs non-PEPFAR countries
uhc_low_pepfar <- full_data %>%
  filter(indicator == "uhc_service_coverage_index",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(pepfar, year) %>%
  summarize(pepfar = pepfar,
            year = year,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2)) %>%
  distinct()

# Life Expectancy at Birth in PEPFAR vs non-PEPFAR
lifeexp_low_pepfar <- full_data %>%
  filter(indicator == "life_expectancy_at_birth_both_sexes_years",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(pepfar, year) %>%
  summarize(pepfar = pepfar,
            year = year,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2)) %>%
  distinct()

# HSC index in USAID vs non-USAID countries
uhc_low_usaid <- full_data %>%
  filter(indicator == "uhc_service_coverage_index",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(country, usaid_supported, year) %>%
  summarize(country = country,
            usaid_supported = usaid_supported,
            unweighted_avg = mean(value),
            weighted_avg = weighted.mean(value, weight)) %>%
  distinct()

# Life Expectancy at Birth in USAID vs non-USAID
lifeexp_low_usaid <- full_data %>%
  filter(indicator == "life_expectancy_at_birth_both_sexes_years",
         income_group == "Low Income Country (World Bank Classification)") %>%
  group_by(usaid_supported, year) %>%
  summarize(usaid_supported = usaid_supported,
            year = year,
            unweighted_avg = round_half_up(mean(value), 2),
            weighted_avg = round_half_up(weighted.mean(value, weight), 2)) %>%
  distinct()

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in PEPFAR vs non-PEPFAR countries over time?
ggplot(uhc_low_pepfar, aes(
  x = year,
  y = weighted_avg,
  group = pepfar,
  color = pepfar)) +
  geom_line() +
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
        plot.title = element_markdown(family = "Source Sans Pro",
                                      size = 14,
                                      color = "#202020"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Data from GH_scorecard_indicators_2022-07-21.csv",
    title = "GAINS IN <b>POPULATION WEIGHTED HEALTH SERVICE COVERAGE INDEX</b> SINCE 2000
            <span style='color: #002a6c;'>PEPFAR</span> AND <span style='color: #6c6463;'>NON-PEPFAR</span> LOWER-INCOME COUNTRIES")

si_save(outputs$hsc_fig)

# What has been the change in Life Expectancy at Birth in PEPFAR vs non-PEPFAR countries over time?
ggplot(lifeexp_low_pepfar, aes(
  x = year,
  y = weighted_avg,
  group = pepfar,
  color = pepfar)) +
  geom_line() +
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
    limits = c(0, 65),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 65)) +
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
         plot.title = element_markdown(family = "Source Sans Pro",
                                       size = 14,
                                       color = "#202020"),
         legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Data from GH_scorecard_indicators_2022-07-21.csv",

      "Dashed red line indicates first PEPFAR authorization in 2003",
    title = "GAINS IN <b>POPULATION WEIGHTED HEALTH SERVICE COVERAGE INDEX</b> SINCE 2000
            <span style='color: #002a6c;'>PEPFAR</span> AND <span style='color: #6c6463;'>NON-PEPFAR</span> LOWER-INCOME COUNTRIES")

si_save(outputs$lifeexp_fig)

# What has been the change in HSC index in USAID vs non-USAID countries over time?
ggplot(uhc_low_usaid, aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_smooth() +
  geom_point() +
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
        plot.title = element_markdown(family = "Source Sans Pro",
                                      size = 14,
                                      color = "#202020"),
        legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Data from GH_scorecard_indicators_2022-07-21.csv",
    title = "GAINS IN <b>POPULATION WEIGHTED HEALTH SERVICE COVERAGE INDEX</b> SINCE 2000
            <span style='color: #002a6c;'>USAID</span> AND <span style='color: #6c6463;'>NON-USAID SUPPORTED</span> LOWER-INCOME COUNTRIES")

# What has been the change in Life Expectancy at Birth in PEPFAR vs non-PEPFAR countries over time?
ggplot(lifeexp_low_pepfar, aes(
  x = year,
  y = weighted_avg,
  group = usaid_supported,
  color = usaid_supported)) +
  geom_line() +
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
    limits = c(0, 65),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 65)) +
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
         plot.title = element_markdown(family = "Source Sans Pro",
                                       size = 14,
                                       color = "#202020"),
         legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Data from GH_scorecard_indicators_2022-07-21.csv",

    "Dashed red line indicates first PEPFAR authorization in 2003",
    title = "GAINS IN <b>POPULATION WEIGHTED LIFE EXPECTEANCY AT BIRTH </b> SINCE 1960
            <span style='color: #002a6c;'>USAID</span> AND <span style='color: #6c6463;'>NON-USAID SUPPORTED</span> LOWER-INCOME COUNTRIES")

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(full_data, outputs$full_data)

# end