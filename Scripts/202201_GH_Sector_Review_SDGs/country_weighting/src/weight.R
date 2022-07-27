# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2022-07-27

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
  

  font_add_google("Source Sans Pro")
  showtext::showtext_auto()

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"

# set inputs and outputs -------------------------------------------------------
  
folder_path <- "catch-22/Dataout/"
  
inputs <- list(
  country_names = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/hand/country_crossmap.csv", 
  template_hsc_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/template_hsccov_usaid.pptx", 
  template_lexp_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/template_lifeexp_usaid.pptx", 
  template_hsc_pepfar = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/template_hsccov_pepfar.pptx",  
  template_hsc_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/template_hsccov_usaid.pptx" )

outputs <- list(
  full_data = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/GH_scorecard_indicators_weights_2022-07-21.csv",
  hsc_fig_uasid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavg_usaid.svg",
  lifeexp_fig_usaid = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavg_usaid.svg",
  hsc_fig_pepfar = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsc_weightedavg_pepfar.svg",
  lifeexp_fig_pepfar = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/lifeexp_weightedavg_pepfar.svg"
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
  # keep only life expectancy and UHC indicators
  # keep only countries in list provided by Karishma
  filter(indicator %in% c(
    "uhc_service_coverage_index",
    "life_expectancy_at_birth_both_sexes_years",
    "uhc_subindex1_capacity_access",
    "uhc_subindex2_ncd",
    "uhc_subindex3_mchn",
    "uhc_subindex4_id"), 
    country %in% names$country_ghlist) %>%
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
         pepfar, usaid_supported, income_group, indicator,
         year, value, goal, date_data_pulled, ref_link) %>%
  distinct()

# What percentage of the data includes countries supported by PEPFAR?
country_by_support <- selected_data %>%
  select(country, pepfar, income_group) %>%
  group_by(country, pepfar, income_group) %>%
  distinct()

countries_by_support <- tabyl(country_by_support$pepfar)

# ~73% (0.727) of countries in this dataset (48) are supported by PEPFAR
# and 18 are not. It seems like we have class imbalance here

# of the countries in each income group, how many are PEPFAR supported?
countries_by_support_income <- tabyl(country_by_support, pepfar, income_group)

# Among Low income selected countries of interest
# 12 are PEPFAR and 6 are non-PEPFAR

pop_data <- world_pop %>%
  pivot_longer(
    cols = year_1960:year_2020,
    names_to = "year",
    values_to = "population") %>%
  select(country, year, population) %>%
  # filter data by only countries of interest
  filter(country %in% names$country_worldpop) %>%
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
    "country","year")) %>%
  drop_na(population) %>%
  distinct() %>%
  select(idea_region, usaid_region, who_region, country, iso, cntry_group,
         pepfar, usaid_supported, income_group, indicator, year, value, goal,
         population, date_data_pulled, ref_link) %>%
  group_by(idea_region, usaid_region, who_region, country, iso,
           income_group, indicator, year) %>%
  mutate(
    value = as.numeric(value),
    population = as.numeric(population), 
    weighted_value = round_half_up(weighted.mean(value, population), 2)) %>%
  distinct()

# visualize --------------------------------------------------------------------

# What has been the change in HSC index in PEPFAR vs non-PEPFAR countries over time?
# filter by indicator and lower income
uhc_low_pepfar <- 
  ggplot(uhc_low_pepfar <- full_data %>%
         filter(indicator =="uhc_service_coverage_index" , 
                income_group == "Low Income Country (World Bank Classification)"), 
  aes(
  x = year,
  y = weighted_value,
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
# filter by indicator and lower income
uhc_low_usaid  <- 
  ggplot(uhc_low_usaid <- full_data %>%
         filter(indicator =="uhc_service_coverage_index" , 
                income_group == "Low Income Country (World Bank Classification)")
                , aes(
  x = year,
  y = weighted_value,
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
lexp_low_pepfar_fig <- 
  ggplot(lexp_low_pepfar <- full_data %>%
         filter(indicator =="life_expectancy_at_birth_both_sexes_years" , 
                income_group == "Low Income Country (World Bank Classification)")
       , aes(
  x = year,
  y = weighted_value,
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
lexp_low_usaid_fig <- 
  ggplot(lexp_low_usaid <- full_data %>%
         filter(indicator =="life_expectancy_at_birth_both_sexes_years" , 
                income_group == "Low Income Country (World Bank Classification)")
       , aes(
  x = year,
  y = weighted_value,
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
uhc_pepfar_dml <- dml(grid.arrange(uhc_low_pepfar,
                              nrow = 1, widths = 12.5))

uhc_pepfar_template <- read_pptx(path = inputs$template_hsc_pepfar)

uhc_pepfar_complete <- ph_with(uhc_pepfar_template, uhc_pepfar_dml,
                          location = ph_location(
                            left = 0.5,
                            top = 2.0,
                            width = 12.5,
                            height = 4.55,
                            newlabel = ""))

print(uhc_pepfar_complete,
      target = "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/output/hsccov_pepfar.pptx")


# # USAID
# hsc_usaid_dml <- dml(grid.arrange(uhc_low_usaid,
#                                    nrow = 1, widths = 12.75))
# 
# # Life Expectancy
# 
# # PEPFAR
# lexp_pepfar_dml <- dml(grid.arrange(lexp_low_pepfar_fig,
#                                    nrow = 1, widths = 12.75))
# 
# # USAID
# lexp_usaid_dml <- dml(grid.arrange(lexp_low_usaid_fig,
#                                   nrow = 1, widths = 12.75))

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(full_data, outputs$full_data)

# end