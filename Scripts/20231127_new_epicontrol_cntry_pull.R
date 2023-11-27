# PROJECT:  catch-22
# AUTHOR:   K. Srikanth
# PURPOSE:  Epi control trends - identify new epi countries between 2021 and 2022 for AFR bureau request
# REF ID:   25bd54d2 
# LICENSE:  MIT
# DATE:     2023-11-27
# NOTE:     derived from agitprop/08_epi_ann_unaids-pepfar-epi-control.R

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gisr)
library(googlesheets4)
library(readxl)
library(stringi)
# remotes::install_github("https://github.com/USAID-OHA-SI/mindthegap.git", ref = "unaids-data")
library(mindthegap) #remotes::install_github("USAID-OHA-SI/mindthegap")


# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()

ref_id <- "25bd54d2"

# IMPORT ------------------------------------------------------------------

#pull UNAIDS estimates data from mindthegap package
df_epi <- pull_estimates(TRUE)


# MUNGE -------------------------------------------------------------------

#epi control defined as when new infections < total deaths to PLHIV + total deaths are declining

#grab new infections + total deaths
df_epi_pepfar <- df_epi %>% 
  filter(
    #stat == "est",
    age == "All",
    sex == "All",
    indicator %in% c("Number New HIV Infections", "Total deaths to HIV Population")) %>%
  # semi_join(pepfar_country_list, by = c("iso" = "countryname_iso")) %>%
  select(year, country, indicator, estimate) %>%
  arrange(country, indicator, year)  

#create variables for infection < deaths, ratio, and epi control logical
df_epi_pepfar <- df_epi_pepfar %>% 
  pivot_wider(names_from = "indicator", values_from = "estimate") %>%
  rename(infections = `Number New HIV Infections`,
         total_deaths = `Total deaths to HIV Population`) %>% 
  # left_join(total_deaths, by = c("year", "country")) %>% 
  group_by(country) %>% 
  mutate(declining_deaths = total_deaths - lag(total_deaths, order_by = year) <= 0) %>% 
  ungroup() %>% 
  mutate(infections_below_deaths = infections < total_deaths,
         ratio = infections / total_deaths,
         direction_streak = sequence(rle(declining_deaths)$lengths),
         epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) 

#pivot columns long
df_epi_pepfar <- df_epi_pepfar %>% 
  pivot_longer(c(infections, total_deaths), names_to = "indicator") %>% 
  arrange(country, indicator, year) %>% 
  mutate(value_mod = ifelse(indicator == "total_deaths", -value, value),
         fill_color = ifelse(indicator == "total_deaths", old_rose, denim))

#grab # of countries reaching epi control in 2022
epi_cntry <- df_epi_pepfar %>% 
  filter(year == max(year),
         indicator == "infections",
         epi_control == TRUE) %>%
  # country %in% sel_cntry) %>% 
  arrange(desc(value)) %>%
  pull(country)

#for viz
df_viz_cntry <- df_epi_pepfar %>% 
  filter(country %in% epi_cntry) %>% 
  mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
         max_plot_pt = max(value),
         lab_pt = case_when(year == max(year) ~ value_mod),
         country = factor(country, epi_cntry)) 

#pull countries that did not reach epi control in 2021 but did reach in 2022 (tab1)
new_epi_cntry <- df_viz_cntry %>% 
  filter(year %in% c(2021,2022)) %>% 
  count(year, country, epi_control) %>% 
  pivot_wider(names_from = "year", values_from = "epi_control") %>% 
  rename(epicontrol_2021 = `2021`,
         epicontrol_2022 = `2022`) %>% 
  mutate(flag_new_epi = ifelse(epicontrol_2021 == FALSE & epicontrol_2022 == TRUE, TRUE, FALSE)) %>%
  filter(flag_new_epi == TRUE) %>% 
  pull(country)

#filter data to just new epi countries to see new infections/total deaths stats (tab2)
df_viz_cntry %>% 
  filter(country %in% new_epi_cntry,
         year %in% c(2021, 2022)) %>% 
  select(year, country, declining_deaths, infections_below_deaths, epi_control, indicator, value) %>% 
  pivot_wider(names_from = "indicator")