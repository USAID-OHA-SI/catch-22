# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  check 95s value for FO request
# REF ID:   b07d367b 
# LICENSE:  MIT
# DATE:     2025-01-14
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)
library(mindthegap)


# GLOBAL VARIABLES --------------------------------------------------------

#load data
df_unaids <- load_unaids(pepfar_only = TRUE)

#review structure
str(df_unaids)


# Countries achievn 95s

key_ind_95s <- c("Percent Known Status of PLHIV",
                 "Percent on ART of PLHIV",
                 "Percent VLS of PLHIV",
                 "Percent on ART with Known Status",
                 "Percent VLS on ART")

df_tt <- df_unaids %>%
  dplyr::filter(year >= 2021,
                # country == {cntry},
                indicator %in% key_ind_95s)

df_tt <- df_tt %>%
  dplyr::select(year, country, indicator, age, sex, estimate, lower_bound, upper_bound) %>%
  tidyr::unite(group, c(sex, age), sep = " ") %>%
  dplyr::mutate(group = stringr::str_remove(group, "All "),
                group = ifelse(group == "0-14", "Peds <15", group),
                indicator = factor(indicator, key_ind_95s),
                bounds = glue::glue("[{lower_bound}%-{upper_bound}%]")) %>%
  dplyr::relocate(bounds, .after = estimate) %>%
  dplyr::filter(group %in% "All") %>%
  dplyr::arrange(group, indicator) %>%
  calc_95_goals()

df_viz <- df_tt %>%
  dplyr::filter(base %in% c("Both", "Relative")) %>%
  dplyr::mutate(achieved = estimate >= goal_rate) %>%
  dplyr::select(-c(set, base, achv_plhiv, achv_relative, lower_bound, upper_bound))


df_viz %>%
  filter(achieved == TRUE) %>% 
  group_by(country, year) %>%
  #mutate(achieved= ifelse(achieved == TRUE, 1,0)) %>% 
  #  mutate(achieved_all = sum(achieved == TRUE) == 3) %>% 
  mutate(achieved_all = ifelse(sum(achieved) == 3, TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(achieved_all == TRUE) %>% 
  count(country, year)