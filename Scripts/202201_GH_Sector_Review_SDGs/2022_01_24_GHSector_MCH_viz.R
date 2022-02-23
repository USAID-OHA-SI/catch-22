# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | N. Petrovic | USAID
# PURPOSE:  GH Sector Review - MCH visuals
# LICENSE:  MIT
# DATE:     2022-01-24
# UPDATED:  
# Note: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(waffle)
library(readxl)
library(janitor)
library(purrr)

# IMPORT ------------------------------------------------------------------

 #https://sdg-tracker.org/good-health

df_under5 <- si_path() %>% 
  return_latest("under5child-mortality") %>% 
  read_csv()   

df_nmr <- si_path() %>% 
  return_latest("neonatal-mortality") %>% 
  read_csv()   

df_mmr <- si_path() %>% 
  return_latest("maternal-mortality") %>% 
  read_csv()   

df_contra <- si_path() %>% 
  return_latest("unmet-need-for-contraception") %>% 
  read_csv()   


df_ds <- si_path() %>% 
  return_latest("FP2020_2020_FullEstimate_Tables_ONLINE") %>% 
  read_xlsx(sheet = "Indicator 4_demand satisfied", skip = 8) %>% 
  select(2:11) %>% 
  rename(country = ...2) %>% 
  pivot_longer(-c(country), names_to = "year", values_to = "demand_satisfied")

#https://www.usaid.gov/global-health/health-areas/maternal-and-child-health/priority-countries
usaid_mch <- c("Afghanistan",
               "Bangladesh",
               "Myanmar",
               "Democratic Republic of Congo",
               "Ethiopia",
               "Ghana",
               "Haiti",
               "India",
               "Indonesia",
               "Kenya",
               "Liberia",
               "Madagascar",
               "Malawi",
               "Mali",
               "Mozambique",
               "Nepal",
               "Nigeria",
               "Pakistan",
               "Rwanda",
               "Senegal",
               "South Sudan",
               "Tanzania",
               "Uganda",
               "Yemen",
               "Zambia")

maternal_source <- "World Health Organization (via World Bank)"

child_source <- "UN IGME SDG Progress 2019"


#MUNGE ---------------------------------------------------------------------

# df_under5 <- df_under5 %>% 
#   clean_names() %>%
#   rename(country = entity,
#          iso = code,
#          value = mortality_rate_under_5_per_1_000_live_births) %>% 
#   mutate(indicator = "under 5 mortality",
#          usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"),
#          goal = 25) %>% 
#   filter(year == 2019,
#          usaid == "USAID") 
#   
# df_nmr <-df_nmr %>% 
#   clean_names() %>%
#   rename(country = entity,
#          iso = code,
#          value = mortality_rate_neonatal_per_1_000_live_births) %>% 
#   mutate(indicator = "neonatal mortality",
#          usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"),
#          goal = 12) %>% 
#   filter(year == 2019,
#          usaid == "USAID")
# 
# 
# df_mmr <-df_mmr %>% 
#   clean_names() %>%
#   rename(country = entity,
#          iso = code,
#          value = maternal_mortality_ratio_modeled_estimate_per_100_000_live_births) %>% 
#   mutate(indicator = "maternal mortality",
#          usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"),
#          goal = 140) %>% 
#   filter(year == 2017,
#          usaid == "USAID")


sdg_reshape <- function(data) {
  
  data_clean <- data %>% 
    clean_names() %>%
    rename(country = entity,
           iso = code) 
  
  indic_name <- names(data_clean)[4]
  
  data_clean <- data_clean %>% 
    mutate(indicator = indic_name,
           usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"),
           indicator = recode(indicator, "maternal_mortality_ratio_modeled_estimate_per_100_000_live_births" = "maternal_mortality",
                                        "mortality_rate_neonatal_per_1_000_live_births" = "neonatal_mortality",
                              "mortality_rate_under_5_per_1_000_live_births" = "under5_mortality"))
  
  most_recent_data <- ifelse(data_clean$indicator == "maternal_mortality", 2017, 2019)
  
  data_clean <- data_clean %>% 
    filter(year == most_recent_data,
           usaid == "USAID") %>% 
    mutate(goal = case_when(indicator == "maternal_mortality" ~ 140,
                            indicator == "neonatal_mortality" ~ 12,
                            indicator == "under5_mortality" ~ 25)) 
  
  
  names(data_clean)[4] <- 'value'
  
  return(data_clean)
  
}

#APPLY RESHAPING FUNCTION
df_mmr_clean <- sdg_reshape(df_mmr)
df_nmr_clean <- sdg_reshape(df_nmr)
df_under5_clean <- sdg_reshape(df_under5)

#BIND ROWS
df_viz <- df_mmr_clean %>% 
  bind_rows(df_nmr_clean, df_under5_clean)

#Munge Contraceptive Prevalence data
df_contra_viz <- df_contra %>% 
  clean_names() %>% 
  rename(country = entity,
         iso = code) %>% 
  filter(country %in% usaid_mch) %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  mutate(country_year=paste(country,"(",year, ")", sep="")) %>% 
  rename(unmet_need =unmet_need_for_contraception_percent_of_married_women_ages_15_49)

#Munge Demand met data
df_ds_viz <- df_ds %>% 
  mutate(year = recode(year, "2020.5" = 2020),
         country = recode(country, "DR Congo" = "Democratic Republic of Congo"),
         goal = 0.75) %>% 
  filter(year == 2020,
         country %in% usaid_mch)

setdiff(usaid_mch, df_ds1$country)

# VIZ ---------------------------------------------

df_viz <- df_viz %>% 
  group_by(country, indicator) %>% 
  mutate(gap = goal - value,
         gap_bar = case_when(value > goal ~ value),
         dot_color = if_else(gap >= 0, scooter, old_rose)) 

#Contraceptive Viz
df_contra_viz <- df_contra_viz %>% 
  group_by(country_year, unmet_need) %>% 
  mutate(gap = 0 - unmet_need,
         gap_bar = case_when(unmet_need > 0 ~ unmet_need),
         dot_color = if_else(gap >= 0, scooter, old_rose)) 

#Demand met Viz
df_ds_viz <- df_ds_viz %>% 
  group_by(country, demand_satisfied) %>% 
  mutate(gap = goal - demand_satisfied,
         gap_bar = case_when(demand_satisfied < goal ~ demand_satisfied),
         dot_color = if_else(gap >= 0, genoa, scooter),
         dot_color = if_else(demand_satisfied <= 0.5, old_rose, dot_color)) 

# contraceptive <- df_contra_viz %>%   
#   #filter(indicator == "under5_mortality") %>% 
#   ggplot(aes(unmet_need, reorder(country_year, unmet_need), color = genoa)) + 
#  # geom_vline(xintercept = 0, linetype = "dashed") +
#  # geom_linerange(aes(xmax = gap_bar, xmin = 0), color = "gray90",
#           #       size = 2.5, na.rm = TRUE) +
#   geom_point(size = 5, alpha = .8, na.rm = TRUE) +
#   scale_y_reordered(limits = rev) +
#   scale_color_identity() +
#   # facet_wrap(~indicator, scales = "free_y") +
#   si_style_xgrid() +
#   labs(x = NULL, y = NULL, color = NULL,
#        title = "Percent of Unmet Need for Contraception Among Women 15-49 (most recent available data)")

#Order demand satisfied by maternal mortality
df_mmr_join <- df_mmr_clean %>% 
  select(country, indicator, value)

#Demand Satisfied Viz
df_ds_viz <- df_ds_viz %>% 
  left_join(df_mmr_join, by = c('country')) %>% 
  pivot_wider(names_from = indicator, values_from = value)

demand_satisfied <- df_ds_viz %>%   
  #filter(indicator == "under5_mortality") %>% 
  ggplot(aes(demand_satisfied, reorder(country, maternal_mortality), color = dot_color)) + 
   geom_vline(xintercept = 0.75, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
   geom_linerange(aes(xmax = goal, xmin = gap_bar), color = "gray90",
         size = 2.5, na.rm = TRUE) +
  geom_point(size = 5, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(label = percent) +
  scale_color_identity() +
  # facet_wrap(~indicator, scales = "free_y") +
  si_style_xgrid() +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Percentange of women whose demand is satisfied by a modern contraceptive method in 2020")
 
#Maternal Mortality
maternal <- df_viz %>%   
  filter(indicator == "maternal_mortality") %>% 
  ggplot(aes(value, reorder(country, value), color = dot_color)) + 
  geom_vline(xintercept = 140, linetype = "dashed") +
  geom_linerange(aes(xmax = gap_bar, xmin = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 5, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(limit = c(0, 1200)) +
  scale_color_identity() +
  # facet_wrap(~indicator, scales = "free_y") +
  si_style_xgrid() +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Maternal Mortality Ratio per 100k live births (2017)")

maternal + demand_satisfied + plot_annotation(
  caption = glue("Source: {maternal_source}
                      USAID SI Analytics | GH Sector Review"),
  title = "Only 1 USAID Priority Country has reached the SDG Maternal Mortality goal of 140 or fewer deaths per 100,000 live births")

si_save("Graphics/maternal-sdg-progress-contraceptive-demand.svg")

# ------------------


#Under 5 Mortality 
under5viz <- df_viz %>%   
  filter(indicator == "under5_mortality") %>% 
  ggplot(aes(value, reorder(country, value), color = dot_color)) + 
  geom_vline(xintercept = 25, linetype = "dashed") +
  geom_linerange(aes(xmax = gap_bar, xmin = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(limit = c(0, 120)) +
  scale_color_identity() +
 # facet_wrap(~indicator, scales = "free_y") +
  si_style_xgrid() +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Under 5 Mortality Rate per 1,000 live births (2019)")

#Neonatal Mortality  
neonatal <- df_viz %>%   
  filter(indicator == "neonatal_mortality") %>% 
  ggplot(aes(value, reorder(country, value), color = dot_color)) + 
  geom_vline(xintercept = 12, linetype = "dashed") +
  geom_linerange(aes(xmax = gap_bar, xmin = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 5, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_color_identity() +
  # facet_wrap(~indicator, scales = "free_y") +
  si_style_xgrid() +
  labs(x = NULL, y = NULL, color = NULL,
       title = "Neonatal Mortality Rate per 1,000 live births (2019)")

viz_child <- under5viz + neonatal + plot_annotation(
  caption = glue("Source: {child_source}
                      USAID SI analytics | GH Sector Review"),
  title = "Only 1 USAID MCHN-priority country has reached the SDG 3.2 Target to reduce under-5 child mortality to less than 25 deaths per 1,000 live births by 2030")

si_save("Graphics/child-sdg-progress.svg")









