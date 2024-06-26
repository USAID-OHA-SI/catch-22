# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Dec 2021 Call to IP/OU -treatment scale up since PEPFAR start for 
# LICENSE:  MIT
# DATE:     2021-12-07
# NOTE: adapted from agitprop/Scripts/archive/04b_usaid_tx_trends.R

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
library(janitor)
library(lubridate)

source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd()

#Archived MSD
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

# MUNGE -------------------------------------------------------------------

#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

df_tx <- df %>% 
  bind_rows(df_arch) %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("TX_CURR", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= curr_fy) %>% 
  group_by(fiscal_year, fundingagency, indicator) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup()

df_tx <- df_tx %>% 
  rename(period = fiscal_year, value = cumulative) %>% 
  mutate(period = str_replace(period, "20", "FY")) %>% 
  arrange(indicator, period) %>% 
  mutate(source = "MSD")

df_tx <- df_tx %>% 
  mutate(bar_alpha = case_when(period == max(period) & str_detect(curr_pd, "Q4", negate = TRUE) ~ .6,
                               TRUE ~ 1),
         year = glue("20{str_sub(period, 3, 4)}") %>% as.integer)

df_tx <- df_tx %>% 
  mutate(ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving antiretroviral therapy",
                               TRUE ~ "Newly enrolled on antiretroviral therapy"))

title_info <- df_tx %>% 
  filter(indicator == "TX_CURR",
         period %in% c(min(period), max(period))) %>% 
  select(year, value) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         yrs = year - lag(year)) %>% 
  filter(year == max(year))

df_tx %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(alpha = bar_alpha, fill = ind_label),
           position = "identity") +
  geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_tx$period) %>% length())+
  scale_fill_manual(values = c(genoa_light, genoa)) +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("USAID HAS ADDED {title_info$added} PATIENTS ONTO TREATMENT IN THE PAST {title_info$yrs} YEARS"),
       caption = glue("Source: {msd_source} (including FY15-18)",
                      "USAID SI Analytics",
                      "Call with IPs/OUs Dec 2021", .sep = " | ")) +
  si_style_nolines()

si_save("Graphics/CTIP-CWOU_tx_trends_usaid.svg")

#data point for context
df_tx %>% 
  filter(period == max(period),
         indicator == "TX_CURR") %>%
  select(indicator, value) %>% 
  mutate(value = clean_number(value, 1))
