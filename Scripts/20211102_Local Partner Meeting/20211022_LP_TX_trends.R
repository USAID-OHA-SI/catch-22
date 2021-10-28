# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  LP meeting - TX_CURR trends
# LICENSE:  MIT
# DATE:     2021-10-22
# note: derived from agitprop/04b_usaid_tx_trends.R


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
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")


#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

#clean number
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

#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
      clean_names() %>% 
      rename(mech_code = mechanism_id) %>% 
      mutate(mech_code = as.character(mech_code),
             partner_type = case_when(partner_type == "Regional" ~ "Local",
                                      partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type)) 

# TX_CURR / TX_NEW -------------------------------------------------------------------

df_tx <- df %>% 
  bind_rows(df_arch) %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("TX_CURR", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= curr_fy) %>% 
  group_by(mech_code) %>% 
  left_join(df_partner, by = c("mech_code")) %>% 
  group_by(fiscal_year, fundingagency, indicator, partner_type) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(partner_type != "TBD") %>% 
  pivot_wider(names_from = partner_type, values_from = cumulative) %>% 
  mutate(Total = International + Local, #pivot to get totals and shares
         share = Local / Total) %>% 
  pivot_longer(cols = International:Total, names_to = "partner_type")

#changes fy to period
df_tx <- df_tx %>% 
  rename(period = fiscal_year) %>% 
  mutate(period = str_replace(period, "20", "FY")) %>% 
  arrange(partner_type, period) %>% 
  mutate(source = "MSD")

#make FY21 0.6 alpha because it is imcomplete
df_tx <- df_tx %>% 
  mutate(bar_alpha = case_when(period == max(period) & str_detect(curr_pd, "Q4", negate = TRUE) ~ .6,
                               TRUE ~ 1),
         year = glue("20{str_sub(period, 3, 4)}") %>% as.integer)

#add labels
df_tx_curr <- df_tx %>% 
  filter(partner_type != "International",
         indicator == "TX_CURR") %>% 
  mutate(ind_label = case_when(partner_type == "Local" ~ "Local Partner contribution to treatment portfolio",
                               TRUE ~ "Total USAID Treatment Portfolio"))

df_tx_new <- df_tx %>% 
  filter(partner_type != "International",
         indicator == "TX_NEW") %>% 
  mutate(ind_label = case_when(partner_type == "Local" ~ "Local Partner contribution to TX_NEW",
                               TRUE ~ "Total newly enrolled on antiretroviral therapy"))

#grab information for titles
title_info_tx_curr <- df_tx_curr %>% 
  filter(partner_type == "Local",
         indicator == "TX_CURR",
         period %in% c(min(period), max(period))) %>% 
  select(year, indicator, value, share) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         yrs = year - lag(year),
         value = value %>%  clean_number(1),
         share = percent(round(share, 2))) %>% 
  filter(year == max(year))

title_info_txnew <- df_tx_new %>% 
  filter(partner_type == "Local",
         indicator == "TX_NEW",
         period %in% c(min(period), max(period))) %>% 
  select(year, indicator, value, share) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         yrs = year - lag(year),
         value = value %>%  clean_number(1),
         share = percent(round(share, 2))) %>% 
  filter(year == max(year))

# VIZ -----------------------------------------

#TX_CURR
df_tx_curr %>% 
  arrange(desc(partner_type)) %>% 
  mutate(share = ifelse(partner_type == "Local", share, NA)) %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(alpha = bar_alpha, fill = ind_label),
           position = "identity") +
  geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_tx$period) %>% length())+
  geom_text(aes(label = percent(share, 1)), vjust = -1, 
            size = 10/.pt, 
            label.size = NA, family = "Source Sans Pro") +
  scale_fill_manual(values = c(genoa, genoa_light)) +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("LOCAL PARTNERS HAVE ADDED {title_info_tx_curr$added} PATIENTS ONTO TREATMENT IN THE PAST {title_info_tx_curr$yrs} YEARS, MAKING UP OVER
                    {title_info_tx_curr$share} OF USAID'S TREATMENT PORTFOLIO IN FY21Q3"),
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines()

si_save("Graphics/partner_tx_trends_usaid.svg")

#data point for context
df_tx_curr %>% 
  filter(period == max(period),
         partner_type == "Total") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

df_tx_curr %>% 
  filter(period == max(period),
         partner_type == "Local") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

#TX_NEW
df_tx_new %>% 
  arrange(desc(partner_type)) %>% 
  mutate(share = ifelse(partner_type == "Local", share, NA)) %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(alpha = bar_alpha, fill = ind_label),
           position = "identity") +
  #geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_tx$period) %>% length())+
  geom_text(aes(label = percent(share, 1)), vjust = -1, 
            size = 10/.pt, 
            label.size = NA, family = "Source Sans Pro") +
  scale_fill_manual(values = c(moody_blue, moody_blue_light)) +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("{title_info_txnew$share} OF PATIENTS NEW ON ANTIRETROVIRAL THERAPY IN FY21Q3 WERE ENROLLED BY LOCAL PARTNERS"),
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines()

si_save("Graphics/partner_txnew_trends_usaid.svg")

#data point for context
df_tx_new %>% 
  filter(period == max(period),
         partner_type == "Total") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

df_tx_new %>% 
  filter(period == max(period),
         partner_type == "Local") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))


    
  

