# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  LP meeting - HTS, KP OVC, and VMMC trends
# LICENSE:  MIT
# DATE:     2021-10-26
# note: derived from...
      #KP: agitprop/08a_long_term_kp_prev_trends.R
      #OVC: catch-22/2021_08_13_ovcwork.R

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

#HTS_TST_POS --------------------------------------------------------------------------

df_hts <- df %>% 
  bind_rows(df_arch) %>%
  left_join(df_partner, by = c("mech_code")) %>%
  filter(fundingagency == "USAID",
         indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(fundingagency, fiscal_year, indicator, partner_type) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(partner_type != "TBD") %>%
  filter(fiscal_year != 2022) %>%
  pivot_wider(names_from = partner_type, values_from = cumulative) %>%
  group_by(fiscal_year) %>%
  mutate(Total = International + Local,
         share = Local / Total)  %>%
  pivot_longer(cols = International:Total, names_to = "partner_type")

#change alpha for FY21 and add labels / colors
df_hts_viz <- df_hts %>% 
  mutate(bar_alpha = case_when(fiscal_year == 2021 ~ .6,
                               TRUE ~ 1),
         #year = glue("20{str_sub(period, 3, 4)}") %>% as.integer,
         ind_label = case_when(partner_type == "Local" ~ "Local Partner contribution",
                               TRUE ~ "Total new clients identified"),
         total = case_when(partner_type == "Total" ~ value),
         fill_color = ifelse(partner_type == "Local", old_rose, old_rose_light))

#get numbers for title
title_info_hts <- df_hts_viz %>% 
  filter(partner_type == "Local", fiscal_year == 2021) %>% 
  select(fiscal_year, indicator, value, share) %>% 
  mutate(
         value = value %>%  clean_number(1),
         share = percent(round(share, 2)))

#filter out years before 2018
df_hts_viz <- df_hts_viz %>% 
  arrange(desc(partner_type)) %>% 
  mutate(share = ifelse(partner_type == "Local", share, NA)) %>%
  filter(partner_type != "International",
         fiscal_year >= 2018) 

#VIZ ---------------
df_hts_viz %>%
  ggplot(aes(fiscal_year, value)) +
  geom_col(aes(alpha = bar_alpha, fill = ind_label),
           position = "identity") +
  #geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_hts_viz$fiscal_year) %>% length())+
  geom_text(aes(label = percent(share, 1)), vjust = -1, 
            size = 10/.pt, 
            label.size = NA, family = "Source Sans Pro") +
  scale_fill_manual(values = c(old_rose, old_rose_light)) +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("{title_info_hts$share} OF CLIENTS NEWLY IDENTIFIED IN FY21Q3 WERE IDENTIFIED BY LOCAL PARTNERS"),
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid()

si_save("Graphics/partner_HTS_trends_usaid.svg")

#data point for context
df_hts_viz %>% 
  filter(fiscal_year == 2021,
         partner_type == "Local") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

df_hts_viz %>% 
  filter(fiscal_year == 2021,
         partner_type == "Total") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

# KP_PREV -------------------------------------------------------------------

df_kp <- df %>% 
  bind_rows(df_arch) %>% 
  filter(indicator == "KP_PREV",
         standardizeddisaggregate == "Total Numerator",
         fundingagency == "USAID") 

df_kp <- df_kp %>% 
  left_join(df_partner, by = c("mech_code")) %>% 
  filter(partner_type != "TBD",
         fiscal_year <= 2021) %>%
  group_by(fiscal_year, fundingagency, indicator, partner_type) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(year = fiscal_year) %>%
  reshape_msd() %>% 
  pivot_wider(names_from = partner_type, values_from = value) %>% 
  mutate(Total = International + Local,
         share = Local / Total) %>% 
  pivot_longer(cols = International:Total, names_to = "partner_type") %>%
  filter(value > 0,
         year >= 2018)

#add labels, colors, and bar alpha
df_kp_viz <- df_kp %>% 
  arrange(indicator, period) %>% 
  mutate(source = "MSD") %>% 
  filter(partner_type != "International") %>%
  mutate(bar_alpha = case_when(year == 2021 ~ .6,
                               TRUE ~ 1),
         ind_label = case_when(partner_type == "Local" ~ "Local Partner contribution to KP_PREV portfolio",
                               TRUE ~ "Total number of Key Population individuals on HIV prevention services"),
         total = case_when(partner_type == "Total" ~ value),
         fill_color = ifelse(partner_type == "Local", denim, denim_light))

#get info for title
title_info_kp <- df_kp_viz %>% 
  filter(partner_type == "Local",
         period %in% c(min(period), max(period))) %>% 
  select(year, indicator, value, share) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         yrs = year - lag(year),
         value = value %>%  clean_number(1),
         share = percent(round(share, 2))) %>% 
  filter(year == max(year))

#VIZ -------------
df_kp_viz %>%
  arrange(desc(partner_type)) %>%
  mutate(share = ifelse(partner_type == "Local", share, NA),
         value_label = ifelse(partner_type == "Local", clean_number(value), NA)) %>% 
  filter(year >= 2019) %>%
  ggplot(aes(period, value)) + 
  #geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = fill_color),
           position = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
            family = "Source Sans Pro SemiBold") +
  geom_text(aes(label = value_label), na.rm = TRUE, vjust = -3, 
            family = "Source Sans Pro SemiBold") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  #scale_x_continuous(expand = c(.005, .005), n.breaks = unique(df_kp_viz$period) %>% length())+
  scale_alpha_identity() +
  scale_fill_manual(values = c(denim, denim_light)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("AS OF FY21Q2, LOCAL PARTNERS PROVIDED HIV PREVENTION SERVICES FOR OVER {title_info_kp$value}
                    KEY POPULATION INDIVIDUALS, MAKING UP ROUGHLY {title_info_kp$share} OF USAID'S KP PREVENTION PORTFOLIO"),
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines() +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines")) + 
  coord_cartesian(expand = F, clip = "off")

si_save("Graphics/partner_KP_trends_usaid_quarterly.svg")

#data points
df_kp_viz %>% 
  filter(period == max(period),
         partner_type == "Total") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

df_kp_viz %>% 
  filter(period == max(period),
         partner_type == "Local") %>%
  select(partner_type, value) %>% 
  mutate(value = clean_number(value, 1))

#OVC -----------------------------------------------------------------------

# HIV+ OVC (BAR CHART) 

df_hiv_ovc <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"& (standardizeddisaggregate == "ReportedStatus" | standardizeddisaggregate == "StatusPosART")),
         fundingagency == "USAID",
         fiscal_year >=2018) %>% 
  left_join(df_partner, by = c("mech_code")) 

#2018 - munge for 2018 because disaggs different
df_hiv_ovc18 <- df_hiv_ovc %>% 
  filter(fiscal_year == 2018,
         standardizeddisaggregate == "StatusPosART") %>% 
  group_by(fiscal_year, indicator, partner_type) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#munge for 2019-2021  
df_hiv_ovc <- df_hiv_ovc %>% 
  filter(fiscal_year > 2018,
         standardizeddisaggregate == "ReportedStatus",
         statushiv == "Positive") %>% 
  group_by(fiscal_year, indicator, partner_type) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#bind and add color
df_hiv_ovc_viz <- df_hiv_ovc18 %>% 
  bind_rows(df_hiv_ovc) %>%
  pivot_wider(names_from = partner_type, values_from = OVC_HIVSTAT) %>%
  mutate(Total = International + Local,
         share = Local / Total)  %>%
  pivot_longer(cols = International:Total, names_to = "partner_type") %>% 
  filter(value > 0) %>%
  mutate(fill_color = ifelse(partner_type == "Local",burnt_sienna, burnt_sienna_light))

title_info_ovc <- df_hiv_ovc_viz %>% 
  filter(partner_type == "Local",
         period %in% c(min(period), max(period))) %>% 
  select(period, value, share) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         value = value %>%  clean_number(1),
         share = percent(round(share, 2))) %>% 
  filter(period == "FY21Q2")

#VIZ
df_hiv_ovc_viz %>% 
  mutate(share = ifelse(partner_type == "Local", share, NA),
         value_label = ifelse(partner_type == "Local", clean_number(value), NA)) %>%
  arrange(desc(partner_type)) %>% 
  filter(partner_type != "International") %>% 
  ggplot(aes(period, value)) + 
  #geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = fill_color),
           position = "identity") +
  scale_fill_identity() + 
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  geom_text(aes(label = percent(share, 1)), vjust = -1, 
            size = 10/.pt, 
            label.size = NA, family = "Source Sans Pro") +
  geom_text(aes(label = value_label), na.rm = TRUE, vjust = -3, 
            family = "Source Sans Pro SemiBold") +
  labs(x = NULL, y = NULL, title = glue("As of {title_info_ovc$period}, local partners have added {title_info_ovc$value} \\
  HIV positive OVCs <18 on a comprehensive package of services,
  making up {title_info_ovc$share} of USAID OVC programming") %>% toupper,
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid() +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines")) + 
  coord_cartesian(expand = F, clip = "off")

si_save("Graphics/partner_OVC_trends_usaid.svg")

#VMMC --------------------------------------------------------------

df_vmmc <- df %>% 
  bind_rows(df_arch) %>% 
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         fundingagency == "USAID") 

#reshape
df_vmmc <- df_vmmc %>% 
  left_join(df_partner, by = c("mech_code")) %>% 
  filter(partner_type != "TBD",
         fiscal_year <= 2021) %>%
  group_by(fiscal_year, fundingagency, indicator, partner_type) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(partner_type != "TBD") %>% 
  pivot_wider(names_from = partner_type, values_from = cumulative) %>% 
  mutate(Total = International + Local,
         share = Local / Total) %>% 
  pivot_longer(cols = International:Total, names_to = "partner_type") %>%
  filter(value > 0,
         fiscal_year >= 2018)

df_vmmc_viz <- df_vmmc %>% 
  filter(partner_type != "International") %>%
  mutate(bar_alpha = case_when(fiscal_year == 2021 ~ .6,
                               TRUE ~ 1),
         ind_label = case_when(partner_type == "Local" ~ "Local Partner contribution to VMMC_CIRC",
                               TRUE ~ "Total VMMC_CIRC"),
         total = case_when(partner_type == "Total" ~ value),
         fill_color = ifelse(partner_type == "Local", genoa, genoa_light))

title_info_vmmc <- df_vmmc_viz %>% 
  filter(partner_type == "Local",
         fiscal_year == 2021) %>% 
  select(fiscal_year, indicator, value, share) %>% 
  mutate(added = (value - lag(value)) %>% clean_number(1),
         value = value %>%  clean_number(1),
         share = percent(round(share, 2)))

df_vmmc_viz %>%
  arrange(desc(partner_type)) %>%
  mutate(share = ifelse(partner_type == "Local", share, NA),
         value_label = ifelse(partner_type == "Local", clean_number(value), NA)) %>% 
  ggplot(aes(fiscal_year, value)) + 
  #geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = ind_label, alpha = bar_alpha),
           position = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
            family = "Source Sans Pro SemiBold") +
  geom_text(aes(label = value_label), na.rm = TRUE, vjust = -3, 
            family = "Source Sans Pro SemiBold") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  #scale_x_continuous(expand = c(.005, .005), n.breaks = unique(df_kp_viz$period) %>% length())+
  scale_alpha_identity() +
  scale_fill_manual(values = c(genoa, genoa_light)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("AS OF FY21Q2, LOCAL PARTNERS PROVIDED HIV PREVENTION SERVICES FOR OVER {title_info_vmmc$value}
                    KEY POPULATION INDIVIDUALS, MAKING UP ROUGHLY {title_info_vmmc$share} OF USAID'S KP PREVENTION PORTFOLIO"),
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid()

si_save("Graphics/partner_VMMC_trends_usaid_annually.svg")


