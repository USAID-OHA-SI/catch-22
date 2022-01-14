# PROJECT:  catch-22
# AUTHOR:   K.Srikanth | USAID
# PURPOSE:  USAID contribution to pepfar results (Dr. Gawande Briefing)
# LICENSE:  MIT
# DATE:     2022-01-03
# NOTE:     adapted from groundhog_day/Scripts/FY20Q2_Review_bar_sparks.R

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

# IMPORT ------------------------------------------------------------------

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd()

# GLOBALS -------------------------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"
viz_out <- "Images"

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

#source info
msd_source <- source_info()

#identify periods for plot
curr_pd <- source_info(return = "period")
curr_qtr <- source_info(return = "quarter")

sum_indic <- function(df1) {
  df1 %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(val = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
}

#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

#MUNGE -----------------------------------------------------------------


ind_list <-  c("HTS_TST_POS", "TX_CURR", "TB_PREV", "TX_NEW") #TX_MMD separate

agency_result <- function(indicator_type, usaid_param) {
  
  #define the indicators
  if (indicator_type == "clinical") {
    ind_list <- c("HTS_TST_POS", "TX_CURR", "TB_PREV", "TX_NEW") #TX_MMD separate
  } else if (indicator_type == "prevention") {
    ind_list <- c("KP_PREV", "VMMC_CIRC", "PrEP_NEW", "AGYW_PREV") #OVC_SERV munged separate, AGYW from team
  }
  
  #usaid
  if (usaid_param == TRUE) {
    df <- df %>% 
      filter(fundingagency == "USAID")
  }

  
  #PEPFAR
  df_pepfar <- 
    df %>%
    filter(indicator %in% ind_list,
           disaggregate == "Total Numerator",
           fiscal_year == 2021) %>% 
    sum_indic() %>% 
    rename(PEPFAR = val)
  
    #MMD
  df_pepfar_mmd <-
    df %>%
    filter(indicator == "TX_CURR",
           fiscal_year == 2021,
           disaggregate == "Age/Sex/ARVDispense/HIVStatus",
           otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months")) %>%
    sum_indic() %>%
    mutate(indicator = "TX_MMD3") %>%
    rename(PEPFAR = val)

    #OVC
  ovc_pepfar <- df %>%
    bind_rows(df %>% mutate(fundingagency = "PEPFAR")) %>%
    filter(fundingagency == "PEPFAR",
           indicator %in% c("OVC_SERV"),
           standardizeddisaggregate %in% c("Total Numerator"),
           # trendscoarse == "<18",
           fiscal_year == 2021) %>%
    #  left_join(df_partner, by = c("mech_code")) %>%
    group_by(fiscal_year, indicator) %>%
    summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(PEPFAR = cumulative)

  if (indicator_type == "clinical") {
    df_pepfar_all <- rbind(df_pepfar, df_pepfar_mmd)
  } else if (indicator_type == "prevention") {
    df_pepfar_all <- rbind(df_pepfar, ovc_pepfar)
  }
  
  #rename USAID
  if (usaid_param == TRUE) {
    df_pepfar_all <- df_pepfar_all %>% 
      rename(USAID = PEPFAR)
  }

return(df_pepfar_all)
  
}

reshape_agency_result <- function(indicator_type) {
  df_pepfar_all <- agency_result(indicator_type, FALSE)
  df_usaid_all <- agency_result(indicator_type, TRUE)
  
  df_long <- left_join(df_usaid_all, df_pepfar_all) %>%
    mutate(share = USAID / PEPFAR,
           target = 1) %>% 
    pivot_longer(cols = USAID:PEPFAR,
                 names_to = "fundingagency",
                 values_to = "value")
  
  df_viz <- df_long %>% 
    mutate(x = .5,
           y = x,
           x_label = ifelse(fundingagency == "USAID", 0.25, 0.75),
           y_label = .25,
           x_ind = .72,
           y_ind = 0.95,
           share_lab = percent(share, accuracy = 1),
           value = round(value, 2),
           val_lab = value %>%  clean_number(1),
           ind_display = case_when(indicator == "TX_CURR" ~ "Antiretroviral  treatment (ART) for women, men, and children",
                                   indicator == "TX_MMD3" ~ "Dispensed multi-month (3+ month) ART for women, men, and children",
                                   indicator == "TX_NEW" ~ "People newly enrolled on ARV treatment",
                                   indicator == "HTS_TST_POS" ~ "People who received HIV Testing Services (HTS) & received positive test results",
                                   indicator == "TB_PREV" ~ "Total number of ART who completed course of TB preventive therapy",
                                   indicator == "KP_PREV" ~ "Key populations reached with individual and/or small group-level HIV prevention interventions",
                                   indicator == "OVC_SERV" ~ "Orphans and vulnerable children (OVC) & their caregivers provided with care and support",
                                   indicator == "VMMC_CIRC" ~ "Males circumcised as part of the voluntary medical male circumcision for HIV prevention program",
                                   indicator == "PrEP_NEW" ~ "People received pre-exposure prophylaxis to prevent HIV"))
  
  df_viz %>% 
    filter(fiscal_year == 2021) %>% 
    ggplot() +
    geom_text(aes(x, 0.65, label = share_lab),
              family = "Source Sans Pro Light", color = moody_blue,
              size = 60/.pt) +
    geom_text(aes(x_label, 0.4, label = paste(str_wrap(val_lab, width = 30), "\n")),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 15/.pt) +
    geom_text(aes(0.5, 0.15, label = paste(str_wrap(ind_display, width = 30), "\n")),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 12/.pt) +
    geom_text(aes(x, y_ind, label = indicator),
              family = "Source Sans Pro SemiBold", color = trolley_grey, 
              size = 15/.pt) +
    expand_limits(x = c(0, 1), y = c(0,1)) +
    facet_grid(~indicator) +
    labs(x = NULL, y = NULL,
         title = "USAID CONTRIBUTIONS TO PEPFAR RESULTS FY21",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          panel.background = element_rect(fill = "#e6e7e84f"),
          panel.border = element_rect(color = trolley_grey, fill = NA))
}




#Play around with viz --------------------------------------

df_viz <- df_long %>% 
  mutate(x = .5,
         y = x,
         x_label = ifelse(fundingagency == "USAID", 0.25, 0.75),
         y_label = .25,
         x_ind = .72,
         y_ind = 0.95,
         share_lab = percent(share, accuracy = 1),
         value = round(value, 2),
         val_lab = value %>%  clean_number(1),
         ind_display = case_when(indicator == "TX_CURR" ~ "Antiretroviral  treatment (ART) for women, men, and children",
                                 indicator == "TX_MMD3" ~ "Dispensed multi-month (3+ month) ART for women, men, and children",
                                 indicator == "TX_NEW" ~ "People newly enrolled on ARV treatment",
                                 indicator == "HTS_TST_POS" ~ "People who received HIV Testing Services (HTS) & received positive test results",
                                 indicator == "TB_PREV" ~ "Total number of ART who completed course of TB preventive therapy"))

  
df_viz %>% 
  filter(fiscal_year == 2021) %>% 
  ggplot() +
  geom_text(aes(x, 0.65, label = share_lab),
            family = "Source Sans Pro Light", color = moody_blue,
            size = 60/.pt) +
   geom_text(aes(x_label, 0.4, label = paste(str_wrap(val_lab, width = 30), "\n")),
             family = "Source Sans Pro Light", color = trolley_grey, 
             size = 15/.pt) +
  geom_text(aes(0.5, 0.15, label = paste(str_wrap(ind_display, width = 30), "\n")),
            family = "Source Sans Pro Light", color = trolley_grey, 
            size = 12/.pt) +
   geom_text(aes(x, y_ind, label = indicator),
             family = "Source Sans Pro SemiBold", color = trolley_grey, 
             size = 15/.pt) +
  expand_limits(x = c(0, 1), y = c(0,1)) +
  facet_grid(~indicator) +
  labs(x = NULL, y = NULL,
       title = "USAID CONTRIBUTIONS TO PEPFAR RESULTS FY21",
       caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "#e6e7e84f"),
        panel.border = element_rect(color = trolley_grey, fill = NA))

si_save("Graphics/usaid_contribution_clinical.svg")