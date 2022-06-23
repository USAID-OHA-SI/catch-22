# PROJECT:  catch-22
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  USAID contribution to PEPFAR targets (regional briefing)
# LICENSE:  MIT
# DATE:     2022-01-03
# UPDATED:  2022-05-31
# NOTE:     adapted from 2022_01_03_gawande_briefing_usaid-pepfar-contribution.R

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
library(countrycode)


# IMPORT ------------------------------------------------------------------


# GLOBAL VARIABLES --------------------------------------------------------


load_secrets()
setwd("~/GitHub/catch-22/Scripts")

# PARAMETERS
region_sel<-"LAC" ## The region for which you are calculating targets


# IMPORT ------------------------------------------------------------------

df <- si_path("path_datapacks") %>% 
  return_latest("Datapack_Agency_IM") %>%
  read.csv()

df_meta <- get_outable(datim_user(), datim_pwd()) %>%
  select(country, country_iso)

# MUNGE -------------------------------------------------------------------


df_meta <- df_meta %>%
  mutate(wb_region = countrycode(df_meta$country_iso, "iso3c", "region"),
         usaid_region = case_when(country == "Ukraine" ~ "Europe",
                                  wb_region == "Sub-Saharan Africa" ~ "Africa",
                                  wb_region == "Latin America & Caribbean" ~ "LAC",
                                  TRUE ~ "Asia")) %>% 
  select(-c(wb_region, country_iso))


df_full <- df %>% 
  rename(country=countryname, funding_agency=fundingagency) %>%
  left_join(df_meta, by = "country") 
  
# GLOBALS -------------------------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"
viz_out <- "Images"

authors <- c("USAID OHA SI Team")

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

##Used to check disaggregates
#ind_list <- c("KP_PREV", "OVC_SERV", "VMMC_CIRC", "PREP_NEW")  
#df %>%
#  filter(indicator %in% ind_list) %>%
#  group_by(indicator, standardizeddisaggregate) %>%
#  tally() %>% View()

#Function that returns targets for a series of indicators (indicator_type == "clinical" or "prevention")
agency_targets <- function(region, indicator_type) {
  
  #define the indicators
  if (indicator_type == "clinical") {
    ind_list <- c("HTS_TST_POS", "TX_CURR", "TB_PREV", "TX_NEW")
    disagg_list<-c("Modality/Age/Sex/Result","Age/Sex/HIVStatus", "Age/Sex/NewExistingArt/HIVStatus","Age/Sex/HIVStatus")
    
  } else if (indicator_type == "prevention") {
    ind_list <- c("KP_PREV", "OVC_SERV", "PREP_NEW", "VMMC_CIRC")  
    disagg_list<-c("KeyPop","Age/Sex/ProgramStatus","Age/Sex","Age/Sex/HIVStatus")
  }
  
  df_region <- df_full %>% 
    filter(fiscal_year=="2023", indicator %in% ind_list,
           standardizeddisaggregate %in% disagg_list, numeratordenom=="N") 
  
  ## Removes the rows with indicator=PREP_NEW and the KeyPop disaggregated 
  ## (since it should be just "Age/Sex")
  df_region<-df_region[!(df_region$indicator=="PREP_NEW" & 
                         df_region$standardizeddisaggregate=="KeyPop"), ]
 
    df_region <- df_region %>%
    #filter(funding_agency!="Dedup") %>% ## Removing Dedup to see if this matches
    bind_rows(mutate(.,funding_agency = "PEPFAR")) %>%
    bind_rows(mutate(.,usaid_region = "global")) %>%
    filter(funding_agency %in% c("PEPFAR","USAID")) %>% 
    group_by(fiscal_year, usaid_region, indicator, funding_agency) %>%
    summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop")
  
    df_region_wide <- df_region %>% 
    pivot_wider(names_from=funding_agency, values_from=targets) %>%
    mutate(share=USAID/PEPFAR) %>%
    filter(usaid_region == region)
  
    print(df_region_wide)
}
  

#RESHAPE (indicator_type == "clinical" or "prevention")
reshape_agency_result <- function(region, indicator_type) {
  
  df_all <- agency_targets(region, indicator_type)
  
  df_long <- df_all%>% 
    pivot_longer(cols = USAID:PEPFAR,
                 names_to = "funding_agency",
                 values_to = "value")
  
  df_viz <- df_long %>% 
    mutate(x = .5,
           y = x,
           x_label = ifelse(funding_agency == "USAID", 0.25, 0.75),
           y_label = .25,
           x_ind = .72,
           y_ind = 0.95,
           share_lab = percent(share, accuracy = 1),
           value = round(value, 2),
           val_lab = value %>%  clean_number(1),
           ind_display = case_when(indicator == "TX_CURR" ~ "Antiretroviral  treatment (ART) for women, men, and children",
                                   indicator == "TX_NEW" ~ "People newly enrolled on ARV treatment",
                                   indicator == "HTS_TST_POS" ~ "People who received HIV Testing Services (HTS) & positive test results",
                                   indicator == "TB_PREV" ~ "Total number of ART who completed course of TB preventive therapy",
                                   indicator == "KP_PREV" ~ "Key populations reached with individual and/or small group-level HIV prevention interventions",
                                   indicator == "OVC_SERV" ~ "Orphans and vulnerable children (OVC) & their caregivers provided with care and support",
                                   indicator == "VMMC_CIRC" ~ "Males circumcised as part of the voluntary medical male circumcision for HIV prevention program",
                                   indicator == "PREP_NEW" ~ "People received pre-exposure prophylaxis to prevent HIV"))
return(df_viz)
}

#VIZ ----------------------------------------------------------------

df_viz_clin <- reshape_agency_result(region_sel, "clinical") #or prevention
df_viz_prev <- reshape_agency_result(region_sel, "prevention")

df_viz_clin <- df_viz_clin %>% 
  mutate(indicator = fct_relevel(indicator, c("TX_CURR", "TX_NEW", "TB_PREV", "HTS_TST_POS")),
         text_color = ifelse(funding_agency == "USAID", scooter, matterhorn)) 

df_viz_prev <- df_viz_prev %>% 
  mutate(indicator =  fct_relevel(indicator, c("KP_PREV", "OVC_SERV", "PREP_NEW", "VMMC_CIRC")),
         text_color = ifelse(funding_agency == "USAID", denim, matterhorn)) 
  
df_viz_clin %>% 
  ggplot() +
  geom_text(aes(x, 0.65, label = share_lab),
            family = "Source Sans Pro SemiBold", color = scooter,
            size = 60/.pt) +
  geom_text(aes(x_label, 0.4, color = text_color, label = paste(str_wrap(val_lab, width = 30), "\n")),
            family = "Source Sans Pro SemiBold", 
            size = 16/.pt) +
  geom_text(aes(x_label, 0.35, color = text_color, label = paste(str_wrap(funding_agency, width = 30), "\n")),
            family = "Source Sans Pro SemiBold", 
            size = 16/.pt) +
  geom_text(aes(0.5, 0.15, label = paste(str_wrap(ind_display, width = 18), "\n")),
            family = "Source Sans Pro", color = matterhorn, 
            size = 12/.pt) +
  geom_text(aes(x, y_ind, label = indicator),
            family = "Source Sans Pro SemiBold", color = matterhorn, 
            size = 20/.pt) +
  expand_limits(x = c(0, 1), y = c(0,1)) +
  facet_grid(~indicator) +
  scale_color_identity() +
  labs(x = NULL, y = NULL,
       title = glue ("USAID CONTRIBUTIONS TO {region_sel} PEPFAR TARGETS FY23") %>% toupper(),
       caption = glue("Source: FY23 Datapack inputs compiled by SIEI/EA
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "#e6e7e84f"),
        panel.border = element_rect(color = scooter, fill = NA))

#si_save(glue("Graphics/usaid_contribution_clinical_FY23_targets_{region_sel}.svg"))
si_save(glue("Images/usaid_contribution_clinical_FY23_targets_{region_sel}.png"))


df_viz_prev %>% 
  ggplot() +
  geom_text(aes(x, 0.65, label = share_lab),
            family = "Source Sans Pro SemiBold", color = denim,
            size = 60/.pt) +
  geom_text(aes(x_label, 0.4, color = text_color, label = paste(str_wrap(val_lab, width = 30), "\n")),
            family = "Source Sans Pro SemiBold", 
            size = 16/.pt) +
  geom_text(aes(x_label, 0.35, color = text_color, label = paste(str_wrap(funding_agency, width = 30), "\n")),
            family = "Source Sans Pro SemiBold", 
            size = 16/.pt) +
  geom_text(aes(0.5, 0.15, label = paste(str_wrap(ind_display, width = 18), "\n")),
            family = "Source Sans Pro", color = matterhorn, 
            size = 12/.pt) +
  geom_text(aes(x, y_ind, label = indicator),
            family = "Source Sans Pro SemiBold", color = matterhorn, 
            size = 20/.pt) +
  expand_limits(x = c(0, 1), y = c(0,1)) +
  facet_grid(~indicator) +
  scale_color_identity() +
  labs(x = NULL, y = NULL,
       title = glue("USAID CONTRIBUTIONS TO {region_sel} PEPFAR TARGETS FY23") %>% toupper(),
       caption = glue("Source: FY23 Datapack inputs compiled by SIEI/EA
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.background = element_rect(fill = "#e6e7e84f"),
        panel.border = element_rect(color = scooter, fill = NA))

#si_save(glue("Graphics/usaid_contribution_prev_FY23_targets_{region_sel}.svg"))
si_save(glue("Images/usaid_contribution_prev_FY23_targets_{region_sel}.png"))


