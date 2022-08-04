# PROJECT:  catch-22
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  USAID contribution to PEPFAR targets (regional briefing)
# LICENSE:  MIT
# DATE:     2022-08-01
# UPDATED:  2022-08-03
# GOAL:     Communicate percent USAID share & PEPFAR total targets 
#           for a subset of indicators
# TYPE:     Infographic with numbers
# NOTE:     Adapted from 2022_06_14_diagram_usaid-pepfar-contribution_targets_DP_dataset
#           Uses full set of targets contained in the MSD
#           Used for regional briefing slides (Asia) in Aug 2022

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



# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()
setwd("~/GitHub/catch-22/Scripts")
data_in <- "Data"
data_out <- "Dataout"
viz_out <- "Images"

authors <- c("USAID OHA SI Team")
msd_source <- source_info()

# PARAMETERS
# Choose fiscal year + region or calculate globally
region_sel<-"global" ## Can also choose global
#region_sel<-"Asia" ## region for targets
FY_sel<-"2023"

#Define the sets of indicators in the preferred visual order
#Current default is 2 but can add additional sets to the list
#Set of indicators to pull from data:
ind_list <- list(c("TX_NEW", "TX_CURR", "TX_PVLS"),
            c("KP_PREV", "HTS_TST_POS", "HTS_SELF", "PrEP_NEW"))  
#How the indicators should appear on plot (e.g. Denominators/Numerators separate)
#Note: function is currently hardwired to split D/N for TX_PVLS only, can be 
#tweaked for others
ind_list_plot <- list(c("TX_NEW", "TX_CURR", "TX_PVLS (D)", "TX_PVLS (N)"),
                      c("KP_PREV", "HTS_TST_POS", "HTS_SELF", "PrEP_NEW"))   
#Colors for each set of parameters
color_list=c(scooter,denim)


# FUNCTIONS -----------------------------------------------------------------------

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

#MUNGE FUNCTIONS-----------------------------------------------------------------

#Cleans data, filters for region and calculates PEPFAR share
agency_targets <- function(df_full, region, FY, ind_vec, ind_vec_plot) {

  df_region <- df_full %>% 
    #Filters for list of indicators as they appear in MSD
    filter(indicator %in% ind_vec) %>% 
    #Applies "_D" suffix to denominators
    glamr::clean_indicator() %>% 
    #Filters for only totals 
    glamr::pluck_totals() %>%
    #Renames indicators to how they should appear
    mutate(indicator=case_when(indicator=="TX_PVLS" ~ "TX_PVLS (N)",
                               indicator=="TX_PVLS_D"~ "TX_PVLS (D)",
                               TRUE ~ indicator)) %>%
    #Filters for year and the list of indicators as they should appear in plot
    filter(fiscal_year==FY, indicator %in% ind_vec_plot) %>%
    #This line can be used to doublecheck w/ Executive Dashboard, which does not 
    #include Dedup. For the final plot Dedup should NOT be removed. 
    ###filter(funding_agency!="Dedup") %>% 
    #Attaches a two dummy data sets to allow for calculating total PEPFAR targets
    #as well as total global targets
    bind_rows(mutate(.,funding_agency = "PEPFAR")) %>%
    bind_rows(mutate(.,usaid_region = "global")) %>%
    filter(funding_agency %in% c("PEPFAR","USAID")) %>% 
    group_by(fiscal_year, usaid_region, indicator, funding_agency) %>%
    #Summary will include all regions, global, all agencies & PEPFAR
    summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop")
  
    #Pivot wide to calculate share
    df_region_wide <- df_region %>% 
    pivot_wider(names_from=funding_agency, values_from=targets) %>%
    mutate(share=USAID/PEPFAR) %>%
    #Filter for selected region  
    filter(usaid_region == region)
  
  print(df_region_wide)
}

#Aggregates all the data/text that will be used in chart in the needed format
reshape_agency_result <- function(df_full, region, FY, ind_vec, ind_vec_plot) {
   
  df_all <- agency_targets(df_full, region, FY, ind_vec, ind_vec_plot)
  
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
                                   indicator == "PrEP_NEW" ~ "People received pre-exposure prophylaxis to prevent HIV",
                                   indicator == "HTS_SELF" ~ "Number of individual HIV self-test kits distributed",
                                   indicator == "TX_PVLS (N)" ~ "Number of ART patients with suppressed viral load", 
                                   indicator == "TX_PVLS (D)" ~ "Number of ART patients with a documented viral load result"))
  return(df_viz)
}

#Creates the indicator plot
plot_indicator_targets<- function (df_viz, color_set) {
  
  plot_ind<-df_viz %>%  
    ggplot() +
    geom_text(aes(x, 0.65, label = share_lab),
              family = "Source Sans Pro SemiBold", color = color_set,
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
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(plot.title = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          panel.background = element_rect(fill = "#e6e7e84f"),
          panel.border = element_rect(color = color_set, fill = NA))
  
  return(plot_ind)
}

# IMPORT ------------------------------------------------------------------

## Import MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY") %>%
  read_msd()

## Import list of countries in regions
df_meta <- get_outable(datim_user(), datim_pwd()) %>%
  select(country, country_iso)

# MUNGE -------------------------------------------------------------------

#Cleaning up region classification to match USAID regions 
df_meta <- df_meta %>%
  mutate(wb_region = countrycode(df_meta$country_iso, "iso3c", "region"),
         usaid_region = case_when(country == "Ukraine" ~ "Europe",
                                  wb_region == "Sub-Saharan Africa" ~ "Africa",
                                  wb_region == "Latin America & Caribbean" ~ "LAC",
                                  TRUE ~ "Asia")) %>% 
  select(-c(wb_region, country_iso))

#Adding regional information
df_full <- df %>% 
  left_join(df_meta, by = "country") 
  
#VIZ ----------------------------------------------------------------

# Loop that creates figures with sets of four indicators per figure.
for (i in 1:2) {
df_viz <- reshape_agency_result(df_full, region_sel, FY_sel, ind_list[[i]], ind_list_plot[[i]]) %>%
               #Reorders to preferred indicator order
               mutate(indicator = fct_relevel(indicator, ind_list_plot[[i]]),
               text_color = ifelse(funding_agency == "USAID", color_list[i], matterhorn)) 

plot_indicator_targets(df_viz, color_list[i])
si_save(glue("Images/usaid_contribution_FY{substr(FY_sel,3,4)}_targets_{region_sel}_set{i}.png"))
}
  




