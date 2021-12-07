# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  MMD Trips Averted Calculation
# LICENSE:  MIT
# DATE:     2021-11-05
# UPDATED:  2021-11-22 (with FY21Q4 and covid analysis)
# Note: MMD numbers adapted from "agitprop/Scripts/11_MMD.R"

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

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd()   


# MUNGE MMD ---------------------------------------------------------------

df %>% 
  filter(fundingagency == "USAID") %>%
  count(countryname) %>% 
  unique() %>% 
  view()
  
        

#keep just TX_CURR/MMD and reshape
df_mmd <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "TX_CURR",
        # operatingunit != "South Africa",
         fiscal_year >= 2020,
         standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
  mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                       TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
  )) %>%
  group_by(fiscal_year, countryname, indicator, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  filter(value > 0)

#create group for o3mo and o6mo via reshaping for plotting
df_mmd <- df_mmd %>% 
  mutate(countryname = recode(countryname,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR")) %>% 
  select(-period_type) %>% 
  pivot_wider(names_from = otherdisaggregate) %>% 
  rowwise() %>% 
  mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
    #unknown = ifelse(unknown < 0, 0, unknown),
    o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  rename(o6mmd = `6 or more months`) %>% 
  select(-`Less than 3 months`, -`3 to 5 months`) %>% 
  pivot_longer(-c(period, countryname, indicator, total), 
               names_to = "otherdisaggregate",
               values_to = "tx_mmd") %>% 
  rename(tx_curr = total) 

#aggregate up to agency level
df_mmd_agency <- df_mmd %>% 
  mutate(otherdisaggregate = recode(otherdisaggregate,
                                    "o3mmd" = "MMD - 3 months or more",
                                    "o6mmd" = "MMD - 6 months or more")) %>% 
  group_by(period, otherdisaggregate) %>% 
  summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(share = tx_mmd / tx_curr)

#CALCULATION ----------------------------------------------------------------

#FY21Q4

  #6 month MMD in FY21Q4
  mmd_6 <- df_mmd_agency %>% 
    filter(period == "FY21Q4",
           otherdisaggregate == "MMD - 6 months or more") %>% 
    select(-c(tx_curr, share))
  
  #3 month MMD in FY21Q4
  mmd_3 <- df_mmd_agency %>% 
    filter(period == "FY21Q4",
           otherdisaggregate == "MMD - 3 months or more") %>% 
    select(-c(tx_curr, share))
  
  #Trips Averted Calculation
  #In FY21, at least [(TX_MMD_3mo + TX_MMD_6mo)*12 - (TX_MMD_3mo*4 + TX_MMD_6mo*2)] trips were averted with having [(TX_MMD_3mo + TX_MMD_6mo)] on MMD
  trips_averted_fy21q4 <- ((mmd_3$tx_mmd + mmd_6$tx_mmd)*12) - ((mmd_3$tx_mmd*4) +(mmd_6$tx_mmd*2))
  
  # Number of people on 3 or 6 month MMD in FY21Q4
  on_mmd <- mmd_3$tx_mmd + mmd_6$tx_mmd

#FY20Q4?

  #6 month MMD FY20Q4
  mmd_6_fy20 <- df_mmd_agency %>% 
    filter(period == "FY20Q4",
           otherdisaggregate == "MMD - 6 months or more") %>% 
    select(-c(tx_curr, share))
  
  #3 month MMD FY20Q4
  mmd_3_fy20 <- df_mmd_agency %>% 
    filter(period == "FY20Q4",
           otherdisaggregate == "MMD - 3 months or more") %>% 
    select(-c(tx_curr, share))


  trips_averted_fy20q4 <- ((mmd_3_fy20$tx_mmd + mmd_6_fy20$tx_mmd)*12) - ((mmd_3_fy20$tx_mmd*4) +(mmd_6_fy20$tx_mmd*2))

#FY20Q2? (MMD ramped up in Fy20Q3 due to covid)

  #6 month MMD FY20Q1
  mmd_6_fy20q2 <- df_mmd_agency %>% 
    filter(period == "FY20Q2",
           otherdisaggregate == "MMD - 6 months or more") %>% 
    select(-c(tx_curr, share))
  
  #3 month MMD FY20Q4
  mmd_3_fy20q2 <- df_mmd_agency %>% 
    filter(period == "FY20Q2",
           otherdisaggregate == "MMD - 3 months or more") %>% 
    select(-c(tx_curr, share))
  
  
  trips_averted_fy20q2 <- ((mmd_3_fy20q2$tx_mmd + mmd_6_fy20q2$tx_mmd)*12) - ((mmd_3_fy20q2$tx_mmd*4) +(mmd_6_fy20q2$tx_mmd*2))
  
#Calculation to get trips averted estimate since the start of COVID (FY20Q3 because of MMD ramp up)  
  # (FY21Q4 Trips averted) + (FY20Q4 Trips Averted - FY20Q2 Trips Averted)
  
 covid_trips_averted <- trips_averted_fy21q4 + (trips_averted_fy20q4 - trips_averted_fy20q2)
  
    
#test dataframe for a waffle plot
  
# fy21_trips <- df_mmd %>% 
#   mutate(otherdisaggregate = recode(otherdisaggregate,
#                                     "o3mmd" = "mmd3",
#                                     "o6mmd" = "mmd6")) %>% 
#   group_by(period, otherdisaggregate) %>% 
#   summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   filter(period == "FY21Q3") %>% 
#   mutate(total_trip = sum(tx_mmd)*12) %>% 
#   pivot_wider(names_from = otherdisaggregate, values_from = tx_mmd) %>% 
#   mutate(mmd3_trips = mmd3*4,
#          mmd6_trips = mmd6*2) %>% 
#   select(-c(tx_curr, mmd3, mmd6)) %>% 
#   pivot_longer(total_trip:mmd6_trips, names_to = "type", values_to = "value") %>% 
#   mutate(share = (value /56296116)*100)
  
