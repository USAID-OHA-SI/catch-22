# PROJECT:  Catch 22
# AUTHOR:   N.Petrovic | K. Srikanth 
# PURPOSE:  Trendline in Malaria & TB morbidity/mortality & progress towards SDGs
# LICENSE:  MIT
# DATE:     2022-01-18 
# UPDATED:
# NOTES: 

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
library(googledrive)
library(vroom)
library(readxl)
library(janitor)
library(GGally)
library(patchwork)
library(svglite)
library(ggtext)

## Sources file with all functions, so far only plot_trend()
source("202202_GHDataReview_SDGvisuals_Trends_functions.R")

# GLOBAL VARS ----------------------------------------------------------------
load_secrets()

authors <- c("N. Petrovic","K. Srikanth")

#Malaria: By 2030 end the epidemic of malaria in all countries. 
#The targeted level of reduction, however, is not defined.
#The WHO Global Technical Strategy has set a target of reducing 
#incidence by 90 percent by 2030. 
#This would infer a target of 9 or fewer cases of 
#malaria per 1,000 people globally in 2030.

goal_malaria<-9

#Tuberculosis incidence is the number of new cases of tuberculosis per 100,000 people.
#Goal: The 2030 target is to end the epidemic of tuberculosis (TB) in all countries. 
#The targeted level of reduction, however, is not defined.
#The World Health Organization's Stop TB Partnership has set a target 
#(not specifically related to the SDGs) of reducing national incidence of 
#TB to fewer than 20 cases per 100,000 by 2030.

goal_tb <-20 

#USAID Malaria priority countries: https://www.pmi.gov/where-we-work/
usaid_mal <- c("Angola", "Guinea", "Nigeria",
               "Benin","Kenya","Rwanda",
               "Burkina Faso","Liberia","Senegal",
               "Cameroon","Madagascar","Sierra Leone",
               "Cote d'Ivoire","Malawi","Tanzania",
               "Democratic Republic of Congo","Mali","Uganda",
               "Ethiopia","Niger","Zambia",
               "Ghana", "Mozambique","Zimbabwe") 

#USAID TB priority countries: https://www.usaid.gov/global-health/health-areas/tuberculosis/countries#tbMapLink
usaid_tb <- c("Afghanistan", "India", "Nigeria", "Ukraine",
              "Bangladesh",	"Indonesia",	"Philippines",	"Uzbekistan",
              "Burma",	"Kenya", "South Africa", "Vietnam",
              "Cambodia", "Kyrgyz Republic", "Tajikistan",	"Zambia",
              "Democratic Republic of Congo", "Malawi", "Tanzania",	"Zimbabwe",
              "Ethiopia",	"Mozambique", "Uganda")


# IMPORT -------------------------------------------------------------------

## Data is from: https://sdg-tracker.org/good-health

## Incidence of malaria per 1,000 population
df_mal_inc <- si_path() %>% 
  return_latest("incidence-of-malaria-sdgs.csv") %>% 
  read_csv()  

## Incidence per 100,000 population
df_tb_inc <- si_path() %>% 
  return_latest("incidence-of-tuberculosis-sdgs.csv") %>% 
  read_csv() 


#MUNGE ---------------------------------------------------------------------

##MALARIA

# Original col names: Entity, Code, Year, 
# Incidence of malaria (per 1,000 population at risk)
names(df_mal_inc)<-c("country","country_code","year","incidence")

df_mal<-df_mal_inc%>%
  mutate(usaid = ifelse(country %in% usaid_mal, "USAID", "Non-USAID")) %>%
  ## Switch to DRC for simplicity of plotting
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

##TB

# Original col names: Entity, Code, Year, 
# Incidence of tuberculosis (per 100,000 people)
names(df_tb_inc)<-c("country","country_code","year","incidence")

df_tb<-df_tb_inc%>%
  ## Switch country names to match USAID designation
  mutate(country = ifelse(country=="Myanmar", "Burma", country)) %>%
  mutate(country = ifelse(country=="Kyrgyzstan","Kyrgyz Republic", country)) %>%
  mutate(usaid = ifelse(country %in% usaid_tb, "USAID", "Non-USAID")) %>%
  ## Switch to DRC for simplicity of plotting
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

#MUNGE ----------------------------------------------------------------------

df_mal<-df_mal %>%
  filter(usaid=="USAID", between(year,2000,2018))

df_mal_viz<-df_mal %>%
  group_by(year) %>%
  summarize(mean_inc = mean(incidence), sd_inc=sd(incidence),
            max_inc=max(incidence), min_inc=min(incidence),
            country_min=country[min_inc==incidence],
            country_max=country[max_inc==incidence]) 

plot_title <-"STEADY DECLINE IN MALARIA INCIDENCE (PER 1000 PEOPLE) 
        IN USAID PRIORITY COUNTRIES, BUT STILL ABOVE GOAL OF 9"
plot_subtitle <-"Malaria incidence averaged across countries with standard deviation 
       and min/max noted"
plot_source <- "Global Health Observatory Data Repository - WHO"

## Note: function is located in separate function file which is sourced at the top
trendplot<- plot_trend(df_mal_viz, plot_title, plot_subtitle,plot_source)

plot(trendplot)

si_save("Graphics/Malaria_trends.svg")


  
