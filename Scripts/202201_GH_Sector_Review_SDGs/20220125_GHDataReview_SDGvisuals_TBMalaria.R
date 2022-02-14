# PROJECT:  catch-22
# AUTHOR:   N. Petrovic & K. Srikanth
# PURPOSE:  GH FO SDG Visuals
# LICENSE:  MIT
# DATE:     2022-02
# UPDATED: 

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

# GLOBAL VARS ----------------------------------------------------------------
load_secrets()


authors <- c("N. Petrovic","K. Srikanth")

## Source: https://sdg-tracker.org/good-health

#Malaria SDG goal: By 2030 end the epidemic of malaria in all countries. 
#The targeted level of reduction, however, is not defined.
#The WHO Global Technical Strategy has set a target of reducing 
#incidence by 90 percent by 2030. 
#This would infer a target of 9 or fewer cases of 
#malaria per 1,000 people globally in 2030.

goal_malaria<-9

#TB SDG Goal: The 2030 target is to end the epidemic of tuberculosis (TB) 
#in all countries. 
#The targeted level of reduction, however, is not defined.
#The World Health Organization's Stop TB Partnership has set a target 
#(not specifically related to the SDGs) of reducing national incidence of 
#TB to fewer than 20 cases per 100,000 by 2030.

goal_tb <-20 

#USAID Malaria priority countries: https://www.pmi.gov/where-we-work/
usaid_mal <- c("Angola", "Benin", "Burkina Faso", "Burma", 
               "Cambodia", "Cameroon", "Cote d'Ivoire", 
               "Democratic Republic of Congo", "Ethiopia", "Ghana",
               "Guinea", "Kenya", "Liberia", "Madagascar",
               "Malawi", "Mali", "Mozambique",
               "Niger", "Nigeria","Rwanda","Senegal","Sierra Leone",
               "Tanzania", "Thailand", "Uganda", "Zambia",
               "Zimbabwe") 

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

## Malaria deaths per 100,000 population (all ages)
df_mal_deaths <- si_path() %>% 
  return_latest("malaria-death-rates.csv") %>% 
  read_csv()  

## Incidence TB per 100,000 population
df_tb_inc <- si_path() %>% 
  return_latest("incidence-of-tuberculosis-sdgs.csv") %>% 
  read_csv() 

## Deaths per 100,000 population (all ages)
df_tb_deaths<- si_path() %>% 
  return_latest("tuberculosis-death-rates.csv") %>% 
  read_csv()  


#MUNGE ---------------------------------------------------------------------

##MALARIA

# Original col names: Entity, Code, Year, Incidence of malaria (per 1,000 population at risk)
names(df_mal_inc)<-c("country","country_code","year","incidence")
# Original col names:  Entity, Code, Year, 
# Deaths - Malaria - Sex: Both - Age: Age-standardized (Rate)
names(df_mal_deaths)<-c("country","country_code","year","rate_deaths")
df_mal <- df_mal_inc %>% full_join(df_mal_deaths, by = c("country","year"))

df_mal<-df_mal%>%
  ## Switch country names to match USAID designation
  mutate(country = ifelse(country=="Myanmar", "Burma", country)) %>%
  mutate(usaid = ifelse(country %in% usaid_mal, "USAID", "Non-USAID")) %>%
  ## Switch to DRC for simpler plot label
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

##TB

# Original col names: Entity, Code, Year, Incidence of tuberculosis (per 100,000 people)
names(df_tb_inc)<-c("country","country_code","year","incidence")
# Original col names:  Entity, Code, Year, 
# Deaths - Tuberculosis - Sex: Both - Age: Age-standardized (Rate)
names(df_tb_deaths)<-c("country","country_code","year","rate_deaths")
df_tb <- df_tb_inc %>% full_join(df_tb_deaths, by = c("country","year"))

df_tb<-df_tb%>%
  ## Switch country names to match USAID designation
  mutate(country = ifelse(country=="Myanmar", "Burma", country)) %>%
  mutate(country = ifelse(country=="Kyrgyzstan","Kyrgyz Republic", country)) %>%
  mutate(usaid = ifelse(country %in% usaid_tb, "USAID", "Non-USAID")) %>%
  ## Switch to DRC for simpler plot label
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

#PLOT -----------------------------------------
  
## Malaria

## Reshape the data so that it captures difference between SDG goal and 
## measured incidence
df_mal_viz <- df_mal %>% filter(usaid == "USAID") %>%
  mutate(gap=goal_malaria-incidence,
         gap_bar = case_when(incidence > goal_malaria ~ incidence),
         dot_color = if_else(gap >= 0, scooter, old_rose)) 

## Create incidence plot. Dashed line is SDG goal of 9 cases/1000.
## Countries signified by blue dots have reached goal
p1<-df_mal_viz %>% filter(year==2018) %>%
  ggplot(aes(x=incidence,y=reorder(country,incidence),color=dot_color)) +
  geom_linerange(aes(xmax = gap_bar, xmin = goal_malaria), color = "gray90",
                size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  geom_vline(xintercept=goal_malaria,linetype="dashed")+
  scale_y_reordered(limits=rev)+
  scale_color_identity()+
  si_style_xgrid()+ 
  labs(x = NULL, y = NULL,
     title = "Malaria incidence per 1,000 people, all ages (2018)")+
  theme(axis.text.y = element_markdown(), 
      plot.title = element_textbox_simple(margin = margin(5.5, 0, 5.5, 5.5), 
                   face="plain", size=12))

## Create mortality plot
p2<-df_mal_viz %>% filter(year==2019) %>% 
  ggplot(aes(x=rate_deaths,y=reorder(country,rate_deaths), color=genoa)) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  scale_y_reordered(limits=rev) +
  si_style_xgrid()+
  scale_color_identity()+
  labs(x = NULL, y = NULL,
       title = "Malaria deaths per 100,000 people, all ages (2019)",
       caption= glue("Sources: Global Health Observatory Data Repository - WHO (2018)
                      Institute for Health Metrics and Evaluation (2019) 
                      US Agency for International Development"))+
  theme(axis.text.y = element_markdown(), 
        plot.title = element_textbox_simple(margin = margin(5.5, 0, 5.5, 5.5), 
                                            face = "plain", size=12))

p1+p2+
plot_annotation(title = "Two PMI focus countries have reached the goal of fewer than 9 cases per 1,000 by 2030,
                set by WHO's Global Technical Malaria Strategy (2016-2030)", 
                theme=theme(plot.title = element_textbox_simple(family = "Source Sans Pro", face="bold", 
                      margin = margin(5.5, 0, 0, 5.5))))


si_save("Graphics/Malaria_SDGs_FINAL.svg")

### TB

## Reshape the data so that it captures difference between SDG goal and 
## measured incidence
df_tb_viz <- df_tb %>% filter(usaid == "USAID") %>%
  filter(year==2019) %>%
  mutate(gap=goal_tb-incidence,
         gap_bar = case_when(incidence > goal_tb ~ incidence),
         dot_color = if_else(gap >= 0, scooter, old_rose)) 

## Create incidence plot. Dashed line is SDG goal of 20 cases/100,000.
## Countries signified by blue dots have reached goal
p1<-df_tb_viz %>% 
  ggplot(aes(x=incidence,y=reorder(country,incidence),color=dot_color)) +
  scale_y_reordered(limits=rev) +
  geom_linerange(aes(xmin = goal_tb, xmax = gap_bar), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  geom_vline(xintercept = goal_tb, linetype = "dashed")+
  scale_color_identity()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL,
       title = "TB incidence per 100,000 people, all ages (2019)")+
  theme(axis.text.y = element_markdown(), 
        plot.title = element_textbox_simple(margin = margin(5.5, 0, 5.5, 5.5), 
                                            face="plain", size=12))
  

## Create mortality plot
p2<-df_tb_viz %>% 
  ggplot(aes(x=rate_deaths,y=reorder(country,rate_deaths),color=genoa)) +
  scale_y_reordered(limits=rev) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  scale_color_identity()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL,
       title = "TB deaths per 100,000 people, all ages (2019)",
       caption = glue("Sources: Global Tuberculosis Report - WHO (2019)
                      Institute for Health Metrics and Evaluation (2019) 
                      US Agency for International Development"))+
  theme(axis.text.y = element_markdown(), 
        plot.title = element_textbox_simple(margin = margin(5.5, 0, 5.5, 5.5), 
                                            face = "plain", size=12))

  p1+p2+
  plot_annotation(title = "No USAID TB focus countries have yet reached the goal of fewer than 20 cases per 100,000 by 2030,
                set by WHO's Stop TB Partnership", 
                theme=theme(plot.title = element_textbox_simple(family = "Source Sans Pro", face="bold", 
                margin = margin(5.5, 0, 0, 5.5))))
  
si_save("Graphics/TB_SDGs_FINAL.svg")




