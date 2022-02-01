# PROJECT:  catch-22
# AUTHOR:   K. Srikanth
# PURPOSE:  GH FO SDG 3 Visuals
# LICENSE:  MIT
# DATE:     2021-09-07
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

# GLOBAL VARS ----------------------------------------------------------------
load_secrets()


authors <- c("Nada Petrovic")

#Malaria: By 2030 end the epidemic of malaria in all countries. 
#The targeted level of reduction, however, is not defined.
#The WHO Global Technical Strategy has set a target of reducing 
#incidence by 90 percent by 2030. 
#This would infer a target of 9 or fewer cases of malaria per 1,000 people globally in 2030.

goal_malaria<-9

#Tuberculosis incidence is the number of new cases of tuberculosis per 100,000 people.
#Goal: The 2030 target is to end the epidemic of tuberculosis (TB) in all countries. 
#The targeted level of reduction, however, is not defined.
#The World Health Organization's Stop TB Partnership has set a target 
#(not specifically related to the SDGs) of reducing national incidence of 
#TB to fewer than 20 cases per 100,000 by 2030.

goal_tb <-20 

usaid_mal <- c("Angola", "Guinea", "Nigeria",
               "Benin","Kenya","Rwanda",
               "Burkina Faso","Liberia","Senegal",
               "Cameroon","Madagascar","Sierra Leone",
               "Cote d’Ivoire","Malawi","Tanzania",
               "Democratic Republic of Congo","Mali","Uganda",
               "Ethiopia","Niger","Zambia",
               "Ghana", "Mozambique","Zimbabwe") 

usaid_tb <- c("Afghanistan", "India", "Nigeria", "Ukraine",
              "Bangladesh",	"Indonesia",	"Philippines",	"Uzbekistan",
              "Burma",	"Kenya", "South Africa", "Vietnam",
              "Cambodia", "Kyrgyz Republic", "Tajikistan",	"Zambia",
              "Democratic Republic of Congo", "Malawi", "Tanzania",	"Zimbabwe",
              "Ethiopia",	"Mozambique", "Uganda")


# IMPORT -------------------------------------------------------------------

## Data is from: https://sdg-tracker.org/good-health

## Incidence per 1,000 population
df_mal_inc <- si_path() %>% 
  return_latest("incidence-of-malaria-sdgs.csv") %>% 
  read_csv()  

## Deaths per 100,000 population
df_mal_deaths <- si_path() %>% 
  return_latest("malaria-death-rates.csv") %>% 
  read_csv()  

## Incidence per 100,000 population
df_tb_inc <- si_path() %>% 
  return_latest("incidence-of-tuberculosis-sdgs.csv") %>% 
  read_csv() 

## Deaths per 100,000 population
df_tb_deaths<- si_path() %>% 
  return_latest("tuberculosis-death-rates.csv") %>% 
  read_csv()  


#MUNGE ---------------------------------------------------------------------

##MALARIA
names(df_mal_inc)<-c("country","country_code","year","incidence")
names(df_mal_deaths)<-c("country","country_code","year","rate_deaths")
df_mal <- df_mal_inc %>% full_join(df_mal_deaths, by = c("country","year"))

df_mal<-df_mal%>%
  mutate(usaid = ifelse(country %in% usaid_mal, "USAID", "Non-USAID")) %>%
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

##TB
names(df_tb_inc)<-c("country","country_code","year","incidence")
names(df_tb_deaths)<-c("country","country_code","year","rate_deaths")
df_tb <- df_tb_inc %>% full_join(df_tb_deaths, by = c("country","year"))

df_tb<-df_tb%>%
  mutate(usaid = ifelse(country %in% usaid_tb, "USAID", "Non-USAID")) %>%
  mutate(country = ifelse(country=="Democratic Republic of Congo", "DRC", country)) 

#PLOT -----------------------------------------

## Malaria
df_mal_viz <- df_mal %>% filter(usaid == "USAID") %>%
  filter(year==2018) 

p1<-df_mal_viz %>% 
  ggplot(aes(x=incidence,y=reorder(country,incidence))) +
  scale_y_reordered() +
  geom_linerange(aes(xmin = 0, xmax = incidence), color = "gray90",
                size = 0.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  geom_vline(xintercept=goal_malaria,
             color = old_rose, size=1.5)+
  annotate(geom="text",x=120, y=20,color=old_rose,size=3.5,
            label = "WHO Global \n Technical Strategy \n Target")+
  labs(x = NULL, y = NULL,
     title = "Malaria incidence per 1,000 people")+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

p2<-df_mal_viz %>% 
  ggplot(aes(x=rate_deaths,y=reorder(country,rate_deaths))) +
  scale_y_reordered() +
  geom_linerange(aes(xmin = 0, xmax = rate_deaths), color = "gray90",
                 size = 0.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  labs(x = NULL, y = NULL,
       title = "Malaria deaths per 100,000 people",
       caption = glue("Sources: World Development Indicators-World Bank (2018)
                      Global Burden of Disease Collaborative Network (2018) 
                      SI analytics: {paste(authors, collapse = '/')}
                      US Agency for International Development"))+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

p1+p2
si_save("Malaria_SDGs.png")

### TB
df_tb_viz <- df_tb %>% filter(usaid == "USAID") %>%
  filter(year==2019) 

p1<-df_tb_viz %>% 
  ggplot(aes(x=incidence,y=reorder(country,incidence))) +
  scale_y_reordered() +
  geom_linerange(aes(xmin = 0, xmax = incidence), color = "gray90",
                 size = 0.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  geom_vline(xintercept=goal_tb,
             color = old_rose, size=1.5)+
  annotate(geom="text",x=150, y=17,color=old_rose,size=3.5,
           label = "WHO Stop TB \n Partnership \n Target")+
  labs(x = NULL, y = NULL,
       title = "TB incidence per 100,000 people")+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

p2<-df_tb_viz %>% 
  ggplot(aes(x=rate_deaths,y=reorder(country,rate_deaths))) +
  scale_y_reordered() +
  geom_linerange(aes(xmin = 0, xmax = rate_deaths), color = "gray90",
                 size = 0.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  labs(x = NULL, y = NULL,
       title = "TB deaths per 100,000 people",
       caption = glue("Sources: World Development Indicators-World Bank (2019)
                      Global Burden of Disease Collaborative Network (2019) 
                      SI analytics: {paste(authors, collapse = '/')}
                      US Agency for International Development"))+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

p1+p2
si_save("TB_SDGs.png")




