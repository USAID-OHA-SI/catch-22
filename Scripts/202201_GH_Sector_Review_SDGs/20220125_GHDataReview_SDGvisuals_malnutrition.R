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

## Goal is for all 3 malnutrition measures to be zero

###


###

usaid_mal <- c("Bangladesh", 
               "Burkina Faso", 
               "Cambodia", 
               "Democratic Republic of the Congo", 
               "Ethiopia", 
               "Ghana", 
               "Guatemala", 
               "Haiti", 
               "Kenya", 
               "Madagascar", 
               "Malawi", 
               "Mali", 
               "Mozambique", 
               "Nepal", 
               "Niger", 
               "Nigeria", 
               "Rwanda", 
               "Senegal", 
               "Tajikistan", 
               "Tanzania", 
               "Uganda", 
               "Zambia", 
               "Burundi", 
               "Honduras", 
               "Zimbabwe") 


# IMPORT -------------------------------------------------------------------
##https://databank.worldbank.org/source/health-nutrition-and-population-statistics

df_mal_wide <- si_path() %>% 
  return_latest("Malnutrition_Data_WB") %>% 
  read_csv()  

#MUNGE ---------------------------------------------------------------------

df_mal_wide <- df_mal_wide %>% 
  clean_names() 

#NOTE: still need to fix w/DRC & double check rest of names

names(df_mal_wide) <- names(df_mal_wide) %>%
  sub("*......yr", "", . ) %>%
  sub("_name", "", .)    

df_mal <- gather(df_mal_wide, key="year", value="percent", "2010":"2020")

df_mal$percent<-as.numeric(df_mal$percent)
df_mal$year<-as.numeric(df_mal$year)
df_mal$series_code <- recode_factor(df_mal$series_code, SH.STA.WAST.ZS = "Wasting", 
                                    SH.SVR.WAST.ZS = "Severe Wasting", 
                                    SH.STA.STNT.ZS ="Stunting", 
                                    SH.STA.STNT.ME.ZS="Stunting (model)")

df_mal<-df_mal %>%
  mutate(usaid = ifelse(country %in% usaid_mal, "USAID", "Non-USAID")) %>%
  filter(usaid == "USAID") %>%
  filter(series_code != "Stunting (model)")

df_mal<-df_mal %>%
drop_na(percent) %>%
group_by(country,series_code) %>% filter(year == max(year)) %>% ungroup() %>%
mutate(country_year=paste(country,"(",year, ")", sep=""))


df_mal<-df_mal %>%
  mutate(series_collapsed=ifelse(series_code=="Severe Wasting",
                                 "Wasting",as.character(series_code)))
   

p1<-df_mal %>%  
  filter(series_code=="Stunting") %>%
  ggplot(aes(x=percent,y=reorder(country_year,percent))) +
  scale_y_reordered() +
  geom_linerange(aes(xmin = 0, xmax = percent), color = "gray90",
                 size = 0.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+ 
  labs(x = NULL, y = NULL,
       title = "Stunting, children under 5 (%)")+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

df_mal_wasting<-filter(df_mal,series_code=="Wasting")

goal_wasting<-5
#plot_title<-"<span style= 'color:#2057a7;'> Wasting </span>"
p2<-df_mal %>%  
  filter(series_collapsed=="Wasting") %>%
  mutate(dot_color=ifelse(series_code == "Severe Wasting", old_rose, "black")) %>%
  mutate(country_year_ordered=factor(country_year, 
  levels=df_mal_wasting$country_year[order(df_mal_wasting$percent)])) %>%
  ggplot(aes(x=percent,y=country_year_ordered,color=dot_color)) +
  geom_linerange(aes(xmin = 0, xmax = percent), color = "gray90",
                 size = 0.5, na.rm = TRUE) + 
  scale_color_identity() +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+
  geom_vline(xintercept=goal_wasting,
             color = "black", size=1.5)+
  annotate(geom="text",x=9, y=7,color="black",size=3.5,
           label = "WHO Global \n Nutrition Target \n for Wasting")+
  labs(x = NULL, y = NULL,
     title = "Wasting and severe wasting (red), children under 5 (%)")+
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

p2+p1


si_save("Malnutrition_SDGs.png")

