# PROJECT:  catch-22
# AUTHOR:   N.Petrovic & K. Srikanth
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
library(svglite)
library(ggtext)

# GLOBAL VARS ----------------------------------------------------------------
load_secrets()
authors <- c("N. Petrovic", "K. Srikanth")


## USAID malnutrition focus countries
## https://www.usaid.gov/global-health/health-areas/nutrition/countries
usaid_mal <- c("Bangladesh", "Burkina Faso", "Cambodia", 
               "Congo, Dem. Rep.", 
               "Ethiopia", "Ghana", "Guatemala", 
               "Haiti", "Kenya", "Madagascar", 
               "Malawi", "Mali", "Mozambique", 
               "Nepal", "Niger", "Nigeria", 
               "Rwanda", "Senegal", "Tajikistan", 
               "Tanzania", "Uganda", "Zambia", 
               "Burundi", "Honduras", "Zimbabwe") 
## 25 countries

## Source: https://sdg-tracker.org/zero-hunger

#Malnutrition SDG goal: By 2030 end all forms of malnutrition, including 
# achieving, by 2025, the internationally agreed targets on stunting 
# and wasting in children under 5 years of age
# According this WHO policy brief on global nutrition targets, a 2012 resolution
# by the WHO aimed to reduce and maintain childhood wasting to less than 5%
# by 2025. 
#http://apps.who.int/iris/bitstream/handle/10665/149018/WHO_NMH_NHD_14.2_eng.pdf?sequence=1

goal_wasting<-5


# IMPORT -------------------------------------------------------------------
## Malnutrition data downloaded with relevant fildters from World Bank
##https://databank.worldbank.org/source/health-nutrition-and-population-statistics

df_mal_wide <- si_path() %>% 
  return_latest("Malnutrition_Data_WB") %>% 
  read_csv()  

#MUNGE ---------------------------------------------------------------------

df_mal_wide <- df_mal_wide %>% 
  clean_names() 

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
  mutate(country = ifelse(country=="Congo, Dem. Rep.","DRC",country)) %>%
  filter(series_code != "Stunting (model)") %>%
drop_na(percent) %>%
group_by(country,series_code) %>% filter(year == max(year)) %>% ungroup() %>%
mutate(country_year=paste(country," (",year, ")", sep=""))

## VISUALIZE
## Collapses wasting & severe wasting into one variable
df_mal_viz<-df_mal %>%
  mutate(series_collapsed=ifelse(series_code=="Severe Wasting",
                                 "Wasting",as.character(series_code))) %>%
  mutate(gap=goal_wasting-percent,
       gap_bar = case_when(percent > goal_wasting ~ percent),
       dot_color = if_else(gap >= 0, scooter, old_rose)) %>%
  mutate(gap=ifelse(series_code=="Severe Wasting", NA ,gap),
       gap_bar=ifelse(series_code=="Severe Wasting", NA ,gap_bar),
       dot_color = if_else(series_code=="Severe Wasting", trolley_grey, dot_color)) 

df_mal_wasting<-filter(df_mal,series_code=="Wasting")

plot_title <- "Percent of children under 5 <span style= 'color:#1e87a5;'> wasted </span> and <span style = 
'color:#808080;'> severely wasted </span>, most recent available data"

p1<-df_mal_viz %>%  
  filter(series_collapsed=="Wasting") %>%
  mutate(country_year_ordered=factor(country_year, 
  levels=df_mal_wasting$country_year[order(df_mal_wasting$percent)])) %>%
  ggplot(aes(x=percent,y=country_year_ordered,color=dot_color)) +
  geom_linerange(aes(xmin = goal_wasting, xmax = gap_bar), color = "gray90",
                 size = 2.5, na.rm = TRUE) + 
  scale_color_identity() +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+
  geom_vline(xintercept=goal_wasting,linetype="dashed")+
  scale_y_reordered(limits=rev)+
  si_style_xgrid()+
  labs(x = NULL, y = NULL, title = plot_title)+
  theme(axis.text.y = element_markdown(), 
      plot.title = element_textbox_simple(margin = margin(5.5, 0, 10, 5.5),
                                          face="plain", size=12)) 


p2<-df_mal %>%  
  filter(series_code=="Stunting") %>%
  ggplot(aes(x=percent,y=reorder(country_year,percent),color=genoa)) +
  scale_y_reordered(limits=rev) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE)+
  scale_color_identity()+
  si_style_xgrid()+ 
  labs(x = NULL, y = NULL,
       title = "Percent children under 5 stunted, most recent available data",
       caption = glue("Source: Joint child malnutrition estimates - UNICEF/WHO/World Bank  
                      USAID SI analytics: {paste(authors, collapse = '/')}"))+
  theme(axis.text.y = element_markdown(), 
        plot.title = element_textbox_simple(margin = margin(5.5, 0, 5.5, 5.5), 
        face="plain", size=12))

p1+p2+
plot_annotation(title = "12 USAID malnutrition focus countries have reached 
        <5% wasting for children under 5, a goal set by the World Health Assembly's 
        Global Nutrition Targets 2025", 
        theme=theme(plot.title = 
        element_textbox_simple(family = "Source Sans Pro", face="bold", 
                              margin = margin(5.5, 0, 0, 5.5))))

si_save("Malnutrition_SDGs_FINAL.svg")

