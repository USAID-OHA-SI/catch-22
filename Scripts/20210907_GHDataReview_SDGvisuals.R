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

# GLOBAL VARS ----------------------------------------------------------------
load_secrets()


authors <- c("Karishma Srikanth")

#under 5 mortality goal of 25 per 1k deaths by 2030
goal <- 25 

#https://www.usaid.gov/global-health/health-areas/maternal-and-child-health/priority-countries
usaid_mch <- c("Afghanistan",
               "Bangladesh",
               "Myanmar",
               "Democratic Republic of the Congo",
               "Ethiopia",
               "Ghana",
               "Haiti",
               "India",
               "Indonesia",
               "Kenya",
               "Liberia",
               "Madagascar",
               "Malawi",
               "Mali",
               "Mozambique",
               "Nepal",
               "Nigeria",
               "Pakistan",
               "Rwanda",
               "Senegal",
               "South Sudan",
               "United Republic of Tanzania",
               "Uganda",
               "Yemen",
               "Zambia")
#change DRC and Tanzania


# IMPORT -------------------------------------------------------------------

df_under5 <- si_path() %>% 
  return_latest("under5") %>% 
  read_xlsx(skip = 9)  

sdg_progress <- si_path() %>% 
  return_latest("sdgprogress") %>% 
  read_xlsx()


#MUNGE ---------------------------------------------------------------------

df_under5 <- df_under5 %>% 
  clean_names() %>% 
  mutate(usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"))

# round value
df_under5$value <- round(df_under5$value, 2)

sdg_progress <- sdg_progress %>% 
  mutate(value = 20) %>% 
  pivot_wider(names_from = year, values_from = value)

sdg_progress <- sdg_progress[,c(1,4,6,3,5,2)]

# VIS -----------------------------------------------------------

df_under5_vis <- df_under5 %>%
  clean_names() %>% 
  mutate(usaid = ifelse(country %in% usaid_mch, "USAID", "Non-USAID"), 
         country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "United Republic of Tanzania" ~ "Tanzania",
                             TRUE ~ country)) %>% 
  filter(usaid == "USAID",
         year == 2009 | year == 2019) %>%
  mutate(value= as.numeric(value),
         country = fct_reorder(country, value)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  left_join(sdg_progress, by = c("country")) %>% 
  mutate(`2029` = ifelse(is.na(`2029`), `2019`, `2029`),
         `2039` = ifelse(is.na(`2039`), `2019`, `2039`),
         `2049` = ifelse(is.na(`2049`), `2019`, `2049`),
         `2059` = ifelse(is.na(`2059`), `2019`, `2059`),
         actual_year = ifelse(actual_year == ">2050", 2051, actual_year),
         fill_color = ifelse(actual_year <= 2030, burnt_sienna, trolley_grey)) 


df_under5_vis %>%
  ggparcoord(columns = 9:14, groupColumn = 8, showPoints = TRUE, scale = "globalminmax") +
  si_style() +
  scale_color_identity() +
  geom_hline(yintercept = goal, color = grey10k) +
  labs(title = "7 USAID countries are on track to achieve SDG 3.2's Under 5 Mortality Rate
       target of 25 or fewer deaths per 1,000 live births",
       subtitle = "Indonesia has already surpassed the goal, with an under 5 mortality rate of 23.9 per 1,000 live births in 2019*", 
       x = NULL,
       y = "Under-5 chuld mortality per 1,000 children",
       caption = "Source: UN Interagency Group on Mortality Estimates 2019,
       UN IGME Progress
       SI Analytics: Karishma Srikanth
       US Agency for International Development")

si_save("Graphics/under5mortality.svg")

