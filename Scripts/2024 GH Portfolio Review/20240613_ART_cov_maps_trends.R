# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  PEPFAR SSA Progress to 2nd 95 Map
# REF ID:   aae5029f 
# LICENSE:  MIT
# DATE:     2024-06-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "aae5029f"

# IMPORT ------------------------------------------------------------------
  
df_est <- pull_estimates(TRUE)
df_tt <- pull_testtreat(TRUE)

# MUNGE -------------------------------------------------------------------

df_viz <- df_tt %>% 
  filter(year %in% c(2012, 2017, 2022),
       indicator == "Percent on ART of PLHIV",
       sex == "All",
       age == "All") %>% 
  count(year, indicator, country, iso, wt = estimate, name = "value") %>% 
  mutate(group= case_when(value < 75 ~ "<75%",
                          value >= 75 & value < 85 ~ "75-84%",
                          value >= 85 & value < 90 ~ "85-89%",
                          value >= 90 ~ ">90%")) %>% 
  mutate(fill_color = case_when(value < 75 ~ "#b3dbc1",
                          value >= 75 & value < 85 ~ "#8dbea3",
                          value >= 85 & value < 90 ~ "#419265",
                          value >= 90 ~ "#2d6647")) 

B3D3C1



#ous <- get_outable(datim_user(), datim_pwd())
ous <- 
  grabr::get_outable(datim_user(), datim_pwd()) %>% 
  mutate(countryname_iso = if_else(country_iso == "SSD", "SDS", country_iso))

afr_map <- rnaturalearth::ne_countries(continent = "africa", returnclass = "sf")

map <- rnaturalearth::ne_countries(continent = "africa", returnclass = "sf") %>% 
  left_join(., df_viz, by = c("sov_a3" = "iso")) %>% 
  mutate(pepfar_fill = case_when(
    sovereignt %in% c("United Republic of Tanzania", "Swaziland") ~ "#2F2E79",
    #sovereignt %in% ou_dreams_afr ~ "#2F2E79",
    TRUE ~ grey20k)
  ) 

# Set up terrain  
afr <- map %>% sf::st_drop_geometry() %>% distinct(sovereignt) %>% pull()
terr <- gisr::get_terrain(afr, terr = rasdata, mask = T)

 ggplot() +
  # geom_tile(data = filter(terr, value < 210), aes(x = x, y = y, alpha = value)) + 
  # scale_alpha(name = "", range = c(0.6, 0), guide = F) +
  geom_sf(data = afr_map, color = trolley_grey, size = 0.1, alpha = 1) +
  geom_sf(data = map %>% filter(!is.na(year)), aes(fill = fill_color), color = "white", size = 0.1, alpha = 1) +
  # ggplot2::geom_sf_text(data = map %>% filter(!is.na(fill_color)),
  #                       ggplot2::aes(label = value), color = "white",
  #                       family = "Source Sans Pro") +
  facet_wrap(~year) +
  scale_fill_identity(guide = "legend") +
  si_style_map() +
  labs(x = NULL, y = NULL,
       title = "Getting to green: In 2022, more countries are approaching or exceeding the 95-95-95 threshold" %>% toupper(),
       subtitle = "Understanding countries different paths and progress towards 95-95-95 goals is essential for COP25 planning",
       caption = glue("Source: {source_note
                      } US Agency for International Development"))
  
  
  
