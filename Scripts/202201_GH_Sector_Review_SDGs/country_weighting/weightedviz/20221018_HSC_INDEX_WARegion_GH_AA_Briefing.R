# AUTHOR:   K. Srikanth | USAID
# PROJECT:  catch-22
# PURPOSE:  HSC index plots for WA region
# REF ID:   4cb8995b 
# LICENSE:  MIT
# DATE:     2022-10-18
# UPDATED: 

# dependencies -----------------------------------------------------------------

library(glamr)
library(glitr)
library(tidyverse)
library(janitor)
library(glue)
library(lubridate)
library(openintro)
library(ggtext)
library(sysfonts)
library(svglite)
library(extrafont)
library(googlesheets4)
library(patchwork)

# global variables -------------------------------------------------------------

ref_id <- "4cb8995b"
load_secrets()
output_loc <- "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/weightedviz/output"
date <- today()
source <- "WHO Global Health Observatory UHC Service Coverage Index"

west_africa <- c("Burkina Faso", "Togo", "Ghana", "Senegal",
                 "Liberia", "Mali", "Benin", "Cote d'Ivoire", "Gambia",
                 "Guinea", "Guinea-Bissau", "Niger", "Nigeria",
                 "Sierra Leone")



# read in ----------------------------------------------------------------------

# produced by catch-22/Scripts/202201_GH_Sector_Review_SDGs/
# country_weighting/src/clean.R
figure_data <- read_sheet("17ZBOzPux0lgkmN48BtE3yzKpeEzDLVmx9B4Pei9nGbw", 
                          sheet = "figure_data")

df_west_afr <- figure_data %>%
  filter(country %in% west_africa)

# visualize --------------------------------------------------------------------

df_west_afr_viz <- df_west_afr %>%
  filter(
    indicator == "uhc_service_coverage_index") %>%
  group_by(year) %>%
  mutate(
    weighted_avg = weighted.mean(value, population)) %>% 
  mutate(endpoints =  case_when(year %in% c(2000, 2019) ~weighted_avg),
         lab_share = case_when(year == 2019 ~ weighted_avg))

df_ou_viz <- df_west_afr %>%
  filter(
    indicator == "uhc_service_coverage_index") %>%
  group_by(year, country) %>%
  mutate(
    weighted_avg = weighted.mean(value, population)) %>% 
  mutate(endpoints = case_when(year %in% c(2000, 2019) ~weighted_avg),
         lab_share = case_when(year == 2019 ~ weighted_avg))


region_viz <- df_west_afr_viz %>% 
  ggplot() +
  geom_area(aes(
    x = year, y = weighted_avg), color = usaid_medblue, fill = usaid_medblue,
    alpha = .4, size = .9, position = "identity") +
  geom_point(aes(x= year, y = endpoints),
             color = usaid_medblue, fill = usaid_medblue, na.rm = TRUE) +
  geom_text(aes(x = year, y = weighted_avg, label = round(lab_share)), na.rm = TRUE,
            #color = trolley_grey,
            hjust = -.2, vjust = .1,family = "Source Sans Pro") +
  # geom_smooth(aes(
  #   x = year, y = weighted_avg), color = usaid_medblue) +
  geom_point(aes(
    x = year, y = value), fill = glitr::denim, color = denim,
    alpha = 0.4,
    position = position_jitter(width = 0.2)) +
  geom_vline(
    xintercept = 2003,
    color = trolley_grey,
    linetype = "longdash",
    alpha = 0.5) +
  si_style_ygrid() +
  # uhc is an index from 0-100
  # scale_y_continuous(
  #   limits = c(0, 100),
  #   breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  # scale_color_manual(
  #   values = c(
  #     "PEPFAR" = usaid_medblue,
  #     "Non-PEPFAR" = usaid_lightgrey),
  #   labels = NULL) +
  theme(
    axis.text = element_text(
      family = "Source Sans Pro",
      size = 10,
      color = "#505050"),
    legend.position = "none") +
  labs(subtitle = "West Africa regional average" %>% toupper(),
    x = NULL,
    y = NULL,
    color = NULL)



ou_viz <-
  df_ou_viz %>% 
  filter(country %in% c("Mali", "Nigeria", "Cote d'Ivoire")) %>% 
  ggplot() +
  geom_area(aes(
    x = year, y = weighted_avg), color = scooter_light, fill = scooter_light,
    alpha = .4, size = .9, position = "identity") +
  geom_point(aes(x= year, y = endpoints), na.rm = TRUE,color = scooter_light, fill = scooter_light) +
  geom_text(aes(x = year, y = weighted_avg, label = round(lab_share)), na.rm = TRUE,
            #color = trolley_grey,
            hjust = -.2, vjust = .1,family = "Source Sans Pro") +
  facet_wrap(~country, nrow = 3) +
  # geom_smooth(aes(
  #   x = year, y = weighted_avg), color = usaid_medblue) +
  # geom_point(aes(
  #   x = year, y = value), fill = glitr::denim, color = denim,
  #   alpha = 0.4,
  #   position = position_jitter(width = 0.2)) +
  geom_vline(
    xintercept = 2003,
    color = trolley_grey,
    linetype = "longdash",
    alpha = 0.5) +
  si_style_ygrid() +
  # uhc is an index from 0-100
  # scale_y_continuous(
  #   limits = c(0, 100),
  #   breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  # scale_color_manual(
  #   values = c(
  #     "PEPFAR" = usaid_medblue,
  #     "Non-PEPFAR" = usaid_lightgrey),
  #   labels = NULL) +
  theme(
    axis.text = element_text(
      family = "Source Sans Pro",
      size = 10,
      color = "#505050"),
    legend.position = "none") +
  labs(
       x = NULL,
       y = NULL,
       color = NULL)


region_viz + ou_viz +
  plot_annotation(title = "AVERAGE POPULATION-WEIGHTED HEALTH SERVICE COVERAGE INDEX SCORES ACROSS THE WEST AFRICA REGION FROM 2000-2019",
                 # subtitle = epi_control,
                  caption = glue::glue("Source: {source} [{date}]
                     Ref ID: {ref_id}"),
                  theme = si_style_ygrid()) &
  theme(axis.text.y = element_markdown(),
        panel.spacing.x = unit(20, "pt"),
        panel.spacing.y = unit(0, "pt"),
        plot.title = element_markdown())

si_save("Graphics/2022-10-18_WestAfrica_UHC_HSC.svg")


