
# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID/PHI
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15
# DATE UPDATED: 2023-05-31 (adapted from JH weightviz.R)

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
library(ggrepel)

# global variables -------------------------------------------------------------

ref_id <- "ae3887aa"
load_secrets()
output_loc <- "catch-22/Scripts/202201_GH_Sector_Review_SDGs/country_weighting/weightedviz/output"
date <- today()

# repeated settings for UHC figures
uhc_settings <- function(ggobj) {
  
  ggobj <- ggobj +
    geom_smooth(aes(
      x = year, y = weighted_avg,
      group = pepfar, color = pepfar)) +
    geom_point(aes(
      x = year, y = value,
      color = pepfar, fill = pepfar),
      alpha = 0.4,
      position = position_jitter(width = 0.2)) +
    geom_vline(
      xintercept = 2003,
      color = trolley_grey,
      linetype = "longdash",
      alpha = 0.5) +
    si_style_ygrid() +
    # uhc is an index from 0-100
    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    scale_x_continuous(
      breaks = c(2000, 2005, 2010, 2015, 2019)) +
    scale_color_manual(
      values = c(
        "PEPFAR" = usaid_medblue,
        "Non-PEPFAR" = usaid_lightgrey),
      labels = NULL) +
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
  
  return(ggobj)
  
}

# repeated settings for Life Exp. figures
lexp_settings <- function(ggobj) {
  
  ggobj <- ggobj +
    geom_smooth(aes(
      x = year, y = weighted_avg,
      group = pepfar, color = pepfar)) +
    geom_point(aes(
      x = year, y = value,
      color = pepfar, fill = pepfar),
      alpha = 0.4,
      position = position_jitter(width = 0.3)) +
    geom_vline(
      xintercept = 2003,
      color = trolley_grey,
      linetype = "longdash",
      alpha = 0.5) +
    si_style_ygrid() +
    scale_y_continuous(
      limits = c(0, 85),
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
    scale_x_continuous(
      breaks = c(1990, 2000, 2010, 2020)) +
    scale_color_manual(
      values = c(
        "PEPFAR" = usaid_medblue,
        "Non-PEPFAR" = usaid_lightgrey),
      labels = NULL) +
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
  
}

# percent change function 
# sample calculation
# percent increase formula = 
# ((weighted_avg(pepfar)2019 - weighted_avg(pepfar)2000/ weighted_avg(pepfar)2000)/
# weighted_avg(pepfar)2000)*100

pct_change <- function(current, starting, sig_dig){
  
  round_half_up(sum(sum(current - starting)/abs(starting))*100, sig_dig)
  
}

# read in ----------------------------------------------------------------------

# produced by catch-22/Scripts/202201_GH_Sector_Review_SDGs/
# country_weighting/src/clean.R
figure_data <- read_sheet("17ZBOzPux0lgkmN48BtE3yzKpeEzDLVmx9B4Pei9nGbw", 
                          sheet = "figure_data")

# visualize --------------------------------------------------------------------

# filter data by low income status for all LI country figures
low_income <- figure_data %>%
  filter(income_group == "Low Income Country (World Bank Classification)")

# SSA LI countries only --------------------------------------------------------

# filter data by Sub-Saharan African Region for all LI, SSA country figures
lic_ssa <- low_income %>%
  filter(usaid_region == "Sub-Saharan Africa")


uhc_ssa_pepfar <- lic_ssa %>%
  filter(
    indicator == "uhc_service_coverage_index") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

uhc_low_id_pepfar <- lic_ssa %>%
  filter(
    indicator == "uhc_subindex4_id") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

uhc_low_ca_pepfar <- lic_ssa %>%
  filter(
    indicator == "uhc_subindex1_capacity_access") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

uhc_ssa_pepfar %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))+
  # geom_smooth(aes(
  #   x = year, y = weighted_avg,
  #   group = pepfar, color = pepfar)) +
  geom_point(aes(
    x = year, y = value,
    color = pepfar, fill = pepfar),
    alpha = 0.4,
    position = position_jitter(width = 0.2)) +
  geom_point(size = 4, alpha = .7) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = c(2003), linetype = "dashed") +
  si_style_ygrid() +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2017, 2019)) +
  geom_text(data = . %>% filter(year ==  2019), 
            aes(label = pepfar), 
            vjust = -1,
           # hjust = -0.7,
            family = "Source Sans Pro") +
  geom_label_repel(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 50 | value < 17, as.character(iso), "")),
    segment.size = 0,
    point.padding = 0,
    nudge_y = 5,
    size = 3,
    family = "Source Sans Pro",
    label.size = NA
    # hjust = -0.4, vjust = 0.4,
    # position = position_jitter(width = -0.4),
    # family = "Source Sans Pro",
    # size = 3
    ) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_medblue,
      "Non-PEPFAR" = usaid_lightgrey),
    labels = NULL) +
  theme(
    # axis.text = element_text(
    #   family = "Source Sans Pro",
    #   size = 18,
    #   color = "#505050"),
    legend.position = "none") +
  labs(
    title = "Since PEPFAR’s inception, overall SCI (0-100) in PEPFAR-supported countries has exceeded that of countries without PEPFAR programs" %>% toupper(),
    subtitle = "Analysis restricted to countries designated as 'Low Income' by the World Bank",
    x = NULL,
    y = NULL,
    color = NULL, 
    fill = NULL,
    group = NULL,
    caption = glue::glue("Source: World Health Organization Health Service Coverage Index"))

si_save("Graphics/ias_sci_overall_remake.svg")


uhc_low_id_pepfar %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))+
  # geom_smooth(aes(
  #   x = year, y = weighted_avg,
  #   group = pepfar, color = pepfar)) +
  geom_point(aes(
    x = year, y = value,
    color = pepfar, fill = pepfar),
    alpha = 0.4,
    position = position_jitter(width = 0.2)) +
  geom_point(size = 4, alpha = .7) +
 geom_line(linewidth = 1) +
  #geom_smooth() +
  geom_vline(xintercept = c(2003), linetype = "dashed") +
  si_style_ygrid() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  geom_text(data = . %>% filter(year ==  2019), 
            aes(label = pepfar), 
            vjust = -1,
            # hjust = -0.7,
            family = "Source Sans Pro") +
  geom_label_repel(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 60, as.character(iso), "")),
    segment.size = 0,
    point.padding = 0,
    nudge_y = 0,
    size = 3,
    family = "Source Sans Pro",
    label.size = NA
    # hjust = -0.4, vjust = 0.4,
    # position = position_jitter(width = -0.4),
    # family = "Source Sans Pro",
    # size = 3
  ) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_medblue,
      "Non-PEPFAR" = usaid_lightgrey),
    labels = NULL) +
  theme(
    # axis.text = element_text(
    #   family = "Source Sans Pro",
    #   size = 18,
    #   color = "#505050"),
    legend.position = "none") +
  labs(
    title = "Within the infectious diease sub-index, there is a similar increase in PEPFAR-supported countries since 2003" %>% toupper(),
    subtitle = "Analysis restricted to countries designated as 'Low Income' by the World Bank",
    x = NULL,
    y = NULL,
    color = NULL, 
    fill = NULL,
    group = NULL,
    caption = glue::glue("Source: World Health Organization Health Service Coverage Index"))

si_save("Graphics/ias_sci_id_remake.svg")


uhc_low_ca_pepfar %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))+
  # geom_smooth(aes(
  #   x = year, y = weighted_avg,
  #   group = pepfar, color = pepfar)) +
  geom_point(aes(
    x = year, y = value,
    color = pepfar, fill = pepfar),
    alpha = 0.4,
    position = position_jitter(width = 0.2)) +
  geom_point(size = 4, alpha = .7) +
  geom_line(linewidth = 1) +
  #geom_smooth() +
  geom_vline(xintercept = c(2003), linetype = "dashed") +
  si_style_ygrid() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  geom_text(data = . %>% filter(year ==  2019), 
            aes(label = pepfar), 
            vjust = -1,
            # hjust = -0.7,
            family = "Source Sans Pro") +
  geom_label_repel(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 50, as.character(iso), "")),
    segment.size = 0,
    point.padding = 0,
    nudge_y = 0,
    size = 3,
    family = "Source Sans Pro",
    label.size = NA
    # hjust = -0.4, vjust = 0.4,
    # position = position_jitter(width = -0.4),
    # family = "Source Sans Pro",
    # size = 3
  ) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_medblue,
      "Non-PEPFAR" = usaid_lightgrey),
    labels = NULL) +
  theme(
    # axis.text = element_text(
    #   family = "Source Sans Pro",
    #   size = 18,
    #   color = "#505050"),
    legend.position = "none") +
  labs(
    title = "Within the infectious diease sub-index, there is a similar increase in PEPFAR-supported countries since 2003" %>% toupper(),
    subtitle = "Analysis restricted to countries designated as 'Low Income' by the World Bank",
    x = NULL,
    y = NULL,
    color = NULL, 
    fill = NULL,
    group = NULL,
    caption = glue::glue("Source: World Health Organization Health Service Coverage Index"))

si_save("Graphics/ias_sci_id_remake.svg")


