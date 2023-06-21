
# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/GHTASC
# PURPOSE:  To adjust estimates by population for comparison
# REF ID:   ae3887aa
# LICENSE:  MIT
# DATE CREATED: 2022-07-15

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

# LI countries only ------------------------------------------------------------
# What has been the change in the UHC index over time 
# in PEPFAR vs non-PEPFAR countries?
uhc_low_pepfar_fig <-
  ggplot(
    # indicator changes with each figure
    uhc_low_pepfar <- low_income %>%
      filter(
        indicator == "uhc_service_coverage_index") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    # value to start labelling outliers changes with each figure
    x = year, y = value, color = pepfar,
    label = if_else(value > 50, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

# apply common UHC figure settings
uhc_low_pepfar_fig <- uhc_settings(uhc_low_pepfar_fig)

# which countries were included in the above figure?
countries_uhc_low_pepfar <- uhc_low_pepfar %>%
  ungroup() %>%
  select(country, pepfar) %>%
  distinct()

# What percentage did the weighted average increase over time?

# pepfar
uhc_low_pct <- uhc_low_pepfar %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_low_pct_pepfar <- uhc_low_pct %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_low_pct_pepfar$weighted_avg[2],  # 2019
           uhc_low_pct_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
uhc_low_pct_nonpepfar <- uhc_low_pct %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_low_pct_nonpepfar$weighted_avg[2], # 2019
           uhc_low_pct_nonpepfar$weighted_avg[1], 0) # 2000

# What has been the change in the UHC sub index on infectious diseases over time
# in PEPFAR vs non-PEPFAR countries?
uhc_low_id_pepfar_fig <-
  ggplot(
    uhc_low_id_pepfar <- low_income %>%
      filter(
        indicator == "uhc_subindex4_id") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 60, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

uhc_low_id_pepfar_fig <- uhc_settings(uhc_low_id_pepfar_fig)

# What percentage did the weighted average increase over time?

# pepfar
uhc_low_id_pct <- uhc_low_id_pepfar %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_low_id_pct_pepfar <- uhc_low_id_pct %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_low_id_pct_pepfar$weighted_avg[2], 
           uhc_low_id_pct_pepfar$weighted_avg[1], 0)

# non-pepfar
uhc_low_id_pct_nonpepfar <- uhc_low_id_pct  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_low_id_pct_nonpepfar$weighted_avg[2], 
           uhc_low_id_pct_nonpepfar$weighted_avg[1], 0)

# What has been the change in the UHC sub index on capacity and access over time
# in PEPFAR vs non-PEPFAR countries?
uhc_low_ca_pepfar_fig <-
  ggplot(
    uhc_low_ca_pepfar <- low_income %>%
      filter(
        indicator == "uhc_subindex1_capacity_access") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 50, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

uhc_low_ca_pepfar_fig <- uhc_settings(uhc_low_ca_pepfar_fig)

# What percentage did the weighted average increase over time?
uhc_low_ca_pct <- uhc_low_ca_pepfar %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_low_ca_pct_pepfar <- uhc_low_ca_pct  %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_low_ca_pct_pepfar$weighted_avg[2], 
           uhc_low_ca_pct_pepfar$weighted_avg[1], 0)

# non-pepfar
uhc_low_ca_pct_nonpepfar <- uhc_low_ca_pct  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_low_ca_pct_nonpepfar$weighted_avg[2], 
           uhc_low_ca_pct_nonpepfar$weighted_avg[1], 0)

# What has been the change in Life Expectancy at Birth in 
# PEPFAR vs non-PEPFAR countries over time?
# low income countries

lexp_low_pepfar_fig <-
  ggplot(
    lexp_low_pepfar <- low_income %>%
      # indicator changes with each figure
      filter(
        indicator == "life_expectancy_at_birth_both_sexes_years") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    # value at which to label "outliers" changes with each figure
    x = year, y = value, color = pepfar,
    label = if_else(value < 20, as.character(iso), "")),
  hjust = -0.4, vjust = 0.3,
  position = position_jitter(width = -0.3),
  size = 2) 

# apply settings common to all Life Exp figs
lexp_low_pepfar_fig <- lexp_settings(lexp_low_pepfar_fig)

# By how many years did the weighted average life expectancy
# increase over time?

lexp_low_pct <- lexp_low_pepfar %>%
  filter(year %in% c("2003", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar,year) %>%
  distinct()

# pepfar 
lexp_low_pct_pepfar <- lexp_low_pct  %>%
  filter(pepfar == "PEPFAR")
round_half_up(lexp_low_pct_pepfar$weighted_avg[2] - 
              lexp_low_pct_pepfar$weighted_avg[1], 0)

# non-pepfar
lexp_low_pct_nonpepfar <- lexp_low_pct  %>%
  filter(pepfar == "Non-PEPFAR")
round_half_up(lexp_low_pct_nonpepfar$weighted_avg[2] - 
              lexp_low_pct_nonpepfar$weighted_avg[1], 0)

# LMI + LI countries -----------------------------------------------------------
# figure_data contains both LI and LMI country data

# What has been the change in uhc index in 
# PEPFAR vs non-PEPFAR LI countries over time?

uhc_comb_pepfar_fig <-
  ggplot(
    uhc_comb_pepfar <- figure_data %>%
      filter(indicator == "uhc_service_coverage_index") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(
    aes(x = year, y = value, color = pepfar,
    label = if_else(value > 70, as.character(iso), "")),
  hjust = -0.5, vjust = 0.5,
 position = position_jitter(width = -0.4),
  size = 2) 

uhc_comb_pepfar_fig <- uhc_settings(uhc_comb_pepfar_fig)

# which countries were included in the above figure?
countries_uhc_comb_pepfar <- uhc_comb_pepfar %>%
  ungroup() %>%
  select(country, pepfar) %>%
  distinct()

# What percentage did the weighted average increase over time?

comb_percent <- uhc_comb_pepfar %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar,year) %>%
  distinct()

# pepfar 
comb_pct_pepfar <- comb_percent %>%
  filter(pepfar == "PEPFAR")
pct_change(comb_pct_pepfar$weighted_avg[2], 
           comb_pct_pepfar$weighted_avg[1], 0)

# non-pepfar
comb_pct_nonpepfar <- comb_percent  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(comb_pct_nonpepfar$weighted_avg[2], 
           comb_pct_nonpepfar$weighted_avg[1], 0)

# What has been the change in the UHC sub index on infectious diseases over time
# in PEPFAR vs non-PEPFAR countries?
uhc_comb_id_pepfar_fig <-
  ggplot(
    uhc_comb_id_pepfar <- figure_data %>%
      filter(indicator == "uhc_subindex4_id") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 75, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

uhc_comb_id_pepfar_fig <- uhc_settings(uhc_comb_id_pepfar_fig)

# What percentage has UHC increased in pepfar vs non-pepfar countries?

comb_uhc_id_percent <- uhc_comb_id_pepfar %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar,year) %>%
  distinct()

# pepfar 
comb_uhc_id_pct_pepfar <- comb_uhc_id_percent %>%
  filter(pepfar == "PEPFAR")
pct_change(comb_uhc_id_pct_pepfar$weighted_avg[2], # 2019
           comb_uhc_id_pct_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
comb_uhc_id_pct_nonpepfar <- comb_uhc_id_percent %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(comb_uhc_id_pct_nonpepfar$weighted_avg[2], # 2019
           comb_uhc_id_pct_nonpepfar$weighted_avg[1], 0) # 2000

# What has been the change in the UHC sub index on capacity and access over time
# in PEPFAR vs non-PEPFAR countries?
uhc_comb_ca_pepfar_fig <-
  ggplot(
    uhc_comb_ca_pepfar <- figure_data %>%
      filter(indicator == "uhc_subindex1_capacity_access") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 80, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

uhc_comb_ca_pepfar_fig <- uhc_settings(uhc_comb_ca_pepfar_fig)

# What percentage has UHC increased in pepfar vs non-pepfar countries?

uhc_comb_ca_percent <- uhc_comb_ca_pepfar  %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  distinct()

# pepfar 
pct_change(uhc_comb_ca_percent$weighted_avg[3], # 2019
           uhc_comb_ca_percent$weighted_avg[4], 0) # 2000

# non-pepfar
pct_change(uhc_comb_ca_percent$weighted_avg[1], # 2019
           uhc_comb_ca_percent$weighted_avg[2], 0) # 2000

# What has been the change in Life Expectancy at Birth in 
# PEPFAR vs non-PEPFAR LI + LMI countries over time?

lexp_comb_pepfar_fig <-
  ggplot(
    lexp_comb_pepfar <- figure_data %>%
      filter(indicator == "life_expectancy_at_birth_both_sexes_years") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value < 40, as.character(iso), "")),
  hjust = -0.4, vjust = 0.3,
  position = position_jitter(width = -0.3),
  size = 2) 

lexp_comb_pepfar_fig <- lexp_settings(lexp_comb_pepfar_fig)

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

uhc_ssa_pepfar %>%
  ggplot(aes()) +
   geom_line(aes(x = year, y = weighted_avg, 
                 alpha = 0.8, group = pepfar, 
                 color = pepfar), linewidth = 1) +
  geom_area(aes(alpha = 0.7,
    x = year, y = weighted_avg,
    group = pepfar, fill = pepfar)) +
 facet_wrap(~fct_rev(pepfar), ncol = 1) +
  annotate("rect", xmin = 2003, xmax = 2019, 
           ymin = 0, ymax = 50, alpha = .1) +
  si_style_ygrid() +
  # uhc is an index from 0-100
  scale_y_continuous(
    limits = c(0, 50),
    breaks = c(0, 25, 45)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2019)) +
  scale_color_manual(
    values = c(
      "PEPFAR" = usaid_medblue,
      "Non-PEPFAR" = usaid_medgrey),
    labels = NULL) +
  scale_fill_manual(
    values = c(
      "PEPFAR" = usaid_medblue,
      "Non-PEPFAR" = usaid_medgrey),
    labels = NULL) +
  theme(
    axis.text = element_text(
      family = "Source Sans Pro",
      size = 18,
      color = "#505050"),
    legend.position = "none",
    strip.background = element_blank(),
      strip.text.x = element_blank()) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL, 
    fill = NULL,
    group = NULL,
    caption = glue::glue("Source: WHO | {ref_id} | J. Hoehner, K. Srikanth"))

# which countries were included in the above figure?
countries_uhc_ssa_pepfar <- uhc_ssa_pepfar %>%
  ungroup() %>%
  select(country, pepfar) %>%
  distinct() %>%
  write_excel_csv("ssalow_countries.csv")

# What percentage has UHC increased in pepfar vs non-pepfar countries?

uhc_ssa_percent <- uhc_ssa_pepfar  %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_ssa_percent_pepfar <- uhc_ssa_percent  %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_ssa_percent_pepfar$weighted_avg[2], # 2019
          uhc_ssa_percent_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
uhc_ssa_percent_nonpepfar <- uhc_ssa_percent  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_ssa_percent_nonpepfar$weighted_avg[2], # 2019
          uhc_ssa_percent_nonpepfar$weighted_avg[1], 0) # 2000

# What has been the change in the UHC sub index on infectious diseases over time
# in PEPFAR vs non-PEPFAR countries?

uhc_ssa_id_pepfar_fig <-
  ggplot(
    uhc_ssa_id_pepfar <- lic_ssa %>%
      filter(
        indicator == "uhc_subindex4_id") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 60, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2)

uhc_ssa_id_pepfar_fig <- uhc_settings(uhc_ssa_id_pepfar_fig)

# What percentage has UHC increased in pepfar vs non-pepfar countries?

uhc_ssa_id_percent <- uhc_ssa_id_pepfar  %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_ssa_id_pct_pepfar <- uhc_ssa_id_percent %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_ssa_id_pct_pepfar$weighted_avg[2], # 2019
           uhc_ssa_id_pct_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
uhc_ssa_id_pct_nonpepfar <- uhc_ssa_id_percent  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_ssa_id_pct_nonpepfar$weighted_avg[2], # 2019
           uhc_ssa_id_pct_nonpepfar$weighted_avg[1], 0) # 2000

# What has been the change in the UHC sub index on capacity and access over time
# in PEPFAR vs non-PEPFAR countries?
uhc_ssa_ca_pepfar_fig <-
  ggplot(
    uhc_ssa_ca_pepfar <- lic_ssa %>%
      filter(
        indicator == "uhc_subindex1_capacity_access") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population)),
    aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 30, as.character(iso), "")),
  hjust = -0.4, vjust = 0.4,
  position = position_jitter(width = -0.4),
  size = 2) 

uhc_ssa_ca_pepfar_fig <- uhc_settings(uhc_ssa_ca_pepfar_fig)

# What percentage has UHC increased in pepfar vs non-pepfar countries?

uhc_ssa_ca_percent <- uhc_ssa_ca_pepfar  %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
uhc_ssa_ca_pct_pepfar <- uhc_ssa_ca_percent %>%
  filter(pepfar == "PEPFAR")
pct_change(uhc_ssa_ca_pct_pepfar$weighted_avg[2], # 2019
           uhc_ssa_ca_pct_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
uhc_ssa_ca_pct_nonpepfar <- uhc_ssa_ca_percent  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(uhc_ssa_ca_pct_nonpepfar$weighted_avg[2], # 2019
           uhc_ssa_ca_pct_nonpepfar$weighted_avg[1], 0) # 2000

# What has been the change in each of the subindeces on over time
# in PEPFAR supported countries?
uhc_ssa_all_pepfar <- lic_ssa %>%
  filter(
    !indicator == "life_expectancy_at_birth_both_sexes_years") %>%
  group_by(year, indicator, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population)) %>%
  filter(pepfar %in% c("PEPFAR")) %>%
  distinct()

uhc_ssa_all_pepfar_labels <- uhc_ssa_all_pepfar  %>%
  filter(year %in% c("2000", "2019"))


ggplot() +
  geom_smooth(data = uhc_ssa_all_pepfar, 
    aes(x = year, y = weighted_avg,
    group = indicator, color = indicator)) +
  geom_text(data = uhc_ssa_all_pepfar_labels,
    aes(x = year, y = weighted_avg, 
    label = glue::glue("{round_half_up(weighted_avg, 0)}"), 
    color = indicator), 
    nudge_x = 0.5, 
    nudge_y = -0.5) +
  geom_vline(
    xintercept = 2003,
    color = trolley_grey,
    linetype = "longdash",
    alpha = 0.5) +
  geom_vline(
    xintercept = 2008,
    color = trolley_grey,
    linetype = "dotted",
    alpha = 0.5) +
  si_style_ygrid() +
  # uhc is an index from 0-100
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_x_continuous(
    breaks = c(2000, 2010, 2019)) +
  scale_color_manual(
    values = c(
      "uhc_service_coverage_index" = usaid_blue,
      "uhc_subindex1_capacity_access" = moody_blue_light,
      "uhc_subindex2_ncd" = old_rose_light,
      "uhc_subindex3_mchn" = golden_sand_light,
      "uhc_subindex4_id" = genoa_light),
    labels = c(
      "uhc_service_coverage_index" = "HSC Index",
      "uhc_subindex1_capacity_access" = "HSC sub-index on service capacity and access",
      "uhc_subindex2_ncd" = "HSC sub-index on noncommunicable diseases",
      "uhc_subindex3_mchn" = "HSC sub-index on reproductive, maternal, newborn, and child health",
      "uhc_subindex4_id" = "HSC sub-index on infectious disease")) +
  theme(
    axis.text = element_text(
      family = "Source Sans Pro",
      size = 10,
      color = "#505050")) +
  theme(legend.position = "none") +
  labs(x = NULL, 
       y = NULL, 
       # title = "HSC INDEX AND SUB-INDECES INCREASED IN PEPFAR SUPPORTED LOW INCOME SUB-SAHARAN AFRICAN COUNTRIES FOLLOWING PRIORITIZATION OF HEALTH SYSTEMS STRENGTHENING",
       # subtitle = "Largest growth seen in the infectious disease sub-index followed by the sub-index on reproductive, maternal, newborn, and child health, the HSC Index overall, the sub-index on noncommunicable diseases, and the sub-index on service capacity and access",
       CAPTION = glue::glue("Source:  | {ref_id} | Jessica Hoehner "))

# What has been the change in Life Expectancy at Birth in 
# PEPFAR vs non-PEPFAR countries over time?
# low income countries

    lexp_ssa_pepfar <- lic_ssa %>%
      filter(
        indicator == "life_expectancy_at_birth_both_sexes_years") %>%
      group_by(year, pepfar) %>%
      mutate(
        weighted_avg = weighted.mean(value, population))
    
  lexp_ssa_pepfar %>%
    ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))+
    geom_point(size = 4, alpha = .7) +
    annotate("rect", xmin = 2003, xmax = 2020, 
             ymin = 45, ymax = 65, alpha = .1) +
    si_style_ygrid() +
    scale_y_continuous(
      limits = c(45, 65),
      breaks = c(45, 55, 65)) +
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
        size = 18,
        color = "#505050"),
      legend.position = "none") +
    labs(
      x = NULL,
      y = NULL,
      color = NULL, 
      fill = NULL,
      group = NULL,
      caption = glue::glue("Source: WHO | {ref_id} | Jessica Hoehner"))
  
# save images ------------------------------------------------------------------


# LI countries by PEPFAR
# UHC LI countries by PEPFAR
si_save(glue("{output_loc}/uhc_lowinc_pepfar_fig_{date}.svg"), 
        plot = uhc_low_pepfar_fig)
# LEXP LI countries by PEPFAR
si_save(glue("{output_loc}/lifexp_lowinc_pepfar_fig_{date}.svg"), 
        plot = lexp_low_pepfar_fig)
# UHC (ID) LI countries by PEPFAR
si_save(glue("{output_loc}/uhcID_lowinc_pepfar_fig_{date}.svg"), 
        plot = uhc_low_id_pepfar_fig)
# UHC (CA) LI countries by PEPFAR
si_save(glue("{output_loc}/uhcCA_lowinc_pepfar_fig_{date}.svg"), 
        plot = uhc_low_ca_pepfar_fig)

# Combined (LI+LMI) countries by PEPFAR
# UHC comb countries by PEPFAR
si_save(glue("{output_loc}/uhc_combinc_pepfar_fig_{date}.svg"), 
        plot = uhc_comb_pepfar_fig)
# LEXP comb countries by PEPFAR
si_save(glue("{output_loc}/lifexp_combinc_pepfar_fig_{date}.svg"), 
        plot = lexp_comb_pepfar_fig)
# UHC (ID) comb countries by PEPFAR
si_save(glue("{output_loc}/uhcID_combinc_pepfar_fig_{date}.svg"), 
        plot = uhc_comb_id_pepfar_fig)
# UHC (CA) comb countries by PEPFAR
si_save(glue("{output_loc}/uhcCA_combinc_pepfar_fig_{date}.svg"), 
        plot = uhc_comb_ca_pepfar_fig)

# LI SSA countries only by PEPFAR
# UHC SSA countries by PEPFAR
si_save(glue("Graphics/uhc_ssainc_pepfar_fig_{date}.svg"), 
        plot = uhc_ssa_pepfar_fig)
# LEXP SSA countries by PEPFAR
si_save(glue("Graphics/lifexp_ssainc_pepfar_fig_{date}.svg"), 
        plot = lexp_ssa_pepfar_fig)
# UHC (ID) SSA countries by PEPFAR
si_save(glue("Graphics/uhcID_ssainc_pepfar_fig_{date}.svg"), 
        plot = uhc_ssa_id_pepfar_fig)
# UHC (CA) SSA countries by PEPFAR
si_save(glue("Graphics/uhcCA_ssainc_pepfar_fig_{date}.svg"), 
        plot = uhc_ssa_ca_pepfar_fig)

# save data --------------------------------------------------------------------

# add full data set here
write_excel_csv(figure_data, glue("{output_loc}/gh_scorecard_figure_data.csv"))
