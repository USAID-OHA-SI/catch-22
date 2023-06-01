# PROJECT:  catch-22
# AUTHOR:   J.Hoehner | USAID/GHTASC
# PURPOSE:  To develop and iterate on visuals for IAS 2023
# REF ID:   ze3987bc
# LICENSE:  MIT
# DATE CREATED: 2023-06-03

# dependencies -----------------------------------------------------------------

library(gagglr)
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

ref_id <- "ze3987bc"

load_secrets()

output_loc <- "Scripts/202201_GH_Sector_Review_SDGs/country_weighting/weightedviz/output"

date <- today()

# percent change function 
# sample calculation
# percent increase formula = 
# ((weighted_avg(pepfar)2019 - weighted_avg(pepfar)2000/ weighted_avg(pepfar)2000)/
# weighted_avg(pepfar)2000)*100

pct_change <- function(current, starting, sig_dig){
  
  round_half_up(sum(sum(current - starting)/abs(starting))*100, sig_dig)
  
}

# repeated settings for SCI subindex figures
sci_settings <- function(ggobj) {
  
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

# read in data -----------------------------------------------------------------

figure_data <- read_sheet("17ZBOzPux0lgkmN48BtE3yzKpeEzDLVmx9B4Pei9nGbw", 
                          sheet = "figure_data")

# munge ------------------------------------------------------------------------

# Overall SCI 

df_overall <- figure_data %>%
  filter(income_group == "Low Income Country (World Bank Classification)",
         usaid_region == "Sub-Saharan Africa", 
         indicator == "sci_service_coverage_index") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# SCI subindex on infectious diseases

# What has been the change in the sci sub index on infectious diseases over time
# in PEPFAR vs non-PEPFAR countries?

df_id <- figure_data %>%
  filter(
    income_group == "Low Income Country (World Bank Classification)",
    usaid_region == "Sub-Saharan Africa",
    indicator == "sci_subindex4_id") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# SCI subindex on capacity and access 

df_ca <- figure_data %>%
  filter(
    income_group == "Low Income Country (World Bank Classification)",
    usaid_region == "Sub-Saharan Africa",
    indicator == "uhc_subindex1_capacity_access") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# Summarize --------------------------------------------------------------------

# What percentage has SCI (overall) increased in pepfar vs non-pepfar countries?

df_pct_overall <- df_overall %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
sci_ssa_percent_pepfar <- df_pct_overall  %>%
  filter(pepfar == "PEPFAR")
pct_change(sci_ssa_percent_pepfar$weighted_avg[2], # 2019
           sci_ssa_percent_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_ssa_percent_nonpepfar <- df_pct_overall  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(sci_ssa_percent_nonpepfar$weighted_avg[2], # 2019
           sci_ssa_percent_nonpepfar$weighted_avg[1], 0) # 2000

# SCI subindex on infectious diseases 

# What percentage has SCI (ID subindex) changed in pepfar vs non-pepfar countries?

df_pct_id <- df_id %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
sci_id_pepfar <- df_pct_id  %>%
  filter(pepfar == "PEPFAR")
pct_change(sci_id_pepfar$weighted_avg[2], # 2019
           sci_id_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_id_nonpepfar <- df_pct_id  %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(sci_id_pepfar_nonpepfar$weighted_avg[2], # 2019
           sci_id_pepfar_nonpepfar$weighted_avg[1], 0) # 2000

# SCI subindex on capacity and access

# What percentage has sci increased in pepfar vs non-pepfar countries?

df_pct_ca <- df_ca  %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar 
sci_ca_pepfar <- df_pct_ca %>%
  filter(pepfar == "PEPFAR")
pct_change(sci_ca_pepfar$weighted_avg[2], # 2019
           sci_ca_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_ca_nonpepfar <- df_pct_ca %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(sci_ca_nonpepfar$weighted_avg[2], # 2019
           sci_ca_nonpepfar$weighted_avg[1], 0) # 2000

# Visualize --------------------------------------------------------------------

# overall SCI ------------------
# visual from March SI newsletter

df_overall %>%
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

si_save(glue("Graphics/sci_overall_{date}.svg"))

# which countries were included in the above figure?
countries_sci_ssa_pepfar <- sci_ssa_pepfar %>%
  ungroup() %>%
  select(country, pepfar) %>%
  distinct()

# SCI sub index on infectious diseases ----------------

df_viz_id <- df_id %>%
  ggplot(aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 60, as.character(iso), "")),
    hjust = -0.4, vjust = 0.4,
    position = position_jitter(width = -0.4),
    size = 2)

sci_settings(df_viz_id)

si_save(glue("Graphics/sciID_{date}.svg"))

# SCI sub index on capacity and access ----------------

df_viz_ca <- df_ca
  ggplot(aes()) +
  geom_text(aes(
    x = year, y = value, color = pepfar,
    label = if_else(value > 30, as.character(iso), "")),
    hjust = -0.4, vjust = 0.4,
    position = position_jitter(width = -0.4),
    size = 2) 

sci_ssa_ca_pepfar_fig <- sci_settings(df_viz_ca)

# sci (CA) SSA countries by PEPFAR
si_save(glue("Graphics/sciCA_{date}.svg"))
