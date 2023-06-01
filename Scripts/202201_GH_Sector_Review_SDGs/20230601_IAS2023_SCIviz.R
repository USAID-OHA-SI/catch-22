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
date <- today()

# functions --------------------------------------------------------------------

# percent change function
# sample calculation
# percent increase formula =
# ((weighted_avg(pepfar)2019 - weighted_avg(pepfar)2000/ weighted_avg(pepfar)2000)/
# weighted_avg(pepfar)2000)*100

pct_change <- function(current, starting, sig_dig) {
  round_half_up(sum(sum(current - starting) / abs(starting)) * 100, sig_dig)
}

# SCI figure function

sci_viz <- function(ggobj, ylims, ybreaks, caption, ...) {
  ggobj <- ggobj +
    geom_point(
      aes(
        x = year, y = value,
        color = pepfar, fill = pepfar
      ),
      alpha = 0.4,
      position = position_jitter(width = 0.2)
    ) +
    geom_point(size = 4, alpha = .7) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = c(2003), linetype = "dashed") +
    si_style_ygrid() +
    scale_y_continuous(
      limits = ylims,
      breaks = ybreaks
    ) +
    scale_x_continuous(
      breaks = c(2000, 2005, 2010, 2015, 2019)
    ) +
    scale_color_manual(
      values = c(
        "PEPFAR" = usaid_medblue,
        "Non-PEPFAR" = usaid_medgrey
      )
    ) +
    scale_fill_manual(
      values = c(
        "PEPFAR" = usaid_medblue,
        "Non-PEPFAR" = usaid_medgrey
      )
    ) +
    theme(
      axis.text = element_text(
        family = "Source Sans Pro",
        size = 18,
        color = "#505050"
      ),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL,
      fill = NULL,
      group = NULL,
      caption = glue::glue("{caption} |
                            Source: WHO | {ref_id} | J. Hoehner, K. Srikanth")
    )

  return(ggobj)
}

# read in data -----------------------------------------------------------------

# produced by catch-22/Scripts/202201_GH_Sector_Review_SDGs/
# country_weighting/src/clean.R
figure_data <- read_sheet("17ZBOzPux0lgkmN48BtE3yzKpeEzDLVmx9B4Pei9nGbw",
  sheet = "figure_data")

# munge ------------------------------------------------------------------------

# Overall SCI

df_overall <- figure_data %>%
  filter(
    income_group == "Low Income Country (World Bank Classification)",
    usaid_region == "Sub-Saharan Africa",
    indicator == "uhc_service_coverage_index") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# SCI sub-index on infectious diseases ----

df_id <- figure_data %>%
  filter(
    income_group == "Low Income Country (World Bank Classification)",
    usaid_region == "Sub-Saharan Africa",
    indicator == "uhc_subindex4_id") %>%
  group_by(year, pepfar) %>%
  mutate(
    weighted_avg = weighted.mean(value, population))

# SCI sub-index on capacity and access ----

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
sci_ssa_percent_pepfar <- df_pct_overall %>%
  filter(pepfar == "PEPFAR")
pct_change(
  sci_ssa_percent_pepfar$weighted_avg[2], # 2019
  sci_ssa_percent_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_ssa_percent_nonpepfar <- df_pct_overall %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(
  sci_ssa_percent_nonpepfar$weighted_avg[2], # 2019
  sci_ssa_percent_nonpepfar$weighted_avg[1], 0) # 2000

# SCI subindex on infectious diseases ----

df_pct_id <- df_id %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar
sci_id_pepfar <- df_pct_id %>%
  filter(pepfar == "PEPFAR")
pct_change(
  sci_id_pepfar$weighted_avg[2], # 2019
  sci_id_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_id_nonpepfar <- df_pct_id %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(
  sci_id_nonpepfar$weighted_avg[2], # 2019
  sci_id_nonpepfar$weighted_avg[1], 0) # 2000

# SCI subindex on capacity and access ----

df_pct_ca <- df_ca %>%
  filter(year %in% c("2000", "2019")) %>%
  select(pepfar, year, weighted_avg) %>%
  arrange(pepfar, year) %>%
  distinct()

# pepfar
sci_ca_pepfar <- df_pct_ca %>%
  filter(pepfar == "PEPFAR")
pct_change(
  sci_ca_pepfar$weighted_avg[2], # 2019
  sci_ca_pepfar$weighted_avg[1], 0) # 2000

# non-pepfar
sci_ca_nonpepfar <- df_pct_ca %>%
  filter(pepfar == "Non-PEPFAR")
pct_change(
  sci_ca_nonpepfar$weighted_avg[2], # 2019
  sci_ca_nonpepfar$weighted_avg[1], 0) # 2000

# Visualize --------------------------------------------------------------------

# overall SCI ------------------

# visual from March SI newsletter

df_overall_viz <- df_overall %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))

sci_viz(df_overall_viz,
  ylims = c(0, 50), ybreaks = c(0, 25, 45),
  caption = "Figure 1: Overall SCI")

si_save(glue("Graphics/sci_overall_{date}.svg"))

# which countries were included in the above figure?
countries_overall_viz <- df_overall_viz %>%
  ungroup() %>%
  select(country, pepfar) %>%
  distinct()

# SCI sub index on infectious diseases ----------------

df_viz_id <- df_id %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))

sci_viz(df_viz_id,
  ylims = c(0, 80),
  ybreaks = c(0, 20, 40, 60),
  caption = "Figure 1.1: SCI sub-index on infectious disease")

si_save(glue("Graphics/sciID_{date}.svg"))

# SCI sub index on capacity and access ----------------

df_viz_ca <- df_ca %>%
  ggplot(aes(year, weighted_avg, group = pepfar, color = pepfar))

sci_viz(df_viz_ca,
  ylims = c(0, 40),
  ybreaks = c(0, 10, 30, 40),
  caption = "Figure 1.2: SCI sub-index on capacity and access")

si_save(glue("Graphics/sciCA_{date}.svg"))