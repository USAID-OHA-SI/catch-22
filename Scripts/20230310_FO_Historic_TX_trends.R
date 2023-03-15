# PROJECT:  Starting Projects
# AUTHOR:   P. Ranade | USAID
# PURPOSE:  Historic TX trends
# LICENSE:  MIT
# DATE:     2023-03-14

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(gagglr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gophr)
library(janitor)
library(lubridate)
library(gt)

# DATIM GENIE REPORT PARAMETERS -------------------------------------------

# PSNU By IM
# DATIM data as of: 2/10/2023, 23:01:05 UTC
# Genie report updated: 2/13/2023, 18:12:54 UTC
# Current period(s): 2020 Target, 2021 Target, 2021 Q1, 2021 Q2, 2021 Q3, 2021 Q4, 2022 Target, 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2023 Target, 2023 Q1
# Daily/Frozen: Frozen
# Indicator: TX_CURR, TX_NEW
# Standardized Disaggregate: Total Numerator
# Fiscal Years: 2015-2023

# GLOBAL VARIABLES --------------------------------------------------------

# authors <- c("Prasann Ranade", "Nada Petrovic", "Karishma Srikanth", "Aaron Chafetz")
# ref_id <- 00000

# GLOBAL VARIABLES --------------------------------------------------------

genie_path <- "C:/Users/STAR/Documents/Data/Genie_PSNU_IM_Global_Frozen_0151da20-0688-4a13-a4d0-857ba8703dbc.txt" 
#id for adorning to plots, making it easier to find on GH
ref_id <- "bfbd380f" 

#list of MSD metadata elements
get_metadata(genie_path) 
metadata$curr_fy


# IMPORT ------------------------------------------------------------------

df_tx <- read_psd(genie_path)   

# MUNGE -------------------------------------------------------------------

#filter out Military SNUs
df_agg <- df_tx %>% 
  filter(str_detect(snu1, "_Mil", negate = TRUE)) %>%
  count(fiscal_year, indicator, wt = cumulative) 

#verify TX numbers
df_agg %>% 
  pivot_wider(names_from = fiscal_year, values_from = n) %>%
  gt() %>% 
  fmt_number(
    columns = -indicator,
    decimals = 2,
    suffixing = TRUE
  )

#filter until 2021
df_agg <- df_agg %>% 
  filter(fiscal_year <= 2021)

#add labels
df_agg <- df_agg %>% 
  mutate(ind_label = case_when(indicator == "TX_CURR" ~ 
                                 "Currently receiving ART treatment",
                               TRUE ~ "Newly enrolled on ART treatment"))

#shorten numbers to K/M
label_scales <- scales::label_number(accuracy=0.1, 
                                     scale_cut=scales::cut_short_scale())

#add title info
title_info <- df_agg %>% 
  filter(indicator == "TX_CURR",
         fiscal_year %in% c(min(fiscal_year), 2021)) %>% 
  select(fiscal_year, n) %>% 
  mutate(added = (n - lag(n)) %>% label_scales(),
         yrs = fiscal_year - lag(fiscal_year)) %>% 
  filter(fiscal_year == 2021)

#pull out current data point for context
val_2021 <- df_agg %>% 
  filter(fiscal_year == 2021, indicator == "TX_CURR") %>%
  select(indicator, n) %>% 
  pull(n) %>% 
  label_scales()

df_viz <- df_agg %>% 
  ggplot(aes(fiscal_year, n)) +
  geom_col(aes(fill = ind_label), position = "identity") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_agg$fiscal_year) %>% length()) +
  geom_text(aes(label = label_scales(n), vjust = -0.3,
                family = "Source Sans Pro")) +
  expand_limits(y = c(0,20000000)) +
  scale_fill_manual(values = c(denim_light, denim)) +
  scale_alpha_identity()  +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("AS OF 2021, PEPFAR AGENCIES HAVE PROVIDED TREATMENT FOR {val_2021} PATIENTS"),
       #subtitle = glue("OVER THE PAST {title_info$yrs} YEARS, USAID HAS ADDED {title_info$added} PATIENTS ONTO TREATMENT"),
       caption = glue("Source: MER {metadata$curr_fy_lab}")) +
  si_style_ygrid()

plot(df_viz)

#save plot
si_save("Images/fo_tx_trends_usaid.png")
si_save("Graphics/fo_tx_trends_usaid.svg")



