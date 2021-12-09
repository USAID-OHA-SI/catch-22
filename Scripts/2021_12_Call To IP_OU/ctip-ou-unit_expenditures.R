# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Call to IP/OU - Unit Expenditure Slope Graph
# LICENSE:  MIT
# DATE:     2021-12-09
# NOTE: used numbers from groundhog_day/Scripts/FY21Q1_GLOBAL_UEs-Agency.R
#       and existing EU viz that pulled from Financial dashboard

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
library(readxl)

# IMPORT ------------------------------------------------------------------

ue_df <- si_path() %>% 
  return_latest("expenditure") %>% 
  read_excel() 

# VIZ -----------------------------------------------------------

ue_df <- ue_df %>% 
  mutate(fill_color = case_when(indicator == "tx_curr" ~ golden_sand,
                                indicator == "tx_new" ~ denim,
                                indicator == "hts_tst_pos" ~ burnt_sienna,
                                indicator == "hts_tst" ~ moody_blue),
         label = case_when(indicator == "tx_curr" & fiscal_year == "FY21" ~ "Current on Treatment",
                           indicator == "tx_new" & fiscal_year == "FY21" ~ "New on Treatment",
                           indicator == "hts_tst_pos" & fiscal_year == "FY21" ~ "Positive Test",
                           indicator == "hts_tst" & fiscal_year == "FY21" ~ "Test"))

ue_df %>%
  ggplot(aes(x = fiscal_year, y = unit_expenditures, group = indicator, color = fill_color, fill = fill_color)) +
  geom_line(size = 1) +
#  geom_line() +
  geom_point(shape = 21, size = 4) +
  geom_text(aes(label = label, vjust = 0, hjust = 2,
                size = 12/.pt, family = "Source Sans Pro")) +
  geom_text(aes(label = unit_expenditures, vjust = -0.5, hjust = 0,
       size = 12/.pt, family = "Source Sans Pro")) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_xgrid() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Unit Expenditures",
       title = "DESPITE THE IMPACT OF COVID-19 ON SERVICES, FY21 USAID UNIT EXPENDITURES WERE SIMILAR TO FY20 ACROSS THE CLINICAL CASCADE",
       caption = glue("Source: {msd_source} and FSD", #"Note: Excludes mechanisms that do not have associated targets"
                                "USAID SI Analytics",
                                "Call with IPs/OU - Dec 2021", .sep = " | "))

si_save("Graphics/CTIP-OU-unit_expenditures1.svg")
