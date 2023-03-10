# PROJECT:  Starting Projects
# AUTHOR:   P. Ranade | USAID
# PURPOSE:  New and current treatment trends over time
# LICENSE:  MIT
# DATE:     2023-03-09

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

#setting filepath for fonts
# font_import(paths = "C:/Users/STAR/AppData/Local/Microsoft/Windows/Fonts")
# library(extrafont)
# fonts()

# GLOBAL VARIABLES --------------------------------------------------------

# authors <- c("Prasann Ranade", "Nada Petrovic", "Karishma Srikanth")
# ref_id <- 00000

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY21-23") %>% 
  read_psd()  
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15-20") %>%
  read_psd()   

# MUNGE -------------------------------------------------------------------

#get dates and source info
#curr_pd <- identifypd(df)
#curr_fy <- identifypd(df, "year")
msd_source <- source_info()

#get TX_CURR and TX_NEW numbers by year 
df_tx <- df %>% 
  #resolve_knownissues %>% 
  bind_rows(df_arch) %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= 2021) %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
  ungroup()

#add cumulative year and source columns
df_tx <- df_tx %>% 
  rename(period = fiscal_year, value = cumulative) %>% 
  mutate(period = str_replace(period, "20", "FY")) %>% 
  arrange(indicator, period) %>% 
  mutate(source = "MSD")

 df_tx <- df_tx %>% 
   mutate(
     bar_alpha = case_when(period == 2021 & 
                             str_detect(2021, "Q4", negate = TRUE) ~ .6,
                                TRUE ~ 1),
          year = glue("20{str_sub(period, 3, 4)}") %>% as.integer)
 View(df_tx)
#add labels
df_tx <- df_tx %>% 
  mutate(ind_label = case_when(indicator == "TX_CURR" ~ 
                                 "Currently receiving ART treatment",
                               TRUE ~ "Newly enrolled on ART treatment"))

#shorten numbers to K/M
label_scales <- scales::label_number(accuracy=0.1, 
                                     scale_cut=scales::cut_short_scale())

#add title info
title_info <- df_tx %>% 
  filter(indicator == "TX_CURR",
         period %in% c(min(period), 2021)) %>% 
  select(year, value) %>% 
  mutate(added = (value - lag(value)) %>% label_scales(),
         yrs = year - lag(year)) %>% 
  filter(year == 2021)

#pull out current data point for context
val_2021 <- df_tx %>% 
  filter(year == 2021, indicator == "TX_CURR") %>%
  select(indicator, value) %>% 
  pull(value) %>% 
  label_scales()

df_viz <- df_tx %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(alpha = bar_alpha, fill = ind_label), position = "identity") +
  #geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") + 
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_tx$period) %>% length()) +
  geom_text(aes(label = label_scales(value), vjust = -0.3,
                 family = "Source Sans Pro")) +
  expand_limits(y = c(0,25000000)) +
  scale_fill_manual(values = c(denim_light, denim)) +
  scale_alpha_identity()  +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("AS OF 2021, PEPFAR AGENCIES HAVE PROVIDED TREATMENT FOR {val_2021} PATIENTS"),
       #subtitle = glue("OVER THE PAST {title_info$yrs} YEARS, USAID HAS ADDED {title_info$added} PATIENTS ONTO TREATMENT"),
       caption = glue("Source: {msd_source} (including FY15-18)")) +
  si_style_ygrid()

plot(df_viz)

#save plot
si_save("Images/fo_tx_trends_usaid.png")
si_save("Graphics/fo_tx_trends_usaid.svg")


  
  