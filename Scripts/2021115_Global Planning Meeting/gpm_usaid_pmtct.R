# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Global Planning Meeting
# LICENSE:  MIT
# DATE:     2021-11-1

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
library(janitor)
library(lubridate)
library(googlesheets4)

# load fonts
folderpath <- "C:/Users/STAR/Desktop/FontFolder" #not necessary, just use if computer doesnt have fonts locally
font_import(folderpath)
library(Rttf2pt1)
library(extrafont)

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth")


#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd()

#less than 2 months EID
df_2mo <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "PMTCT_EID",
         standardizeddisaggregate == "Age",
         ageasentered == "<=02 Months", 
         fiscal_year <= curr_fy) %>%
  group_by(fiscal_year, fundingagency, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  rename(less2mo = value)

#EID total denominator
df_denom <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "PMTCT_EID",
         standardizeddisaggregate == "Total Denominator",
         fiscal_year <= curr_fy) %>% 
  group_by(fiscal_year, fundingagency, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  rename(total_denom = value)

#join and calculate testing coverage
df_viz <- df_2mo %>% 
  left_join(df_denom, by = c("period", "fundingagency", "period_type", "indicator")) %>% 
  mutate(coverage = less2mo / total_denom)

#VIZ -----------------------------------

#grab the latest stat from max period
latest_stat <- df_viz %>% 
  filter(period == max(period)) %>% 
  pull()

#grab the max period
latest_pd <- df_viz %>% 
  slice_max(order_by = period, n = 1) %>% 
  pull(period)

#% testing coverage across periods
v1 <- df_viz %>% 
  ggplot(aes(period, coverage, group = period_type)) +
  geom_blank(aes(y = 1.1 * coverage)) +
  geom_line(size = 1.5, color = burnt_sienna_light) +
  geom_point(shape = 21, color = burnt_sienna_light, fill = burnt_sienna_light, size = 12, stroke = 2) +
  geom_text(aes(label = percent(coverage, 1)),
            family = "Source Sans Pro", size = 12/.pt) +
  expand_limits(y = .2) +
  labs(title = glue("USAID's EID <2 month testing coverage was {percent(latest_stat, 1)} in {latest_pd}"),
       x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) + coord_cartesian(expand = F, clip = "off")


#Bar chart of # <2 mo EID
v2 <- df_viz %>%
  mutate(value_label = clean_number(less2mo)) %>% 
  ggplot(aes(period, less2mo)) + 
  #geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = burnt_sienna),
           position = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = value_label), na.rm = TRUE, vjust = -0.5, 
            family = "Source Sans Pro SemiBold") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right", expand = c(.005, .005)) +
  #scale_x_continuous(expand = c(.005, .005), n.breaks = unique(df_kp_viz$period) %>% length())+
  #scale_alpha_identity() +
  #scale_fill_manual(values = c(denim, denim_light)) +
  labs(x = NULL, y = NULL, fill = NULL,
       caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_nolines() +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines")) + 
  coord_cartesian(expand = F, clip = "off")

#patchwork together
(v1/v2) +
  plot_layout(heights = c(3, 8))

si_save("GPM_PMTCT.svg") 
