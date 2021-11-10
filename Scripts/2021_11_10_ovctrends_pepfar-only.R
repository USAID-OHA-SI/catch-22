# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  OVC care - combined plots for PEPFAR only
# LICENSE:  MIT
# DATE:     2021-11-10
# NOTE:     adapted from "catch-22/Scripts/2021_08_13_ovcwork.R"

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(ICPIutilities)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(packcircles)

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}




# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Aaron Chafetz", "Karishma Srikanth")


#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd() 

df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

# TRENDS IN KNOWN STATUS PROXY --------------------------------------------

df_knowstatus <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"& standardizeddisaggregate == "ReportedStatus"),
        # fundingagency == "USAID",
         fiscal_year >=2018) %>%
  mutate(otherdisaggregate = ifelse(is.na(otherdisaggregate), "NA", otherdisaggregate)) %>% 
  filter(otherdisaggregate != "No HIV Status") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(knownstat = OVC_HIVSTAT/OVC_SERV)

#grab the % know status for max period
latest_stat <- df_knowstatus %>% 
  filter(period == max(period)) %>% 
  pull()

#grab the max period
latest_pd <- df_knowstatus %>% 
  slice_max(order_by = period, n = 1) %>% 
  pull(period)

#visual 1: OVC known status
v1 <- df_knowstatus %>% 
  filter(!is.na(knownstat)) %>% 
  ggplot(aes(period, knownstat, group = period_type)) +
  geom_blank(aes(y = 1.1 * knownstat)) +
  geom_line(size = 1.5, color = scooter_light) +
  geom_point(shape = 21, color = scooter_light, fill = scooter_light, size = 10, stroke = 2) +
  geom_text(aes(label = percent(knownstat, 1)),
            family = "Source Sans Pro", size = 10/.pt) +
  expand_limits(y = .2) +
  labs(subtitle = glue("As of {latest_pd}, {percent(latest_stat, 1)} of OVC <18 in PEPFAR know their status"),
       x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  coord_cartesian(expand = F, clip = "off")

# HIV+ OVC (BAR CHART) --------------------------------------------

df_hiv_ovc <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"& (standardizeddisaggregate == "ReportedStatus" | standardizeddisaggregate == "StatusPosART")),
        # fundingagency == "USAID",
         fiscal_year >=2018) 

#2018 - munge for 2018 because disaggs different
df_hiv_ovc18 <- df_hiv_ovc %>% 
  filter(fiscal_year == 2018,
         standardizeddisaggregate == "StatusPosART") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#munge for 2019-2021  
df_hiv_ovc <- df_hiv_ovc %>% 
  filter(fiscal_year > 2018,
         standardizeddisaggregate == "ReportedStatus",
         statushiv == "Positive") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#latest stat
latest_stat_ovc <- df_hiv_ovc %>% 
  filter(period == max(period)) %>% 
  pull() %>% 
  clean_number()

#bind and add color
df_hiv_ovc <- df_hiv_ovc18 %>% 
  bind_rows(df_hiv_ovc) %>%
  mutate(bar_color = genoa)

#visual 2 - bar chart of # HIV+ OVC over time
v2 <- df_hiv_ovc %>% 
filter(OVC_HIVSTAT > 0) %>% 
  ggplot(aes(period, OVC_HIVSTAT)) + 
  geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = bar_color), na.rm = TRUE) +
  scale_fill_identity() + 
  scale_y_continuous(label = scales::comma,
                     expand = c(.005, .005)) +
  geom_text(aes(label = clean_number(OVC_HIVSTAT), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro")) +
  labs(x = NULL, y = NULL,
       subtitle = glue("As of {latest_pd_art}, PEPFAR enrolled over {latest_stat_ovc}
                    OVCs <18 with HIV on a comprehensive package of services")) +
  si_style_ygrid() +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        axis.text.y = element_blank()) + 
  coord_cartesian(expand = F, clip = "off")

# ART COVERAGE % -------------------------------------------------------------------

df_art <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"),
        # fundingagency == "USAID",
         fiscal_year >=2018)

#create shares 
df_art <- df_art %>%
  filter(indicator == "OVC_HIVSTAT",
         otherdisaggregate == "Receiving ART") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator) %>%
  rename(ovc_art = OVC_HIVSTAT) %>% 
  left_join(df_hiv_ovc, by = c("period","period_type")) %>% 
  filter(ovc_art > 0 & OVC_HIVSTAT) %>% 
  mutate(art_share = ovc_art / OVC_HIVSTAT)

#grab the latest stat from max period
latest_stat_art <- df_art %>% 
  filter(period == max(period)) %>% 
  pull()

#grab the max period
latest_pd_art <- df_art %>% 
  slice_max(order_by = period, n = 1) %>% 
  pull(period)

#visual 3: OVC < 18 on ART
v3 <- df_art %>% 
  ggplot(aes(period, art_share, group = period_type)) +
  geom_blank(aes(y = 1.1 * art_share)) +
  geom_line(size = 1.5, color = genoa_light) +
  geom_point(shape = 21, color = genoa_light, fill = genoa_light, size = 10, stroke = 2) +
  geom_text(aes(label = percent(art_share, 1)),
            family = "Source Sans Pro", size = 10/.pt) +
  expand_limits(y = .2) +
  labs(subtitle = glue("As of {latest_pd_art}, almost {percent(latest_stat_art, 1)} of OVC <18 in PEPFAR are on ARTs"),
       x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) + coord_cartesian(expand = F, clip = "off")


# COMBINE PLOTS -----------------------------------------------------------

v_right <- (v3/v2) +
  plot_layout(heights = c(4, ))

v1/v_right + plot_layout(heights= c(2,8)) +  plot_annotation(
  caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) & 
  theme(plot.title = element_text(family = "Source Sans Pro",
                                  size = 14,
                                  face = "bold",
                                  color =  "#202020",
                                  hjust = 0),
        plot.caption = element_text(family = "Source Sans Pro",
                                    size = 9,
                                    color = "#909090",
                                    hjust = 1, vjust = 1))

si_save("Graphics/20211110_OVC_justPEPFAR.svg")


