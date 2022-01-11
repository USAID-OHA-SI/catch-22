# PURPOSE: GPM - PopVLS viz
# AUTHOR: K Srikanth | SI
# LICENSE: MIT
# DATE: 2021-11-12
# UPDATED: 2022-01-05
# NOTES: changed to all age groups and update to pull_unaids


# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(tidyverse)
library(gophr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(here)
library(patchwork)
library(googlesheets4)
library(janitor)
library(ggnewscale)
library(mindthegap)
library(glue)

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

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

#Read in data with munge_unaids

#UNAIDS data
df_unaids_int <- pull_unaids("Test & Treat - Integer", pepfar_only = TRUE) # num  VLS
df_unaids_pct <- pull_unaids("Test & Treat - Percent", pepfar_only = TRUE) # % VLS
df_est <- pull_unaids("HIV Estimates - Integer", pepfar_only = TRUE) # num PLHIV


# GLOBALS -----------------------------------------------------------------

ind_sel <- c("KNOWN_STATUS", "PLHIV_ON_ART", "VLS")

authors <- c("Karishma Srikanth")

#goal - pop VLS goals are different
goal <- c(95, 91, 85)

#get list of pepfar country names
pepfar_cntry <- get_outable(datim_user(), datim_pwd()) %>% 
  filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
  pull(countryname)

#MUNGE ---------------------------------------------------------------

# num PLHIV
df_est <- df_est %>%
  filter(country %in% pepfar_cntry,
         indicator == "PLHIV",
         year == 2020,
         stat == "est",
         age == "all",
         sex == "all") %>%
  select(country, indicator, value) %>% 
  pivot_wider(names_from = indicator, values_from = value)

#% indicators
df_unaids_pct <- df_unaids_pct %>% 
  filter(year == max(year),
         sex == "all",
         age == "all",
         stat == "est",
         country %in% pepfar_cntry,
         indicator %in% ind_sel)

# num VLS
vls_num <- df_unaids_int %>% 
  filter(year == max(year),
         sex == "all",
         age == "all",
         stat == "est",
         country %in% pepfar_cntry,
         indicator == "VLS")

# viz with % indicators
df_viz <- df_unaids_pct %>% 
  left_join(df_est) %>% 
  filter(country != "Vietnam") %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country),
         goal = case_when(indicator == "PLHIV_ON_ART" ~ 91,
                          indicator == "KNOWN_STATUS" ~ 95,
                          indicator == "VLS" ~ 85),
         indicator = case_when(indicator == "PLHIV_ON_ART" ~ "On Treatment",
                               indicator == "KNOWN_STATUS" ~ "Known Status",
                               indicator == "VLS" ~ "Virally Suppressed",
                               TRUE ~ indicator),
         PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))

df_viz <- df_viz %>% 
  group_by(country) %>% 
  mutate(value = round(value, 2),
         grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
                         value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                         TRUE ~ 0),
         achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
         dot_color = case_when(grouping == "Known Status" ~ old_rose,
                               grouping == "On Treatment" ~ golden_sand,
                               grouping == "Virally Suppressed" ~ scooter,
                               grouping == "Achieved" ~ genoa,
                               # grouping == "Z_Achieved" ~ genoa,
                               TRUE ~ trolley_grey)) %>% 
  fill(grouping, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gap_bar = case_when(value < goal ~ value),
         country = reorder_within(country, gap, grouping, max, na.rm = TRUE))
  
#VIZ ---------------------------------------------

epi_ctrl_cnt <- df_viz %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

df_viz %>% 
  filter(indicator == "Virally Suppressed",
         !iso %in% c("AGO", "SSD", "TZA", "CMR", "COD")) %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = 85, linetype = "dashed") + 
 # geom_vline(xintercept = 91, linetype = "dashed") + 
 # geom_vline(xintercept = 95, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
 # scale_x_continuous(label = percent) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF 2020, {epi_ctrl_cnt} PEPFAR COUNTRIES HAVE ACHIEVED POPULATION VIRAL LOAD SUPPRESSION"),
       subtitle = "Population Viral Load Suppression defined as the number of PLHIV virally suppressed over total number of PLHIV",
       caption = glue("Source: UNAIDS 90-90-90 Percent Indicators",
                      "USAID SI Analytics",
                      "OHA Briefing with Dr. Gawande, January 2022", .sep = " | ")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/pop-vls-last90.svg")




