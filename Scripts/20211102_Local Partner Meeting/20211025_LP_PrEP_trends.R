# PROJECT:  catch-22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  LP meeting - PrEP trends
# REF ID:   cd414609 
# LICENSE:  MIT
# DATE:     2021-10-25
# UPDATED: 2022-10-28
# note: derived from agitprop/09a_usaid_prep_scaleup.R


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

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------

path_msd <-  si_path() %>% 
  return_latest("OU_IM_FY20")

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY20") %>% 
  read_msd()

#Archived MSD
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

ref_id <- "cd414609"


#source info

get_metadata(path_msd, caption_note = "Created by: OHA SI Team")

# curr_pd <- identifypd(df)
# curr_fy <- identifypd(df, "year")
# msd_source <- source_info()

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}




#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
  clean_names() %>% 
  rename(mech_code = mechanism_id) %>% 
  mutate(mech_code = as.character(mech_code),
         partner_type = case_when(partner_type == "Regional" ~ "Local",
                                  partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type))

#PrEP --------------------------------------------------------------

#bind archived + current MSD and filter for PrEP - add the 
df_prep <- df %>%
  bind_rows(df_arch) %>% 
  filter(funding_agency == "USAID",
         indicator == "PrEP_NEW",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year >= 2017) %>% 
  group_by(mech_code) %>% 
  left_join(df_partner, by = c("mech_code")) %>% 
  ungroup()

#current fy prep for local partners
prep_cum <- df_prep %>% 
  filter(fiscal_year == metadata$curr_fy, partner_type == "Local") %>%
  count(wt = cumulative)

#count number of countries with PrEP
df_cntry_cnt <- df_prep %>% 
  filter(cumulative > 0,
         partner_type == "Local") %>% 
  distinct(fiscal_year, country) %>% 
  count(fiscal_year, name = "n_countries")

#aggregate result to USAID level
df_prep <- df_prep %>% 
  group_by(fiscal_year, funding_agency,  partner_type) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>%
 # filter(fiscal_year != 2022) %>%
  reshape_msd() %>% 
  filter(partner_type != "TBD") %>%
  pivot_wider(names_from = partner_type, values_from = value) %>% 
  mutate(Total = International + Local,
         share = Local / Total) %>% 
  pivot_longer(cols = International:Total, names_to = "partner_type") %>% 
  mutate(value = na_if(value, 0)) %>% 
  select(-period_type) %>% 
  arrange(period) 

# Full list of pds

#PrEP in and out of quarterly and semi-annual reporting so need a complete set  
#current period
curr_pd <- metadata$curr_pd

#current period as number
curr_pd_num <- curr_pd %>% 
  str_remove("FY") %>% 
  str_replace("Q", ".") %>% 
  as.numeric()

#identify current fiscal year for max date
curr_fy <- metadata$curr_fy %>% 
  str_sub(3,4) %>% as.numeric()

#propagate list of periods not in prep to add to df
full_pds <- expand_grid(fiscal_year = c(17:curr_fy),
                        quarter = c(1:4)) %>% 
  unite(period, c(fiscal_year, quarter), sep = ".") %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period <= curr_pd_num) %>% 
  mutate(period = period %>% 
           paste0("FY", .) %>% 
           str_replace("\\.", "Q")) 

extra_pds <- full_pds %>% 
  filter(!period %in% unique(df_prep$period))

#VIZ ---

fy_start <-  full_pds %>% 
  filter(str_detect(period, "Q1")) %>% 
  pull()

pd_breaks <- full_pds %>% 
  # filter(str_detect(period, "Q(1|3)")) %>% 
  pull()

df_viz <- df_prep %>% 
  bind_rows(extra_pds) %>% 
  arrange(period)  %>% 
  filter(partner_type != "Total") %>% 
  mutate(line_type = ifelse(partner_type == "Local", "solid", "dashed"),
         fill_color = ifelse(partner_type == "Local", scooter, scooter_light))

title_info_prep <- df_prep %>% 
  filter(period == metadata$curr_pd, 
         partner_type == "Local") %>% 
  mutate(share = percent(round(share, 2)))


df_viz %>% 
  ggplot(aes(period, value, group = partner_type, linetype = line_type, fill = fill_color, color = fill_color)) + 
  geom_area(alpha = .2, 
            size = 1, na.rm = TRUE) +
  #  geom_area(fill = fill, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
  geom_vline(xintercept = fy_start, color = "white", 
             size = .9, linetype = "dotted") +
  #geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
  scale_y_continuous(label = clean_number, position = "right", expand = c(.01, .01)) +
  scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL, y = NULL, 
       title = glue("Local partners have initiated {clean_number(prep_cum, 0)} \\
                      onto PrEP this year and made up over {title_info_prep$share} of USAID's PrEP portfolio in {metadata$curr_pd}") %>% toupper,
       subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
       caption = glue("{metadata$caption}")) +
  si_style_ygrid() +
  theme(legend.position = 'none')

si_save(glue("Graphics/partner_prep_trends_usaid_{metadata$curr_pd}.svg"))

# Old caption:
# "Local partners have initiated {clean_number(prep_cum, 0)} \\
#                       onto PrEP this year across \\
#                       {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
#                       countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
#                       countries
#                       in 2017, and made up over {title_info_prep$share} of USAID's PrEP portfolio in {metadata$curr_pd}"
