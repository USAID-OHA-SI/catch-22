# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Budget summary by funds for GH portfolio reivew
# REF ID:   36a4c56d 
# LICENSE:  MIT
# DATE:     2023-07-19
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "36a4c56d"

# IMPORT ------------------------------------------------------------------
  
  bugdet_id <- "1c13egLNssTFmTqGZnYiUJrsBpUJPFvLH7kmaHembTR8"
  
  df_budget <- read_sheet(as_sheets_id(bugdet_id), sheet = 1, skip =2) %>% 
    janitor::clean_names()
  
  df_hop <- read_sheet(as_sheets_id(bugdet_id), sheet = 3, skip =2) %>% 
    janitor::clean_names()

# MUNGE -------------------------------------------------------------------

df_viz2 <- df_budget %>% 
  mutate(fiscal_year = str_detect(row_labels, "FY"),
         fiscal_year = ifelse(fiscal_year == TRUE, row_labels, NA)) %>% 
  fill(fiscal_year) %>% 
  filter(row_labels %ni% c("FY20", "FY21", "FY22", "Grand Total")) %>% 
  rename(accounts = row_labels,
         total = x5) %>% 
  pivot_longer(cols = c(2:4), names_to = "fund_type", values_to = "value") %>% 
  mutate(fill_color = case_when(fund_type == "outlayed" ~ denim_light,
                                fund_type == "unliquidated" ~ "#5882D8",
                                fund_type == "unobligated" ~ "#000c4f"))


df_viz2 %>% 
  filter(accounts != "ESF") %>% 
  ggplot(aes(x = fiscal_year, y = value, fill = fill_color)) +
  geom_col() +
  facet_wrap(~accounts, scales = "free_y") +
  geom_text(position="stack",
            aes(x = fiscal_year, y = value, label = value),
            family = "Source Sans Pro", color = nero) +
  si_style_ygrid() +
  scale_fill_identity() +
  labs(x = NULL, y = NULL,
       title = "FY20 & FY21 fund accounts are 99% obligated, and FY22 fund accounts are 95% obligated" %>% toupper(),
       subtitle = "ESF-ARPA (Economic Support Fund through the American Rescue Plan Act) accounted for some funding in FY21 as COVID-19 supplemental funding",
       caption = glue("Source: FY20-22 USAID PEPFAR Budget by Fund | Ref id: {ref_id}"))

si_save("Graphics/GHSectorReview_Fund_Review.svg")


# HOP ---------------------------------------------------

df_viz_hop1 <- df_hop %>% 
  select(1:2) %>% 
  filter(!is.na(category),
         category != "Total") %>% 
  rename(budget = hop23_tom_request) %>% 
  mutate(total = sum(budget),
         share = budget / total) 

df_viz_hop2 <- df_hop %>% 
  select(5:6) %>% 
  filter(!is.na(focus_area_5),
         focus_area_5 != "Grand Total") %>% 
  rename(budget = hop23_hsm_request) %>% 
  mutate(total = sum(budget),
         share = budget / total) 




#HOP23 TOM Budget by Category
v1 <- df_viz_hop1 %>% 
  ggplot(aes(y = fct_reorder(category, budget))) + 
  #geom_col(aes(x = total), fill = trolley_grey_light) + 
  geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = category, xmin = total, xmax =total),
                color = trolley_grey) +
  geom_col(aes(budget, alpha = 0.8), fill = golden_sand) +
  geom_text(aes(x = budget + 5000000, 
                label = label_number(0.1, scale_cut = cut_short_scale())(budget)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = budget + 5000000,
                label = percent(share, 0.1),
                family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  # coord_flip() +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL,
       y = NULL,
       title = "HOP23 TOM Request: Total $130.4m" %>% toupper())


#HOP23 TOM Budget by Category
v2 <- df_viz_hop2 %>% 
  ggplot(aes(y = fct_reorder(focus_area_5, budget))) + 
  #geom_col(aes(x = total), fill = trolley_grey_light) + 
  geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = focus_area_5, xmin = total, xmax =total),
                color = trolley_grey) +
  geom_col(aes(budget, alpha = 0.8), fill = scooter) +
  geom_text(aes(x = budget + 5000000, 
                label = label_number(0.1, scale_cut = cut_short_scale())(budget)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = budget + 5000000,
                label = percent(share, 0.1),
                family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  #coord_flip() +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL,
       y = NULL,
       title = "HOP23 HSM Request: Total $33.8m" %>% toupper(),
       # title = "USAID's share of total COP/ROP23 planned funding is 52.3%" %>% toupper(),
       caption = glue("Source: HOP23 TOM Budget by Category & HOP23 HSM Budget Request by Mechanism | Ref id: {ref_id}"))

v1 + v2

si_save("Graphics/HOP23_remake.svg")

           