# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  GH portfolio review: COP23 Target and Budget Share
# REF ID:   38c539ed 
# LICENSE:  MIT
# DATE:     2023-07-18
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
  
  ref_id <- "38c539ed"

# IMPORT ------------------------------------------------------------------
  
bugdet_id <- "1c13egLNssTFmTqGZnYiUJrsBpUJPFvLH7kmaHembTR8"

target_id <- "1qfdjuRc3k0XbKjDJqTzhCk3GH5gH74YnE5oUemwituM"
  
df_budget <- read_sheet(as_sheets_id(bugdet_id), sheet = "COP ROP by Agency", skip =2) %>% 
  janitor::clean_names()

df_target <- read_sheet(as_sheets_id(target_id)) 

# MUNGE -------------------------------------------------------------------

df_budget_viz <- df_budget %>% 
  mutate(agency = ifelse(agency == "USAID/WCF", "USAID", agency)) %>% 
  group_by(agency) %>% 
  summarise(val = sum(sum_of_total_planned_funding, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(!is.na(agency)) %>% 
  mutate(total = sum(val),
         share = val/total,
         fill_color = ifelse(agency == "USAID", denim, denim_light))

df_target_viz <- df_target %>% 
  mutate(val_type = "COP23 Targets") %>% 
 # filter(str_detect(dataname, "TX_CURR")) %>% 
  group_by(val_type) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  pivot_longer(cols = -c(1), names_to = "agency") %>% 
  mutate(total = sum(value),
         share = value/total,
         fill_color = ifelse(agency == "USAID", scooter, scooter_light)) %>% 
   filter(agency != "Dedup")

ind_sel <- c("HTS_TST", "TX_NEW", "TX_CURR", "TX_PVLS_D", "VMMC_CIRC", "PrEP_NEW", "OVC_SERV")

df_targes_lim <- df_target %>% 
  pivot_longer(-starts_with("data"), 
               names_to = "funding_agency",
               values_drop_na = TRUE) %>% 
  mutate(indicator = str_extract(dataname, "(?<=Targets ).*(?= \\()"),
         indicator = ifelse(str_detect(dataname, "\\(D\\)"), paste0(indicator, "_D"), indicator),
         fiscal_year = 2024) %>% 
  filter(indicator %in% ind_sel)

df_targes_lim <- df_targes_lim %>% 
  bind_rows(df_targes_lim %>% mutate(funding_agency = "PEPFAR")) %>% 
  filter(funding_agency %in% c("USAID", "PEPFAR")) %>% 
  count(fiscal_year, funding_agency, indicator, wt = value) 
  
  
# VIZ -----------------------------------------------------------------------

#AGENCY BUDGET SHARE
df_budget_viz %>% 
  ggplot(aes(y = fct_reorder(agency, val))) + 
  #geom_col(aes(x = total), fill = trolley_grey_light) + 
  geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = agency, xmin = total, xmax =total),
                color = trolley_grey) +
  geom_col(aes(val, fill = fill_color, alpha = 0.8)) +
  geom_text(aes(x = val + 400000000, 
                label = label_number(0.1, scale_cut = cut_short_scale())(val)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = val + 400000000,
                label = percent(share, 0.1),
            family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL,
       y = NULL,
       title = "USAID's share of total COP/ROP23 planned funding is 52.3%" %>% toupper(),
       caption = glue("Source: COP23 COP Matrix Report | Ref id: {ref_id}"))

si_save("Graphics/COP23_GHPortfolio_BudgetShare.svg")
si_save("Images/COP23_GHPortfolio_BudgetShare.png")

#AGENCY TARGET SHARE
df_target_viz %>% 
  ggplot(aes(y = fct_reorder(agency, value))) + 
  #geom_col(aes(x = total), fill = trolley_grey_light) + 
  geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = agency, xmin = total, xmax =total),
                color = trolley_grey) +
  geom_col(aes(value, fill = fill_color, alpha = 0.8)) +
  geom_text(aes(x = value + 18000000, 
                label = label_number(0.1, scale_cut = cut_short_scale())(value)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = value + 18000000,
                label = percent(share, 0.1),
                family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
  labs(x = NULL,
       y = NULL,
       title = "USAID accounts for almost 42% of total COP/ROP23 Targets" %>% toupper(),
       caption = glue("Source: COP23 COP Matrix Report | Ref id: {ref_id}"))

si_save("Graphics/COP23_GHPortfolio_TargetShare.svg")
si_save("Images/COP23_GHPortfolio_TargetShare.png")

df_targes_lim %>% 
  pivot_wider(names_from = "funding_agency", values_from = "n") %>% 
  mutate(usaid_share = USAID/PEPFAR) %>% 
  ggplot(aes(y = fct_reorder(indicator, PEPFAR))) + 
  geom_col(aes(PEPFAR, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = indicator, xmin = PEPFAR, xmax =PEPFAR),
                color = trolley_grey) +
  geom_col(aes(USAID, alpha = 0.8),  fill = scooter) +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()))+
  geom_text(aes(x = USAID + 3000000,
                label = label_number(0.1, scale_cut = cut_short_scale())(USAID)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = PEPFAR,
                label = label_number(0.1, scale_cut = cut_short_scale())(PEPFAR)),
            family = "Source Sans Pro", color = nero, vjust = -0.5) +
  geom_text(aes(x = USAID + 3000000,
                label = percent(usaid_share, 0.1),
                family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  labs(x = NULL, y = NULL,
       title = "USAID covers up to 48% of COP/ROP23 Targets across the clinical cascade" %>% toupper(),
       caption = glue("Source: COP/ROP23 Targets, DATIM | Ref id: {ref_id}"))

si_save("Graphics/COP23_GHPortfolio_TargetShare_Cascade.svg")

