# AUTHOR:   K. Srikanth & L.Baraki | USAID
# PURPOSE:  GH portfolio review: COP23 Target and Budget Share
# REF ID:   38c539ed 
# LICENSE:  MIT
# DATE:     2023-07-18
# UPDATED: 2024-06-13
#https://github.com/USAID-OHA-SI/catch-22/blob/ffd98d225766792649a9d45ceac56a705bf74c1c/Scripts/2023%20GH%20Portfolio%20Review/2023_07_18_GHSector_review_budget_share.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(systemfonts)
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
    merdata <- file.path(glamr::si_path("path_msd"))
    genie_path <- return_latest(folderpath = merdata,
                               pattern = "Genie-OU_IM")

  # Grab metadata
    metadata <- get_metadata(genie_path)
  
  ref_id <- "38c539ed"

  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
# IMPORT ------------------------------------------------------------------
  

#Targets 
  #target_id <- "1qfdjuRc3k0XbKjDJqTzhCk3GH5gH74YnE5oUemwituM"
  #df_target <- read_sheet(as_sheets_id(target_id)) 
  df_genie <- read_psd(genie_path)
  
#Budget 
#bugdet_id <- "1c13egLNssTFmTqGZnYiUJrsBpUJPFvLH7kmaHembTR8"
#df_budget <- read_sheet(as_sheets_id(bugdet_id), sheet = "COP ROP by Agency", skip =2) %>% 
 # janitor::clean_names()

df_fast <- read_excel("Data/COP23_FY25 & ROP24_FY25-26 FAST Dossier.xlsx",
                      range = "A4:D18",
                      #skip = 3
                      ) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(funding_agency = !!names(.[1]),
                "2024" =  !!names(.[2]),
                "2025" = !!names(.[3]),
                "2026" = !!names(.[4])
                )




# MUNGE -------------------------------------------------------------------

ind_sel <- c("HTS_TST", "TX_NEW", "TX_CURR",
             "TX_PVLS_D", #Total Denom
             "VMMC_CIRC", "PrEP_NEW", "OVC_SERV")

  
df_targets <- df_genie %>% 
  filter(#funding_agency == "USAID",
         indicator %in% ind_sel,
         standardizeddisaggregate %in% c("Total Numerator","Total Denominator"), 
         fiscal_year == metadata$curr_fy + 1) 

df_targets_lim <- df_targets %>% 
  bind_rows(df_targets %>% mutate(funding_agency = "PEPFAR")) %>% #PEPFAR is just cumulative across agencies 
  group_by(indicator, fiscal_year, funding_agency) %>% 
  summarise(across(c("targets"), sum, na.rm = TRUE), .groups = "drop") %>% 
  count(fiscal_year, funding_agency, indicator, wt = targets) %>% #exclude indicator to get the USAID coverage % (~41)
  filter(funding_agency %in% c("PEPFAR", "USAID"))
  
  
#df_budget_viz <- df_budget %>% 
 # mutate(agency = ifelse(agency == "USAID/WCF", "USAID", agency)) %>% 
  #group_by(agency) %>% 
  #summarise(val = sum(sum_of_total_planned_funding, na.rm = TRUE)) %>%
  #ungroup() %>% 
  #filter(!is.na(agency)) %>% 
  #mutate(total = sum(val),
   #      share = val/total,
    #     fill_color = ifelse(agency == "USAID", denim, denim_light))

df_budget_viz <- df_fast %>% 
  clean_agency() %>% 
  group_by(funding_agency) %>% 
  summarise(val = sum(`2025`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(funding_agency)) %>% 
  mutate(total = sum(val),
         share = val/total, 
         fill_color = ifelse(funding_agency =="USAID", denim, denim_light))
  
  
  
# VIZ -----------------------------------------------------------------------


#AGENCY TARGET SHARE vs PEPFAR 
df_targets_lim %>% 
  pivot_wider(names_from = "funding_agency", values_from = "n") %>% 
  mutate(usaid_share = USAID/PEPFAR) %>% 
  ggplot(aes(y = fct_reorder(indicator, PEPFAR)
             )) + 
  geom_col(aes(PEPFAR, alpha = 0.8), fill = trolley_grey_light) + #PEPFAR is the grey bar
  geom_errorbar(aes(y = indicator, xmin = PEPFAR, xmax =PEPFAR),
                color = trolley_grey) +
  geom_col(aes(USAID, alpha = 0.8),  fill = scooter) +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()))+#scales::label_number(scale_cut = cut_short_scale()))+
  #facet_wrap(~indicator) + 
 geom_text(aes(x = USAID, #+ 2500000, #3000000,
               label = clean_number(USAID)),
                #label = label_number(0.1, scale_cut = cut_short_scale())(USAID)), #USAID Share 
            family = "Source Sans Pro", color = glitr::scooter,
           hjust = -1.5,
           vjust = -0.5) +
  
  geom_text(aes(x = PEPFAR, #+ 3000000,
                label = clean_number(PEPFAR)),
                #label = label_number(0.1, scale_cut = cut_short_scale())(PEPFAR)), #PEPFAR Share 
           family = "Source Sans Pro", color = nero,
           fontface = "bold",
           hjust = -1.5,
           vjust = -0.95) +
  
  geom_text(aes(x = USAID + 2750000, #3000000,
                label = percent(usaid_share, 0.1), #USAID Percentage 
                family = "Source Sans Pro", color = trolley_grey, vjust = 1),
            vjust = 2.5,
            size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  theme(plot.title = ggtext::element_markdown()) + 
  labs(x = NULL, y = NULL,
       title = glue("<span style = 'color:{scooter}'>USAID COVERS UP TO 41%</span> OF COP/ROP24 TARGETS ACROSS THE CLINICAL CASCADE"), #%>% 
         #toupper(),
       caption = glue("Source: COP/ROP24 Targets, DATIM | Ref id: {ref_id}")) #FY25

si_save("Graphics/COP24_GHPortfolio_TargetShare_Cascade.svg")
si_save("Graphics/COP24_GHPortfolio_TargetShare_Cascade.png")
#si_save("Graphics/COP24_GHPortfolio_TargetShare_Cascade.png", scale = 1.2, width = 10, height = 7)


#AGENCY BUDGET SHARE
df_budget_viz %>% 
  ggplot(aes(y = fct_reorder(funding_agency, val))) + #funding_agency
  #geom_col(aes(x = total), fill = trolley_grey_light) + 
  geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
  geom_errorbar(aes(y = funding_agency, xmin = total, xmax =total), #funding_agency
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
       title = "USAID's share of total COP/ROP24 planned funding is 52.2%" %>% toupper(),
       caption = glue("Source: FAST Dossier | Ref id: {ref_id}")) #FY25

#si_save("Graphics/COP24_GHPortfolio_BudgetShare.svg")
si_save("Graphics/COP24_GHPortfolio_BudgetShare.png")
