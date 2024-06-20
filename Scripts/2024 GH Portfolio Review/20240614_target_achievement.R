# PROJECT: Portfolio Review - Target Achievement
# PURPOSE: Munge and Analysis of MSD
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  3ecf81bc
# LICENSE: MIT
# DATE:   2024-06-14
# NOTES:  https://github.com/USAID-OHA-SI/agitprop/blob/c34f0e8d6184a1e9090687239ebac682ed3f3eac/Scripts/archive/05_usaid_target_achievement.R#L4

# LOCALS & SETUP ============================================================================

# Libraries
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(systemfonts)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)

# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "OU_IM_FY22")


metadata <- get_metadata()

# Grab metadata
metadata <- get_metadata(file_path)

# REF ID for plots
ref_id <- "3ecf81bc"

# Functions  
ind_sel <- c("PrEP_NEW", "VMMC_CIRC",
             "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR",
             "OVC_SERV"
             )

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# LOAD DATA ============================================================================  

df <- read_psd(file_path) %>% 
  clean_agency()

# MUNGE ============================================================================

df_achv <- df %>% 
  filter(funding_agency == "USAID", 
         indicator %in% ind_sel,
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == metadata$curr_fy) #%>% 
  #resolve_knownissues(store_excl = TRUE)

df_achv <- df_achv %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(c("cumulative", "targets"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #ungroup() %>% 
  adorn_achievement(metadata$curr_qtr)


df_viz <- df_achv %>%
  mutate(achv_round = round(achievement*100),
         achv_round = ifelse(achv_round > 100, 100, achv_round),
         gap = 100-achv_round) %>% 
  pivot_longer(c(achv_round, gap), names_to = "status") %>% 
  mutate(achv_color = ifelse(status == "gap", "#EBEBEB", achv_color),
         achv_color = ifelse(achv_color == trolley_grey_light, 
                             glitr::hw_electric_indigo,#trolley_grey,
                             achv_color),
         achv_alpha = ifelse(status == "gap", .1, 1),
         indicator = factor(indicator, ind_sel),
         ind_lab = case_when(indicator == "PrEP_NEW" ~ "Newly enrolled on antiretroviral pre-exposure prophylaxis",
                             indicator == "VMMC_CIRC" ~ "Voluntary medical male circumcision for HIV prevention",
                             indicator == "HTS_TST" ~ "Receiving HIV testing service and results",
                             indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                             indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                             indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy"),
         ind_lab = str_wrap(ind_lab, width = 25),
         val_lab = ifelse(indicator == ind_sel[1], #label 1 or all 7
           #indicator %in% ind_sel[1:7], 
                          glue("Results - {clean_number(cumulative)}\nTargets - {clean_number(targets)}"),
                          glue("{clean_number(cumulative)}\n{clean_number(targets)}")),
         full_lab = glue("{ind_lab}\n\n{val_lab}"),
         achievement = case_when(status == "achv_round" ~ achievement)) %>% 
  arrange(indicator) %>% 
  mutate(full_lab = fct_inorder(full_lab)) 

# VIZ ============================================================================


df_viz %>% 
  mutate(total = 100) %>% 
  filter(!is.na(achievement)) %>% 
  # pivot_wider(names_from = "funding_agency", values_from = "n") %>% 
  # mutate(usaid_share = USAID/PEPFAR) %>% 
  mutate(indicator = fct_relevel(indicator, c("OVC_SERV", "TX_CURR", "TX_NEW", "HTS_TST_POS","HTS_TST", "VMMC_CIRC", "PrEP_NEW"
    #"PrEP_NEW", "VMMC_CIRC", "HTS_TST", "HTS_TST_POS","TX_NEW", "TX_CURR", "OVC_SERV"
    ))) %>% 
  ggplot(aes(y = indicator #fct_reorder(indicator, value)
             )) + 
  geom_col(aes(total), fill = trolley_grey_light) +
  geom_errorbar(aes(y = indicator, xmin = total, xmax =total),
                color = trolley_grey) +
  geom_col(aes(value, alpha = 0.8, fill = achv_color)) +
  scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()))+
  geom_text(aes(x = value,
                label = val_lab,
                hjust = 1.5,
                family = "Source Sans Pro",
                color = nero #nero
  )) +
  geom_text(aes(x = value, #value or total ?
                label = glue("{value}%"),
                size = 12/.pt,
                hjust = -.45,
                family = "Source Sans Pro", #color = trolley_grey
                fontface = "bold"
                #color = "white"
  )) +
  # geom_text(aes(x = PEPFAR,
  #               label = label_number(0.1, scale_cut = cut_short_scale())(PEPFAR)),
  #           family = "Source Sans Pro", color = nero, vjust = -0.5) +
  # geom_text(aes(x = USAID + 3000000,
  #               label = percent(usaid_share, 0.1),
  #               family = "Source Sans Pro", color = trolley_grey, vjust = 1),
  #           size = 3.5) +
  scale_fill_identity() +
  scale_color_identity()+
  scale_alpha_identity() +
  si_style_xgrid() +
  theme(legend.position = "none") + 
  labs(x = NULL, y = NULL,
       title = "USAID % achievement towards FY24 targets, as of Q2" %>% toupper(),
       caption = metadata$caption) 

si_save("Graphics/target_achv.svg")
si_save("Graphics/target_achv.png")
#si_save("Graphics/target_achv_v2.png")

# SPINDOWN ============================================================================

