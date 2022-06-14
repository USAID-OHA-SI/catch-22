# PROJECT:  catch22
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  Calculates percentage of targets by Agency globally and regionally
# LICENSE:  MIT
# DATE:     2022/6/10
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(countrycode)
  # library(fontawesome)
  # library(emojifont)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()


  ind_sel <- c("TX_CURR","OVC_SERV","KP_PREV","PREP_NEW", "TX_NEW", "VMMC_CIRC")
  disagg_sel <- c("Age/Sex/HIVStatus", "Age/Sex/ProgramStatus","KeyPop", "Age/Sex")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path("path_datapacks") %>% 
    return_latest("Datapack_Agency_IM") %>%
    read.csv()
  
  df_meta <- get_outable(datim_user(), datim_pwd()) %>%
    select(country, country_iso)
  

# MUNGE -------------------------------------------------------------------
  
    
  df_meta <- df_meta %>%
    mutate(wb_region = countrycode(df_meta$country_iso, "iso3c", "region"),
           usaid_region = case_when(country == "Ukraine" ~ "Europe",
                                    wb_region == "Sub-Saharan Africa" ~ "Africa",
                                    wb_region == "Latin America & Caribbean" ~ "LAC",
                                    TRUE ~ "Asia")) %>% 
    select(-c(wb_region, country_iso))
  
  
  df_filt <- df %>% 
    rename(country=countryname, funding_agency=fundingagency) %>%
    filter(indicator %in% ind_sel, 
           standardizeddisaggregate %in% disagg_sel)
  
  # Used to check disaggregates
  # df_filt %>%
  #  group_by(indicator, standardizeddisaggregate) %>%
  #  tally() 
  
  
  df_full <- df_filt %>% 
    left_join(df_meta, by = "country") 
  
  df_region <- df_full %>% 
  filter(fiscal_year=="2023") %>%
  bind_rows(mutate(.,funding_agency = "PEPFAR")) %>%
  bind_rows(mutate(.,usaid_region = "Global")) %>%
  filter(funding_agency %in% c("PEPFAR","USAID")) %>% 
  group_by(fiscal_year, usaid_region, indicator, funding_agency) %>%
  summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop")
    
  df_region_wide <- df_region %>% 
  pivot_wider(names_from=funding_agency, values_from=targets) %>%
  mutate(USAID_percent_targets=round(USAID/PEPFAR*100,0)) %>%
  filter(usaid_region %in% c("Africa"))
  
  View(df_region_wide)
  


  
  

  
 