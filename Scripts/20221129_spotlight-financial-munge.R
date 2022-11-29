# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  import/structure public PEPFAR financial datasets
# REF ID:   14973035 
# LICENSE:  MIT
# DATE:     2022-11-29
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(vroom)
  library(janitor)
  
# GLOBAL VARIABLES --------------------------------------------------------
  
  #public data sources - https://data.pepfar.gov/datasets
  # financial resource document - https://www.intrahealth.org/sites/ihweb/files/asap-resource-other-attachments/pepfarfinancialclassificationsreferenceguide52920.pdf
  path_source <- "https://s3.amazonaws.com/media.data.pepfar.gov/fin_mngt/"
  path_budget <- paste0(path_source, "budget_by_budget_code/PEPFAR_OU_Budgets_by_Budget_Code.txt")
  path_exp <- paste0(path_source, "pepfar_expenditures_by_program/PEPFAR_Program_Expenditures.txt")

  
# IMPORT ------------------------------------------------------------------
  
  #expenditure data
  df_exp <- vroom(path_exp) 
  
  #budget data
  df_budget <- vroom(path_budget) 
  

# MUNGE -------------------------------------------------------------------

## EXPENDITURE
  
  #reshape long by year to make tidy & clean column names
  df_exp <- df_exp %>% 
    pivot_longer(where(is.double),
                 names_to = "fiscal_year",
                 values_drop_na = TRUE) %>% 
    clean_names() 
  
  #remove PEPFAR aggregate data and cols with limited utility
  # need to be mindful there are totals in the tidy dataset, but they don't 
  # perfectly match the sum of the program ares (odd)
  df_exp <- df_exp %>% 
    filter(!is.na(program)) %>% 
    select(!c(interaction_type, beneficiary, sub_beneficiary)) 
  
  #clarify program names
  df_exp <- df_exp %>% 
    mutate(program = recode(program,
                            "ASP"  = "ASP: Above-site programs",
                            "C&T"  = "C&T: Care and treatment",
                            "HTS"  = "HTS: HIV testing services",
                            "PM"   = "PM: Program management",
                            "PREV" = "PREV: Prevention",
                            "SE"   = "SE: Socio-economic"))
  
## BUDGET
  
  #reshape long by year to make tidy & clean column names
  df_budget <- df_budget %>% 
    pivot_longer(where(is.double),
                 names_to = "fiscal_year",
                 values_drop_na = TRUE) %>% 
    clean_names() 
  
  #remove PEPFAR aggregate data
  df_budget <- df_budget %>%
    filter(country_operating_unit != "Total")
  