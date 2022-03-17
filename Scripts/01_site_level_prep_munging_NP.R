# AD HOC REQUEST

# For the following mech codes:"17549","81744","84479","18494","17719", "84485", "84476", "70310","70306","80007",                                                                    
#"17651","82001","160728","81993","18274","81997","81992","82000","160727","85000","81998","81995","17649","18273",
#"82102","85143","160827"
## (For Kenya only in Kisumu, Nairobi & Mombasa)
## 
# 
# The following data points:
# PrEP providing facility names
# Facility Location (urban, peri-urban, or rural)
# Name of province/county/city where facility is located
# Name of implementing partner
# Established PrEP clients (PrEP Curr)
# Proportion female (%) of Established PrEP and PrEP New
# New PrEP clients (PrEP New)
# PrEP client targets for COP20 and COP21


# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam & Nada Petrovic | SI
# LICENSE: MIT
# DATE: 2022-03-08
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(extrafont)
    library(glitr)
    library(glamr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
 
  # Functions & lookup tables

# DATA PATH ============================================================================  

  ous <- list.files(path = merdata, 
                    pattern = "Site_IM_FY20-22.*Lesotho|South_Africa|Uganda|Kenya|Zimbabwe\\.txt",
                    full.names = TRUE)
  filepath <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY20-22")
  df <- read_msd(filepath) 
  

# Lookup table & FUNCTION -----------------------------------------------------------

    lookup <- tibble::tribble(~operatingunit,~mech_code,
                              "Lesotho", "17549|81744",
                              "Kenya", "84479|18494|17719|84485|84476",
                              "South Africa", "70310|70306|80007",
                              "Uganda", "17651|82001|160728|81993|18274|81997|81992|82000|160727|85000|81998|81995|17649|18273",
                              "Zimbabwe", "82102|85143|160827")
    
     
  # STEPS
   #1 - load data frame and only prep indicators
   #2 - get the operatingunit into an object
   #3 - subset the lookup by that ou
   #4 - create a mech_code filter for each OU using lookup table
   #5 - combine all data together
   #6 - merge in psnu-level targets
    
    
load_site_msd <- function(ou_list, ref_table=lookup) {
    
    df <- read_msd(ou_list) %>% filter(str_detect(indicator, "PrEP"), fiscal_year>2020)
    
    ou <- unique(df$operatingunit)
    
    # Grab the column from the lookup table that you need to filter the msd
    # To get the limited partners requested
    fltr <- lookup %>% filter(operatingunit %in% ou) %>% pull(mech_code)
    
    ##Kenya request only in "Kisumu", "Nairobi" and "Mombasa"
    if(ou == "Kenya"){
      df <- df %>% 
        filter(str_detect(mech_code, fltr)) %>% 
        mutate(psnu_filter = case_when(
          str_detect(psnu, "Kisumu") ~ 1,
          str_detect(psnu, "Nairobi") ~ 1,
          str_detect(psnu, "Mombasa") ~ 1, 
          TRUE ~ 0)
        ) %>% 
        filter(psnu_filter == 1) %>% 
        select(-psnu_filter)
      
    } else {
        df <- df %>% filter(str_detect(mech_code, fltr))
    }
    
    return(df)
  }


# LOAD DATA AND MUNGE -----------------------------------------------------

## Load psnu-level targets
  df_psnu<- df %>% filter(operatingunit %in% c("Lesotho","Kenya", "South Africa",
                         "Uganda", "Zimbabwe"), str_detect(indicator, "PrEP")) %>%
            select(psnuuid, mech_code, standardizeddisaggregate, sex, indicator, fiscal_year, targets) %>%
            rename(targets_psnu=targets)
 
# Use purrr to load site-level data & combine everything via the custom function and list of site level msds
df_prep <- purrr::map_dfr(ous, ~load_site_msd(ou_list = .x)) 
  

# Choose relevant columns  
df_prep_site<-df_prep %>% 
    select(operatingunit, sitename, facility, psnu, psnuuid, snu1, mech_code, primepartner, 
           indicator, standardizeddisaggregate, sex, fiscal_year, targets, qtr1, qtr2,
           qtr3, qtr4, cumulative)

#Aggregate site-level data by age
 df_prep_summary<- df_prep_site %>%
   filter(standardizeddisaggregate=="Age/Sex") %>%
          group_by(operatingunit, sitename, facility, psnu, psnuuid, snu1, mech_code, primepartner, indicator, fiscal_year, sex) %>% 
          summarise(across(where(is.double), sum, na.rm = TRUE),
              .groups = "drop") 

 ## Remove lines of data labeled as "Above Site/facility Level" 
 ## All coded as "above site" are also "above facility"
 ## Some coded as "above facility" have site names -- not sure if this is just an error
 ## All have zero in results
 ## colSums(select(filter(df_prep_summary, str_detect(facility,"above")),qtr1, qtr2, qtr3, qtr4, cumulative)) 
 ## Appear to generally be just be psnu-level targets, but seems safer to just merge in 
 ## psnu-level targets from PSNU_by_IM data
 
 
 df_prep_summary<-df_prep_summary %>% filter(str_detect(facility,"above")==FALSE) %>%
                  filter(str_detect(facility,"above")==FALSE) %>%
                  select(-targets)
 
 
#Aggregate PSNU-level targets by age
 df_psnu_targets_summary<-df_psnu %>% 
   filter(standardizeddisaggregate=="Age/Sex") %>%
   group_by(psnuuid, mech_code, indicator, fiscal_year, sex) %>%
   summarise(across(where(is.double), sum, na.rm = TRUE),
             .groups = "drop") 
 
 
# Join to PSNU-level targets
   
df_prep_joined<-inner_join(df_prep_summary,df_psnu_targets_summary, by=c("psnuuid", "mech_code", 
                                                                   "indicator", "fiscal_year","sex")) %>%
                select(-psnuuid)
                 
write.csv(df_prep_joined,"PrEP_data_FINAL.csv", row.names = FALSE)



  # NOTES:
  # PrEP_CT first time reported is in FY22Q1
  # PrEP targets are only available at PSNU level 

  
  
  
  
  
  
  
  
  
  
   

    
