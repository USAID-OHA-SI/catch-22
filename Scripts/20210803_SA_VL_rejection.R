# PROJECT:  catch_22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Cleaning Messy Spreadsheet for South Africa VL Data
# DATE:     2021-08-03

# DEPENDENCIES -------------------------------------------------------------------

library(tidyverse)
library(glamr)
library(scales)
library(ICPIutilities)
library(lubridate)
library(RColorBrewer)
library(skimr)
library("patchwork")
library(ggrepel)
library(glitr)
library(readxl)
library(janitor)


# GLOBAL VARIABLES ---------------------------------------------------------------------

file_name <- "VL_rejections_SA.xlsx" 
sheet_list <- excel_sheets(file_name)

#specify the age filter
age_filter <- c("Sum", "AGE TESTED YEARS(Group)")

# CLEAN AND MAP ---------------------------------------------------------------------

combine_data <- function(file, tab){
  
  # read in data and skip first 2 lines
  df <- read_excel("VL_rejections_SA.xlsx",
                         sheet = tab,
                         skip = 2)
  # rename the columns according the file mapping file
  # add indicator and table variable
  df <- df %>% 
    rename(district = "DISTRICT NAME",
           sub_district = "SUB DISTRICT NAME",
           facility_n = "FACILITY NAME",
           age = "REJECTION REASON") %>% 
    mutate(indicator = "VL_rejection",
           table = "NICD") %>% 
    select(-contains("Total"),
           -starts_with("..."))
  
  # fill columns
  # filter out age - anything that NA, Sum, or "Age tested year" 
  df <- df %>% 
    fill(district, sub_district, facility_n) %>% 
    filter(!age %in% age_filter,
           !is.na(age)) %>% 
    clean_names()
  
  # pivot longer over the disaggregates
  df_clean <- df %>% 
    pivot_longer(-c(district, sub_district, facility_n, age, indicator, table),
                 names_to = c("disaggregate"),
                 values_drop_na = TRUE)
  
  return(df_clean)
  
}

df_all <- map_dfr(sheet_list,
                  ~combine_data(file_name, .x))
