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
library(patchwork)
library(ggrepel)
library(glitr)
library(readxl)
library(janitor)
library(googledrive)


# GLOBAL VARIABLES ---------------------------------------------------------------------

gdrive_folder <- "1uYQNdzOKpNu5_EOkOfOLt-xtO-vATqNP"
file_name <- "USAID Rejections (1).xlsx" 
save_name <- "VL_rejections_SA.csv"

#specify the age filter
age_filter <- c("Sum", "AGE TESTED YEARS(Group)")

#authenticate for GDrive
load_secrets()

# IMPORT ------------------------------------------------------------------

import_drivefile(gdrive_folder,
                 file_name, zip = "FALSE")

# IDENTIFY SHEETS ---------------------------------------------------------

sheet_list <- excel_sheets(file.path("Data",file_name))

# CLEAN AND MAP ---------------------------------------------------------------------

combine_data <- function(file, tab){
  
  # read in data and skip first 2 lines
  df <- read_excel(file,
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
    mutate(age = recode(age, 
                        "< 1" = "<01",
                        "1 -4" = "01-04",
                        "5-9" = "05-09")) %>% 
    clean_names()
  
  # pivot longer over the disaggregates
  df_clean <- df %>% 
    pivot_longer(-c(district, sub_district, facility_n, age, indicator, table),
                 names_to = c("disaggregate"),
                 values_drop_na = TRUE)
  
  return(df_clean)
  
}


# PROCESS DATA ------------------------------------------------------------


df_all <- map_dfr(sheet_list,
                  ~combine_data(file.path("Data",file_name), .x))



# EXPORT AND UPLOAD -------------------------------------------------------

  #export locally
  write_csv(df_all, 
            file.path("Dataout", save_name), 
                      na = "")

  #upload to GDrive
  drive_upload(file.path("Dataout", save_name), 
               path = as_id(gdrive_folder),
               name = save_name,
               type = "spreadsheet")
  
  #remove local copy
  unlink(file.path("Dataout", save_name))
