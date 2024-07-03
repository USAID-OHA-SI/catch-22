# PROJECT:  catch22
# PURPOSE:  combine OKR schedules into a unified source document
# AUTHOR:   A.Chafetz | USAID
# REF ID:   c2592afc 
# LICENSE:  MIT
# DATE:     2024-06-26
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(googledrive)
  library(googlesheets4)
  library(janitor)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "c2592afc"  #a reference to be places in viz captions 
  
  #OKR 2024 folder
  gs_folder_id <- as_id("1alhiHrDBJzRftcyJRW-THohz9LbJezzJ")
  # drive_browse(gs_folder_id)
  
# FUNCTION - EXTRACT TARGET ID --------------------------------------------

  #function to extract the targetID for shortcut files
  extract_targetid <- function(x){
    if (is.null(x$shortcutDetails$targetId)) {
      NA_character_
    } else {
      x$shortcutDetails$targetId
    }
  }
  

# IDENTIFY FILES ----------------------------------------------------------

  
  #identify all schedules in the drive folder
  schedules <- drive_ls(gs_folder_id)
  
  schedules %>% glamr::gdrive_metadata(show_details = T)
  
  #get the source GS ID for each file (targetId for shortcuts, id for source)
  schedules <- schedules %>%
    mutate(meta = names(drive_resource)) %>% 
    #mutate(meta = as_tibble(drive_resource))
    unnest(drive_resource)
    #unlist(.$drive_resource) %>% as_tibble()
    mutate(#target_id = map_chr(drive_resource, extract_targetid),
           target_id = case_when(
             str_detect(drive_resource$mimeType, "shortcut") ~ drive_resource$shortcutDetails$targetId,
             TRUE ~ NA_character_
           ),
           target_id = ifelse(is.na(target_id), id, target_id),
           target_id = as_id(target_id)) 
  
  #identify the sheets to read in (should be Schedule; excludes README and Dropdown)
  schedules_shts <- schedules %>% 
    mutate(sheets = map_chr(target_id, \(x) gs4_get(x)$sheets$name %>% paste(collapse = ","))) %>% 
    separate_longer_delim(sheets, ",") %>% 
    filter(str_detect(sheets, "README|Dropdown", negate = TRUE))
  
  schedules %>% 
    mutate(sheets = map_chr(target_id, function(x) {
      #print(x)
      #print(drive_get(x))
      print(gs4_get(x))
      gs4_get(x)$sheets$name %>% paste(collapse = ",")
    })) %>% 
    separate_longer_delim(sheets, ",") %>% 
    filter(str_detect(sheets, "README|Dropdown", negate = TRUE))
  
# IMPORT ------------------------------------------------------------------

  #read in all the files (with the Schedule sheet name) and combine
  # schedules %>% 
  #   filter(str_detect(name, "Maddy", negate = TRUE)) %>% 
  #   pull() %>% 
  #   map(read_sheet, sheet = "Schedule", .name_repair = make_clean_names) %>% 
  #   list_rbind()
  
  #read in al the files/sheets and combine
  df_sch <- schedules_shts %>% 
    select(target_id, sheets) %>% 
    pmap(~read_sheet(ss = ..1, 
                     sheet = ..2, 
                     .name_repair = make_clean_names)) %>% 
    list_rbind()

# MUNGE -------------------------------------------------------------------

  #exclude empty rows
  df_sch <- df_sch %>% 
    filter(!is.na(task))
  
