# PROJECT:  catch22
# PURPOSE:  combine OKR schedules into a unified source document
# AUTHOR:   A.Chafetz + B.Kagniniwa | USAID
# REF ID:   c2592afc 
# LICENSE:  MIT
# DATE:     2024-06-26
# UPDATED:  2024-07-08

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(googledrive)
  library(googlesheets4)
  library(janitor)
  library(glamr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "c2592afc"  #a reference to be places in viz captions 
  
  #OKR 2024 folder
  gs_folder_id <- as_id("1alhiHrDBJzRftcyJRW-THohz9LbJezzJ")
  # drive_browse(gs_folder_id)
  

# IDENTIFY FILES ----------------------------------------------------------

  
  #identify all schedules in the drive folder
  schedules <- drive_ls(gs_folder_id)
  
  #get the source GS ID for each file (targetId for shortcuts, id for source)
  schedules <- schedules %>%
    gdrive_metadata(show_details = T) %>% 
    mutate(mime_type = str_extract(mime_type, "[^\\.]*$")) %>% 
    distinct(kind, mime_type, id, name, shortcut_details_target_id) %>% 
    rename(target_id = shortcut_details_target_id) %>% 
    mutate(
      target_id = case_when(
        mime_type != "shortcut" & target_id == "NULL" ~ id, 
        TRUE ~ target_id
      )
    ) 
  
  #identify the sheets to read in (should be Schedule; excludes README and Dropdown)
  schedules_shts <- schedules %>% 
    mutate(across(everything(), unlist)) %>% 
    mutate(sheets = map_chr(target_id, function(x) {
      
      gs<- schedules %>% filter(target_id == x) 
      gs_name <- glue::glue("{gs %>% pull(mime_type)}: {gs %>% pull(name)}")
      
      print(gs_name)
        
      tryCatch({
          gsheet <- gs4_get(x)
          gsheet$sheets$name %>% paste(collapse = ",")
        },
        error = function(e) {
          message("ERROR")
          message(conditionMessage(e))
          glue::glue("ERROR ACCESSING [{gs_name}]")
        }
      )
      
    })) %>% 
    separate_longer_delim(sheets, ",") %>% 
    filter(str_detect(sheets, "README|Dropdown", negate = TRUE))
  
# IMPORT ------------------------------------------------------------------

  #read in all the files/sheets and combine
  df_sch <- schedules_shts %>% 
    filter(str_detect(sheets, "^ERROR.*\\[.*\\]", negate = TRUE),
           str_to_lower(sheets) == "schedule") %>% 
    select(target_id, sheets) %>% 
    pmap(~read_sheet(ss = ..1, 
                     sheet = ..2, 
                     .name_repair = make_clean_names)) %>% 
    list_rbind()

# MUNGE -------------------------------------------------------------------

  #exclude empty rows
  df_sch <- df_sch %>% 
    filter(!is.na(task))
  
