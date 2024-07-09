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
  library(cli)
  library(glitr)

  
# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "c2592afc"  #a reference to be places in viz captions 
  
  #OKR 2024 folder
  gs_folder_id <- as_id("1alhiHrDBJzRftcyJRW-THohz9LbJezzJ")
  # drive_browse(gs_folder_id)
  

# IDENTIFY FILES ----------------------------------------------------------

  #identify all schedules in the drive folder
  schedules <- drive_ls(gs_folder_id)
  
  #inspect an example
  schedules$drive_resource[4] %>% View()
  
  #get the source GS ID for each file (targetId for shortcuts, id for source)
  schedules <- schedules %>%
    gdrive_metadata(show_details = T) %>% 
    mutate(mime_type = str_extract(mime_type, "[^\\.]*$")) %>% 
    distinct(kind, mime_type, id, name, target_id = shortcut_details_target_id) %>% 
    mutate(target_id = ifelse(mime_type == "spreadsheet", id, target_id),
           across(everything(), unlist)) 
  
  #identify the sheets to read in (should be Schedule; excludes README and Dropdown)
  schedules_shts <- schedules %>% 
    mutate(sheets = map_chr(target_id, function(x) {
      
      gs <- schedules %>% filter(target_id == x) 
      gs_name <- glue::glue("{gs %>% pull(mime_type)}: {gs %>% pull(name)}")
      
      tryCatch({
          gsheet <- gs4_get(x)
          cli_alert_success(gs_name)
          gsheet$sheets$name %>% paste(collapse = ",")
        },
        error = function(e) {
          cli_alert_danger('ERROR ACCESSING [{gs_name}]')
          # message(conditionMessage(e))
          return("ERROR")
        }
      )
      
    })) 
  
  #expand df to have 1 row per sheet, removing unnecessary sheets
  schedules_shts <- schedules_shts %>% 
    separate_longer_delim(sheets, ",") %>% 
    filter(str_detect(sheets, "README|Dropdown|ERROR", negate = TRUE))
  
# IMPORT ------------------------------------------------------------------

  #read in all the files/sheets and combine
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
  
  #convert date time to date
  df_sch <- df_sch %>% 
    mutate(across(contains("date"), as_date))

# VIZ ---------------------------------------------------------------------


  df_okr_13 <- df_sch %>% 
    filter(okr_number == 1.3)

  df_okr_13 <- df_okr_13 %>% 
    mutate(task_disp = ifelse(str_length(task) <= 20, task, str_glue("{str_sub(task, end = 20)}...")),
           task_disp = task_disp %>% fct_inorder() %>% fct_rev())
  
  df_okr_13 <- df_okr_13 %>% 
    select(task, task_disp, status,
           target_start_date, target_end_date = anticipated_end_date,
           actual_start_date, actual_end_date) %>% 
    pivot_longer(ends_with("date"),
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)_(.*)_date",
                 values_to = "date")
  
  df_okr_13 <- df_okr_13 %>% 
    mutate(pt_shp = case_when(status == "In progress" ~ 8,
                              TRUE ~ 21),
           fill_color = case_when(type == "target" & status == "Completed" ~ slate,
                                  type == "target" & is.na(end) ~ orchid_bloom,
                                  type == "target" ~ slate,
                                  TRUE ~ electric_indigo
                                  ))

  df_okr_13 %>% 
    ggplot(aes(y = task_disp, group = type, color = fill_color, fill = fill_color)) +
    geom_vline(xintercept = Sys.Date(), color = tango) +
    geom_segment(aes(start, xend = end), na.rm = TRUE,
                 position = position_dodge(width = .75)) +
    geom_point(aes(start, shape = pt_shp), na.rm = TRUE,
               fill = "white", stroke = 1.1,
               position = position_dodge(width = .75)) +
    geom_point(aes(end, shape = pt_shp), na.rm = TRUE,
               stroke = 1.1,
               position = position_dodge(width = .75)) +
    scale_x_date(position = "top") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_shape_identity() +
    labs(x = NULL, y = NULL,
         caption = str_glue("Source: SI Branch PM Processes & Tools [accessed {today()}] | Ref Id: {ref_id}")) +
    si_style()
