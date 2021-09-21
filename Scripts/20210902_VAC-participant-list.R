# PROJECT:  catch-22
# AUTHOR:   K.Srikanth, A.Chafetz | USAID
# PURPOSE:  combine survey particupants across 11 forms
# LICENSE:  MIT
# DATE:     2021-09-02
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)
library(fuzzyjoin)
library(readr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  #Gdrive auth
  drive_auth()
  gs4_auth()
  
  #master
  master_id <- as_id("1dCoz81St9ntSIYt6o1ZZ57U-6O3JD6f0szXUMtMjqT4")
  
  #folder
  responses_fldr <- as_id("1E7op7s6vuIkGLAOgmZJyyu82Gx0gfni6")


# FUNCTIONS ---------------------------------------------------------------

  #function to extract name and email from completed forms
  extract_email <- function(id, name){
    
    sess <- name %>% 
      str_extract("Pre|Post|Session [:digit:]") %>% 
      str_remove("ession ") %>% 
      tolower()
    
    df <- read_sheet({id}, .name_repair = make_clean_names) %>%
      select(matches("email_address|name$")) %>% 
      mutate(!!sess := 1)
    
    if("email_address" %in% names(df))
      df <- mutate(df, email_address = tolower(email_address))
    
    return(df)
  }
  
  #function to do a fuzzy join between each form
  hot_fuzz <- function(df_x, df_y, dist = 2){
    
    if("email_address" %in% names(df_y)){
      df_x <- mutate(df_x, email_address = replace_na(email_address, "blank"))
      df_y <- mutate(df_y, email_address = replace_na(email_address, "blank"))
      df_new <- df_x %>% 
        stringdist_full_join(df_y, 
                             by = c(email_address = "email_address"),
                             max_dist = dist) %>%
        mutate(email_address.x = ifelse(is.na(email_address.x), email_address.y, email_address.x)) %>% 
        select(-email_address.y) %>% 
        rename(email_address = email_address.x) %>% 
        arrange(email_address)
    } else {
      df_x <- mutate(df_x, name = replace_na(name, "blank"))
      df_y <- mutate(df_y, name = replace_na(name, "blank"))
      df_new <- df_x %>% 
        stringdist_full_join(df_y, 
                             by = c(name = "name"),
                             ignore_case = TRUE,
                             max_dist = dist)
    }
    
    if("name.y" %in% names(df_new)){
      df_new <- df_new %>% 
        mutate(name.x = ifelse(is.na(name.x), name.y, name.x)) %>% 
        select(-name.y) %>% 
        rename(name = name.x)
    }
    
    return(df_new)
  }
  
# MUNGE -------------------------------------------------------------------

  #read in master file
  master <- read_sheet(master_id, .name_repair = make_clean_names) %>% 
    select(name:country) %>% 
    rename(email_address = e_mail) %>% 
    mutate(email_address = tolower(email_address))
  
  #list each of the session id in folder
  files <- drive_ls(responses_fldr) %>% 
    mutate(order = str_extract(name, "[:digit:]"),
           order = case_when(str_detect(name, "Pre") ~ "0",
                             str_detect(name, "Post") ~ "10",
                             TRUE ~ order),
           order= str_pad(order, width = 2, pad = "0")) %>% 
    arrange(order)
  
  
  #list of dfs for emails by form
  bundle <- map2(.x = files$id,
                 .y = files$name,
                 .f = extract_email) 
  
  #combine dfs by fuzzy join
  combo <- bundle[[2]] %>% 
    hot_fuzz(bundle[[3]]) %>% 
    hot_fuzz(bundle[[4]]) %>% 
    hot_fuzz(bundle[[5]]) %>% 
    hot_fuzz(bundle[[6]]) %>% 
    hot_fuzz(bundle[[7]]) %>% 
    hot_fuzz(bundle[[8]]) %>% 
    hot_fuzz(bundle[[9]]) %>% 
    hot_fuzz(bundle[[10]]) %>% 
    hot_fuzz(bundle[[11]]) %>% 
    hot_fuzz(bundle[[1]])
  
  #remove duplicates
  combo <- distinct(combo)
  
  #convert NAs
  combo <- combo %>% 
    mutate(across(c(email_address, name), ~na_if(., "blank")))
  
  #sum total number completed
  tot <- combo %>% 
    pivot_longer(-c(email_address, name),
                 names_to = "session",
                 values_drop_na = TRUE) %>% 
    count(email_address, name, wt = value, name = "tot_completed")
  
  #sum total live completed
  tot_live <- combo %>% 
    pivot_longer(-c(email_address, name),
                 names_to = "session",
                 values_drop_na = TRUE) %>% 
    filter(session %in% c("s1", "s4", "s9")) %>% 
    count(email_address, name, wt = value, name = "tot_live_completed")
  
  
  #merge total onto main df
  combo <- combo %>% 
    left_join(tot, by = c("email_address", "name")) %>% 
    left_join(tot_live, by = c("email_address", "name")) %>% 
    relocate(tot_completed, tot_live_completed, pre, .after = name)
  
  #join forms to master list
  combo <- master %>% 
    hot_fuzz(combo)
  
  #clean up master list
  combo <- combo %>% 
    mutate(across(c(email_address, name), ~na_if(., "blank"))) %>% 
    arrange(email_address)
  
  #remove duplicate lines
  combo <- combo %>% 
    mutate(across(c(tot_completed, tot_live_completed), ~replace_na(., 0))) %>% 
    group_by(email_address) %>% 
    filter(tot_completed == max(tot_completed)) %>% 
    ungroup() %>% 
    tidylog::distinct(email_address, .keep_all = TRUE)
  
  #how may total particpants completed 2 or more of the live sessions?
  combo %>% 
    filter(tot_live_completed >= 2)
  
  #export
  filename <- paste0("USAID-VAC-Training_particpant-list_", Sys.Date(), ".csv")
  write_csv(combo, file.path("Dataout", filename), na = "")
  
  
  
