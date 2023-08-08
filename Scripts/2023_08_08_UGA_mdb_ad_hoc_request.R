# PROJECT: Mission Director Tables for Uganda Q3
# PURPOSE: Munge and Analysis of Uganda Q3 data
# AUTHOR:  Tim Esssam | SI
# REF ID:  fb393d4c
# LICENSE: MIT
# DATE:   2023-08-07
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(selfdestructin5)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    # genie_path <- return_latest(folderpath = merdata,
    #   pattern = "Uganda-Daily")
    file_path <- return_latest(folderpath = merdata,
                                pattern = "PSNU_IM.*20230616_v2")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "fb393d4c"
    
  # Functions  
  ou_list <- "Uganda"  
  
  shrink_rows <- function(gt_obj){
    gt_obj %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) 
  }  
  
  legend_url <- "https://user-images.githubusercontent.com/5873344/258912756-0b3cbda2-a2a5-4d8f-8d74-e7b3d0a20397.png"

  legend_chunk <- gt::md(glue::glue("<img src= '{legend_url}' style='height:23px;'>"))
# LOAD DATA ============================================================================  

    df_msd <- read_psd(file_path) 


# ACHV TABLES -------------------------------------------------------------
    
    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd) 
    
    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(df_msd)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd) 
    
    
    # Create global tables
    mdb_tbl %>% 
      create_mdb(ou = "Global", type = "main", metadata$curr_pd, metadata$caption, legend = legend_chunk) %>% 
      gtsave(path = "Images", filename = glue::glue("GLOBAL_{metadata$curr_pd}_mdb_main.png"),
             vwidth = 1366, vheight = 784, zoom = 2)
    
    mdb_tbl_tx %>% 
      create_mdb(ou = "Global", type = "treatment", metadata$curr_pd, metadata$caption) %>% 
      gtsave(path = "Images", filename = glue::glue("GLOBAL_{metadata$curr_pd}_mdb_tx.png"),
             vwidth = 1366, vheight = 784, zoom = 2)
    
    
    # Create Uganda specific tables
    mdb_tbl %>% 
      create_mdb(ou = ou_list, type = "main", metadata$curr_pd, metadata$source, legend = legend_chunk) %>%
      shrink_rows() %>% 
      gtsave_extra(path = "Images", filename = glue::glue("{ou_list}_{metadata$curr_pd}_mdb_main.png"),
                   vwidth = 1366, vheight = 784, zoom = 2)  
    
    create_mdb(mdb_tbl_tx, ou = ou_list, type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) %>% 
      gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_{ou_list}_MMD_VL_MD.png"),
                   vwidth = 1366, vheight = 784, zoom = 2) 
    
    
    
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

