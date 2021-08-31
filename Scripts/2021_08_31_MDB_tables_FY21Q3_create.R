# PURPOSE: Creating Q3 MDB graphics with integrated legend
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-08-31
# NOTES: Creates MDB graphics and saves them locally to a desired folder.

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(gt)
    library(selfdestructin5)
    library(fontawesome)
  
  # Set paths  
    mdb_out <- "Graphics" 
    load_secrets()
    merdata <- glamr::si_path("path_msd")
    
    legend_chunk <- gt::md("Legend: Cumulative Indicators <img src='https://user-images.githubusercontent.com/5873344/131568557-1d2ecf02-bfb8-4f9d-8bff-844871536f92.png' style='height:15px;'>    &emsp; Snapshot (TX_CURR) <img src='https://user-images.githubusercontent.com/5873344/131568585-cef89158-ee77-41a8-9dc7-a3d83ca0ba2c.png' style='height:15px;'>")
    
  # Function to add source note
    add_legend <- function(gt_obj){
      gt_obj %>% 
        tab_source_note(source_note = legend_chunk)
    }
    
    add_subtitle <- function(gt_obj){
      gt_obj %>% 
        tab_header(
          title = md("**GLOBAL RESULTS**"),
          subtitle = legend_chunk
        )
    }

# LOAD DATA ============================================================================  
    
    # Load OU_IM table - using FY21 Q2 data from Panorama
    ou_im <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-21_20210813_v1_1") %>%
      read_msd() 


# MUNGE ============================================================================
    
    # Time metadata needed  
    pd <- create_pd(ou_im)
    msd_source <- pd %>% msd_period(period = .)
    
    # Main Table
    mdb_df   <- make_mdb_df(ou_im)
    
    # Create the reshaped df that is gt() ready
    mdb_tbl  <- reshape_mdb_df(mdb_df, pd)
  
# VIZ ============================================================================
    
    # Generate base table for global results
    create_mdb(mdb_tbl, ou = "Global", type = "main", pd, msd_source, legend = legend_chunk) 
    
    # Try a specific country now
    create_mdb(mdb_tbl, ou = "Zambia", type = "main", pd, msd_source, legend = legend_chunk) 

    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(ou_im)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)
    
    create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source)

# BATCH -------------------------------------------------------------------

    # create batch tables
    distinct_agg_type <- function(df, type = "OU"){
      df %>% 
        filter(agg_type == {{type}}) %>% 
        distinct(operatingunit) %>% 
        pull()
    }    
    
    # Write the different types to character objects
    ous <- distinct_agg_type(mdb_tbl, "OU")
    glb <- distinct_agg_type(mdb_tbl, "Agency")
    rgl <- distinct_agg_type(mdb_tbl, "Region-Country")
    
    # Use purr to map across the list and create tables for all entries in each object
    purrr::map(list(glb, ous, rgl) %>% flatten(), 
               ~create_mdb(mdb_tbl, ou = .x, type = "main", pd, msd_source, legend = legend_chunk) %>%
                 gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_KEY_INDICATORS_MD.png")))    
    
    
    # TREATMENT
    ous_tx <- distinct_agg_type(mdb_tbl_tx, "OU")
    glb_tx <- distinct_agg_type(mdb_tbl_tx, "Agency")
    rgl_tx <- distinct_agg_type(mdb_tbl_tx, "Region-Country")
    
    map(list(glb_tx, ous_tx, rgl_tx) %>% flatten(), 
        ~create_mdb(mdb_tbl_tx, ou = .x, type = "treatment", pd, msd_source) %>% 
          gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_MMD_VL_MD.png")))

    
# TODO: Export main table and VLC table and migrate to google drive
   # Can drop the html formatted columns in each data frame before writing
   # mdb_tbl %>% write_csv()
   # mdb_tbl_tx  
    