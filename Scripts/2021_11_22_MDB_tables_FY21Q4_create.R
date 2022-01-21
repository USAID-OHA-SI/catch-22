# PURPOSE: Creating Q3 MDB graphics with integrated legend
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-11-22
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
    
  # Creates a markdown chunk to be inserted as a subtitle with legend pngs
    legend_chunk <- gt::md(glue::glue("Legend: <img src= '{legend_snapshot}' style='height:15px;'> "))
    
  # Bold Q3 data in the VLS/VLC table
    bold_column <- function(gt_obj, col){
      gt_obj %>% 
        tab_style(
          style = list(
            gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
            gt::cell_text(weight = 700)
          ),
          locations = cells_body(
            columns = {{col}},
          )
        )
    }
    
  # Bold Agency names - used to increase stroke on row group label
    bold_rowgroup <- function(gt_obj){
      gt_obj %>% 
        tab_style(
          style = list(
            gt::cell_text(weight = 700)
          ),
          locations = cells_row_groups(groups = everything())
        )
    }
    
  # Generate a list of OUs to loop over 
    distinct_agg_type <- function(df, type = "OU"){
      df %>% 
        filter(agg_type %in% {{type}}) %>% 
        distinct(operatingunit) %>% 
        pull()
    }  
    
  # Embiggen font size
    embiggen <- function(gt_obj){
      gt_obj %>% 
        tab_options(
          source_notes.font.size = 10,
          table.font.size = 15,
          footnotes.font.size = 10)
    }

# LOAD DATA ============================================================================  
    
    # Load OU_IM table - using FY21 Q2 data from Panorama
    ou_im <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-22_20211112") %>%
      read_msd() 

    msd_path <- si_path() %>% return_latest("OU_IM_FY19-22_20211112")

# MUNGE ============================================================================
    
    # Time metadata needed  
    pd <- create_pd(ou_im)
    msd_source <- pd %>% msd_period(period = .)
    qtr <- source_info(msd_path, return = "quarter")
    
    # Main Table
    mdb_df   <- make_mdb_df(ou_im)
    
    # Create the reshaped df that is gt() ready
    mdb_tbl  <- reshape_mdb_df(mdb_df, pd)
  
# VIZ ============================================================================
    
    # Generate base table for global results
    create_mdb(mdb_tbl, ou = "Global", type = "main", pd, msd_source, legend = legend_chunk) %>% 
      embiggen
    
    # Try a specific country now
    create_mdb(mdb_tbl, ou = "Zambia", type = "main", pd, msd_source, legend = legend_chunk) %>% embiggen

    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(ou_im)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)
    
    create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source) %>% 
      bold_column(., Q4) %>% 
      bold_rowgroup(.) %>% 
      embiggen()

# BATCH ------------------------------------------------------------------
    
    # Write the different types to character objects
    all_ous <- map(c("OU", "Agency", "Region-Country"), ~distinct_agg_type(mdb_tbl, .x)) %>% 
      flatten_chr()
    
    # Use purr to map across the list and create tables for all entries in each object
    purrr::walk(all_ous[27], 
               ~create_mdb(mdb_tbl, ou = .x, type = "main", pd, msd_source, legend = legend_chunk) %>% 
                 bold_rowgroup() %>% 
                 embiggen() %>% 
                 gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_KEY_INDICATORS_MD.png")))    
    
        # TREATMENT - generally same as above, but being safe just in case
    all_tx_ous <- map(c("OU", "Agency", "Region-Country"), ~distinct_agg_type(mdb_tbl_tx, .x)) %>% 
      flatten_chr()
    
    purrr::walk(all_tx_ous[24], 
                ~create_mdb(mdb_tbl_tx, ou = .x, type = "treatment", pd, msd_source) %>%
                  bold_column(., Q4) %>% 
                  bold_rowgroup() %>% 
                  embiggen() %>% 
                  tab_source_note(
                    source_note =  "South Africa has no national MMD program and has been excluded from MMD coverage rates.") %>%
                  tab_source_note(
                    source_note =  "VLC and VLS calculations based on unadjusted TX_CURR presented in main indicator table.") %>% 
                  gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_MMD_VL_MD.png")))
    
# EXPORT RAW DATA ------------------------------------------------------------------

  #Export main table and VLC table and migrate to google drive
   mdb_tbl %>% 
     select(-c(present_z_direction, present_tint_achv, indicator2)) %>% 
     rename(FY20_cumulative = past_results_cumulative,
            FY20_targest = past_targets,
            FY20_achv = past_targets_achievement,
            FY21_cumulative = present_results_cumulative,
            FY21_targets = present_targets,
            FY21_achv = present_targets_achievement,
            FY21_results_Q3 = present_z_aresults,
            FY21_change = present_z_change) %>% 
     arrange(operatingunit, agency, indicator) %>% 
     write_csv("Dataout/FY21Q3_core_indicators.csv", na = "")
   
   mdb_tbl_tx %>% 
     select(-c(indicator2, change_dir)) %>% 
     rename(FY20_results = results,
            FY21Q1_results = Q1,
            FY21Q2_results = Q2,
            FY21Q3_results = Q3) %>% 
     arrange(operatingunit, agency, indicator) %>% 
     write_csv("Dataout/FY21Q3_tx_indicators.csv", na = "")
   
# VIZ For GLOBAL PLANNING MEETING ============================================================================
   
   # Generate base table for global results
   create_mdb(mdb_tbl %>% filter(agency == "USAID"), ou = "Global", type = "main", pd, msd_source) %>% 
     cols_hide(6:8) %>% 
     embiggen %>% 
     tab_options(
       column_labels.font.size = 18
       ) %>% 
     tab_header(
       title = "", 
      subtitle = ""
      ) %>% 
   gtsave(., path = mdb_out, filename = glue::glue("GPM_KEY_INDICATORS_MD_Summary.png"))  
   
   
   

    
    