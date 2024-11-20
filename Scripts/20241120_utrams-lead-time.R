# PROJECT:  catch-22
# PURPOSE:  Review sub-standard UTRAMS lead time
# AUTHOR:   A.Chafetz | USAID
# REF ID:   b0b5b4cb 
# LICENSE:  MIT
# DATE:     2024-11-20
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "b0b5b4cb"  #a reference to be places in viz captions 
  
  path_utrams <-  si_path("path_downloads") %>% return_latest("UTRAMS")
  

# IMPORT ------------------------------------------------------------------
  
  df_utrams <- read_excel(path_utrams,.name_repair = make_clean_names)
  

# MUNGE -------------------------------------------------------------------

  v_status <- c(
    "1. Pending Lead Assignment",
    "2. Lead Assigned",
    "2a. Supervisor approved",
    "3. Complete",
    "3a. Cancelled",
    "3b. Transferred to Another Bureau"
  )


  df_fy24 <- df_utrams %>% 
    mutate(across(ends_with("date"), as_date)) %>% 
    filter(bureau == "BUREAU - GH",
           request_status %in% v_status,
           origin_date >= "2023-10-01", 
           origin_date < "2024-10-01"
           ) 

  df_fy24 <- df_fy24 %>% 
    mutate(lead_time = start_date - origin_date,
           lead_time = as.numeric(lead_time),
           lead_time_desired = origin_date + weeks(4),
           too_short = start_date <= lead_time_desired
           )
  
  
  df_viz <- df_fy24 %>%
    bind_rows(df_fy24 %>% mutate(office = "BUREAU - GH")) %>% 
    count(office, too_short) %>% 
    group_by(office) %>% 
    mutate(total = sum(n),
           share = n / total,
           office_lab = glue("{office}<br>*{label_comma()(total)} trips*")) %>% 
    ungroup() %>% 
    mutate(share_lab = case_when(too_short == TRUE ~ share),
           share_pos = .04,
           fill_color = case_when(office == "BUREAU - GH" & too_short == TRUE ~ orchid_bloom,
                                  office == "BUREAU - GH" ~ si_palettes$slate_t[4],
                                  too_short == TRUE ~ si_palettes$orchid_bloom_t[3],
                                  TRUE ~ si_palettes$slate_t[5]))
  
  
  df_oha <- df_utrams %>% 
    mutate(across(ends_with("date"), as_date)) %>% 
    filter(office == "OHA",
           request_status %in% v_status,
           origin_date >= "2023-10-01", 
           origin_date < "2024-10-01"
    ) 
  
  df_oha <- df_oha %>% 
    mutate(lead_time = start_date - origin_date,
           lead_time = as.numeric(lead_time),
           lead_time_desired = origin_date + weeks(4),
           too_short = start_date <= lead_time_desired
    )
  
  
  df_viz_oha <- df_oha %>%
    bind_rows(df_oha %>% mutate(division = "OFFICE - OHA")) %>% 
    count(division, too_short) %>% 
    group_by(division) %>% 
    mutate(division = str_remove(division, "OHA\\/"),
           division = ifelse(division == "OHA", "Unclassified", division),
           total = sum(n),
           share = n / total,
           division_lab = glue("{division}<br>*{label_comma()(total)} trips*")) %>% 
    ungroup() %>% 
    mutate(share_lab = case_when(too_short == TRUE ~ share),
           share_pos = .04,
           fill_color = case_when(division == "OFFICE - OHA" & too_short == TRUE ~ orchid_bloom,
                                  division == "OFFICE - OHA" ~ si_palettes$slate_t[4],
                                  too_short == TRUE ~ si_palettes$orchid_bloom_t[3],
                                  TRUE ~ si_palettes$slate_t[5]))

# VIZ ---------------------------------------------------------------------


  ## GH
  
  v1 <- df_viz %>% 
    mutate(n_v = ifelse(office == "BUREAU - GH", NA, n)) %>% 
    ggplot(aes(n_v, fct_reorder(office_lab, n, sum))) +
    geom_col(fill = si_palettes$electric_indigo_t[2]) +
    scale_x_continuous(label = label_comma(), expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "OHA had nearly twice has many trips\nas the next office in FY24" %>% toupper,
         # caption = glue("Source: UTRAMS Report [2024-11-20] | Ref id: {ref_id}")
         ) +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(),
          # axis.text.x = element_blank()
    )
  
  v_shr <- df_viz %>% 
    filter(office == "BUREAU - GH",
           too_short == TRUE) %>% 
    pull(share)
  
  v2 <- df_viz %>% 
    ggplot(aes(share, fct_reorder(office_lab, n, sum), 
               fill = fill_color)) +
    geom_col() +
    geom_text(aes(label = label_percent(1)(share_lab), x = .05),
              na.rm = TRUE,
              family = "Source Sans 3", color = "white") +
    scale_fill_identity() +
    scale_x_continuous(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = glue("IN FY24, {label_percent(1)(v_shr)} OF GH TRIPS WERE SUBMITTED<br>WITH <span style='color: {orchid_bloom};'>LESS THAN 4 WEEKS</span> IN ADVANCE OF TRAVEL")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          axis.text.x = element_blank(),
          plot.title = element_markdown()
          )
  
  v1 + v2 +
    plot_annotation(caption = glue("Note: TDYs range from Oct 1, 2023 - Sep 20, 2024 inclusive of all request statuses
                                   Calculation: Desired lead time = start_date <= origin_date + 4 weeks,
                                   Source: UTRAMS Report [2024-11-20] | Ref id: {ref_id}"),
                    theme = si_style())
  

  si_preview()
  
  si_save("Images/utrams_gh.png")
  
  ## OHA
  
  
  v_shr_oha <- df_viz_oha %>% 
    filter(division == "OFFICE - OHA",
           too_short == TRUE) %>% 
    pull(share)
  
  v1_oha <- df_viz_oha %>% 
    mutate(n_v = ifelse(division == "OFFICE - OHA", NA, n)) %>% 
    ggplot(aes(n_v, fct_reorder(division_lab, n, sum))) +
    geom_col(fill = si_palettes$electric_indigo_t[2]) +
    scale_x_continuous(label = label_comma(), expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "OHA TDYS in FY24" %>% toupper,
         # caption = glue("Source: UTRAMS Report [2024-11-20] | Ref id: {ref_id}")
    ) +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(),
          # axis.text.x = element_blank()
    )
  
  v2_oha <- df_viz_oha %>% 
    ggplot(aes(share, fct_reorder(division_lab, n, sum), 
               fill = fill_color)) +
    geom_col() +
    geom_text(aes(label = label_percent(1)(share_lab), x = .05),
              na.rm = TRUE,
              family = "Source Sans 3", color = "white") +
    scale_fill_identity() +
    scale_x_continuous(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = glue("IN FY24, {label_percent(1)(v_shr_oha)} OF OHA TRIPS WERE SUBMITTED<br>WITH <span style='color: {orchid_bloom};'>LESS THAN 4 WEEKS</span> IN ADVANCE OF TRAVEL")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          axis.text.x = element_blank(),
          plot.title = element_markdown()
    )
  
 
  v1_oha + v2_oha +
    plot_annotation(caption = glue("Note: TDYs range from Oct 1, 2023 - Sep 20, 2024 inclusive of all request statuses
                                   Calculation: Desired lead time = start_date <= origin_date + 4 weeks,
                                   Source: UTRAMS Report [2024-11-20] | Ref id: {ref_id}"),
                    theme = si_style())
  
  
  si_preview()
  
  si_save("Images/utrams_oha.png")
  