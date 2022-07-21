# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  SIC - remake SID score graphic
# LICENSE:  MIT
# REF ID:   9e751bb2
# DATE:     2022-07-06
# UPDATED:  2022-07-08

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "9e751bb2"
  
  sic_cntry <- c("Eswatini", "Kenya", "Uganda",  "Malawi", #"Rwanda",
                 "Lesotho", "Botswana", "Namibia", "Vietnam")
  
  gs_id <- as_sheets_id("1eD5PNKUkCSsPkJcK_JgxLd5gl2-Rhq7bEeUd83QCoWY")

# IMPORT ------------------------------------------------------------------
 
  df_sid <- read_sheet(gs_id) %>% 
    fill(country)

# MUNGE -------------------------------------------------------------------

  df_sid %>% 
    group_by(country) %>% 
    mutate(lower = min(avg_sid_score_weighted),
           upper = max(avg_sid_score_weighted),
           avg = mean(avg_sid_score_weighted),
           lab = case_when(avg_sid_score_weighted == lower & country %in% sic_cntry ~ avg),
           fill_color = ifelse(country %in% sic_cntry, genoa, trolley_grey_light),
           font_color = ifelse(country %in% sic_cntry, genoa, matterhorn),
           cntry_colored = ifelse(country %in% sic_cntry,
                                  glue("<span style='color:{font_color}'>**{country}**</span>"),
                                  glue("<span style='color:{font_color}'>{country}</span>"))) %>% 
    ungroup() %>% 
    ggplot(aes(avg_sid_score_weighted, fct_reorder(cntry_colored, avg_sid_score_weighted, mean))) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = grey10k) +
    geom_point(color = "white", alpha = .2) +
    geom_point(aes(color = font_color), alpha = .2) +
    geom_point(aes(x = avg, fill = fill_color), color = matterhorn, shape = 21, size = 6) +
    geom_text(aes(x = avg, label = number(lab, .1)), na.rm = TRUE,
              family = "Source Sans Pro", size = 8/.pt, color = "white") +
    expand_limits(x = c(0, 10)) +
    scale_x_continuous(expand = c(.005, .005)) +
    coord_cartesian(clip = "off") +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         title = toupper("Countries identified as 'Sustained Impact' did not have higher SID scores"),
         subtitle = "2021 SID Average Scores | large points represent country average across question areas (smaller points)",
         caption = glue("Data sourced from USAID SID Global Analysis Tableau Workbook | Ref id: {ref_id}")) +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(),
          axis.text.x = element_blank())

  si_save("Images/sid.png")  
  
  
  
  df_sid %>% 
    mutate(is_sic = ifelse(country %in% sic_cntry, 
                       "Identified Sustained Impact Country",
                       "Other PEPFAR Countries"),
           sid_area = str_remove(sid_area, "^[:digit:]{1,2}\\. ") %>% str_trim) %>%
    group_by(sid_area, is_sic) %>% 
    mutate(lower = min(avg_sid_score_weighted),
           upper = max(avg_sid_score_weighted),
           avg = mean(avg_sid_score_weighted),
           lab = case_when(avg_sid_score_weighted == lower ~ avg)) %>% 
    ungroup() %>% 
    group_by(sid_area) %>% 
    mutate(fill_color = case_when(lab == max(lab, na.rm = TRUE) & is_sic == "Identified Sustained Impact Country" ~ genoa,
                                  lab == max(lab, na.rm = TRUE) ~ moody_blue,
                                  !is.na(lab) ~ trolley_grey_light),
           font_color = ifelse(lab == max(lab, na.rm = TRUE), "white", matterhorn)) %>% 
    ungroup() %>% 
    ggplot(aes(avg_sid_score_weighted, fct_reorder(sid_area, avg_sid_score_weighted, mean))) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = grey10k) +
    geom_point(color = "white", alpha = .2) +
    geom_point(alpha = .2) +
    geom_point(aes(x = avg, fill = fill_color), color = matterhorn, shape = 21, size = 6) +
    geom_text(aes(x = avg, label = number(lab, .1), color = font_color),
              na.rm = TRUE,
              family = "Source Sans Pro", size = 8/.pt) +
    facet_wrap(~is_sic) +
    expand_limits(x = c(0, 10)) +
    scale_x_continuous(expand = c(.005, .005)) +
    coord_cartesian(clip = "off") +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         title = toupper("Countries identified as 'Sustained Impact' did not have higher SID scores"),
         subtitle = "2021 SID Average Scores | large points represent group average for countries  (smaller points)",
         caption = glue("Sustained Impact Countries: {paste0(sic_cntry, collapse = ', ')}
                        Data sourced from USAID SID Global Analysis Tableau Workbook | Ref id: {ref_id}")) +
    si_style_xgrid() +
    theme(axis.text.x = element_blank())
  
  
  si_save("Images/sid_areas.png")  
  