# PURPOSE: Creating graphics for cherry picked OVC data
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-11-10
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(tidyverse)
library(gophr)
library(scales)
library(gt)
library(googlesheets4)
library(ggtext)


# CONNECT -----------------------------------------------------------------

  df <- read_sheet("https://docs.google.com/spreadsheets/d/1X2ws9H3mYvCG1L0BLs4l68Go54aoBFdRVDrlWj5_D4w/edit#gid=0")

  df_viz <- 
    df %>% 
    mutate(col_color = case_when(
      program == "Clinical" ~ genoa_light,
      program == "OVC & Clinical" ~ genoa,
      program == "Enrolled in OVC" ~ genoa,
      TRUE ~ trolley_grey_light)) %>% 
    mutate(ou_indicator = paste(ou, indicator, sep = "-"),
           program_order = fct_relevel(program, c("Not enrolled in OVC", "Enrolled in OVC", "Clinical", "OVC & Clinical")))
  
  df_viz %>% 
    ggplot(aes(x = group, y = value, group = program_order)) +
    geom_col(aes(fill = program_order), width = 0.8, position = position_dodge(width = 0.75),
             alpha = 1) +
    geom_text(aes(label = percent(value, 1)), 
              position = position_dodge(width = .75), vjust = -0.3, size = 9/.pt, family = "Source Sans Pro",
              color = grey90k) +
    facet_wrap(~ou_indicator, scales = "free_x") +
    # scale_fill_hue(c = 50) +
    si_style_ygrid() +
    scale_y_continuous(labels = percent) +
    coord_cartesian(expand = T) +
    labs(title = NULL, x = NULL, y = NULL, fill = "Enrollment Type") +
    theme(axis.text.y = element_blank()) +
    guides(fill=guide_legend(ncol=2)) +
    scale_fill_manual(values = c("Clinical" = genoa_light, "OVC & Clinical" = genoa,
                                "Enrolled in OVC" = genoa, "Not enrolled in OVC" = si_palettes$siei_achv[4]))
    
    
  
  