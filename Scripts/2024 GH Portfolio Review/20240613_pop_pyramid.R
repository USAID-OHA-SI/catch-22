# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  pop pyramids for Uganda to show youth bulge 
# REF ID:   d0329373 
# LICENSE:  MIT
# DATE:     2024-06-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------


library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)
library(mindthegap)

# IMPORT -----------------------------------------------------------------------

subnat_filepath <- si_path() %>% 
  return_latest("NAT_SUBNAT")

df_subnat <- read_psd(subnat_filepath)

metadata_natsubnat <-get_metadata(subnat_filepath)


subnat_2023 <- df_subnat %>% 
  filter(fiscal_year == 2023)
# PREP -------------------------------------------------------------------------

# df = df_natsubnat comes from 91_setup.R, could add a test to make sure 
# it is actually natsubnat data
# cntry is a string
# level is either country or psnu

prep_pop_pyramid <- function(df, cntry, level){
  
  # clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry, 
      indicator %in% c("PLHIV", "POP_EST"))
  
  if(nrow(df_filt) == 0)
    return(NULL)
  
  df_filt %>%
    assertr::verify(indicator %in% c("PLHIV", "POP_EST") &
                      fiscal_year == max(fiscal_year) &
                      country == cntry,
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly.
                                               Please check the first filter in prep_pop_pyramid().")),
                    description = glue::glue("Verify that the filters worked"))
  
  grp_vars <- c("fiscal_year", "country", "indicator", "sex", "ageasentered")
  
  if(level != "country"){
    level_uid <- glue::glue("{level}uid")
    grp_vars <- c(grp_vars, {level}, level_uid)
    
  }
  
  df_filt <- df_filt %>%
    dplyr::mutate(indicator = ifelse(indicator == "POP_EST", "Population (Est)", indicator)) %>% 
    dplyr::group_by(dplyr::pick(grp_vars)) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(population = if_else(sex == "Male", -targets, targets))
  
  df_viz <- df_filt %>%
    tidyr::drop_na(sex, ageasentered)
  
  grp_vars_viz <- c("indicator")
  if(level != "country")
    grp_vars <- c(grp_vars_viz, {level}, {level_uid})
  
  df_viz <- df_viz %>% 
    dplyr::group_by(dplyr::across(grp_vars_viz)) %>% 
    dplyr::mutate(axis_max = max(targets, na.rm = TRUE),
                  axis_min = -axis_max) %>% 
    dplyr::ungroup()
  
  df_viz <- df_viz %>% 
    dplyr::mutate(facet_grp = forcats::fct_rev(indicator))
  
  return(df_viz)
  
}


prep_pop_pyramid_dreams <- function(df_prep_psnu, df_mer){
  
  if(is.null(df_prep_psnu) || nrow(df_prep_psnu) == 0)
    return(NULL)
  
  #identify DREAMS PSNUs
  df_dreams <- df_mer %>% 
    filter(fiscal_year == max(fiscal_year),
           country == unique(df_prep_psnu$country),
           dreams == "Y") %>% 
    distinct(fiscal_year, psnuuid)
  
  #filter join to only keep DREAMS PSNUs
  df_prep_psnu_dreams <-  dplyr::semi_join(df_prep_psnu, df_dreams, 
                                           by = join_by(fiscal_year, psnuuid))
  
  if(nrow(df_prep_psnu_dreams) == 0)
    return(NULL)
  
  #keep only pop and order psnus
  df_prep_psnu_dreams <- df_prep_psnu_dreams %>% 
    dplyr::filter(indicator == "Population (Est)") %>% 
    dplyr::mutate(psnu = forcats::fct_reorder(psnu, targets, .fun = sum, .desc = TRUE),
                  facet_grp = psnu)
  
  return(df_prep_psnu_dreams)
}

# VIZ --------------------------------------------------------------------------

# df = df_natsubnat comes from 91_setup.R, could add a test to make sure 

viz_pop_pyramid <- function(df, type = NULL){
  
  q <- ifelse(is.null(type), 
              glue::glue("Population dynamics present an additional risk to ending HIV as a pandemic threat by 2030") %>% toupper,
              glue::glue("Is there a youth bulge the country needs to plan for in DREAMS PSNUs?") %>% toupper
  )
  
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  q <- stringr::str_replace(q, "THE COUNTRY", toupper(unique(df$country)))
  
  ref_id <- "aa8bd5b4"
  vrsn <- 2
  
  subt <-  ifelse(is.null(type),
                  glue::glue("Comparison between <span style='color:{genoa}'>Males</span> & <span style='color:{moody_blue}'>Females</span> by age band in {unique(df$country)},{max(df$fiscal_year)}"),
                  glue::glue("Comparison between <span style='color:{genoa}'>Males</span> & <span style='color:{moody_blue}'>Females</span> Population (Est) by age band")
  )
  
  cap_note <- ""
  
  f_nrow <- 1
  
  # Display only a subset of PSNUs
  if("psnuuid" %in% names(df)){
    df_psnus <- df %>% 
      dplyr::filter(indicator == "Population (Est)") 
    
    n_max <- 8
    
    cap_note <- ifelse(length(unique(df_psnus$psnuuid)) > n_max, "Note: Limited to the largest PSNUs by Population (Est) \n", "")
    
    v_lim_uids <- df_psnus %>% 
      dplyr::count(psnuuid, wt = targets, name = "targets", sort = TRUE) %>% 
      dplyr::slice_head(n = n_max) %>%  
      dplyr::pull(psnuuid)
    
    f_nrow <- ifelse(length(v_lim_uids) > 4, 2, 1)
    
    df <- dplyr::filter(df, psnuuid %in% v_lim_uids) 
  }
  
  
  df %>%
    ggplot2::ggplot(aes(population, ageasentered, fill = sex)) +
    ggplot2::geom_blank(aes(axis_max)) +
    ggplot2::geom_blank(aes(axis_min)) +
    ggplot2::geom_col(alpha = .8, na.rm = TRUE) +
    ggplot2::geom_vline(aes(xintercept = 0), color = "white", linewidth = 1.1)+
    ggplot2::facet_wrap(~facet_grp, scales = "free_x", nrow = f_nrow) +
    ggplot2::scale_fill_manual(values = c("Male" = glitr::genoa, 
                                          "Female" = glitr::moody_blue)) +
    ggplot2::scale_x_continuous(
      labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
    ) +
    ggplot2::labs(title = {q},
                  subtitle =  {subt},
                  x = NULL, y = NULL, fill = NULL,
                  caption = 
                    glue("{cap_note}{metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    glitr::si_style_yline() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = element_text(hjust = .5),
      plot.subtitle = element_markdown(),
      panel.spacing = unit(.2, "picas"))
  
}

prep_pop_pyramid(subnat_2023, "Uganda", "country") %>% viz_pop_pyramid()
si_save("Images/FY24 GH Portfolio Review/2023_UGA_pop_pyramid.png")

prep_pop_pyramid(df_subnat, "Uganda", "country") %>% viz_pop_pyramid()
si_save("Images/FY24 GH Portfolio Review/2024_UGA_pop_pyramid.png")

