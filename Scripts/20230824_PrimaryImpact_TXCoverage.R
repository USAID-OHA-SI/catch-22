# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Pull TX coverage for OUs and Primary Impact Districts
# REF ID:   292124e6 
# LICENSE:  MIT
# DATE:     2023-08-25
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

# IMPORT ---------------------------------------------------------------

df_natsubnat <- si_path() %>%
  return_latest("NAT_SUBNAT") %>%
  read_psd()

get_metadata(type = "NAT_SUBNAT")

df_natsubnat_ind <- tibble::tribble(
  ~indicator, ~standardizeddisaggregate,
  "DIAGNOSED_SUBNAT",       "Age/Sex/HIVStatus",
  "PLHIV",       "Age/Sex/HIVStatus",
  "POP_EST",                 "Age/Sex",
  "TX_CURR_SUBNAT",       "Age/Sex/HIVStatus",
  "VL_SUPPRESSION_SUBNAT",       "Age/Sex/HIVStatus"
)


#filter to select indicators/disaggs
df_natsubnat <- df_natsubnat %>%
  semi_join(df_natsubnat_ind, by = c("indicator", "standardizeddisaggregate"))

rm(df_natsubnat_ind)

#clean PSNU names
df_natsubnat <- clean_psnu(df_natsubnat)

#google id for Primary impact district xwalk 
g_id <- "10KLNSuu6nZtV0p2FOS7dY8by0_Fa9gvyLUH1zKqBAvc"
pi_districts <- read_sheet(g_id)

# MUNGE -----------------------------------------------------------------


#get prioritization levels -
prioritization_map <- grabr::get_outable() %>% 
  filter(country %in% c("Kenya", "Malawi", "Cote d'Ivoire", "Nigeria", "Ghana")) %>% 
  select(operatingunit, operatingunit_uid, psnu_lvl)

#iterate
map_df <- map2(.x = prioritization_map$operatingunit_uid,
               .y = prioritization_map$psnu_lvl, ~grabr::get_ouorgs(.x, .y) %>% mutate(psnu_level = .y,
                                                                                       ou_uid = .x)) %>%
  reduce(rbind)


#pull out hierachies for all but Ghana
snu_map <- df_natsubnat %>% 
  filter(operatingunit %in% c("Kenya", "Malawi", "Cote d'Ivoire", "Nigeria", "Ghana"),
         fiscal_year == 2023) %>% 
  count(operatingunit, operatingunituid, snu1, snu1uid, psnu, psnuuid)

#Malawi - isolate PI district at PSNU level
mwi_map <- snu_map %>% 
  select(-n) %>% 
  filter(operatingunit %in% c("Malawi")) %>% 
  left_join(pi_districts %>%
              filter(level3name %in% c("Malawi")) %>% select(uidlevel3, level3name, uidlevel6, level6name),
            by = c('operatingunit' = 'level3name', 'operatingunituid' = 'uidlevel3', 'psnuuid' = 'uidlevel6')) %>% 
  filter(!is.na(level6name)) %>% 
  select(-c(snu1, snu1uid)) %>% 
  rename(district = psnu,
         district_uid = psnuuid) %>% 
  select(-c(level6name))

#Kenya, Nigeria, CDI - isolate PI district at snu1 level
ken_nga_cdi_join <- snu_map %>% 
  select(-n) %>% 
  filter(operatingunit != "Malawi") %>% 
  left_join(pi_districts %>%
              filter(level3name != "Malawi") %>% select(uidlevel3, level3name, uidlevel4, level4name),
            by = c('operatingunit' = 'level3name', 'operatingunituid' = 'uidlevel3', 'snu1uid' = 'uidlevel4')) %>% 
  filter(!is.na(level4name)) %>% 
  select(-c(psnu, psnuuid)) %>% 
  rename(district = snu1,
         district_uid = snu1uid) %>% 
  select(-c(level4name)) 


#filter df down PI districts
df_snu_level <- df_natsubnat %>% 
  filter(country %in% c("Kenya", "Cote d'Ivoire", "Nigeria"),
         snu1uid %in% ken_nga_cdi_join$district_uid) 

df_mwi_level <- df_natsubnat %>% 
  filter(country %in% c("Malawi"),
         psnuuid %in% mwi_map$district_uid) 

#bind together
df_final <- df_snu_level %>% 
  rbind(df_mwi_level)

#DEAL WITH GHANA - filter to PSNUS only in Western Region snu2 
df_ghana <- df_natsubnat %>% 
  filter(country == "Ghana",
         psnu %in% c("Ahanta West", "Effia-Kwesimintsim", "Ellembele",
                     "Jomoro",
                     "Mpohor",
                     "Nzema East",
                     "Prestea-Huni Valley",
                     "Sekondi Takoradi",
                     "Shama",
                     "Tarkwa-Nsueam",
                     "Wassa Amenfi Central",
                     "Wassa Amenfi East",
                     "Wassa Amenfi West",
                     "Wassa East"
         )) 


# FUNCTIONS -------------------------------------------------------------------------------


prep_txcoverage_district <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ind_sel <- c("PLHIV","TX_CURR_SUBNAT")
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
                  fiscal_year == metadata$curr_fy,
                  indicator %in% ind_sel,
                  standardizeddisaggregate == "Age/Sex/HIVStatus",
                  ageasentered != "Unknown Age") 
  
  if(nrow(df_gap) == 0)
    return(NULL)
  
  if(cntry == "Malawi") {
    df_gap <- df_gap %>% 
      dplyr::count(fiscal_year, country, psnu, indicator, wt = targets, name = "value") %>% 
      tidyr::pivot_wider(names_from = indicator,
                         names_glue = "{tolower(indicator)}")
  } else if (cntry == "Ghana") {
    df_gap <- df_gap %>% 
      mutate(snu2 = "Western Region") %>% 
      dplyr::count(fiscal_year, country, snu2, indicator, wt = targets, name = "value") %>% 
      tidyr::pivot_wider(names_from = indicator,
                         names_glue = "{tolower(indicator)}")
  }
  else {
    df_gap <- df_gap %>% 
      dplyr::count(fiscal_year, country, snu1, indicator, wt = targets, name = "value") %>% 
      tidyr::pivot_wider(names_from = indicator,
                         names_glue = "{tolower(indicator)}")
  }
  
  
  if("plhiv" %ni% names(df_gap))
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::mutate(cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(facet_grp = glue::glue("{unique(df_gap$fiscal_year)} Treatment coverage gaps across {unique(df_gap$country)} Priority Primary Impact Districts <br>TX_CURR_SUBNAT ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(tx_curr_subnat, na.rm = TRUE))}) coverage of PLHIV ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(plhiv, na.rm = TRUE))})"),
                  lab_text = scales::percent(cov_tx, 1)) %>% 
    dplyr::ungroup()
  
  #reshape long for combining with TX_NEW plotting (and remove dup labeling in plot)
  df_viz <- df_viz %>% 
    tidyr::pivot_longer(c("plhiv", "tx_curr_subnat"),
                        names_to = "indicator") %>% 
    dplyr::mutate(indicator = toupper(indicator),
                  dplyr::across(c(cov_tx, plhiv_marker, lab_text), \(x) ifelse(indicator == "PLHIV", x, NA)))
  
  return(df_viz)
  
} 


viz_tx_cov <- function(df, cntry) {
  
  #group_var <- ifelse(cntry == "Malawi", "psnu", "snu1")
  
  df %>% 
    ggplot2::ggplot(aes(value, snu1, fill = fill_color, color = fill_color)) +
    ggplot2::geom_blank(aes(value*1.1), na.rm = TRUE) +
    ggplot2::geom_col(data = df %>% filter(indicator == "PLHIV"),
                      fill = NA, width = .8, alpha = .8, na.rm = TRUE) +
    ggplot2::geom_col(data = df %>% filter(indicator != "PLHIV"), 
                      width = .8, alpha = .8, na.rm = TRUE) +
    ggplot2::geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker),
                           na.rm = TRUE, color = "white", linetype = "dotted") +
    ggplot2::geom_text(aes(label = lab_text), na.rm = TRUE,
                       family = "Source Sans Pro", color = glitr::suva_grey,
                       size = 10/.pt, hjust = -.5) +
    # ggplot2::facet_grid(snu1 ~ facet_grp, switch = "y", scales = "free_x") +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                                expand = c(.005, .005)) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = {df$facet_grp} %>% toupper(),
                  caption = glue("{metadata$source} | USAID/OHA/SIEI")) +
    ggplot2::coord_cartesian(clip = "off") +
    glitr::si_style_xgrid() +
    ggplot2::theme(strip.text.y = element_blank(),
                   strip.text.x = element_markdown(),
                   strip.placement = "outside",
                   panel.spacing.x = unit(1, "lines"),
                   panel.spacing.y = unit(.5, "lines")
    )
  
}


#OU totals
prep_txcoverage_ou <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ind_sel <- c("PLHIV","TX_CURR_SUBNAT")
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
                  fiscal_year == metadata$curr_fy,
                  indicator %in% ind_sel,
                  standardizeddisaggregate == "Age/Sex/HIVStatus",
                  ageasentered != "Unknown Age") 
  
  if(nrow(df_gap) == 0)
    return(NULL)
  
  
  df_gap <- df_gap %>% 
    dplyr::count(fiscal_year, country,indicator, wt = targets, name = "value") %>% 
    tidyr::pivot_wider(names_from = indicator,
                       names_glue = "{tolower(indicator)}")
  
  
  if("plhiv" %ni% names(df_gap))
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::mutate(cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(facet_grp = glue::glue("{unique(df_gap$fiscal_year)} Treatment coverage gaps across {unique(df_gap$country)} Priority Primary Impact Districts <br>TX_CURR_SUBNAT ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(tx_curr_subnat, na.rm = TRUE))}) coverage of PLHIV ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(plhiv, na.rm = TRUE))})"),
                  lab_text = scales::percent(cov_tx, 1)) %>% 
    dplyr::ungroup()
  
  #reshape long for combining with TX_NEW plotting (and remove dup labeling in plot)
  df_viz <- df_viz %>% 
    tidyr::pivot_longer(c("plhiv", "tx_curr_subnat"),
                        names_to = "indicator") %>% 
    dplyr::mutate(indicator = toupper(indicator),
                  dplyr::across(c(cov_tx, plhiv_marker, lab_text), \(x) ifelse(indicator == "PLHIV", x, NA)))
  
  return(df_viz)
  
} 

# APPLY FUNCTIONS -----------------------------------------------

#Apply function for district calcs
prep_txcoverage_district(df_ghana, "Ghana")
prep_txcoverage_district(df_final, "Malawi")
prep_txcoverage_district(df_final, "Nigeria")
prep_txcoverage_district(df_final, "Cote d'Ivoire")
prep_txcoverage_district(df_final, "Kenya")

#calc for OUs
prep_txcoverage_ou(df_natsubnat, "Kenya")
prep_txcoverage_ou(df_natsubnat, "Nigeria")
prep_txcoverage_ou(df_natsubnat, "Malawi")
prep_txcoverage_ou(df_natsubnat, "Ghana")
prep_txcoverage_ou(df_natsubnat, "Cote d'Ivoire")


#viz ?
prep_txcoverage_district(df_final, "Kenya") %>% 
  # rename(snu1 = psnu) %>% 
  mutate(fill_color = scooter) %>% 
  viz_tx_cov("Kenya")


