# PROJECT:  catch-22
# AUTHOR:   A.Chafetz, T.Essam, K.Srikanth | USAID
# PURPOSE:  Explore VLS aross USAID mechanisms
# LICENSE:  MIT
# DATE:     2021-10-18
# UPDATED: 

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
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  msd_source <- source_info()
  pd <- source_info(return = "period")

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   


# MUNGE -------------------------------------------------------------------

  #filter for necessary indicators
  df_vls <- df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator()
  
  #aggregate to country x mech x fy x ind level
  df_vls <- df_vls %>%
    bind_rows(df_vls %>% mutate(mech_code = "National")) %>% 
    group_by(countryname, mech_code, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(type = ifelse(mech_code == "National", "national", "mech"), .before = mech_code)

  #reshape long by period, then wide by indicator for calculations
  df_vls <- df_vls %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}")

  #create alternate denom for VLC
  df_vls <- df_vls %>% 
    group_by(countryname, mech_code) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = "period")) %>% 
    ungroup()

  #limit to most recent period & TX_PVLS_D > 0
  df_vls <- df_vls %>% 
    filter(period == max(period),
           tx_pvls_d > 0)
  
  #calc VLC/S
  df_vls <- df_vls %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  df_vls %>% 
    filter(type == "national") %>%
    summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  #identify where 80% of TX_CURR is for viz facet
  df_ctry_grps <- df_vls %>% 
    filter(mech_code == "National") %>% 
    count(countryname, wt = tx_curr) %>% 
    arrange(desc(n)) %>% 
    mutate(cumsum = cumsum(n),
           share = cumsum/sum(n),
           grp = case_when(share < .83 ~ "Top 80%",
                           share < .95 ~ "Next 15%",
                           TRUE ~ "Remaining 5%")) %>% 
    select(countryname, grp)
  
  df_vls <- rename_official(df_vls)
  
  #viz dataframe
  df_viz <- df_vls %>% 
    left_join(df_ctry_grps, by = "countryname") %>% 
    mutate(vls_mech = case_when(mech_code != "National" ~ vls),
           vls_nat = case_when(mech_code == "National" ~ vls),
           fill_color = ifelse(vls_mech > .9, scooter, moody_blue),
           countryname = recode(countryname, 
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR"),
           clean_name = case_when(str_detect(mech_name, "EpiC") ~ "EpiC",
                                  primepartner == "Abt Associates Inc." ~ "Abt",
                                  primepartner == "Family Health International" ~ "FHI360",
                                  str_detect(primepartner, "INTRAHEALTH") ~ "IntraHealth",
                                  str_detect(primepartner, "HEALTH THROUGH") ~ "Health Through Walls",
                                  primepartner == "Caris Foundation International" ~ "Caris Foundation",
                                  primepartner == "Johns Hopkins University, The" ~ "JHU",
                                  primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                  primepartner == "PAKACHERE INSTITUTE OF HEALTH AND DEVELOPMENT COMMUNICATION" ~ "Pakachere",
                                  primepartner == "WITS HEALTH CONSORTIUM (PTY) LTD" ~ "Wits",
                                  TRUE ~ str_to_title(primepartner)),
           grp = factor(grp, c("Top 80%", "Next 15%", "Remaining 5%")),
           mech_lab = case_when(vls_mech < .9 ~ glue("{clean_name}\n({mech_code})")))
  
  
# VIZ ---------------------------------------------------------------------

  
  df_usaid <- df_vls %>% 
    filter(type == "national") %>%
    summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  df_viz %>% 
    ggplot(aes(vls_mech, fct_reorder(countryname, vls_nat, na.rm = TRUE))) +
    geom_blank() +
    annotate("rect",
             xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
             fill = trolley_grey_light, alpha = .4) +
    geom_vline(xintercept = .9, linetype = "dashed") +
    geom_point(aes(size = tx_curr, 
      color = fill_color), alpha = .6,
               position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
    geom_text_repel(aes(label = mech_lab), na.rm = TRUE, max.overlaps = 30,
                    family = "Source Sans Pro", color = "#505050", size = 9/.pt) +
    geom_errorbar(aes(xmin = vls_nat, xmax = vls_nat), size = 1.1, color = grey60k) +
    scale_x_continuous(label = percent_format(1)) +
    facet_grid(grp ~ ., scale = "free_y", space = "free") +
    scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M"),
               range = c(2,10)) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    expand_limits(x = .75) +
    labs(y = NULL, x = "Viral Load Supression Rate (TX_PVLS/TX_PVLS_D)",
         # title = glue("While USAID is at {percent(df_usaid$vls, 1)} Viral Load Suppression in {pd}, a number of implementating mechanism fall short of the 90% goal") %>% toupper() %>% str_wrap(),
         size = glue("Current on Treatment ({pd})"),
         
         caption = glue("Source: {msd_source}
                        SI Analytics: {paste0(authors, collapse = '/')}
                        US Agency for International Development")) +
    si_style(facet_space = .5) +
    theme(#legend.position = "none",
          axis.text.y = element_text(size = 9),
          strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))

  si_save("Graphics/FY21Q3_USAID_VLS-partners.svg")  
  
  
  df_viz %>% 
    filter(type == "mech") %>% 
    count(vls < .9)
  