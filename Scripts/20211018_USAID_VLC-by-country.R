# PROJECT:  catch-22
# AUTHOR:   A.Chafetz, T.Essam, K.Srikanth | USAID
# PURPOSE:  Explore VLS aross USAID mechanisms
# LICENSE:  MIT
# DATE:     2021-10-18
# UPDATED:  2021-10-20

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
  
  #site adjusted dataset
  site_adj_path <- "../right_size/Dataout/TX_CURR_NN_Calcs.csv"
  
# IMPORT ------------------------------------------------------------------
  
  # df <- si_path() %>% 
  #   return_latest("OU_IM") %>% 
  #   read_rds()   

  df_siteadj <- read_csv(site_adj_path)

# MUNGE -------------------------------------------------------------------

  # #filter for necessary indicators
  # df_vls <- df %>% 
  #   filter(fundingagency == "USAID",
  #          indicator %in% c("TX_CURR", "TX_PVLS"),
  #          standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  #   clean_indicator()
  # 
  # #aggregate to country x mech x fy x ind level
  # df_vls <- df_vls %>%
  #   bind_rows(df_vls %>% mutate(mech_code = "National")) %>% 
  #   group_by(operatingunit, mech_code, fiscal_year, indicator) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   mutate(type = ifelse(mech_code == "National", "national", "mech"), .before = mech_code)
  # 
  # #reshape long by period, then wide by indicator for calculations
  # df_vls <- df_vls %>% 
  #   reshape_msd() %>% 
  #   select(-period_type) %>% 
  #   pivot_wider(names_from = indicator,
  #               names_glue = "{tolower(indicator)}")
  # 
  # #create alternate denom for VLC
  # df_vls <- df_vls %>% 
  #   group_by(operatingunit, mech_code) %>% 
  #   mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = "period")) %>% 
  #   ungroup()
  # 
  # #limit to most recent period & TX_PVLS_D > 0
  # df_vls <- df_vls %>% 
  #   filter(period == max(period),
  #          tx_pvls_d > 0)
  # 
  # #calc VLC/S
  # df_vls <- df_vls %>% 
  #   mutate(vlc = tx_pvls_d/tx_curr_lag2,
  #          vls = tx_pvls/tx_pvls_d,
  #          vls_alt = tx_pvls/tx_curr_lag2,
  #          vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
  #          vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  # 
  # df_vls %>% 
  #   filter(type == "national") %>%
  #   summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
  #   mutate(vlc = tx_pvls_d/tx_curr_lag2,
  #          vls = tx_pvls/tx_pvls_d,
  #          vls_alt = tx_pvls/tx_curr_lag2,
  #          vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
  #          vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  # 
  # #identify where 80% of TX_CURR is for viz facet
  # df_ctry_grps <- df_vls %>% 
  #   filter(mech_code == "National") %>% 
  #   count(operatingunit, wt = tx_curr) %>% 
  #   arrange(desc(n)) %>% 
  #   mutate(cumsum = cumsum(n),
  #          share = cumsum/sum(n),
  #          grp = case_when(share < .83 ~ "Top 80%",
  #                          share < .95 ~ "Next 15%",
  #                          TRUE ~ "Remaining 5%")) %>% 
  #   select(operatingunit, grp)
  # 
  # df_vls <- rename_official(df_vls)
  
# VIZ ---------------------------------------------------------------------
  
  # #viz dataframe
  # df_viz <- df_vls %>% 
  #   left_join(df_ctry_grps, by = "operatingunit") %>% 
  #   mutate(vls_mech = case_when(mech_code != "National" ~ vls),
  #          vls_nat = case_when(mech_code == "National" ~ vls),
  #          fill_color = ifelse(vls_mech > .9, scooter, moody_blue),
  #          operatingunit = recode(operatingunit, 
  #                               "Democratic Republic of the Congo" = "DRC",
  #                               "Papua New Guinea" = "PNG",
  #                               "Dominican Republic" = "DR"),
  #          clean_name = case_when(str_detect(mech_name, "EpiC") ~ "EpiC",
  #                                 primepartner == "Abt Associates Inc." ~ "Abt",
  #                                 primepartner == "Family Health International" ~ "FHI360",
  #                                 str_detect(primepartner, "INTRAHEALTH") ~ "IntraHealth",
  #                                 str_detect(primepartner, "HEALTH THROUGH") ~ "Health Through Walls",
  #                                 primepartner == "Caris Foundation International" ~ "Caris Foundation",
  #                                 primepartner == "Johns Hopkins University, The" ~ "JHU",
  #                                 primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
  #                                 primepartner == "PAKACHERE INSTITUTE OF HEALTH AND DEVELOPMENT COMMUNICATION" ~ "Pakachere",
  #                                 primepartner == "WITS HEALTH CONSORTIUM (PTY) LTD" ~ "Wits",
  #                                 TRUE ~ str_to_title(primepartner)),
  #          grp = factor(grp, c("Top 80%", "Next 15%", "Remaining 5%")),
  #          mech_lab = case_when(vls_mech < .9 ~ glue("{clean_name})")))
  # 
  # df_usaid <- df_vls %>% 
  #   filter(type == "national") %>%
  #   summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
  #   mutate(vlc = tx_pvls_d/tx_curr_lag2,
  #          vls = tx_pvls/tx_pvls_d,
  #          vls_alt = tx_pvls/tx_curr_lag2,
  #          vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
  #          vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  # 
  # df_viz %>% 
  #   ggplot(aes(vls_mech, fct_reorder(operatingunit, vls_nat, na.rm = TRUE))) +
  #   geom_blank() +
  #   annotate("rect",
  #            xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
  #            fill = trolley_grey_light, alpha = .4) +
  #   geom_vline(xintercept = .9, linetype = "dashed") +
  #   geom_point(aes(size = tx_curr, 
  #                  color = fill_color), alpha = .6,
  #              position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
  #   geom_text_repel(aes(label = mech_lab), na.rm = TRUE, max.overlaps = 30,
  #                   family = "Source Sans Pro", color = "#505050", size = 9/.pt) +
  #   geom_errorbar(aes(xmin = vls_nat, xmax = vls_nat), size = 1.1, color = grey60k) +
  #   scale_x_continuous(label = percent_format(1)) +
  #   facet_grid(grp ~ ., scale = "free_y", space = "free") +
  #   scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M"),
  #              range = c(2,10)) +
  #   scale_color_identity() +
  #   coord_cartesian(clip = "off") +
  #   expand_limits(x = .75) +
  #   labs(y = NULL, x = "Viral Load Supression Rate (TX_PVLS/TX_PVLS_D)",
  #        # title = glue("While USAID is at {percent(df_usaid$vls, 1)} Viral Load Suppression in {pd}, a number of implementating mechanism fall short of the 90% goal") %>% toupper() %>% str_wrap(),
  #        size = glue("Current on Treatment ({pd})"),
  #        
  #        caption = glue("Source: {msd_source}
  #                       SI Analytics: {paste0(authors, collapse = '/')}
  #                       US Agency for International Development")) +
  #   si_style(facet_space = .5) +
  #   theme(#legend.position = "none",
  #     axis.text.y = element_text(size = 9),
  #     strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))
  # 
  # si_save("Graphics/FY21Q3_USAID_VLS-partners.svg")  
  # 
  # 
  # df_viz %>% 
  #   filter(type == "mech") %>% 
  #   count(vls < .9)
  
# MUNGE SITE ADJUSTMENT ---------------------------------------------------

  df_vls_adj <- df_siteadj %>% 
    filter(fundingagency == "USAID",
           period == pd)
  
  df_vls_adj <- df_vls_adj %>% 
    bind_rows(df_vls_adj %>% 
                mutate(across(c(mech_code, mech_name, primepartner), ~ "National"))) %>% 
    mutate(type = ifelse(mech_code == "National", "national", "mech")) %>% 
    group_by(operatingunit, type, mech_code, mech_name, primepartner) %>% 
    summarise(across(c(tx_curr, tx_curr_lag2 = tx_curr_lag2_site, tx_pvls, tx_pvls_d), 
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    filter(tx_pvls_d > 0)
  
  
  #calc VLC/S
  df_vls_adj <- df_vls_adj %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  
  #identify where 80% of TX_CURR is for viz facet
  df_ctry_grps_adj <- df_vls_adj %>% 
    filter(mech_code == "National") %>% 
    count(operatingunit, wt = tx_curr) %>% 
    arrange(desc(n)) %>% 
    mutate(cumsum = cumsum(n),
           share = cumsum/sum(n),
           grp = case_when(share < .83 ~ "Top 80%",
                           share < .95 ~ "Next 15%",
                           TRUE ~ "Remaining 5%")) %>% 
    select(operatingunit, grp)
  
  

# VIZ - ADJ ---------------------------------------------------------------


  #viz dataframe
  df_viz_adj <- df_vls_adj %>% 
    left_join(df_ctry_grps_adj, by = "operatingunit") %>% 
    mutate(vls_mech = case_when(type != "national" ~ vls),
           vls_nat = case_when(type == "national" ~ vls),
           fill_color = ifelse(vls_mech > .9, scooter, moody_blue),
           operatingunit = recode(operatingunit, 
                                "Democratic Republic of the Congo" = "DRC",
                                "Papua New Guinea" = "PNG",
                                "Dominican Republic" = "DR"),
           clean_name = case_when(str_detect(mech_name, "EpiC") ~ "EpiC",
                                  primepartner == "Abt Associates Inc." ~ "Abt",
                                  str_detect(primepartner, "(Family Health International|Fhi)") ~ "FHI360",
                                  str_detect(primepartner, "INTRAHEALTH") ~ "IntraHealth",
                                  str_detect(primepartner, "HEALTH THROUGH") ~ "Health Through Walls",
                                  primepartner == "Caris Foundation International" ~ "Caris Foundation",
                                  primepartner == "Johns Hopkins University, The" ~ "JHU",
                                  primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                  primepartner == "PAKACHERE INSTITUTE OF HEALTH AND DEVELOPMENT COMMUNICATION" ~ "Pakachere",
                                  primepartner == "WITS HEALTH CONSORTIUM (PTY) LTD" ~ "Wits",
                                  TRUE ~ str_to_title(primepartner)),
           grp = factor(grp, c("Top 80%", "Next 15%", "Remaining 5%")),
           mech_lab = case_when(vls_mech < .9 ~ glue("{clean_name}")))
  
  df_usaid_adj <- df_vls_adj %>% 
    filter(type == "national") %>%
    summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  df_viz_adj %>% 
    ggplot(aes(vls_mech, fct_reorder(operatingunit, vls_nat, na.rm = TRUE))) +
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
         # title = glue("While USAID is a<t {percent(df_usaid_adj$vls, 1)} Viral Load Suppression in {pd}, a number of implementating mechanism fall short of the 90% goal") %>% toupper() %>% str_wrap(),
         size = glue("Current on Treatment ({pd})"),
         
         caption = glue("Source: Site Adjusted DATIM Pull [2021-10-05] 
                        SI Analytics: {paste0(authors, collapse = '/')}
                        US Agency for International Development")) +
    si_style(facet_space = .5) +
    theme(#legend.position = "none",
      axis.text.y = element_text(size = 9),
      strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))  
  
  df_viz_adj %>% 
    filter(type == "mech") %>% 
    count(vls < .9)
  
  
  si_save("Graphics/FY21Q3_USAID_VLS-partners_adj.svg")
  
  
  
  
# MUNGE SITE ADJUSTMENT ---------------------------------------------------
  
  df_vlc_adj <- df_siteadj %>% 
    filter(fundingagency == "USAID",
           period == pd,
           vlc_valid == TRUE)
  
  df_vlc_adj <- df_vlc_adj %>% 
    bind_rows(df_vlc_adj %>% 
                mutate(across(c(mech_code, mech_name, primepartner), ~ "National"))) %>% 
    mutate(type = ifelse(mech_code == "National", "national", "mech")) %>% 
    group_by(operatingunit, type, mech_code, mech_name, primepartner) %>% 
    summarise(across(c(tx_curr, tx_curr_lag2 = tx_curr_lag2_site, tx_pvls, tx_pvls_d), 
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    filter(tx_pvls_d > 0)
  
  
  #calc VLC/S
  df_vlc_adj <- df_vlc_adj %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  df_usaid_vlc_adj <- df_vlc_adj %>% 
    filter(type == "national") %>%
    summarise(across(starts_with("tx"), sum, na.rm = TRUE)) %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d,
           vls_alt = tx_pvls/tx_curr_lag2,
           vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
           vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap))
  
  #viz dataframe
  df_viz_vlc <- df_vlc_adj %>% 
    left_join(df_ctry_grps_adj, by = "operatingunit") %>% 
    mutate(vls_mech = case_when(mech_code != "National" ~ vls_alt),
           vls_mech = ifelse(vls_mech > 1.01, 1.01, vls_mech),
           vls_nat = case_when(mech_code == "National" ~ vls_alt),
           vlc_mech = case_when(mech_code != "National" ~ vlc),
           vlc_mech = ifelse(vlc_mech > 1.1, 1.1, vlc_mech),
           vlc_nat = case_when(mech_code == "National" ~ vlc),
           fill_color = ifelse(vlc_mech > .9, scooter, moody_blue),
           fill_color_vls = ifelse(vls_mech > .9, scooter, moody_blue),
           operatingunit = recode(operatingunit, 
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
           mech_lab = case_when(vlc_mech < .9 ~ glue("{clean_name})")))
  
  df_viz_vlc %>% 
    ggplot(aes(vlc_mech, fct_reorder(operatingunit, vlc_nat, na.rm = TRUE))) +
    geom_blank() +
    annotate("rect",
             xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
             fill = trolley_grey_light, alpha = .4) +
    geom_vline(xintercept = .9, linetype = "dashed") +
    geom_point(aes(size = tx_curr, 
                   color = fill_color), alpha = .6,
               position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
    # geom_text_repel(aes(label = mech_lab), na.rm = TRUE, max.overlaps = 30,
    #                 family = "Source Sans Pro", color = "#505050", size = 9/.pt) +
    geom_errorbar(aes(xmin = vlc_nat, xmax = vlc_nat), size = 1.1, color = burnt_sienna) + #grey60k
    scale_x_continuous(label = percent_format(1)) +
    facet_grid(grp ~ ., scale = "free_y", space = "free") +
    scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M"),
               range = c(2,10)) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    expand_limits(x = .75) +
    labs(y = NULL, x = "Viral Load Coverage Rate (TX_PVLS_D/TX_CURR [2 Qtrs Prior])",
         # title = glue("While USAID is at {percent(df_usaid_adj$vls, 1)} Viral Load Suppression in {pd}, a number of implementating mechanism fall short of the 90% goal") %>% toupper() %>% str_wrap(),
         size = glue("Current on Treatment ({pd})"),
         
         caption = glue("Source: Site Adjusted DATIM Pull [2021-10-05] 
                        Note: Adjusted site TX_CURR data account for site transitions & exclude non-eligible VLC calculations 
                        SI Analytics: {paste0(authors, collapse = '/')} | US Agency for International Development")) +
    si_style(facet_space = .5) +
    theme(legend.position = "none",
      axis.text.y = element_text(size = 9),
      strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))  
  
  
  si_save("Graphics/FY21Q3_USAID_VLC-partners_adj.svg")
  
  
  
  
  

  
  df_viz_vlc %>% 
    ggplot(aes(vls_mech, fct_reorder(operatingunit, vls_nat, na.rm = TRUE))) +
    geom_blank() +
    annotate("rect",
             xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
             fill = trolley_grey_light, alpha = .4) +
    geom_vline(xintercept = .9, linetype = "dashed") +
    geom_point(aes(size = tx_curr, 
                   color = fill_color_vls), alpha = .6,
               position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
    # geom_text_repel(aes(label = mech_lab), na.rm = TRUE, max.overlaps = 30,
    #                 family = "Source Sans Pro", color = "#505050", size = 9/.pt) +
    geom_errorbar(aes(xmin = vls_nat, xmax = vls_nat), size = 1.1, color = burnt_sienna) + #grey60k
    scale_x_continuous(label = percent_format(1)) +
    facet_grid(grp ~ ., scale = "free_y", space = "free") +
    scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M"),
               range = c(2,10)) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    expand_limits(x = .75) +
    labs(y = NULL, x = "Viral Load Suppression Rate (TX_PVLS_N/TX_CURR [2 Qtrs Prior])",
         # title = glue("WHILE THE CONVENTIONAL VIRAL LOAD SUPPRESSION (VLS) RATE WAS {percent(df_usaid_adj$vls, 1)} IN {pd},
         #              USAID ONLY HAD {percent(df_usaid_adj$vls_alt, 1)} OF TREATMENT PATIENTS VIRALLY SUPPRESSED"),
         size = glue("Current on Treatment ({pd})"),
         
         caption = glue("Note: Adjusted site TX_CURR data account for site transitions & exclude non-eligible VLC calculations 
                        Source: Site Adjusted DATIM Pull [2021-12-15] | USAID SI Analytics | Calls with IPs/OUs Dec 2021")) +
    si_style(facet_space = .5) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 9),
          strip.text.y = element_text(hjust = .5, family = "Source Sans Pro SemiBold"))
  
  
  si_save("Graphics/FY21Q4_USAID_VLS-partners_adj.svg", height = 4.25)
  