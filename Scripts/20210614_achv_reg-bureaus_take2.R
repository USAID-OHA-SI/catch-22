# PROJECT:  catch22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  take II on viz for bureau meetings
# LICENSE:  MIT
# DATE:     2021-06-14
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(countrycode)
  # library(fontawesome)
  # library(emojifont)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()


  ind_sel <- c("PrEP_NEW", "OVC_SERV","HTS_TST_POS", "TX_NEW", "TX_CURR")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  # df_hist <- si_path() %>% 
  #   return_latest("OU_IM_FY15") %>% 
  #   read_rds()  
  
  df_nat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds() 
  
  df_meta <- get_outable(datim_user(), datim_pwd()) %>%
    select(countryname, countryname_iso)
  

# MUNGE -------------------------------------------------------------------
  
  curr_fy <- identifypd(df, "year")
  curr_pd <- identifypd(df, "quarter")
    
  df_meta <- df_meta %>%
    mutate(wb_region = countrycode(df_meta$countryname_iso, "iso3c", "region"),
           usaid_region = case_when(countryname == "Ukraine" ~ "Europe",
                                    wb_region == "Sub-Saharan Africa" ~ "Africa",
                                    wb_region == "Latin America & Caribbean" ~ "LAC",
                                    TRUE ~ "Asia")) %>% 
    select(-c(wb_region, countryname_iso))
  
  df_msd <- df %>% 
    filter(fundingagency == "USAID",
           indicator %in% ind_sel,
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, countryname, indicator) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    calc_achievement()
    
  df_plhiv <- df_nat %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in%  c(2019:2021)) %>%
    group_by(countryname, plhiv_fy = fiscal_year) %>% 
    summarise(plhiv = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(countryname) %>%
    filter(plhiv_fy == max(plhiv_fy)) %>%
    ungroup()  
  
  df_full <- df_msd %>% 
    left_join(df_meta, by = "countryname") %>% 
    left_join(df_plhiv, by = "countryname")
  

# ACHIEVEMNT VIZ ----------------------------------------------------------

  df_viz <- df_full %>% 
    mutate(curr_qtr = ifelse(fiscal_year == curr_fy, curr_pd, 4),
           qtr_goal = ifelse(indicator %in% snapshot_ind, 1, 1*(curr_qtr/4)),
           achv_label = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ glue("<{100*(qtr_goal-.25)}%") %>% as.character,
                                  achievement <= qtr_goal-.1 ~ glue("{100*(qtr_goal-.25)}-{100*(qtr_goal-.11)}%") %>% as.character,
                                  achievement <= qtr_goal+.1 ~ glue("{100*(qtr_goal-.1)}-{100*(qtr_goal+.1)}%") %>% as.character,
                                  TRUE ~ glue("+{100*(qtr_goal+.1)}%") %>% as.character),
           achv_color = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ old_rose_light,
                                  achievement <= qtr_goal-.1 ~ burnt_sienna_light,
                                  achievement <= qtr_goal+.1 ~ "#5BB5D5",
                                  TRUE ~ trolley_grey_light),
           # achv_icon = case_when(achievement <= qtr_goal-.25 ~ glue("{fontawesome('fa-exclamation-circle')}"),
           #                       achievement <= qtr_goal-.1 ~ glue("{fontawesome('fa-minus-circle')}"),
           #                       achievement <= qtr_goal+.1 ~ glue("{fontawesome('fa-check-circle')}"),
           #                       TRUE ~ glue("{fontawesome('fa-plus-circle')}")),
           achv_alpha = ifelse(achievement <= qtr_goal+.1, .75, 1))

  
  df_viz <- df_viz %>% 
    filter(!is.na(achievement)) %>% 
    mutate(fy = glue("FY{str_sub(fiscal_year,3,4)}"),
           indicator = factor(indicator, ind_sel),
           plhiv = ifelse(is.na(plhiv), 0, plhiv)) %>% 
    group_by(usaid_region) %>% 
    mutate(max_plhiv = plhiv == max(plhiv, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(ctry_lab = case_when(max_plhiv == TRUE ~ glue("{countryname}<br><span style = 'font-size:8pt;color:{trolley_grey}'>*PLHIV = {comma(plhiv, 1)}*</span>"),
                                plhiv == 0 ~ glue("{countryname}<br><span style = 'font-size:8pt;color:{trolley_grey}'>*No data*</span>"),
                                TRUE ~ glue("{countryname}<br><span style = 'font-size:8pt;color:{trolley_grey}'>*{comma(plhiv, 1)}*</span>")))

 
  
  df_viz %>% 
    filter(usaid_region == "LAC") %>% 
    ggplot(aes(fy, fct_reorder(ctry_lab, plhiv), color = achv_color, alpha = achv_alpha)) +
    geom_point(size = 5) +
    geom_text(aes(label = round(achievement*100, 0)), family = "Source Sans Pro",
              size = 6/.pt, color = "#6d6e71") +
    # geom_text(aes(label = achv_icon), family='fontawesome-webfont', size=6) +
    facet_grid(~indicator) +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_x_discrete(position = "top") +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    theme(panel.spacing.x = unit(6,"pt"),
          strip.placement = "outside",
          # strip.background = element_rect(fill = NA),
          # panel.grid = element_blank(),
          # axis.ticks = element_blank(),
          # panel.background = element_blank(),
          axis.text.y = element_markdown()
          )

  # si_save("Images/ind_achv.png", width = 5, height = 5.625)
  si_save("Graphics/ind_achv.svg", width = 5, height = 5.625)



# MMD ---------------------------------------------------------------------

  df_tx <- df %>% 
    filter(fundingagency == "USAID",
           fiscal_year >= 2020,
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(months = case_when(standardizeddisaggregate == "Total Numerator" ~ "total",
                              otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months",
                                                       "ARV Dispensing Quantity - 6 or more months") ~ "o3mo",
                              TRUE ~ "u3mo")) %>% 
    group_by(fiscal_year, countryname, indicator, months) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd()
  
  df_tx <- df_tx %>% 
    spread(months, value, fill = 0) %>% 
    left_join(df_meta)

  df_tx_agg <- df_tx %>% 
    group_by(period, usaid_region) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(share_o3mo = o3mo/total,
           share_label = case_when(period %in% c(max(period), min(period)) ~ share_o3mo))


# VLC ---------------------------------------------------------------------

  df_vl <- df %>% 
    filter(fundingagency == "USAID",
           (indicator == "TX_CURR" & standardizeddisaggregate == "Total Numerator") |
             (indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator")) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, countryname, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    spread(indicator, value)

  df_vl_agg <- df_vl %>% 
    left_join(df_meta) %>% 
    group_by(usaid_region, period) %>% 
    summarise(across(where(is.double), sum, na.rm =T), .groups = "drop")
  
  df_vl_agg <- df_vl_agg %>% 
    arrange(usaid_region, period) %>% 
    group_by(usaid_region) %>% 
    mutate(VLC = TX_PVLS_D/lag(TX_CURR, n = 2)) %>% 
    ungroup() %>% 
    filter(str_detect(period, "FY19", negate = TRUE)) %>% 
    mutate(share_label = case_when(period %in% c(max(period), min(period)) ~ VLC))

 

# INDEX -------------------------------------------------------------------

  df_index <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result")) %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "index", "other")) %>%
    group_by(fiscal_year, countryname, mod_type) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd()

  df_index_agg <- df_index %>% 
    left_join(df_meta) %>% 
    group_by(usaid_region, period, mod_type) %>% 
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
    group_by(usaid_region, period) %>% 
    mutate(index_share = value/sum(value)) %>% 
    ungroup() %>% 
    filter(mod_type == "index",
           str_detect(period, "FY19", negate = TRUE)) %>% 
    mutate(share_label = case_when(period %in% c(max(period), min(period)) ~ index_share))
    

# NET NEW -----------------------------------------------------------------

 df_nn <- df %>% 
   filter(fundingagency == "USAID",
          indicator == "TX_NET_NEW",
          standardizeddisaggregate == "Total Numerator") %>% 
   group_by(fiscal_year, countryname, indicator) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
   reshape_msd()
 
 df_nn_agg <- df_nn %>% 
   left_join(df_meta) %>% 
   group_by(usaid_region, period) %>% 
   summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
   filter(str_detect(period, "FY19", negate = TRUE)) %>% 
   mutate(val_label = case_when(period %in% c(max(period), min(period)) ~ value))
 

 

# PLOT FUNCTION -----------------------------------------------------------

bureau_plot <- function(bureau){
  
  # v1 <-  df_index_agg %>% 
  #   filter(usaid_region == "LAC") %>% 
  #   ggplot(aes(period, index_share, group = usaid_region)) +
  #   geom_area(alpha  = .6, fill = denim, color = denim, size = 1) +
  #   geom_text(aes(label = percent(share_label, 1)), family = "Source Sans Pro",
  #             size = 10/.pt, vjust = -.5, color = trolley_grey, na.rm = TRUE) +
  #   expand_limits(y = 1) +
  #   scale_y_continuous(label = percent) +
  #   labs(x = NULL, y = NULL,
  #        subtitle = "Share of HIV+ Identified Through Index Testing") +
  #   si_style_ygrid()
  
  v1_b <- df_nn_agg %>% 
    filter(usaid_region == {{bureau}}) %>% 
    ggplot(aes(period, value, group = usaid_region)) +
    geom_col(fill = "white") +
    geom_col(alpha  = .6, fill = denim) +
    geom_text(aes(label = comma(val_label)), family = "Source Sans Pro",
              size = 10/.pt, vjust = -.5, color = trolley_grey, na.rm = TRUE) +
    expand_limits(y = 1) +
    scale_y_continuous(label = comma) +
    labs(x = NULL, y = NULL,
         subtitle = "TX_NET_NEW Results") +
    si_style_ygrid()
  
  v2 <- df_tx_agg %>% 
    filter(usaid_region == {{bureau}}) %>% 
    ggplot(aes(period, share_o3mo, group = usaid_region)) +
    geom_area(alpha  = .6, fill = scooter, color = scooter) +
    geom_text(aes(label = percent(share_label)), family = "Source Sans Pro",
              size = 10/.pt, vjust = -.5, color = trolley_grey, na.rm = TRUE) +
    expand_limits(y = 1) +
    scale_y_continuous(label = percent) +
    labs(x = NULL, y = NULL,
         subtitle = "Share of +3 Months of ART Dispensed") +
    si_style_ygrid()
  
  v3 <- df_vl_agg %>% 
    filter(usaid_region == {{bureau}}) %>% 
    ggplot(aes(period, VLC, group = usaid_region)) +
    geom_area(alpha  = .6, fill = moody_blue, color = moody_blue, size = 1) +
    geom_text(aes(label = percent(share_label)), family = "Source Sans Pro",
              size = 10/.pt, vjust = -.5, color = trolley_grey, na.rm = TRUE) +
    expand_limits(y = 1) +
    scale_y_continuous(label = percent) +
    labs(x = NULL, y = NULL,
         subtitle = "Share of Viral Load Coverage") +
    si_style_ygrid()
  
  plot_title <- glue("USAID SUPPORT ACROSS {toupper(bureau)} BUREAU COUNTRIES")
  
  v1_b/v2/v3 + plot_annotation(
    title = str_wrap(plot_title, 95),
    caption = glue("Source: FY21Q2i MSD
                      SI analytics: Aaron Chafetz
                     US Agency for International Development")) & 
    theme(plot.title = element_text(family = "Source Sans Pro",
                                    size = 14,
                                    face = "bold",
                                    color =  "#202020",
                                    hjust = 0),
          plot.caption = element_text(family = "Source Sans Pro",
                                      size = 9,
                                      color = "#909090",
                                      hjust = 1, vjust = 1))
  
  si_save(glue("FY21Q1_bureau-trends_{bureau}.svg"), path = "Graphics", width = 4.089, height = 5.625)
}

walk(unique(df_meta$usaid_region), bureau_plot)
 