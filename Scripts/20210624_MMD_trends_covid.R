# PROJECT:  
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-06-24
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
  library(ISOcodes)
  library(lubridate)
  library(COVIDutilities)
  library(jsonlite)
  library(zoo)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  #Stringency Index API url - start/end date 
  ox_start <- "2020-01-01"
  ox_end <- today()
  url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
                  ox_start, ox_end, sep = "/")
  rm(ox_end, ox_start)

  #quarter starts (for viz)
  qtrs <- seq.Date(as.Date("2020-01-01"), today(), by = "3 months")


# IMPORT DATA -------------------------------------------------------------

  #MER data
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() 
  
  #MER country iso codes
  df_meta <- get_outable(datim_user(), datim_pwd()) %>%
    select(countryname, countryname_iso)
  
  #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
  json <- url_ox %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  #COVID cases (JHU)
  df_covid <- pull_jhu_covid()


# MUNGE OXFORD DATA -------------------------------------------------------
  
  #covert from json to dataframe
  df_stringency <- json %>%
    unlist() %>%
    enframe()
  
  #clean up table
  df_stringency <- df_stringency %>% 
    rowwise() %>%
    mutate(
      parts = length(unlist(str_split(name, "[.]"))),
      tbl = first(unlist(str_split(name, "[.]"))),
      tbl = gsub("\\d", "", tbl)
    ) %>%
    filter(parts == 4) %>%    # Keep the data, section with the longest parts
    separate(name,
             into = c("name", "date", "iso", "variable"),
             sep = "[.]") %>%                   # Separate column into multiple parts
    select(date:value) %>%               # Get rid of extra columns
    filter(date != value, iso != value) %>%     # Exclude repetition
    mutate(date = ymd(date), value = as.numeric(value)) %>% 
    spread(variable, value) %>% 
    select(-contains("legacy"))
  
  #add colors from FT - https://ig.ft.com/coronavirus-lockdowns/)
  df_stringency <- df_stringency %>% 
    mutate(bins = case_when(is.na(stringency)  ~ "NA",
                            stringency < 1     ~ "<1",
                            stringency < 25    ~ "1-24",
                            stringency < 50    ~ "25-49",
                            stringency < 75    ~ "50-74",
                            stringency < 85    ~ "75-84",
                            TRUE               ~ "85-100"),
           color = case_when(is.na(stringency) ~ "#D9CDC3",
                             stringency < 1    ~ "#D3E8F0",
                             stringency < 25   ~ "#FAE1AF",
                             stringency < 50   ~ "#FDAC7A",
                             stringency < 75   ~ "#F6736B",
                             stringency < 85   ~ "#DA3C6A",
                             TRUE              ~ "#A90773"
           ))
  
  #filter to PEPFAR countries
  df_stringency <- df_stringency %>% 
    filter(iso %in% df_meta$countryname_iso)
  
  #add country name
  df_stringency <- df_stringency %>% 
    left_join(df_meta, by = c("iso" = "countryname_iso"))
  
  #order colors
  df_stringency <- df_stringency %>% 
    mutate(bins = factor(bins, c("NA","<1", "1-24", "25-49", "50-74", "75-84", "85-100")),
           color = factor(color, c("#D9CDC3", "#D3E8F0","#FAE1AF", "#FDAC7A", "#F6736B", "#DA3C6A", "#A90773")))
  
  #order vars
  df_stringency <- df_stringency %>% 
    select(-c(confirmed, deaths, stringency_actual)) %>% 
    select(date, countryname, iso, everything())
  
  rm(json)
  
  
  
# MUNGE COVID DATA --------------------------------------------------------
  
  #add ISO codes
  df_covid <- ISO_3166_1 %>% 
    select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name, 
                         "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                         "Myanmar" = "Burma",
                         "Lao People's Democratic Republic" = "Laos",
                         "Tanzania, United Republic of" = "Tanzania",
                         "Viet Nam" = "Vietnam"),
           Name = ifelse(str_detect(Name, "Ivoire"), "Cote d'Ivoire", Name)) %>% 
    left_join(df_covid, ., by = c("countryname" = "Name")) %>% 
    mutate(countryname = recode(countryname, 
                                "Congo (Kinshasa)" = "Democratic Republic of the Congo"))
  
  #filter to just PEPFAR countries
  df_covid_pepfar <- df_covid %>% 
    filter(iso %in% df_meta$countryname_iso)
  
  #create a rolling average
  df_covid_pepfar <- df_covid_pepfar %>% 
    arrange(date) %>% 
    group_by(countryname) %>% 
    mutate(rollingavg_7day = rollmean(daily_cases, 7, fill = NA, align = c("right"))) %>% 
    ungroup() 
  
  #10th case
  df_tenthcase_date <- df_covid_pepfar %>% 
    filter(tenth_case == 1) %>% 
    group_by(iso) %>% 
    filter(date == min(date)) %>% 
    ungroup() %>% 
    select(iso, date, tenth_case) 
  
# IDENTIFY REGIONS --------------------------------------------------------
  
  df_meta <- df_meta %>%
    mutate(wb_region = countrycode(df_meta$countryname_iso, "iso3c", "region"),
           region = case_when(countryname == "Ukraine" ~ "Europe",
                              wb_region == "Sub-Saharan Africa" ~ "Africa",
                              wb_region == "Latin America & Caribbean" ~ "LAC",
                              TRUE ~ "Asia")) %>% 
    select(-c(wb_region, countryname_iso))
  
# MUNGE MMD ---------------------------------------------------------------

  df <- left_join(df, df_meta)
  
  df_tx <- df %>% 
    filter(region == "Africa",
           !countryname %in% c("South Africa"),
           indicator == "TX_CURR",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(cat = case_when(standardizeddisaggregate == "Total Numerator" ~ "tx_curr",
                           otherdisaggregate == "ARV Dispensing Quantity - Less than 3 months" ~ "tx_mmd_u3mo",
                           TRUE ~ "tx_mmd_o3mo")) %>% 
    group_by(countryname, fiscal_year, cat) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type)
  
  df_tx <- df_tx %>% 
    pivot_wider(names_from = cat,
                values_fill = 0) %>% 
    arrange(countryname, period) %>% 
    mutate(tx_curr_adj = ifelse(tx_curr < tx_mmd_o3mo + tx_mmd_u3mo, tx_mmd_o3mo + tx_mmd_u3mo, tx_curr),
           is_adj = tx_curr < tx_mmd_o3mo + tx_mmd_u3mo,
           tx_mmd_unk = tx_curr_adj - tx_mmd_o3mo - tx_mmd_u3mo,
           .after = tx_curr)

  df_tx <- df_tx %>% 
    pivot_longer(starts_with("tx_mmd"),
                 names_to = "mmd_type",
                 names_prefix = "tx_mmd_") %>%
    mutate(mmd_type = factor(mmd_type, c("unk", "u3mo", "o3mo")),
           share = value/tx_curr_adj)

 
  df_tx <- df_tx %>% 
    mutate(period_end = period %>% 
             str_remove("FY") %>% 
             yq(), .after = period)
    
  df_tx <- df_tx %>% 
    group_by(countryname) %>% 
    mutate(latest_tx_curr_adj = ifelse(period == max(period), tx_curr_adj, 0),
           latest_tx_curr_adj = max(latest_tx_curr_adj),
           .after = tx_curr_adj) %>% 
    ungroup() %>% 
    arrange(countryname, mmd_type, period)

  df_tx <- df_tx %>% 
    mutate(color = case_when(mmd_type == "unk" ~ trolley_grey_light,
                             mmd_type == "u3mo" ~ "white",
                             mmd_type == "o3mo" ~ genoa_light),
           color = factor(color, c(trolley_grey_light, "white", genoa_light)))

  lrg_ctry <- df_tx %>% 
    filter(period == max(period)) %>% 
    group_by(countryname) %>% 
    summarise(tx_curr = max(tx_curr)) %>% 
    ungroup() %>% 
    slice_max(order_by = tx_curr, n = 12) %>% 
    pull(countryname)

# MERGE DATA --------------------------------------------------------------

  df_stringency_viz <- df_stringency %>% 
    left_join(df_tenthcase_date) %>% 
    filter(date <= max(df_tx$period_end)) %>% 
    select(date, countryname, stringency, bins, color, tenth_case) %>% 
    mutate(placement = -.1)
  # 
  # df_early <- expand.grid(countryname = lrg_ctry,
  #                         date = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-21"), by = "day"),
  #                         color = "#D3E8F0") %>% 
  #   as_tibble() %>% 
  #   mutate(across(c(countryname, color), as.character))
  

  df_viz <- df_stringency_viz %>% 
    # bind_rows(df_early) %>% 
    tidylog::full_join(df_tx %>% 
                         select(-color, -value) %>%
                         pivot_wider(names_from = mmd_type,
                                     values_from = share) %>% 
                         rename(date = period_end)) %>%
    filter(countryname %in% lrg_ctry) %>% 
    mutate(area_unk = u3mo + o3mo + unk,
           area_mmd = u3mo + o3mo) 


# VIZ TITLE INFO ----------------------------------------------------------
 
   df_info <- df_tx %>% 
    group_by(period, mmd_type) %>% 
    summarise(across(c(tx_curr, tx_curr_adj, value), sum, na.rm =TRUE),.groups = "drop") %>% 
    mutate(share = value/tx_curr_adj) %>% 
    filter(mmd_type == "o3mo",
           period %in% c("FY20Q1", max(period)))
  
  pd <- max(df_info$period)
  
  start_share <- df_info %>% 
    filter(period == "FY20Q1") %>% 
    pull(share) %>% 
    percent()
  
  end_share <- df_info %>% 
    filter(period == max(period)) %>% 
    pull(share) %>% 
    percent()
  
  end_curr <- df_info %>% 
    filter(period == max(period)) %>% 
    pull(tx_curr) %>% 
    number(accuracy = .1, scale = 1e-6, suffix = "M")
  
  info_title <- glue("As of {pd}, {end_share} of the {end_curr} in PEPFAR's largest 12 countries were on 3 or more months of MMD, up from {start_share} before the pandemic") %>% 
    toupper()
  
# VIZ ---------------------------------------------------------------------
  
  df_viz %>% 
    ggplot(aes(date)) +
    geom_area(aes(y = area_unk), fill = trolley_grey_light, alpha = .6, na.rm = TRUE) +
    geom_area(aes(y = area_mmd), fill = "white", na.rm = TRUE) +
    geom_area(aes(y = o3mo), color = genoa, fill = genoa_light, alpha = .6, size = 1, na.rm = TRUE) +
    geom_col(aes(y = tenth_case), fill = "#505050", alpha = .6, na.rm = TRUE) +
    geom_vline(xintercept = qtrs, color = "white", linetype = "dashed") +
    geom_col(aes(y = placement, fill = color), na.rm = TRUE) +
    geom_hline(yintercept = -.01, size = 2.2, color = "white") +
    geom_hline(yintercept = c(0, .25, .5, .75, 1), 
               linetype = "dotted", alpha = .5) +
    facet_wrap(~fct_reorder(countryname, latest_tx_curr_adj, max, na.rm = TRUE, .desc = TRUE)) +
    scale_fill_identity() +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b %y", 
                 breaks = qtrs) +
    labs(x = NULL, y = NULL,
         title = str_wrap(info_title, 96),
         caption = "Sources: PEPFAR FY21Q1 MSD, JHU COVID-19 feed + stringecy index from Blavatnik School of Government at Oxford University,
         SI analytics: Aaron Chafetz
         US Agency for International Development") +
    si_style_nolines() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))

  si_save("Graphics/MMD_COVID_Large_Countries.svg")  


  
  df_viz_scatter <- df_tx %>% 
    filter(mmd_type == "o3mo") %>% 
    mutate(color = ifelse(period_end <="2020-04-01", genoa, genoa_light),
           pre = case_when(period_end <= "2020-04-01" ~ share),
           post = case_when(period_end >= "2020-04-01" ~ share),
           group = ifelse(latest_tx_curr_adj > 1e6, "Large Countries (+1m TX_CURR)", "Small Countries")) 
  
  set.seed(42)
  v_scatter <- df_viz_scatter %>% 
    ggplot(aes(period_end, share)) +
    geom_rect(aes(xmin = as.Date("2019-12-15"), xmax = as.Date("2020-04-15"), 
                  ymin = 0, ymax = 1), 
              fill = grey10k, alpha = 0.05) +
    geom_vline(xintercept = who_pandemic() %>% pull(date),
               color = "#909090", linetype = "dashed") +
    geom_jitter(aes(size = tx_curr_adj, color = color), width = 10, alpha = .8) +
    geom_smooth(aes(y = pre, weight = tx_curr_adj), se = FALSE, na.rm = TRUE,
                method='lm', formula= y~x, color = genoa) +
    geom_smooth(aes(y = post, weight = tx_curr_adj), se = FALSE, na.rm = TRUE,
                method='lm', formula= y~x, color = genoa_light) +
    # facet_wrap(~group) +
    # ggrepel::geom_label_repel(aes(label = countryname)) +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    scale_x_date(date_labels = "%b %y", breaks = qtrs, expand = c(.005, .005)) +
    scale_size(label = comma) +
    labs(x = NULL, y = NULL, size = NULL,
         subtitle = "MMD +3 months") +
    si_style_ygrid() +
    theme(legend.position = "none")
  
  
  df_viz_cases <- df_covid_pepfar %>% 
    filter(countryname %in% unique(df_viz_scatter$countryname),
           date <= max(df_viz_scatter$period_end)) %>%
    group_by(date) %>% 
    summarise(daily_cases = sum(daily_cases, na.rm = TRUE), .groups = "drop") %>% 
    mutate(seven_day = rollmean(daily_cases, 7, fill = NA, align = c("right")))

  v_cases <- df_viz_cases %>% 
    ggplot(aes(date, daily_cases)) +
    geom_col(fill = old_rose_light, alpha = .2, na.rm = TRUE) +
    geom_path(aes(y = seven_day), color = old_rose, size = 1, na.rm = TRUE) +
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%b %y", breaks = qtrs) +
    labs(x = NULL, y = NULL,
         subtitle = "Daily COVID Case Counts") +
    si_style_ygrid() 
  
  
  v_scatter / v_cases + plot_layout(heights = c(2, 1)) +
    plot_annotation(title = "RELATIVELY FLAT TREND LINE IN +3 MONTHS MMD AFTER FY20Q3",
                    caption = "Sources: PEPFAR FY21Q1 MSD, JHU COVID-19 feed
         SI analytics: Aaron Chafetz/Tim Essam
         US Agency for International Development") &
    theme(plot.title = element_text(family = "Source Sans Pro",
                                    size = 14,
                                    face = "bold",
                                    color =  "#202020",
                                    hjust = 0),
          plot.caption = element_text(family = "Source Sans Pro",
                                      size = 9,
                                      color = "#909090",
                                      hjust = 1, vjust = 1))
  
  
  si_save("Graphics/MMD_COVID_Scatter_AFR.svg")  
  