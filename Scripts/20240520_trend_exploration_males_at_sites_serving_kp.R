# AUTHOR:   B. Betz | USAID
# PURPOSE:  Explore trends where KP disaggs are not reported
# REF ID:   fb701fd5 
# LICENSE:  MIT
# DATE:     2024-06-04
# RELATED: https://github.com/USAID-OHA-SI/catch-22/blob/main/Scripts/20240515_KP_disagg_trend_exploration.R
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(stringr)
  library(janitor)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
  filepath <- str_c(si_path(), "/site-level/") %>% return_latest()
  metadata <- get_metadata(filepath) 
  
  ref_id <- "fb701fd5"
  
  indicator_list <- c("PrEP_NEW", "HTS_TST")
  kp_list <- c("MSM", "TG")
  kps <- str_c(kp_list, collapse = " & ")
  exclude_young <- c("01-04", "05-09", "01-09", "10-14", "<01", "<15")

  ## define functions ----
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }

  kp_setup <- function(df) {
    df_ft <- df |> 
      # filter to relevant disaggs for standard KP analysis 
      filter(str_detect(standardizeddisaggregate, "KeyPop|Total|Sex|Preg"),
             indicator %in% indicator_list) |> 
      # Recode PVLS indicators to include NumDenom and Funding Agency = CDC
      mutate(indicator = 
               recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
             funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) |> 
      # recode indicators as factor, rename fy, and create disagg variables for kp
      mutate(indicator = factor(indicator, levels = indicator_list),
             fy = fiscal_year,
             partner = prime_partner_name,
             disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
             disagg = recode(disagg, "KeyPop" = "KP"),
             tx_ml_reason = case_when(indicator=="TX_ML" ~ 
                                        str_extract(otherdisaggregate, 
                                                    "(?<=Outcome\\s-\\s).+")),
             keypop = str_extract(otherdisaggregate,
                                  "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
             keypop = recode(keypop, "People in prisons" = "Prisoners"),
             kp_prev_status = case_when(standardizeddisaggregate == "KeyPop/Status" ~ 
                                          str_extract(categoryoptioncomboname, 
                                                      "(?<=\\,\\s).+$"))
      ) |> 
      arrange(indicator)
    
    return(df_ft)
  }
  
  kp_clean <- function(df, time = NULL, focus = NULL) {
    #setup analytic prep for quarterly trend analysis
    if (!is.null(time)) {
      if (time == "fyq") {
        df2 <- df |> 
          filter(fiscal_year >= max(fiscal_year) - 1
                 # create alternative for q2 or q3 when targets from COP shift this)
          ) %>%
          # select fields relevant for quarterly analysis, excluding cumulative and targets
          select(operatingunit, country, snu1, psnu, partner, mech_code, 
                 mech_name, indicator, funding_agency, numeratordenom, 
                 disagg, otherdisaggregate, sex, ageasentered, 
                 standardizeddisaggregate, otherdisaggregate, keypop, 
                 safe_for_net_new, safe_for_vlc,
                 kp_prev_status, fy, qtr1, qtr2, qtr3, qtr4) |> 
          # recode indicator and arrange for viz PURPOSES
          mutate(indicator = factor(indicator, levels = indicator_list))|>  
          arrange(indicator) |> 
          # pivot to long format
          pivot_longer(qtr1:qtr4, names_to = "qtr", 
                       values_to = "results" ) |> 
          mutate(results = coalesce(results, 0),
                 qtr = str_replace(qtr, "qtr","Q"),
                 fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr))
        
        #setup analytic prep for fiscal year achievement, trend, and target achievement analysis
      } else if (time == "fy") {
        df2 <- df |> 
          filter(standardizeddisaggregate != "KeyPop/Status") |> 
          mutate(
            cumulative = coalesce(cumulative, 0),
            targets = coalesce(targets, 0),
          ) |> 
          select(operatingunit, country, snu1, psnu, partner, 
                 mech_code, mech_name, indicator, funding_agency, 
                 numeratordenom, disagg, standardizeddisaggregate, 
                 safe_for_net_new, safe_for_vlc, sex, ageasentered,
                 tx_ml_reason, keypop, fy, targets, cumulative) |> 
          mutate(indicator = factor(indicator, 
                                    levels = indicator_list)) |> 
          arrange(indicator)
        
        #print warning input if time is missing   
      }  else {
        stop("Invalid value for 'time'. Please specify 'quarterly' or 'annual'.")
      }
      
    } else if (!is.null(focus)) {    
      if (focus == "mmd") {
        df2 <- df |> 
          filter(str_detect(indicator,"TX_CURR(?!_Lag)"),
                 str_detect(standardizeddisaggregate, "ARVDispense|Total")) |> 
          mutate(cumulative = coalesce(cumulative, 0),
                 targets = coalesce(targets, 0),
                 partner = prime_partner_name,
                 fy = fiscal_year,
                 arv = str_extract(otherdisaggregate, "(?<=-\\s).+")) |> 
          select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, standardizeddisaggregate, arv, otherdisaggregate, fy, targets, cumulative)
      } else if (focus == "modality") {
        df2 <- df |> 
          filter(str_detect(standardizeddisaggregate, "KeyPop|Total") == FALSE,
                 str_detect(indicator, "HTS_TST") == TRUE
          ) %>%
          pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
          mutate(results = coalesce(results, 0),
                 qtr = str_replace(qtr, "qtr","Q"),
                 fy = fiscal_year,
                 fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr),
                 age = trendscoarse) %>%
          select(operatingunit, country, snu1, psnu, prime_partner_name, mech_code, mech_name, indicator, funding_agency, numeratordenom,
                 standardizeddisaggregate, modality, fy, fyq, results, age, ageasentered, sex)
      }  else {
        stop("Invalid value for 'focus'. Please set 'mmd' or 'modality' for focus")
      }
    } else {
      stop("At least one of 'time' and 'focus' must be specified.")
      
    }
    
    return(df2)
  }
  
  
# IMPORT ------------------------------------------------------------------
  
  df <- read_psd(filepath)
  

# MUNGE -------------------------------------------------------------------
  dfw <- df |>  
    kp_setup() |> 
    filter(!ageasentered %in% exclude_young,
           standardizeddisaggregate != "Age/Sex/ARVDispense/HIVStatus",
           indicator %in% indicator_list
    ) |> 
    mutate(indicator = factor(indicator, levels = indicator_list))  %>%
    arrange(indicator) |>
    # pivot to long format
    pivot_longer(qtr1:qtr4, names_to = "qtr", 
                 values_to = "results" )  %>%
    mutate(results = coalesce(results, 0),
           year = if_else(qtr == "qtr1", fy-1, fy),
           qtr = str_replace(qtr, "qtr","Q"),
           fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr),
           month = recode(qtr, 
                          "Q1" = "Oct",
                          "Q2" = "Jan",
                          "Q3" = "Apr",
                          "Q4" = "Jul"),
           period = str_c("01", month, as.character(year), sep = " "),
           date = as.Date(lubridate::parse_date_time(period, orders = "dmy")), #|> 
             # format("%d %b %Y"),
           # date = lubridate::parse_date_time(period, orders = "my") |> 
             # format("%b %Y"),
           # mutate(date = lubridate::parse_date_time(str_c("01 ", period), orders = "dmy")),
           # date = as.Date(paste0("01 ", df$date), format = "%d %b %Y"),
           geo = case_when(sitetype == "Above Site" ~ psnu,
                           sitetype == "Community" ~ community,
                           sitetype == "Facility" ~ facility),
           geo_uid = case_when(sitetype == "Above Site" ~ psnuuid,
                               sitetype == "Community" ~ communityuid,
                               sitetype == "Facility" ~ facilityuid),
           geo_type = sitetype) 
  
  dfw <- dfw[order(dfw$date), ]
  
  #Isolate results for MSM and TG
    tgmsm <- dfw %>%
    filter(           keypop %in% kp_list,
                      results >0) |> 
    mutate(pop = kps)
  
    #identify last date of reporting of these Key Populations
    date <-  tgmsm |> filter(year==max(year)) |>  count(date) |> pull()
  
    #isolate results for males, for primary trend analysis
  m <- dfw %>%
    filter(sex == "Male",
           results > 0
    ) 
  
  #isolate adult females, a comparison group
  f <-  dfw |> 
    filter(sex == "Female",
           results > 0
    ) |> 
    mutate(pop = "Adult Females")
  
  #identify orgunits (site, community, psnu) where MSM and TG results were reported
  geo_pre <- tgmsm |> filter(fy<= 2023) |> 
    mutate(geo = case_when(sitetype == "Above Site" ~ psnu,
                           sitetype == "Community" ~ community,
                           sitetype == "Facility" ~ facility),
           geo_uid = case_when(sitetype == "Above Site" ~ psnuuid,
                               sitetype == "Community" ~ communityuid,
                               sitetype == "Facility" ~ facilityuid),
           geo_type = sitetype) |> 
    count(geo_type, geo_uid, geo) |> select(-n, -geo)
  
  geo_pre |> count(geo_type)
  
  ## set threshold high concentrations of KP (>10%, for multiple quarters) ----
  proportion <- 0.05
  threshold <- scales::percent(proportion, accuracy = 1)
  
  #apply threshold to filter wide df to sites who meet
  msm_tg_focused_sites <- dfw %>%
    filter(           otherdisaggregate %in% kp_list | sex == "Male",
                      results >0) |>   
    inner_join(geo_pre, by = c("geo_uid", "geo_type")) |> 
    mutate(pop = if_else(is.na(sex), otherdisaggregate, sex)) |> 
    group_by(indicator, facility, facilityuid, geo_uid, date, pop) |> 
    summarize(results = sum(results, na.rm = TRUE), .groups = "drop") |> 
    pivot_wider(names_from = pop, values_from = results) |> clean_names() |>
    mutate(across(c("male","msm","tg"), ~replace_na(.,0)),
            kp_perc = (tg+msm)/male
            ) |>
    filter(kp_perc >= proportion) |>
    # count(facilityuid, geo_uid, indicator, fyq) |> 
    # filter(n>=1) |>  
    group_by(geo_uid) |> summarize()
  
  # summary(msm_tg_focused_sites$kp_perc)
  
  #keep only geographies that are historically focused on KP groups, in line with the proportion of clients set above
  geo <- geo_pre |> inner_join(msm_tg_focused_sites)
  
  #
  kp_orgunits <- m |> 
    inner_join(geo, by = c("geo_uid", "geo_type")) |> 
    mutate(pop = "Adult Males reported by facilities that historically reported MSM & TG results") 
  
  m_nonkp_orguits <- m |> 
    anti_join(geo, by = c("geo_uid", "geo_type"))|> 
    mutate(pop = "Adult Males reported by facilities that did NOT historically report MSM & TG")
  
  
  ## define analytic frame -----------
  af <- kp_orgunits |> 
    # filter(kp_focused_facility == TRUE) |>
    # mutate(pop = pop2) |> 
    bind_rows(f, m_nonkp_orguits, tgmsm) |> 
    group_by(country, indicator, pop, period, fy, fyq, date) |> 
    summarise(value = sum(results, na.rm = TRUE), .groups = "drop") |> 
    filter(value > 0) |> 
    mutate(
      bar_color = case_when(pop == "Adult Males reported by facilities that historically reported MSM & TG results" ~ "dark grey",
                            TRUE ~ "light grey"),
      point_color = case_when(pop == "Adult Males reported by facilities that historically reported MSM & TG results" ~ "grey"),
      
      qtr = as.double(str_extract(fyq, "[1-4]$"))  ) |> 
    group_by(indicator, pop) |>
    mutate(lag_value = lag(value, 1, order_by = fyq)) |>
    ungroup() |> 
    arrange(indicator, pop, fyq) |> 
    mutate(pct_change = (value - lag_value ) / lag_value,
           pct_previous = value / lag_value) |> 
    filter(!pop == "MSM & TG") |> 
    mutate(full_lab = 
             glue("{clean_number(value)}\n ({scales::percent(pct_change, 1)})")) |> 
    filter(!is.na(lag_value),
           fy >= 2023 | fyq == "FY22 Q4") |> 
    print()
  

  # Visualize -------------------------------------------------------------------
  custom_colors <- c("dark_grey" = "#5d5e60", "light grey" = trolley_grey_light)
  dates <- af |> count(date) |> pull(date)
  
  plot <- af  %>% 
    ggplot(aes(x = date, y = pct_previous, color = bar_color,
               group = pop )) +
    geom_line() +
    # geom_line(data = . %>% filter(pop == "MSM & TG"), size = 2, color = burnt_sienna_light) +
    geom_point(data = . %>% filter(pop == "Adult Males reported by facilities that historically reported MSM & TG results"), 
               aes(fill = "#5d5e60"), size = 1.5, alpha = 1) +
    facet_grid(rows = vars(indicator), 
               # scales = "free_y", 
               switch = "y") +
    geom_text(data = . %>% filter(pop == "Adult Males reported by facilities that historically reported MSM & TG results"), aes(label = full_lab), 
              # vjust = -0.5
    ) +
    # geom_text(data = . %>% filter(pop == "Adult Males reported by facilities that historically reported MSM & TG results"), aes(label = scales::percent(pct_change, accuracy = 1)), vjust = 1.5) +
    # geom_text(data = . %>% filter(pop == "MSM & TG"), aes(label = scales::comma(value)), vjust = 1, color = burnt_sienna) +
      scale_x_date(date_labels = "%b %Y", breaks = dates) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale()), 
                       # limits = c(0.65, 1.35)
    ) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    si_style_xline() +
    labs(x = NULL, y = NULL,
         title = glue::glue("Trends across clinical indicators (percent of previous period)" %>% toupper()),
         subtitle = glue::glue("Results shown for adult males reported by facilities who historically reported MSM & TG results as more than {threshold} of their results.
       Comparisons include results among males males reported by facilities that did NOT historically meet that threshold and adult females.
                             Country of Interest: {af$country}"),
       caption = glue::glue("Source: {metadata$caption}")
       # caption = glue("
                      # Note: {metadata$country} stopped reporting on Key Population disaggregates after {date}")
    ) +
    theme(
          # axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          legend.position = "none",
          strip.text.y.left = element_text(angle=360),
          plot.caption = element_text(vjust = -1.5, size = 10),  # Center the caption and adjust text size if needed
          plot.margin = margin(t = 10, r = 10, b = 30, l = 10)  # Increase bottom margin to fit the caption
    ) +
    geom_line(aes(y=1), alpha = 0.5, color = trolley_grey_light, linetype = "dashed")
  
  plot
  
  
country <- af |> count(country) |> pull(country)
filename <- str_c("Images/", country, "/", country, proportion, ".png")

ggsave(filename, plot = last_plot(), height = 4.5*1.5, width = 8*1.5)
