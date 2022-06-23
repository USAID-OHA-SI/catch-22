# PROJECT:  catch-22
# AUTHOR:   K.Srikanth | USAID
# PURPOSE:  GH scorecard data pull on common indicators
# LICENSE:  MIT
# DATE:     2022-05-03
# UPDATED:  
# test - 2.0 (gitkraken)

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(janitor)
library(mindthegap)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------

#USAID TB priority countries: https://www.usaid.gov/global-health/health-areas/tuberculosis/countries#tbMapLink
usaid_tb <- c("Afghanistan", "India", "Nigeria", "Ukraine",
              "Bangladesh",	"Indonesia",	"Philippines",	"Uzbekistan",
              "Burma",	"Kenya", "South Africa", "Vietnam",
              "Cambodia", "Kyrgyz Republic", "Tajikistan",	"Zambia",
              "Democratic Republic of Congo", "Malawi", "Tanzania",	"Zimbabwe",
              "Ethiopia",	"Mozambique", "Uganda")

#USAID Malaria priority countries: https://www.pmi.gov/where-we-work/
usaid_mal <- c("Angola", "Benin", "Burkina Faso", "Burma", 
               "Cambodia", "Cameroon", "Cote d'Ivoire", 
               "Democratic Republic of Congo", "Ethiopia", "Ghana",
               "Guinea", "Kenya", "Liberia", "Madagascar",
               "Malawi", "Mali", "Mozambique",
               "Niger", "Nigeria","Rwanda","Senegal","Sierra Leone",
               "Tanzania", "Thailand", "Uganda", "Zambia",
               "Zimbabwe") 

#https://www.usaid.gov/global-health/health-areas/maternal-and-child-health/priority-countries
usaid_mch <- c("Afghanistan",
               "Bangladesh",
               "Myanmar",
               "Democratic Republic of Congo",
               "Ethiopia",
               "Ghana",
               "Haiti",
               "India",
               "Indonesia",
               "Kenya",
               "Liberia",
               "Madagascar",
               "Malawi",
               "Mali",
               "Mozambique",
               "Nepal",
               "Nigeria",
               "Pakistan",
               "Rwanda",
               "Senegal",
               "South Sudan",
               "Tanzania",
               "Uganda",
               "Yemen",
               "Zambia")

#site adjusted dataset
folder_path <- "Data/"

# IMPORT ------------------------------------------------------------------

#UHC data
df_uhc <- folder_path %>% 
  return_latest("who_uhc_service_coverage_index_total") %>% 
  read_csv() %>% 
  clean_names()

df_uhc_sub1 <- folder_path %>% 
  return_latest("who_uhc_sci_subindex_service_access") %>% 
  read_csv() %>% 
  clean_names()

df_uhc_sub2 <- folder_path %>% 
  return_latest("who_uhc_sci_subindex_ncd") %>% 
  read_csv() %>% 
  clean_names()

df_uhc_sub3 <- folder_path %>% 
  return_latest("who_uhc_sci_subindex_mchn") %>% 
  read_csv() %>% 
  clean_names()

df_uhc_sub4 <- folder_path %>% 
  return_latest("who_uhc_sci_subindex_id") %>% 
  read_csv() %>% 
  clean_names()


#Under 5 Mortality 
df_under5 <- si_path() %>% 
  return_latest("under5child-mortality") %>% 
  read_csv() 

#HIV
df_tt <- pull_unaids("Test & Treat - Percent", FALSE)

#TB ---

## Incidence TB per 100,000 population
df_tb_inc <- si_path() %>% 
  return_latest("incidence-of-tuberculosis-sdgs.csv") %>% 
  read_csv() 

## Deaths per 100,000 population (all ages)
df_tb_deaths<- si_path() %>% 
  return_latest("tuberculosis-death-rates.csv") %>% 
  read_csv()  

## Incidence of malaria per 1,000 population
df_mal_inc <- si_path() %>% 
  return_latest("incidence-of-malaria-sdgs.csv") %>% 
  read_csv()  

## Malaria deaths per 100,000 population (all ages)
df_mal_deaths <- si_path() %>% 
  return_latest("malaria-death-rates.csv") %>% 
  read_csv()

#Life expectancy - WPP 2019
df_wpp <- read_xlsx("Data/WPP2019_SA1_INT_F01_ANNUAL_DEMOGRAPHIC_INDICATORS.xlsx",
                    sheet = "ESTIMATES",
                    skip = 16) %>% 
  clean_names()

#MASTER LIST - region meta data
google_id <- "14sBgteFOvPfQi5lB29334zNO_6aP4VPsVinhNTYUFa8"

region_meta <- read_sheet(google_id, sheet = 2, skip = 2) %>% 
  clean_names() %>% 
  rename(iso = iso_alpha_3)

#WHO Region
who_region <- df_uhc %>% 
  rename(iso = spatial_dim_value_code,
         country = location,
         year = period, 
         who_region = parent_location) %>% 
  select(country, iso, who_region) %>% 
  distinct() 


# FUNCTIONS -------------------------------------------------------------------

munge_uhc <- function(df) {
  
  df <- df %>% 
    select(indicator, spatial_dim_value_code, location, period, value) %>% 
    rename(iso = spatial_dim_value_code,
           country = location,
           year = period) %>% 
   # filter(year == max(year)) %>% 
    mutate(country = recode(country, "United Republic of Tanzania" = "Tanzania",
                           # "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                            "Viet Nam" = "Vietnam",
                            "Bolivia (Plurinational State of)" = "Bolivia",
                            "Myanmar" = "Burma",
                            "Lao People's Democratic Republic" = "Laos",
                            "The former Yugoslav Republic of Macedonia" = "North Macedonia",
                            "Venezuela (Bolivarian Republic of)" = "Venezuela",
                            "Côte d’Ivoire" = "Cote d'Ivoire",
                            "Czechia" = "Czech Republic",
                           "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                           "Brunei Darussalam" = "Brunei"
                            ),
      usaid = ifelse(country %in% usaid_mch, "USAID MCHN", "Non-USAID MCHN"))
  
  return(df)
}

sdg_reshape <- function(data) {
  
  data_clean <- data %>% 
    clean_names() %>%
    rename(country = entity,
           iso = code) 
  
  indic_name <- names(data_clean)[4]
  
  data_clean <- data_clean %>% 
    mutate(indicator = indic_name,
           country = recode(country, "Myanmar" = "Burma",
                                   "Timor" = "Timor-Leste",
                            "North Korea" = "Democratic People's Republic of Korea",
                            "South Korea" = "Republic of Korea",
                            "Moldova" = "Republic of Moldova",
                            "Russia" = "Russian Federation",
                            "Iran" = "Iran (Islamic Republic of)",
                            "Syria" = "Syrian Arab Republic",
                            "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                            "Cape Verde" = "Cabo Verde",
                            "Czechia" = "Czech Republic",
                            "United States" = "United States of America"),
           indicator = recode(indicator, "maternal_mortality_ratio_modeled_estimate_per_100_000_live_births" = "maternal_mortality",
                              "mortality_rate_neonatal_per_1_000_live_births" = "neonatal_mortality",
                              "mortality_rate_under_5_per_1_000_live_births" = "under5_mortality",
                              "incidence_of_tuberculosis_per_100_000_people" = "tb_incidence",
                              "deaths_tuberculosis_sex_both_age_age_standardized_rate" = "tb_deaths",
                              "incidence_of_malaria_per_1_000_population_at_risk" = "malaria_incidence",
                              "deaths_malaria_sex_both_age_age_standardized_rate" = "malaria_deaths"),
           usaid = ifelse(country %in% usaid_mch & indicator %in% c("maternal_mortality", "neonatal_mortality",
                                                                    "under5_mortality"), "USAID MCHN", "Non-USAID MCHN"))
  
  #most_recent_data <- ifelse(data_clean$indicator == "maternal_mortality", 2017, 2019)
  
  data_clean <- data_clean %>% 
    filter(
     # year == max(year),
           !is.na(iso)) %>% 
    mutate(goal = case_when(indicator == "maternal_mortality" ~ 140,
                            indicator == "neonatal_mortality" ~ 12,
                            indicator == "under5_mortality" ~ 25,
                            indicator == "tb_incidence" ~ 20,
                            indicator == "malaria_incidence" ~ 9)) 
  
  
  names(data_clean)[4] <- 'value'
  
  return(data_clean)
  
}

# MUNGE ---------------------------------------------------------------------

#Apply UHC munging function
df_uhc <- munge_uhc(df_uhc)
df_uhc_sub1 <- munge_uhc(df_uhc_sub1)
df_uhc_sub2 <- munge_uhc(df_uhc_sub2)
df_uhc_sub3 <- munge_uhc(df_uhc_sub3)
df_uhc_sub4 <- munge_uhc(df_uhc_sub4)

df_tt <- df_tt %>% 
  mutate(country = recode(country, "Democratic People Republic of Korea" = "Democratic People's Republic of Korea",
                          "Cape Verde" = "Cabo Verde",
                          "Brunei Darussalam"  = "Brunei")) %>% 
  filter(country != regions)

df_uhc_final <- df_uhc %>% 
  bind_rows(df_uhc_sub1, df_uhc_sub2, df_uhc_sub3, df_uhc_sub4) %>% 
  mutate(indicator = recode(indicator,
                            "UHC Service Coverage Index (SDG 3.8.1)" = "uhc_service_coverage_index",
                            "UHC Service Coverage sub-index on service capacity and access" = "uhc_subindex1_capacity_access",
                            "UHC Service Coverage sub-index on noncommunicable diseases" = "uhc_subindex2_ncd",
                            "UHC Service Coverage sub-index on reproductive, maternal, newborn and child health" = "uhc_subindex3_mchn",
                            "UHC Service Coverage sub-index on infectious diseases" = "uhc_subindex4_id")) %>% 
  arrange(country) %>% 
  mutate(goal = NA,
        ref_link = "https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4834",
         date_data_pulled = lubridate::today())
  
#Apply reshaping function for SDG
df_under5_clean <- sdg_reshape(df_under5) %>% 
 # select(-c(goal)) %>% 
  mutate(ref_link = "https://sdg-tracker.org/good-health",
         date_data_pulled = lubridate::today())

#TB
df_tb_clean <- sdg_reshape(df_tb_inc) %>% 
 # select(-c(goal)) %>% 
  bind_rows(sdg_reshape(df_tb_deaths)) %>% 
  mutate(usaid = ifelse(country %in% usaid_tb & indicator %in% c("tb_incidence", "tb_deaths"), "USAID TB", "Non-USAID TB"),
    ref_link = "https://sdg-tracker.org/good-health",
         date_data_pulled = lubridate::today())

#malaria
df_mal_clean <- sdg_reshape(df_mal_inc) %>%
  # select(-c(goal)) %>% 
  bind_rows(sdg_reshape(df_mal_deaths)) %>% 
  mutate(usaid = ifelse(country %in% usaid_mal & indicator %in% c("malaria_incidence", "malaria_deaths"), "USAID Malaria", "Non-USAID Malaria"),
         ref_link = "https://sdg-tracker.org/good-health",
         date_data_pulled = lubridate::today()) 


#HIV
df_vls <- df_tt %>% 
  filter(
         age == "all",
         sex == "all",
         stat == "est",
         indicator == "VLS",
         !is.na(iso)
         ) %>% 
  select(indicator, iso, country, year, value, pepfar) %>% 
  rename(cntry_group = pepfar) %>% 
  mutate(goal = 95,
         indicator = recode(indicator, "VLS"  = "VLS among PLHIV"),
    ref_link = "https://aidsinfo.unaids.org/",
         date_data_pulled = lubridate::today())

#clean up life expectancy estimates
df_wpp <- df_wpp %>% 
  rename(country = region_subregion_country_or_area,
         year = reference_date_1_january_31_december) %>% 
  select(country, country_code, type, parent_code, year, starts_with('life_expectancy_at_birth_both')) %>% 
  pivot_longer(c(starts_with("life")), names_to = "indicator", values_to = "value") %>% 
  mutate(value = as.numeric(value),
         country = recode(country, "Samoa" = "American Samoa",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "Brunei Darussalam" = "Brunei",
                          "Myanmar" = "Burma",
                          "Côte d'Ivoire" = "Cote d'Ivoire",
                          "Curaçao" = "Curacao",
                          "Czechia" = "Czech Republic",
                          "Dem. People's Republic of Korea" = "Democratic People's Republic of Korea",
                          "China, Hong Kong SAR" = "Hong Kong",
                          "Lao People's Democratic Republic" = "Laos",
                          "China, Macao SAR" = "Macao",
                          "Micronesia (Fed. States of)" = "Micronesia (country)",
                          "State of Palestine" = "Palestine",
                          "China, Taiwan Province of China" = "Taiwan",
                          "United Republic of Tanzania" = "Tanzania",
                          "Venezuela (Bolivarian Republic of)" = "Venezuela",
                          "Viet Nam" = "Vietnam"),
         indicator = recode(indicator, "life_expectancy_at_birth_both_sexes_combined_years" = "life_expectancy_at_birth_all_sexes")) %>% 
  filter(type == "Country/Area") 




#FINAL DF --------------------------------------------------------------'

#bind all the data together
df_gh_final <- df_uhc_final %>% 
  bind_rows(df_under5_clean) %>% 
  bind_rows(df_tb_clean) %>% 
  bind_rows(df_mal_clean) %>% 
  rename(cntry_group = usaid) %>% 
  bind_rows(df_vls) %>% 
  arrange(country) %>% 
  filter(country != "World")

#join ISOs for WPP
country_iso <- df_gh_final %>% count(country, iso) %>% select(-(n))

df_wpp_clean <- df_wpp %>% 
  left_join(country_iso, by = c("country")) %>% 
  mutate(iso = case_when(country == "Western Sahara" ~ "ESH",
                         TRUE ~ iso)) %>% 
  mutate(cntry_group = NA,
         goal = NA,
         ref_link = "https://population.un.org/wpp/Download/SpecialAggregates/EconomicTrading/",
         date_data_pulled = lubridate::today()) %>% 
  select(indicator, iso, country, year, value, cntry_group, ref_link, date_data_pulled)

df_gh_final <- df_gh_final %>%
  bind_rows(df_wpp_clean) %>% 
  arrange(country) %>% 
  left_join(region_meta
            %>% dplyr::select(iso, usaid_supported, usaid_region, idea_region, income_group)
            , by = "iso")

df_gh_final <- df_gh_final %>% 
  left_join(who_region
            %>% dplyr::select(iso, who_region)
            , by = "iso") 

date <- lubridate::today()
  
write_csv(df_gh_final, glue::glue("Dataout/GH_scorecard_indicators_{date}.csv"))
  
    
  
