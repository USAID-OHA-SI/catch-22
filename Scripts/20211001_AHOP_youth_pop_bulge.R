# PURPOSE: Youth Bulge SSA - AHOP visuals
# AUTHOR: Karishma Srikanth | SI
# LICENSE: MIT
# DATE: 2021-10-01
# NOTES: 

# DEPENDENCIES & GLOBALS ------------------------------------------------------------------

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(tidytext)
    library(here)
    library(readxl)
    library(janitor)
    library(ggrepel)
   

  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
    # load fonts
    folderpath <- "C:/Users/STAR/Downloads/font folder"
    font_import(folderpath)
    library(extrafont) 
    
    #Globals
    file_path <- "Data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"

# IMPORT ------------------------------------------------------------------------------  

    # Read in Estimates data until 2020
    df_est <- read_xlsx(file_path,
                     sheet = "ESTIMATES",
                     skip = 16) %>% 
      clean_names()
    
    #Read in Medium variant data projections to 2050
    df_variant <- read_xlsx(file_path,
                        sheet = "MEDIUM VARIANT",
                        skip = 16) %>% 
      clean_names()
    
    #Read in population by age / sex data for population pyramid
    df_pyramid_male <- read_xlsx("Data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx",
              sheet = "ESTIMATES",
              skip = 16) %>% 
      clean_names()
    
    df_pyramid_female <- read_xlsx("Data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx",
                                 sheet = "ESTIMATES",
                                 skip = 16) %>% 
      clean_names()
    
    
    #pepfar country list
    pepfar_xwalk <- pepfar_country_xwalk
    
# MUNGE ------------------------------------------------------------------------------
  
  #CLEAN FOR REGIONS VIS
    
  #function to clean and tidy the data first
  clean_pop_data <- function(df) {
    df %>% 
      rename(region_country = region_subregion_country_or_area,
             year = reference_date_as_of_1_july, 
             val_15_19 = x15_19,
             val_20_24 = x20_24) %>% 
      mutate(val_15_19 = as.numeric(val_15_19),
             val_20_24 = as.numeric(val_20_24),
             value = val_15_19 + val_20_24) #summed value for 15-24
  }
    
  #apply function to estimates and variant data   
df_est_clean <- clean_pop_data(df_est) %>% 
  select(c(variant, region_country, country_code, type, parent_code, year, val_15_19, val_20_24))
  
df_variant_clean <- clean_pop_data(df_variant) %>% 
  select(c(variant, region_country, country_code, type, parent_code, year, val_15_19, val_20_24)) %>% 
  filter(!year == 2020)

#PREP FOR REGIONS VIS
df_region_vis <- df_est_clean %>% 
  bind_rows(df_variant_clean) %>% 
  filter(type == "SDG region",
         year <= 2050) %>% 
  mutate(fill_color = ifelse(region_country == "SUB-SAHARAN AFRICA", usaid_medblue, trolley_grey),
         line_type = ifelse(year <= 2020, 'dashed', 'solid'), 
         value_mil = value*1000,
         region_country = fct_rev(region_country)) %>% 
  select(region_country, year, type, fill_color, line_type, value, value_mil)

#collapse the 2 regions in Oceania
df_region_vis$region_country <- fct_collapse(df_region_vis$region_country, 
             OCEANIA = c("AUSTRALIA/NEW ZEALAND", 'OCEANIA (EXCLUDING AUSTRALIA AND NEW ZEALAND)'))

#summarize oceania by year
df_oceania <- df_region_vis %>% 
  filter(region_country == "OCEANIA") %>% 
  group_by(region_country, year, type, fill_color, line_type) %>% 
  summarize_at(vars(value, value_mil), sum, na.rm = TRUE) %>%
  ungroup() 
  
#bind oceania back
df_region_vis <- df_region_vis %>% 
  filter(region_country != "OCEANIA") %>% 
  bind_rows(df_oceania) 

  #CLEAN FOR POP PYRAMID VIS

df_pyramid_clean_m <- df_pyramid_male %>% 
  rename(region_country = region_subregion_country_or_area,
         year = reference_date_as_of_1_july) %>% 
  pivot_longer(c(x0_4:x100), names_to = "age", values_to = "value") %>% 
  mutate(value = as.numeric(value),
         sex = "male",
         age = str_remove(age, "x")) %>% 
  select(-c(index, notes, parent_code)) %>% 
  filter(year == 2020)

#PREP FOR PYRAMID VIS
df_pyramid_clean_f <- df_pyramid_female %>% 
  rename(region_country = region_subregion_country_or_area,
         year = reference_date_as_of_1_july) %>% 
  pivot_longer(c(x0_4:x100), names_to = "age", values_to = "value") %>% 
  mutate(value = as.numeric(value),
         sex = "female",
         age = str_remove(age, "x")) %>% 
  select(-c(index, notes, parent_code)) %>% 
  filter(year == 2020)

df_pyramid_vis <- df_pyramid_clean_m %>% 
  bind_rows(df_pyramid_clean_f) %>% 
  filter(region_country == "SUB-SAHARAN AFRICA") %>% 
  mutate(fill_color = ifelse(sex == "male", usaid_lightblue, usaid_medblue),
         age = str_replace(age, "_", "-"),
         total_pop = sum(value), 
         pct_pop = value/total_pop)

#reorder age factor levels
df_pyramid_vis$age <- factor(df_pyramid_vis$age, levels=c("0-4", "5-9", "10-14",
                                                          "15-19", "20-24", "25-29",
                                                          "30-34", "35-39", "40-44",
                                                          "45-49", "50-54", "55-59",
                                                          "60-64", "65-69", "70-74",
                                                          "75-79", "80-84", "85-89",
                                                          "90-94", "95-99", "100"))

# VIZ ------------------------------------------------------------------------------

#regions vis
df_region_vis %>% 
  ggplot(aes(year, value_mil, color = fill_color, group = region_country)) +
  geom_line(size = 1.2,  show.legend = FALSE) +
  geom_point(data = df_region_vis %>% filter(year %in% c(1950, 1990, 2020, 2050), region_country == "SUB-SAHARAN AFRICA"),
             size = 4) +
  geom_text(data = df_region_vis %>% filter(year %in% c(1950, 1990, 2020, 2050), region_country == "SUB-SAHARAN AFRICA"),
            aes(label = number(value_mil, scale = 1e-6, suffix = "M")),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text_repel(data = df_region_vis %>% filter(year == 2050), aes(label = region_country), family = "Source Sans Pro",
                  size = 12/.pt, hjust = -1, vjust = 1) +
  #geom_text_repel(aes(label = lab), na.rm = TRUE, family = "Source Sans Pro", size = 9/.pt, hjust = -.2) +
  coord_cartesian(clip = "off") +
  expand_limits(y = 0, x = c(1950, 2055)) +
  scale_y_continuous(label = label_number_si()) + 
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050)) +
  scale_color_identity() +
  si_style_ygrid() +
  #facet_wrap(~operatingunit, scales = "free_y") +
  labs(title = "By 2020, the youth population in Sub-Saharan Africa will have doubled from the
       start of the HIV epidemic, frm 94 million in 1990 to 218 million in 2020", 
       x = NULL, y = "Number of youths aged 5-24 years",
       caption = "Source: UN World Population Prospects, 2019
         US Agency for International Development")

si_save("Graphics/youthbulge_regions.svg")

#pop pyramid vis
df_pyramid_vis %>% 
  ggplot(aes(x = age, y = ifelse(sex == "male", -value, value),  fill = fill_color)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  geom_text(data = df_pyramid_vis %>% filter(age %in% c("0-4", "5-9", "10-14", "15-19", "20-24")),
            aes(label = percent(round(pct_pop, 3))),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  scale_fill_identity() +
  si_style_nolines()+
  scale_y_continuous(breaks = c(-75000, -50000, -25000, 0, 25000, 50000, 75000), labels = label_number_si()) +
  labs(title = "In 2021, youth aged 10-24 years made up over 30% of the population in Sub-Saharan Africa", 
       x = NULL, y = NULL,
       caption = "Source: UN World Population Prospects, 2019
         US Agency for International Development")

si_save("Graphics/SSA_pop_pyramid.svg")

#check to see the percent population breakdown- over 60 of population is under 24, 30% between 15-24
df_pyramid_vis %>% 
  filter(age %in% c("10-14", "15-19", "20-24")) %>% 
  group_by(sex) %>% 
  summarize_at(vars(pct_pop), sum, na.rm = TRUE)
  
