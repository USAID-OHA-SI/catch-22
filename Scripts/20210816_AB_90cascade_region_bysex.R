## PROJECT: agitprop
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: 95-95-95 Achievement - Africa Bureau Regional Level
## NOTE: modified to create disaggregate visuals by age/sex
## LICENSE: MIT
## DATE:    2021-08-16


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(googlesheets4)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(svglite)

# GLOBAL VARIABLES --------------------------------------------------------

#creds  
load_secrets()

authors <- c("Karishma Srikanth")

#goal
goal <- 90

#regions
regions <- c("West and Central Africa",
             "Middle East and North Africa",
             "East and Southern Africa",
             "Asia and the Pacific",
             "Caribbean",
             "Eastern Europe and Central Asia",
             "Latin America",
             "Western & Central Europe and North America",
             "Global")

africa_region <- c("East and Southern Africa",
                   "West and Central Africa",
                   "Middle East and North Africa")

regions_viz <- c("West and Central Africa___Known Status",
                 "Middle East and North Africa___Known Status",
                 "East and Southern Africa___On Treatment",
                 "Asia and the Pacific___Known Status",
                 "Caribbean___Known Status",
                 "Eastern Europe and Central Asia___Known Status",
                 "Latin America___Known Status",
                 "Western & Central Europe and North America___Virally Suppressed",
                 "Global___Known Status")


# IMPORT ---------------------------------------------------------------------

df_fem <- read_csv("Data/Treatment cascade_Progress towards 90-90-90 target - Female adults (15+).csv") %>% 
  dplyr::select(-(contains(c("Footnote"))))

df_male <- read_csv("Data/Treatment cascade_Progress towards 90-90-90 target - Male adults (15+) (1).csv") %>% 
  dplyr::select(-(contains(c("Footnote"))))


# MUNGE AND TIDY ----------------------------------------------------------------

# TRANSPOSE TABLE

    #female adults
    df_fem <- df_fem %>% 
      t() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      as_tibble()  
    
    #make first row the header
    header.true <- function(df_fem) {
      names(df_fem) <- as.character(unlist(df_fem[1,]))
      df_fem[-1,]
    }
    
    df_fem <- header.true(df_fem)

    #male adults
    df_male <- df_male %>% 
      t() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      as_tibble()  
    
    #make first row the header
    header.true <- function(df_male) {
      names(df_male) <- as.character(unlist(df_male[1,]))
      df_male[-1,]
    }
    
    df_male <- header.true(df_male)

#TIDY DATA

#female
df_tidy_fem <- df_fem %>% 
  rename(year = ...1,
         indicator = Region) %>% 
  separate(year, sep = "_", into = c("year", "type")) %>% 
  separate(indicator, sep = "_", into = c("indicator", "type")) %>% 
  mutate(type = ifelse(is.na(type), "point_est", type),
         indicator = case_when(indicator ==
                                 "Percent of people living with HIV who know their status" ~ "Known Status",
                               indicator ==
                                 "Percent of people on ART who achieve viral suppression" ~ "Virally Suppressed",
                               indicator ==
                                 "Percent of people who know their status who are on ART" ~ "On Treatment",
                               TRUE ~ indicator)) %>% 
  pivot_longer(cols = regions,
               names_to = "country")

#male
df_tidy_male <- df_male %>% 
  rename(year = ...1,
         indicator = Region) %>% 
  separate(year, sep = "_", into = c("year", "type")) %>% 
  separate(indicator, sep = "_", into = c("indicator", "type")) %>% 
  mutate(type = ifelse(is.na(type), "point_est", type),
         indicator = case_when(indicator ==
                                 "Percent of people living with HIV who know their status" ~ "Known Status",
                               indicator ==
                                 "Percent of people on ART who achieve viral suppression" ~ "Virally Suppressed",
                               indicator ==
                                 "Percent of people who know their status who are on ART" ~ "On Treatment",
                               TRUE ~ indicator)) %>% 
  pivot_longer(cols = regions,
               names_to = "country")


#clean the year names
df_tidy_fem$year <- substr(df_tidy_fem$year, 1,4)
df_tidy_male$year <- substr(df_tidy_male$year, 1,4)


#PIVOT WIDER

#female
df_tidy_fem <- df_tidy_fem %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  rename(value = point_est) %>% 
  arrange(desc(year), country) %>% 
  mutate(value = case_when(value == ">95" ~ "95", TRUE ~ value),
         sex = "Female")

#male
df_tidy_male <- df_tidy_male %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  rename(value = point_est) %>% 
  arrange(desc(year), country) %>% 
  mutate(value = case_when(value == ">95" ~ "95", TRUE ~ value),
         sex = "Male") 

#make vlaue numeric
df_tidy_fem$value <- as.numeric(df_tidy_fem$value)
df_tidy_male$value <- as.numeric(df_tidy_male$value)

#save data locally
write_csv(df_tidy_fem, "Dataout/90cascade_regional_female.csv")
write_csv(df_tidy_male, "Dataout/90cascade_regional_male.csv")


# VIS -----------------------------------------------------

#create female dataframe from vis
df_viz_fem <- df_tidy_fem %>% 
  group_by(country) %>% 
  filter(year == 2020) %>% 
  mutate(grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal- value,
                         value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                         TRUE ~ 0),
         achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
         dot_color = case_when(grouping == "Known Status" ~ old_rose,
                               grouping == "On Treatment" ~ golden_sand,
                               grouping == "Virally Suppressed" ~ scooter,
                               grouping == "Achieved" ~ genoa,
                               # grouping == "Z_Achieved" ~ genoa,
                               TRUE ~ trolley_grey)) %>% 
  fill(grouping, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gap_bar = case_when(value < goal ~ value),
         country = reorder_within(country, gap, grouping, max, na.rm = TRUE))

#create male dataframe from vis
df_viz_male <- df_tidy_male %>% 
  group_by(country) %>% 
  filter(year == 2020) %>% 
  mutate(grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal- value,
                         value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                         TRUE ~ 0),
         achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
         dot_color = case_when(grouping == "Known Status" ~ old_rose,
                               grouping == "On Treatment" ~ golden_sand,
                               grouping == "Virally Suppressed" ~ scooter,
                               grouping == "Achieved" ~ genoa,
                               # grouping == "Z_Achieved" ~ genoa,
                               TRUE ~ trolley_grey)) %>% 
  fill(grouping, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gap_bar = case_when(value < goal ~ value),
         country = reorder_within(country, gap, grouping, max, na.rm = TRUE))

#number achieved (F)
epi_ctrl_cnt_fem <- df_viz_fem %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

#number achieved (M)
epi_ctrl_cnt_male <- df_viz_male %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

#VIS FOR FEMALE ADULTS (15+)
df_viz_fem %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = goal, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF 2020, {epi_ctrl_cnt_fem} REGIONS HAS ACHIEVED EPIDEMIC CONTROL FOR FEMALE ADULTS (15+)"),
       caption = glue("Source: UNAIDS 90-90-90 15+ (2020)
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/UNAIDS_Epi_Progress_Female_adults.svg")

df_viz_male %>% 
  # mutate(country = reorder_within(country, regions_viz, country)) %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = goal, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF 2020, {epi_ctrl_cnt_male} REGIONS HAVE ACHIEVED EPIDEMIC CONTROL FOR MALE ADULTS (15+)"),
       caption = glue("Source: UNAIDS 90-90-90 15+ (2020)
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/UNAIDS_Epi_Progress_Male_adults.svg")
