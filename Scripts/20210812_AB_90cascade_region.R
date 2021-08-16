## PROJECT: catch22
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: 95-95-95 Achievement - Africa Bureau Regional Level cleaning script
## LICENSE: MIT
## DATE:    2021-08-12


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

authors <- c("Tim Essam", "Karishma Srikanth")

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

df <- read_csv("Data/Treatment cascade_Progress towards 90-90-90 targets - All ages.csv") %>% 
  dplyr::select(-(contains(c("Footnote"))))

# MUNGE AND TIDY ----------------------------------------------------------------

# transpose the table
df <- df %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  as_tibble()  

#make first row the header
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

df <- header.true(df)

#tidy data
df_tidy <- df %>% 
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
df_tidy$year <- substr(df_tidy$year, 1,4)

#pivot estimates wider
df_tidy <- df_tidy %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  rename(value = point_est) %>% 
  arrange(desc(year), country) %>% 
  mutate(value = case_when(value == ">95" ~ "95", TRUE ~ value)) 

#make vlaue numeric
df_tidy$value <- as.numeric(df_tidy$value)

str(df_tidy$value)


# VIS -----------------------------------------------------

df_viz <- df_tidy %>% 
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

epi_ctrl_cnt <- df_viz %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

#save data as csv
write_csv(df_viz, "Dataout/unaids_vizdata.csv")

df_viz %>% 
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
       title = glue("AS OF 2020, {epi_ctrl_cnt} REGIONS HAVE ACHIEVED EPIDEMIC CONTROL"),
       caption = glue("Source: UNAIDS 90-90-90 15+ (2020)
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/UNAIDS_Epi_Progress.svg")
