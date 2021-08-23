## PROJECT: agitprop
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: 95-95-95 Achievement - AFrica Region OU's only
## LICENSE: MIT
## DATE:    2021-08-13


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(googlesheets4)
library(extrafont)
library(scales)
library(tidytext)
library(glue)


# GLOBAL VARIABLES --------------------------------------------------------

#creds  
load_secrets()

#unaids data (saved for Mind the Gap)
gs_id <- as_sheets_id("1Ui1r5ynn9xYky86hHMO9kmdNZdWmANI5nAQ1N2b0wdM")

authors <- c("Karishma Srikanth", "Tim Essam")

#goal
goal <- .9

# IMPORT ------------------------------------------------------------------

#read from Mind the Gap data
df_unaids <- read_sheet(as_sheets_id(gs_id), "UNAIDS") %>%
  dplyr::mutate(year = as.integer(year))

df_impatt <- read_sheet(as_sheets_id(gs_id), "ARTshare")

african_cntry <- c("Eswatini",
                   "Rwanda",
                   "Botswana",
                   "Malawi",
                   "Namibia",
                   "Uganda",
                   "Burundi",
                   "Zambia",
                   "Tanzania",
                   "Ethiopia",
                   "Cameroon",
                   "Cote D'Ivoire",
                   "Democratic Republic of the Congo",
                   "Angola",
                   "South Sudan",
                   "Kenya",
                   "Lesotho",
                   "South Africa",
                   "Zimbabwe",
                   "Nigeria",
                   "Mozambique",
                   "Ghana",
                   "Burkina Faso",
                   "Liberia",
                   "Mali",
                   "Senegal",
                   "Sierra Leone",
                   "Togo")

# MUNGE -------------------------------------------------------------------


df_impatt <- df_impatt %>% 
  rename(country = countryname) %>% 
  filter(country %in% african_cntry) %>% 
  group_by(country) %>% 
  summarise(PLHIV = sum(PLHIV, na.rm = TRUE)) %>% 
  ungroup()

df_unaids <- df_unaids %>% 
  filter(year == max(year),
         sex == "All",
         country %in% african_cntry)

df_viz <- df_unaids %>% 
  left_join(df_impatt) %>% 
  filter(country != "Vietnam") %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country),
         indicator = recode(indicator, "On ART" = "On Treatment"),
         PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))

#save data locally
write_csv(df_viz, "Dataout/unaids_90cascade_africanOU.csv")

df_viz <- df_viz %>% 
  group_by(country) %>% 
  mutate(value = round(value, 2),
         grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
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


# PLOT --------------------------------------------------------------------

epi_ctrl_cnt <- df_viz %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

df_viz %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = goal, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, alpha = .8, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(label = percent) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF 2020, {epi_ctrl_cnt} AFRICAN COUNTRIES HAVE ACHIEVED EPIDEMIC CONTROL"),
       caption = glue("Source: UNAIDS 90-90-90 15+ (2020)
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/UNAIDS_Epi_Progress_AfricanOU.svg")
