# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  GH AA visuals - UHC life Expectancy
# REF ID:   d3144478 
# LICENSE:  MIT
# DATE:     2022-07-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "d3144478"

# IMPORT ------------------------------------------------------------------
  
  folder_path <- "Dataout/"
  
  df <- folder_path %>% 
    return_latest("GH_scorecard_indicators_2022-07-21") %>% 
  read_csv()

# MUNGE -------------------------------------------------------------------

top10 <- c("Democratic Republic of the Congo",
           "Ethiopia","Kenya",
           "Mozambique", "Nigeria", "South Africa",
           "Tanzania","Uganda", "Zambia", "Zimbabwe")
  
df_viz <- df %>% 
  filter(indicator == "life_expectancy_at_birth_both_sexes_years",
         country %in% top10) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  rename(life_expectancy = life_expectancy_at_birth_both_sexes_years) %>% 
  mutate(endpoints = case_when(year %in% c(max(year), min(year))~life_expectancy),
         lab_val = case_when(year == max(year) ~ life_expectancy))

df_viz %>% 
  ggplot(aes(year, life_expectancy, group = usaid_supported, color = denim, fill = denim_light)) +
 # geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5) +
  geom_area(alpha = .4, size = .9, position = "identity") +
 geom_point(aes(y = endpoints), na.rm = TRUE) +
   geom_text(aes(label = round(lab_val, 1)), na.rm = TRUE,
            hjust = 2, vjust = -0.5,family = "Source Sans Pro") +
  facet_wrap(~fct_reorder2(country, year, life_expectancy, .desc = TRUE), nrow = 2)+
  scale_color_identity() +
  scale_fill_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       title = glue("Rebounds in life expectancy at birth in top 10 USAID-supported African countries after scale up of PEPFAR, TB, and Malaria Programs"),
       subtitle = "Top 10 USAID-supported programs, baed on FY21 Funding",
       caption = glue("Source: 2022 Revision of World Population Prospects"))
  
  si_save("Images/life-expectancy-top10-ou.png")
  si_save("Graphics/life-expectancy-top10-ou.svg")
  
  
  df_mmd_ou %>%
  filter(countryname %in% top_cntry) %>%
  ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
  geom_area(alpha = .4, size = .9, position = "identity") +
  geom_point(aes(y = endpoints), na.rm = TRUE) +
  geom_text(aes(label = percent(lab_share, 1)), na.rm = TRUE,
            hjust = -.2, vjust = .1,family = "Source Sans Pro") +
  facet_wrap(~fct_reorder2(country_lab, period, tx_curr, .desc = TRUE)) +
  scale_y_continuous(label = percent, 
                     breaks = seq(0, 1, .5)) +
  scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1", "FY21Q3", "FY22Q1")) +
  scale_color_identity(aesthetics = c("color","fill")) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL,
       title = glue("USAID HAS MADE LIMITED GAINS TOWARDS GETTING TREATMENT PATIENTS ON +6 MONTHS OF MMD SINCE FY20Q3"),
       subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
       caption = glue("MMD 3 months or more = 3-5 months and 6 months or more | Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid() +
  theme(panel.spacing.y = unit(.5, "line"),
        panel.spacing.x = unit(.5, "line"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 7),
        panel.grid.major.y = element_line(color = "#E8E8E8"),
        panel.grid.minor.y = element_line(color = "#E8E8E8"),
        strip.text = element_markdown())    


