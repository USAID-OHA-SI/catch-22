# PURPOSE: LP Funding - Global Planning Meeting
# AUTHOR: K Srikanth | SI
# LICENSE: MIT
# DATE: 2021-11-09
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(tidyverse)
library(gophr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(here)
library(patchwork)
library(googlesheets4)
library(janitor)
library(ggnewscale)

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")


#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# LOAD DATA ============================================================================  

sheet_id <- "14m5Z7WzJyRpE_EGOETbYaE1RglnRaBtqb1CdDTqDA-c"


df <- read_sheet(sheet_id, sheet = "LP% Projections (SBU)") %>%
  clean_names()

# MUNGE ===============================================================================

df_funding <- df %>% 
  filter(cop != "COP24",
         cop != "COP25",
         adjusted_category != "%LP",
         ou_type == "LTS") %>% 
  select(-c(date_updated)) %>% 
  group_by(cop, adjusted_category) %>% 
  summarise(across(budget, sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = adjusted_category, values_from = budget) %>% 
  mutate(share = `LPBudget` / `Budget`) %>% 
  group_by(cop) %>% 
  mutate(target = Budget *0.7) %>% 
  pivot_longer(cols = Budget:LPBudget, names_to = "category", values_to = "value") %>% 
  mutate(fill_color = ifelse(category == "LPBudget", old_rose, trolley_grey_light),
         fill_color = ifelse(cop %in% c("COP22", "COP23") & category == "LPBudget", old_rose_light, fill_color)) 


df_funding %>% 
  mutate(share = ifelse(category == "LPBudget", share, NA),
         value_label = ifelse(category == "LPBudget", clean_number(value), NA)) %>% 
  #mutate(share = ifelse(partner_type == "Local", share, NA)) %>% 
  ggplot(aes(cop, value)) +
  geom_col(aes(fill = fill_color),
           position = "identity") +
  geom_errorbar(aes(y = target, ymax = target, ymin = target, color = trolley_grey), linetype = "dashed", size = 0.5) +
  #geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right") +
 # scale_x_continuous(expand = c(.005, .005),
                  #   n.breaks = unique(df_funding$cop) %>% length())+
  geom_text(aes(label = value_label, vjust = -1, 
            size = 12/.pt, family = "Source Sans Pro")) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  si_style_nolines() +
  new_scale_fill() +
  geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
                 color = ifelse(share > 0.3, "black", grey90k)), 
             vjust = 1.3, 
             size = 10/.pt, 
             label.size = NA, family = "Source Sans Pro") +
  scale_fill_si(palette = "old_roses", lim = c(0.4, 1), alpha = 0.85, labels = percent,
                oob = squish) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title = "USAID/PEPFAR is projected to program over half of all funding through Local Partners in COP21" %>% toupper(),
       caption = glue("Note: LTS OUs only. Percentages exclude GHSC-PSM/RTK and M&O,
                     Source: SBU Local Partner Strategy Dashboard",
                      "USAID SI Analytics",
                      "Global Planning Meeting 2021-11-15", .sep = " | "))

si_save("Graphics/gpm_LPfunding.svg")

  




