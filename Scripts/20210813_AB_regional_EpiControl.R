## PROJECT: catch22
## AUTHOR:  K. Srikanth | USAID
# PURPOSE:  Epi control trend graphs by region - Africa Bureau Request
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
library(svglite)
library(ggtext)

#GLOBAL VARIABLES ---------------------------------------------------------

#source info & definition of epidemic control
source <- "UNAIDS, https://aidsinfo.unaids.org/" 

authors <- c("Karishma Srikanth", "Tim Essam")

# High burden
top3 <- c("Kenya", "Zimbabwe", "Ethiopia") #not used at regional level

date_pulled <- "2021-08-12"

note <- str_wrap("HIV epidemic control is the point at which the number number of new HIV infections falls below the number of AIDS-related deaths", width = 40)

epi_control <- str_wrap("PEPFAR defines national HIV epidemic control as the point at which the total number of new HIV infections falls below the total number of deaths from all causes among HIV-infected individuals, with both new infections and deaths among HIV-infected individual slowing and declining.", width = 100)

plot_title <- "STEADY DECLINE IN THE NUMBER OF <span style= 'color:#2057a7;'> NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> AIDS-RELATED DEATHS </span> SINCE THE EARLY 2000s IN MOST AFRICAN REGIONS"

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


# IMPORT ---------------------------------------------------------------------

new_inf <- read_csv("Data/New HIV infections_New HIV infections - All ages_Population_ All ages.csv") %>% 
  dplyr::select(-(contains(c("Footnote"))))

aids_death <- read_csv("Data/AIDS-related deaths_AIDS-related deaths - All ages_Population_ All ages.csv") %>% 
  dplyr::select(-(contains(c("Footnote"))))


# MUNGE AND TIDY ----------------------------------------------------------------

# TRANSPOSE

    #infections
    new_inf <- new_inf %>% 
      t() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      as_tibble()  
    
    #make first row the header
    header.true <- function(new_inf) {
      names(new_inf) <- as.character(unlist(new_inf[1,]))
      new_inf[-1,]
    }
    
    new_inf <- header.true(new_inf)

    #aids deaths
    aids_death <- aids_death %>% 
      t() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      as_tibble()  
    
    #make first row the header
    header.true <- function(aids_death) {
      names(aids_death) <- as.character(unlist(aids_death[1,]))
      aids_death[-1,]
    }
    
    aids_death <- header.true(aids_death)

#TIDY DATA

    #tidy infection data
    new_inf_tidy <- new_inf %>% 
      rename(year = Region) %>% 
      separate(year, sep = "_", into = c("year", "type")) %>% 
      mutate(type = ifelse(is.na(type), "point_est", type)) %>% 
      pivot_longer(cols = regions,
                   names_to = "region") %>% 
      mutate(value = case_when(value == "<1000" ~ "1000", TRUE ~ value)) %>% 
      pivot_wider(names_from = type,
                  values_from = value) %>% 
      rename(value = point_est) %>% 
      mutate(across(c(year, value:upper), ~str_replace_all(., ("(\\&lt;| )"), "") %>% as.numeric),
             metric = "new_hiv_infections") 
    
    

    #tidy aids death data
    aids_death_tidy <- aids_death %>% 
      rename(year = Region) %>% 
      separate(year, sep = "_", into = c("year", "type")) %>% 
      mutate(type = ifelse(is.na(type), "point_est", type)) %>% 
      pivot_longer(cols = regions,
                   names_to = "region") %>%
      mutate(value = case_when(value == "<100" ~ "100",
                               value == "<200" ~ "200",
                               value == "<500" ~ "500",
                               value == "<1000" ~ "1000", TRUE ~ value)) %>% 
      pivot_wider(names_from = type,
                  values_from = value) %>% 
      rename(value = point_est) %>% 
      mutate(across(c(year, value:upper), ~str_replace_all(., ("(\\&lt;| )"), "") %>% as.numeric),
             metric = "aids_deaths")

#MERGE AIDS DEATHS AND NEW INFECTIONS
epi_clean <- full_join(new_inf_tidy, aids_death_tidy, by = c("year", "region", "value", "lower", "upper")) %>% 
  mutate(metric = case_when(is.na(metric.x) ~ "aids_related_death",
                            is.na(metric.y) ~ "new_HIV_infections")) %>% 
  select(-c(metric.x, metric.y))

write_csv(epi_clean,"Dataout/regional_epicontrol.csv")

# VIS ---------------------------------------------------------------------------

#prepare data frame for vis
epi_clean <- epi_clean %>% 
  mutate(line_color = ifelse(str_detect(metric, "HIV"), denim, golden_sand),
         fill_color = ifelse(str_detect(metric, "HIV"), denim, old_rose)) %>% 
  group_by(region, metric) %>% 
  mutate(max_val = max(value)) %>% 
  ungroup() %>% 
  mutate(value_mod = ifelse(str_detect(metric, "death"), -value, value),
         ymax = ifelse(region %in% top3, 2.5e5, 4e4),
         ymin = ifelse(region %in% top3, -1e5, -2e4))

#regions in order for vis
epi_clean$region <- as.factor(epi_clean$region)
epi_clean$facet = factor(epi_clean$region, levels = c("West and Central Africa", "Middle East and North Africa",
                                                      "East and Southern Africa", "Asia and the Pacific",
                                                      "Caribbean", "Eastern Europe and Central Asia",
                                                      "Latin America", "Western & Central Europe and North America",
                                                      "Global"))


#geom area
epi_clean %>% 
  ggplot(aes(x = year, group = metric)) +
  geom_area(aes(y = value_mod, fill = fill_color), alpha = .25) +
  geom_line(aes(y = value_mod, color = fill_color)) +
  geom_point(data = . %>% filter(year == max(year)), 
             aes(y = value_mod, fill = fill_color), 
             shape = 21, color = "white", size = 3) +
  geom_text(data = . %>% filter(year == max(year)), 
            aes(y = value_mod, 
                color = fill_color, 
                label = paste0(abs(value_mod/1000) %>% comma(1.0), "K")),
            hjust = -0.3,
            family = "Source Sans Pro Light") +
  geom_hline(yintercept = 0, color = grey80k) +
  facet_wrap(~facet, scales = "free_y") +
  scale_y_continuous(labels = label_number_si()) +
  geom_blank(aes(y = ymin)) +
  geom_blank(aes(y = ymax)) +
  scale_color_identity() +
  scale_fill_identity()+
  labs(x = NULL, y = NULL,
       title = plot_title,
       subtitle = epi_control,  
       caption =  glue("{source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid() +
  scale_x_continuous(breaks = seq(1990, 2025, 5), limits = c(1990, 2024)) +
  coord_cartesian(expand = T, clip = "off") +
  theme(axis.text.y = element_markdown(), 
        plot.title = element_textbox_simple(margin = margin(5.5, 0, 0, 5.5)),
        plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light",
                                               margin = margin(5.5, 5.5, 5.5, 5.5))) 

si_save("Graphics/epi_control_region.svg")



