# PROJECT:  catch22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review results for HFR survey
# REF ID:   25205341 
# LICENSE:  MIT
# DATE:     2022-09-02
# UPDATED:  2022-09-16

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googlesheets4)
  library(janitor)
  library(Wavelength)
  library(vroom)
  library(RColorBrewer)
  library(waffle)
  library(ggrepel)

# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "25205341"
  
  gs_id <- as_sheets_id("1FI7takwu7ugu86k98VziF3poNEM5JxSBCht5zCqA82A")

  response_order <- c("Disagree", "Strongly Disagree", "Agree", "Strongly Agree", "placeholder", "Neutral")
  
  fill_col <- append(brewer.pal(5, "BrBG"), "#ffffff")[order(c(1, 2, 6, 4, 3, 5))]
  
  fill_col <- replace(fill_col, fill_col == "#F5F5F5", "#999999")
  
  names(fill_col) = response_order

  barrier_order <- c("Major Barrier", "Minor Barrier", "Not a Barrier", "NA")
  fill_col_barrier <- fill_col[c(1:3,6)]
  names(fill_col_barrier) = barrier_order
  
  resources_order <- c("Needed, but no resources to support", 
                       "Needed & resources would be available", 
                       "Exists currently")
  fill_col_resources <- brewer.pal(5, "BrBG")[c(1, 2, 5)]
  names(fill_col_resources) = resources_order
  
  loe_order <- c("Minimal", "Low", "Moderate", "High", "Extreme")

  fill_col_loe <- brewer.pal(5, "BrBG")[order(c(5, 4, 1, 2, 3))] %>% replace(. == "#F5F5F5", "#C1C1C1")
  names(fill_col_loe) = loe_order
  
# IMPORT ------------------------------------------------------------------

  df <- read_sheet(gs_id,
                   .name_repair = make_clean_names)   


  #latest cumulative MER results (for HFR indicator) 
  df_mer <- return_latest("../Wavelength//out/DATIM/", "GLOBAL_DATIM") %>% 
    vroom()
  

# REFERENCE INFO ----------------------------------------------------------

  (cntry_reporting <- length(unique(df$country_affiliation)))
  
  source_info <- glue("Source: 2022 State of HFR Survey [Sep 2022] | Responses from {cntry_reporting} countries | Ref ID: {ref_id}")
  
# REVIEW COMPLETENESS -----------------------------------------------------
  #remove duplication from country teams
  df <- df %>% 
    filter(str_detect(email_address, "^(kow|cnh|kiz)", negate = TRUE))
  
  df_response <- df_mer %>% 
    mutate(country = ifelse(operatingunit == countryname, operatingunit, glue("{operatingunit}/{countryname}"))) %>%
    filter(!is.na(mer_results)) %>% 
    distinct(country, mech_code, orgunituid, indicator) %>% 
    count(country, sort = TRUE, name = "site_mech_ind_combos") %>% 
    mutate(responded = country %in% df$country_affiliation) %>% 
    arrange(responded, desc(site_mech_ind_combos), country)
  
  length(unique(df$country_affiliation))
  
  length(unique(df$country_affiliation)) == sum(df_response$responded)
  df$country_affiliation[duplicated(df$country_affiliation)]
  setdiff(unique(df$country_affiliation), filter(df_response, responded) %>% pull(country))
  setdiff(filter(df_response, responded) %>% pull(country), unique(df$country_affiliation))
  
  
  df_response %>% 
    count(responded)
  
  df_response %>% 
    mutate(country = country %>% 
             recode("Democratic Republic of the Congo" = "DRC") %>% 
             str_replace("West Africa Region", "WAR") %>% 
             str_replace("Western Hemisphere Region", "WHR") %>% 
             str_replace("Asia Region", "AR"),
           cntry_size = glue("{country} ({comma(site_mech_ind_combos)})"),
           # cntry_size = ifelse(responded == FALSE,
           #                     glue("**{country}** ({comma(site_mech_ind_combos)})"),
           #                     glue("{country} ({comma(site_mech_ind_combos)})")),
           x = 1) %>% 
    ggplot(aes(x, fct_rev(fct_inorder(cntry_size)), fill = responded)) +
    geom_tile(color = "white") +
    # scale_fill_manual(values = c(old_rose, denim_light)) +
    scale_fill_manual(values = c(fill_col[['Strongly Disagree']], fill_col[['Strongly Agree']])) +
    scale_x_discrete(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 11/.pt))
          # axis.text.y = element_markdown(size = 10/.pt))
  
  si_save("Graphics/HFR_Responses.svg") #width = 2.5
  
  rm(df_mer)



# UTILITY -----------------------------------------------------------------

df_utility <- df %>% 
  select(country = country_affiliation, starts_with("x1_")) %>% 
  pivot_longer(-country,
               names_to = "indicator",
               values_to = "response") %>% 
  mutate(indicator = str_extract(indicator, "(?<=row_).*"),
         indicator = recode(indicator,
                            hfr_is_a_priority_in_fy22_for_our_mission = "Is a priority for our mission",
                            hfr_data_are_used_to_evaluate_monitor_our_partners_progress = "Used to evaluate partner progress",
                            hfr_feeds_into_programmatic_planning_and_adjustments = "Feed into programmatic planning/adjustments",
                            hfr_data_are_used_by_our_country_team_monthly = "Used monthly",
                            hfr_data_are_used_in_interagency_sgac_non_pepfar_discussions = "Used in interagency discussions",
                            hfr_provides_valuable_insights_for_our_team_s_work = "Provides valuable insights",
                            hf_rs_value_is_worth_the_effort_put_into_collection_and_management = "Worth the effort expended to collect/manage",
                            hfr_should_continue_to_be_a_requirement_for_all_partners_and_sites = "Should be required (all partners/sites)"
         ),
         score = recode(response,
                        "Strongly Disagree" = -2,
                        "Disagree" = -1,
                        "Neutral" = 0,
                        "Agree" = 1,
                        "Strongly Agree" = 2),
         response_value = 1,
         response_value_direction = ifelse(str_detect(response, "Disagree"), -1, 1),
         response_neutral = response == "Neutral")

df_resp <- df_utility %>% 
  mutate(indicator, response = recode(response,
                                      "Strongly Disagree" = "Disagree",
                                      "Strongly Agree"= "Agree")) %>% 
  count(indicator, response) %>% 
  group_by(indicator) %>% 
  mutate(share = n/sum(n),
         share = percent(share, 1)) %>% 
  ungroup() %>% 
  filter(indicator %in% c("Provides valuable insights", "Should be required (all partners/sites)"),
         response == "Agree")

df_ph <- df_utility %>% 
  count(indicator, wt = sum(score > 0, na.rm = TRUE)) %>% 
  mutate(response_value_direction = max(n) + 2 - n,
         response = "placeholder") %>% 
  select(-n)


df_viz_utility <- df_utility %>% 
  bind_rows(df_ph) %>% 
  mutate(response = factor(response, response_order),
         response_value_pos = ifelse(str_detect(response, "Agree"), 1, 0))  

  # mutate(group = ifelse(
  #   indicator %in% c("Provides valuable insights", 
  #                    "Worth the effort to collect", 
  #                    "Should be required (all partners/sites)"), "Value", "Use")) 

df_viz_utility %>% 
  ggplot(aes(response_value_direction, fct_reorder(indicator, response_value_pos, sum, na.rm = TRUE),
             fill = fct_rev(response))) +
  geom_col(alpha = .9) +
  geom_vline(xintercept = -18:44, color = "white") +
  geom_vline(xintercept = 0, color = "#999999", size = 1.1) +
  annotate(geom = "text",
           x = -.5, y = Inf, hjust = 1,
           label = "Disagree", family = "Source Sans Pro", color = matterhorn) +
  annotate(geom = "text",
           x = .5, y = Inf, hjust = 0,
           label = "Agree", family = "Source Sans Pro", color  = matterhorn) +
  annotate(geom = "text",
           x = 28, y = Inf, hjust = 0,
           label = "Neutral", family = "Source Sans Pro", color  = matterhorn) +
  # facet_grid(fct_rev(group) ~ ., scales = "free_y", space = "free") +
  scale_x_continuous(expand = c(.005, .005)) +
  scale_fill_manual(values = fill_col) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL,
       title = glue("Of the {length(unique(df_utility$country))} countries that responded, { df_resp %>%
    filter(indicator == 'Provides valuable insights') %>%
    pull()} believe HFR `Provides valuable insights`") %>% toupper,
       subtitle = glue("{df_resp %>%
    filter(indicator == 'Should be required (all partners/sites)') %>%
    pull()} thought it should continued to be required for all partner and sites"),
       caption = source_info) +
  si_style_nolines() +
  theme(axis.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none")


si_save("Graphics/HFR_utility.svg")


# BARRIERS ----------------------------------------------------------------

  df_barriers <- df %>% 
    select(country = country_affiliation, starts_with("x2a_")) %>% 
    pivot_longer(-country,
                 names_to = "indicator",
                 values_to = "response") %>%
    mutate(indicator = str_extract(indicator, "(?<=data_).*"),
           indicator = recode(indicator,
                              lack_of_staff_time = "Staff time", 
                              lack_of_staff_capacity_to_conduct_analyses = "Staff capacity", 
                              lack_of_usaid_washington_support = "HQ support", 
                              data_are_difficult_to_access = "Access", 
                              data_difficult_to_use_analyze = "Ease of Analysis", 
                              insufficient_tools_technology_to_analyze_the_data = "Tools/technology", 
                              analyses_difficult_to_apply_to_program_decisions = "Applicability to program decisions", 
                              leadership_technical_teams_not_requesting_analyses = "Leadership need", 
           )) %>% 
    count(indicator, response) 
  
  df_barriers <- df_barriers %>%
    mutate(sort_order = case_when(response %in% c("Major Barrier", "Minor Barrier") ~ n),
           fill_alpha = ifelse(response %in% c("Major Barrier", "Minor Barrier"), .9, .1),  
           response = factor(response, rev(barrier_order))) 
  
  
  text_barrier <-  df_barriers %>% 
    filter(indicator == "Staff time") %>% 
    mutate(share = n/sum(n)) %>% 
    filter(response == "Major Barrier") %>% 
    pull() %>% 
    percent()
  
  df_barriers %>% 
    ggplot(aes(n, fct_reorder(indicator, sort_order, sum, na.rm = TRUE), 
               fill = response, alpha = fill_alpha)) +
    geom_col() +
    geom_vline(xintercept = 0:cntry_reporting, color = "white") +
    geom_vline(xintercept = length(unique(df_utility$country))/2, color = matterhorn, linetype = "dotted") +
    scale_x_continuous(expand=c(.005, .005), position = "top") +
    scale_fill_manual(values = fill_col_barrier) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         title = glue("STAFF TIME WAS REPORTED AS A MAJOR BARRIER FOR {text_barrier} OF COUNTRIES"),
         subtitle = glue("Lack of leadership and/or technical requests involving HFR is also a large barrier for its use"),
         caption = source_info) +
    si_style_nolines() +
    theme(legend.position = "none")
  
  
  si_save("Graphics/HFR_barriers.svg")
  
  
  df_barriers %>% 
    mutate(barrier = response %in% c("Major Barrier", "Minor Barrier")) %>% 
    count(indicator, barrier, wt = n) %>% 
    group_by(indicator) %>% 
    mutate(share = n/sum(n)) %>% 
    filter(barrier == TRUE) %>% 
    arrange(desc(share))
  
  df %>% 
    select(country = country_affiliation, starts_with("x2b_")) %>% 
    rename(other_barriers = 2) %>% 
    filter(other_barriers %ni% c(NA, "NA")) %>% 
    pull() %>% 
    clipr::write_clip()
  

# LOE ---------------------------------------------------------------------

  df_loe <- df %>% 
    select(country = country_affiliation, starts_with("x3_")) %>% 
    pivot_longer(-country,
                 names_to = "indicator",
                 values_to = "response") %>% 
    mutate(indicator = indicator %>% 
             str_extract("(?<=usaid_).*") %>% 
             str_replace("_", " ") %>% 
             str_to_title(),
           response = factor(response, c("Minimal", "Low", "Moderate", "High", "Extreme")))
  
  loe_high <- df_loe %>% 
    filter(indicator == "Implementing Partners") %>% 
    count(response %in% c("High", "Extreme")) %>% 
    mutate(share = n/sum(n)) %>% 
    filter(`response %in% c("High", "Extreme")` == TRUE) %>% 
    pull() %>% 
    percent()
  
  df_loe %>% 
    ggplot(aes(response, fct_rev(indicator), color = response)) + 
    geom_point(position = position_jitter(height = .3, width =  .3, seed = 42), 
               alpha = .6, size = 5) +
    geom_text_repel(data = . %>% filter((indicator == "Implementing Partners" & response %in% c("Minimal", "Low")) |
                                          (indicator == "Mission" & response == "High")),
                    aes(label = country), force = 20, #segment.colour = NA,
                    family = "Source Sans Pro", color = matterhorn,
              position = position_jitter(height = .3, width =  .3, seed = 42)) +
    scale_color_manual(values = fill_col_loe) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = glue("{loe_high} OF COUNTRIES REPORTED THE LEVEL OF EFFORT FOR HFR WAS HIGH OR EXTREME"),
         caption = source_info) +
    si_style_nolines() +
    theme(legend.position = "none")
          
  
  si_save("Graphics/HFR_loe.svg") 
           
           

# IN-COUNTRY SYSTEM -------------------------------------------------------

  df %>% 
    select(country = country_affiliation, starts_with("x4a_")) %>% 
    rename(response = 2) %>% 
    mutate(has_system = response != "No") %>% 
    count(has_system) %>% 
    mutate(share = n / sum(n))
  
  
# PREFERENCE --------------------------------------------------------------

  df_pref <- df %>% 
    select(country = country_affiliation, starts_with("x5_")) %>% 
    rename(preference = 2) %>% 
    mutate(preference = case_when(str_detect(preference, "Allow") ~ "Country determination",
                                  str_detect(preference, "Continue") ~ "Business as usual",
                                  str_detect(preference, "Mandate") ~ "Only largest org units")) %>% 
    count(preference) %>% 
    mutate(y = "response",
           preference = factor(preference, 
                               c("Business as usual",
                                 "Only largest org units",
                                 "Country determination")),
           share = n/sum(n))
  
  
  bau <- df_pref %>% 
    filter(preference == "Business as usual") %>% 
    pull() %>% 
    percent()
  
  cdo <- df_pref %>% 
    filter(preference == "Country determination") %>% 
    pull() %>% 
    percent()

  df_pref %>% 
    ggplot(aes(n, y, fill = preference, label = n)) +
    geom_col() +
    geom_text(aes(x = n/2), size = 6,
              family = "Source Sans Pro", color = "white") +
    facet_grid(~ preference, scale = "free_x", space = "free") +
    scale_fill_manual(values = c("Business as usual" = fill_col[['Strongly Agree']], 
                                 "Country determination" = fill_col[['Disagree']],
                                 "Only largest org units" = fill_col[['Strongly Disagree']])) +
    labs(x = NULL, y = NULL,
         title = glue("While {bau} of countries prefer keeping HFR as it is, {cdo} want to determine which org units report") %>% toupper,
         caption = source_info) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text = element_blank())
  
  si_save("Graphics/HFR_preference.svg") 
  

# CONTINUE ----------------------------------------------------------------

  df_cont <- df %>% 
    select(country = country_affiliation, starts_with("x8_")) %>% 
    rename(willingness = 2) #%>% 
    # count(willingness)
 
  # df_cont <- tibble(willingness = 1:5,
  #                   fill_color = brewer.pal(5, "BrBG")) %>% 
  #   left_join(df_cont, ., by = "willingness") %>% 
  
  df_cont <- df_cont %>% 
    mutate(willingness_c = glue("x{willingness}"),
           response = recode(willingness_c, 
                          x1 = "Strongly Disagree",
                          x2 = "Disagree",
                          x3 = "Neutral",
                          x4 = "Agree",
                          x5 = "Strongly Agree"),
           # response = factor(response, response_order),
           indicator = "continue",
           score = recode(response,
                          "Strongly Disagree" = -2,
                          "Disagree" = -1,
                          "Neutral" = 0,
                          "Agree" = 1,
                          "Strongly Agree" = 2),
           response_value = 1,
           response_value_direction = ifelse(str_detect(response, "Disagree"), -1, 1),
           response_neutral = response == "Neutral")

  df_ph_cont <- df_cont %>% 
    count(indicator, wt = sum(score > 0, na.rm = TRUE)) %>% 
    mutate(response_value_direction = max(n) + 2 - n,
           response = "placeholder") %>% 
    select(-n)
  
  
  df_viz_cont <- df_cont %>% 
    bind_rows(df_ph_cont) %>% 
    mutate(response = factor(response, response_order),
           response_value_pos = ifelse(str_detect(response, "Agree"), 1, 0))
  
  
  df_resp_cont <- df_viz_cont %>% 
    mutate(response = recode(response,
                             # "Strongly Disagree" = "Disagree",
                             "Strongly Agree"= "Agree")) %>% 
    count(indicator, response) %>% 
    group_by(indicator) %>% 
    mutate(share = n/sum(n),
           share = percent(share, 1)) %>% 
    ungroup()
  
  
  
  df_viz_cont %>% 
    ggplot(aes(response_value_direction, fct_reorder(indicator, response_value_pos, sum, na.rm = TRUE),
               fill = fct_rev(response))) +
    geom_col(alpha = .9) +
    geom_vline(xintercept = -23:30, color = "white") +
    geom_vline(xintercept = 0, color = "#999999", size = 1.1) +
    annotate(geom = "text",
             x = -.5, y = Inf, hjust = 1,
             label = "Disagree", family = "Source Sans Pro", color = matterhorn) +
    annotate(geom = "text",
             x = .5, y = Inf, hjust = 0,
             label = "Agree", family = "Source Sans Pro", color  = matterhorn) +
    annotate(geom = "text",
             x = 25, y = Inf, hjust = 0,
             label = "Neutral", family = "Source Sans Pro", color  = matterhorn) +
    # facet_grid(fct_rev(group) ~ ., scales = "free_y", space = "free") +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_fill_manual(values = fill_col) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("Of the {cntry_reporting} countries that responded, { df_resp_cont %>%
    filter(response == 'Agree') %>%
    pull()} would continue with a similar model if OHA stopped HFR") %>% toupper,
         subtitle = glue("{df_resp_cont %>%
    filter(response == 'Strongly Disagree') %>%
    pull(n)} countries indicated they would 'very likely stop'"),
         caption = source_info) +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          legend.position = "none")
  
  
  si_save("Graphics/HFR_continue.svg") 
  
  df_viz_cont %>% 
    filter(response == "Strongly Disagree") %>% 
    pull(country) %>% 
    clipr::write_clip()

# DUPLICATIVE -------------------------------------------------------------

  df %>% 
    select(country = country_affiliation, starts_with("x4b_")) %>% 
    rename(system = 2) %>% 
    mutate(duplicative = str_detect(system, "completely duplicative")) %>% 
    count(duplicative) %>% 
    mutate(share = n/sum(n))
  
# RESOURCES ---------------------------------------------------------------

  
  df_resources <- df %>% 
    select(country = country_affiliation, starts_with("x9_")) %>% 
    pivot_longer(-country,
                 names_to = "indicator",
                 values_to = "response") %>% 
    mutate(indicator = indicator %>% 
             str_extract("(?<=ip_).*") %>% 
             str_replace_all("_", " ") %>% 
             str_to_sentence(),
           indicator = recode(indicator,
                              "Data storage management through a server" = "Data storage/management (server)",
                              "Data analysis communication" = "Data analysis/communication",
                              "Data collection processing" = "Data collection/processing")) %>% 
    count(indicator, response) 


  df_resources <- df_resources %>%
    mutate(sort_order = case_when(str_detect(response, "but") ~ n),
           fill_alpha = ifelse(str_detect(response, "Needed"), .9, .1),  
           response = factor(response, c("Exists currently",
                                         "Needed & resources would be available", 
                                         "Needed, but no resources to support")
           )) 

  text_resources <-  df_resources %>%
    filter(indicator == "Data storage/management (server)") %>%
    mutate(share = n/sum(n)) %>%
    filter(response == "Needed, but no resources to support") %>%
    pull() %>%
    percent()

  df_resources %>% 
    ggplot(aes(n, fct_reorder(indicator, sort_order, sum, na.rm = TRUE), 
               fill = response, alpha = fill_alpha)) +
    geom_col() +
    geom_vline(xintercept = 0:(cntry_reporting+1), color = "white") +
    geom_vline(xintercept = cntry_reporting/2, color = matterhorn, linetype = "dotted") +
    scale_x_continuous(expand=c(.005, .005), position = "top") +
    scale_fill_manual(values = fill_col_resources) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         title = "MOST COUNTRIES INDICATED A NEED FOR SERVER ACCESS TO PICK UP HFR ON THEIR OWN",
         subtitle = glue("{text_resources} of countries indicated they would not get access to a server in the future"),
         caption = source_info) +
    si_style_nolines() +
    theme(legend.position = "none")
  
  
  si_save("Graphics/HFR_resources.svg") 
  
  
  

# COMBO -------------------------------------------------------------------

  df_resources2 <- df %>% 
    select(country = country_affiliation, starts_with("x9_")) %>% 
    pivot_longer(-country,
                 names_to = "indicator",
                 values_to = "response") %>% 
    mutate(response_value = recode(response,
                                   "Exists currently" = 5,
                                   "Needed & resources would be available" = 3, 
                                   "Needed, but no resources to support" = 0)) %>% 
    count(country, wt = response_value, name = "readiness_score")
  
  
  df_cont2 <- df %>% 
    select(country = country_affiliation, starts_with("x8_")) %>% 
    rename(willingness = 2) %>% 
    mutate(willingness_c = glue("x{willingness}"),
           response = recode(willingness_c, 
                             x1 = "Strongly Disagree",
                             x2 = "Disagree",
                             x3 = "Neutral",
                             x4 = "Agree",
                             x5 = "Strongly Agree"))
  
  df_process <- df %>% 
    select(country = country_affiliation, starts_with("x4a_")) %>% 
    pivot_longer(-country,
                 names_to = "indicator",
                 values_to = "response") %>% 
    mutate(process_exists = response != "No") %>% 
    select(country, process_exists)
  
  df_combo <- df_resources2 %>% 
    tidylog::left_join(df_cont2) %>% 
    tidylog::left_join(df_response) %>% 
    tidylog::left_join(df_process) %>% 
    arrange(willingness) %>% 
    mutate(response = fct_inorder(response))


  df_combo %>% 
    ggplot(aes(readiness_score, willingness, size = site_mech_ind_combos,
               fill = response, color = response,
               )) +
    geom_point(data = . %>% filter(process_exists),
               shape = 21,
               position = position_jitter(height = .3, width =  .3, seed = 42), 
               alpha = .6) +
    geom_point(data = . %>% filter(!process_exists),
               shape = 21, fill = NA,
               position = position_jitter(height = .3, width =  .3, seed = 42), 
               alpha = .6) +
    geom_text_repel(aes(label = country), size =3, color = matterhorn,
                    force = 50) +
    scale_fill_manual(values = append(brewer.pal(5, "BrBG"), "#ffffff"), aesthetics = c("color", "fill")) +
    scale_size(range = c(2, 10)) +
    coord_cartesian(clip = "off") +
    labs(title = "'LARGER' COUNTRIES TEND TO BE MORE LIKELY TO CONTINUE WITH HFR IF STOPPED BY OHA AND HAVE THE RESOURCES TO MANAGE IT" %>% str_wrap,
         caption = source_info) +
    si_style() +
    theme(axis.text = element_blank(),
          legend.position = "none")
  
  si_save("Graphics/HFR_willing-ready.svg")
  

# MATRIX ------------------------------------------------------------------

  
  df_cont3 <- df %>% 
    select(country = country_affiliation, starts_with("x8_")) %>% 
    rename(willingness = 2) %>% 
    mutate(response_w = recode(as.character(willingness), 
                             "1" = "Disagree",
                             "2" = "Disagree",
                             "3" = "Neutral",
                             "4" = "Agree",
                             "5" = "Agree"))
  
  
  df_sys <- df %>% 
    select(country = country_affiliation, starts_with("x4a_")) %>% 
    rename(response_sys= 2) %>%
    mutate(has_sys = response_sys != "No")
  
  tidylog::left_join(df_cont3, df_sys) %>% 
    tidylog::left_join(df_response) %>% 
    arrange(response_w, has_sys) %>% 
    filter(response_w == "Agree",
           has_sys == FALSE) %>%
    mutate(country = country %>% 
             recode("Democratic Republic of the Congo" = "DRC") %>% 
             str_replace("West Africa Region", "WAR") %>% 
             str_replace("Western Hemisphere Region", "WHR") %>% 
             str_replace("Asia Region", "AR")) %>% 
    arrange(desc(site_mech_ind_combos)) %>% 
    pull(country) %>% 
    clipr::write_clip()
    ggplot(aes(reponse_w, has_sys)) +
    geom_point()
    prinf()
  
# STORIES -----------------------------------------------------------------

  df %>% 
    select(country = country_affiliation, starts_with("x10_")) %>% 
    rename(stories = 2) %>% 
    filter(stories %ni% c(NA, "NA")) %>% 
    pull() %>% 
    clipr::write_clip()
  
  