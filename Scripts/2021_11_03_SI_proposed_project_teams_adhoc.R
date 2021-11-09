# Purpose: Analysis of Proposed SI Tasks/Projects
# Author: Tim Essam | SI,
# Date: 2021-11-03
# Notes: Ad hoc request from K.A.T.


# LIBRARIES ---------------------------------------------------------------

  library(googlesheets4)
  library(glitr)
  library(glamr)
  library(gisr)
  library(tidyverse)
  library(gophr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(here)


# FETCH DATA --------------------------------------------------------------

google_id <- "1Pl4r3fmO_fmfdu0OTROS6tw-b88VbFqzd7MZfwE-WJM"
df <- read_sheet(google_id)


# VIZ ---------------------------------------------------------------------


plot_project <- function(var){
  df %>%
    filter(!is.na({{var}})) %>% 
    mutate(project = str_wrap(project, width = 30)) %>%
    group_by({{var}}) %>%
    mutate(tot_proj = n()) %>%
    ungroup() %>%
    mutate(person_order = fct_reorder({{var}}, tot_proj, .desc = T)) %>%
    ggplot(aes(x = {{var}}, y = project, fill = role)) +
    geom_tile(size = 0.5, color = "white") +
    facet_wrap(~person_order, scales = "free") +
    si_style_nolines(facet_space = 0.25) +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_blank()) +
    scale_fill_manual(values = c("lead" = genoa, "support" = trolley_grey_light)) +
    scale_y_discrete(lim = rev) +
    coord_cartesian(clip = "off", expand = F)
}

plot_project(team_proposed) + labs(title = "PROPSED SI ROLES", x = NULL, y = NULL)


plot_project_grid <- function(var){
  df %>%
    mutate(project = str_wrap(project, width = 50)) %>%
    group_by({{var}}) %>%
    mutate(tot_proj = n()) %>%
    ungroup() %>%
    mutate(person_order = fct_reorder({{var}}, tot_proj, .desc = T)) %>%
    ggplot(aes(x = {{var}}, y = project, fill = role)) +
    geom_tile(size = 0.5, color = "white") +
    facet_grid(~person_order, scales = "free") +
    si_style(facet_space = 0.25) +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_blank()) +
    scale_fill_manual(values = c("lead" = genoa, "support" = trolley_grey_light)) +
    scale_y_discrete(lim = rev)
}

plot_project_grid(team_proposed)

