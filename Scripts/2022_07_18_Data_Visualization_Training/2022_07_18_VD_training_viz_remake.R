# PURPOSE: Munge and Analysis of Local Partners Data to show a successful visualization remake
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-07-06
# NOTES: For the remake section of the training

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(googlesheets4)
    
    
  # Functions  
  

# LOAD DATA ============================================================================  
  load_secrets()
  
  id <- "https://docs.google.com/spreadsheets/d/1uyL1zDZTsoEZN6aeHGoTOQmKmOmYQTXcB6dAkp2vRCo/edit#gid=0"
  df <- read_sheet(id)
  
  blue <- "#3C67BE"
  orange <- "#F27A2A"

# MUNGE ============================================================================
  
  df1 <- 
    df %>% 
    mutate(orig = ou_long,
           ou = fct_reorder(ou, Local), 
           ou_long = fct_reorder(ou_long, Local),
           ou = fct_relevel(ou, "Total", after = Inf),
           ou_long = fct_relevel(ou_long, "Total", after = Inf),
           orig = fct_relevel(orig, "Total", after = Inf)) 
  
  df_long <- 
    df1 %>% 
    pivot_longer(cols = Local:International, 
                 names_to = "type",
                 values_to = "share") %>% 
    mutate(fill_color = case_when(
      type == "Local" ~ blue,
      TRUE ~ orange)
    ) %>% 
    mutate(facet_order = case_when(
      ou_long == "Total" ~ "",
      goal == 1 ~ "> 70%",
      TRUE ~ "< 70%"),
      facet_order = fct_relevel(facet_order, c("> 70%", "< 70%", ""))) %>% 
    mutate(recolor = case_when(
      facet_order == "" & type == "Local" ~ old_rose,
      goal == 1 & type == "Local" ~ "#7f001c",
      goal == 0 & type == "Local" ~ "#ff989f",
      TRUE ~ grey20k
    ))
  
# VIZ ============================================================================
    
  # Recreate basic plot but sorted
  # df %>% 
  #   ggplot(aes(y = ou_long)) +
  #   geom_col(aes(x = 1), fill = orange) +
  #   geom_col(aes(x = local), fill = blue) +
  #   labs(x = NULL, y = NULL, 
  #        title = "FY 21 Local vs. International Budget - All Agencies")
  
# Plot extras
  plot_statics <- function(...){
    ggplot2::theme_minimal() %+replace%
      theme(legend.position = "bottom", 
            axis.ticks = element_blank(), 
            panel.grid.major = element_line(color = grey30k), 
            panel.background = element_rect(fill = "white")) 
  }
  
# Original plot  
  df_long %>% 
    ggplot(aes(y = orig, x = share)) +
    geom_col(aes(fill = type), width = 0.5)+
    scale_fill_manual(values = c("Local" = blue, "International" = orange)) +
    geom_label(aes(label = percent(share)), size = 8/.pt, 
               position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics()
  

  si_save("Images/lp_remake_base.png")

# Sort the data
  df_long %>% 
    ggplot(aes(y = ou_long, x = share)) +
    geom_col(aes(fill = type), width = 0.5)+
    scale_fill_manual(values = c("Local" = blue, "International" = orange)) +
    geom_label(aes(label = percent(share)), size = 8/.pt, 
               position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics()
    
  si_save("Images/lp_remake_base_sorted.png")
  
# Add better colors
  # Sort the data
  df_long %>% 
    ggplot(aes(y = ou_long, x = share)) +
    geom_col(aes(fill = type), width = 0.5)+
    scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
    geom_label(aes(label = percent(share)), size = 8/.pt, 
               position = position_stack(vjust = 0.5), 
               family = "Source Sans Pro") +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics()

  si_save("Images/lp_remake_base_recolor.png")
  

# declutter ---------------------------------------------------------------

  df_long %>% 
    mutate(facet_order = case_when(
      ou_long == "Total" ~ "",
      goal == 1 ~ "> 70%",
      TRUE ~ "< 70%"),
    facet_order = fct_relevel(facet_order, c("> 70%", "< 70%", "")))%>% 
    ggplot(aes(y = ou_long, x = share)) +
    geom_col(aes(fill = type), width = 0.75) +
    facet_grid(facet_order~., scales = "free_y", drop = T, space = "free") +
    scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
    geom_text(data = . %>% filter(type == "Local"), 
               aes(label = percent(share, 1)), size = 8/.pt, 
               hjust = 0, 
              family = "Source Sans Pro") +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics() +
    theme(legend.position = "none")
  
  si_save("Images/lp_remake_base_declutter1.png")
  
  
  df_long %>% 
    mutate(facet_order = case_when(
      ou_long == "Total" ~ "",
      goal == 1 ~ "> 70%",
      TRUE ~ "< 70%"),
      facet_order = fct_relevel(facet_order, c("> 70%", "< 70%", "")))%>% 
    ggplot(aes(y = ou_long, x = share)) +
    geom_col(aes(fill = type), width = 0.75) +
    geom_vline(xintercept = seq(0, 1, 0.25), size = 0.25, color = "white") +
    facet_grid(facet_order~., scales = "free_y", drop = T, space = "free") +
    scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
    geom_text(data = . %>% filter(type == "Local"), 
              aes(label = percent(share, 1)), size = 8/.pt, 
              hjust = 0, 
              family = "Source Sans Pro") +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .25)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics() +
    theme(legend.position = "none")
  
  si_save("Images/lp_remake_base_declutter2.png")
  

# RECOLOR -----------------------------------------------------------------
  
  # Some major work here. First, let's drop the stacked data b/c it makes things a PITA.
  # We will put a bar running 0 - 1 in the background then use color splashed to emphasize things
  
  
  df_long_flt <- 
    df_long %>% 
    filter(type != "International") 
    
    
    df_long_flt %>% 
      ggplot(aes(y = ou_long, x = share)) +
    geom_col(aes(fill = recolor), width = 0.75) +
    geom_vline(xintercept = seq(0, 1, 0.25), size = 0.25, color = "white") +
    geom_vline(xintercept = 0, size = 0.5, color = grey70k) +
    geom_vline(xintercept = 0.7, size = 0.25, color = grey70k, linetype = "dotted") +  
    facet_grid(facet_order~., scales = "free_y", drop = T, space = "free") +
    #scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
    scale_fill_identity( )+
    geom_text(aes(label = percent(share, 1)), size = 8/.pt, 
              hjust = 0, 
              family = "Source Sans Pro") +
    labs(x = NULL, y = NULL, 
         title = "FY 21 Local vs. International Budget - All Agencies",
         fill = NULL) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .25)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics() +
    si_style_nolines()+
    theme(legend.position = "none")
  
  si_save("Images/lp_remake_base_declutter2.png")
  
  
# SPINDOWN ============================================================================

