# PROJECT: Data Visualization Training Summer Series
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   07d6bde9
# LICENSE: MIT
# DATE: 2022-07-07
# NOTES: Tim Essam | SI
  

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
  library(glue)

# REF ID for plots
  ref_id <- "07d6bde9"

# Functions
# Plot extras
plot_statics <- function(...) {
  ggplot2::theme_gray() %+replace%
    theme(
      legend.position = "bottom",
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = grey30k),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      axis.line = element_blank()
    )
}



# LOAD DATA ============================================================================
load_secrets()

id <- "https://docs.google.com/spreadsheets/d/1uyL1zDZTsoEZN6aeHGoTOQmKmOmYQTXcB6dAkp2vRCo/edit#gid=0"
df <- read_sheet(id)

blue <- "#3C67BE"
orange <- "#F27A2A"

# MUNGE ============================================================================

# Rerrange the country order, move Total to the top (0) or bottom (Inf)
df1 <-
  df %>%
  mutate(
    orig = ou_long,
    ou = fct_reorder(ou, Local),
    ou_long = fct_reorder(ou_long, Local),
    ou = fct_relevel(ou, "Total", after = Inf),
    ou_long = fct_relevel(ou_long, "Total", after = Inf),
    orig = fct_relevel(orig, "Total", after = Inf)
  )

# Pivoting it long for stacking/arranging, eventually drop this in favor
# of a simple long dataset.
df_long <-
  df1 %>%
  pivot_longer(
    cols = Local:International,
    names_to = "type",
    values_to = "share"
  ) %>%
  mutate(fill_color = case_when(
    type == "Local" ~ blue,
    TRUE ~ orange
  )) %>%
  mutate(
    facet_order = case_when(
      ou_long == "Total" ~ "",
      goal == 1 ~ "> 70%",
      TRUE ~ "< 70%"
    ),
    facet_order = fct_relevel(facet_order, c("", "> 70%", "< 70%"))
  ) %>%
  mutate(recolor = case_when(
    facet_order == "" & type == "Local" ~ "#34344A",
    goal == 1 & type == "Local" ~ "#80475E",
    goal == 0 & type == "Local" ~ "#CC5A71",
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



# original ----------------------------------------------------------------

# Original plot
lp_plot <- function(df, yvar, ...) {
  df %>%
    ggplot(aes(y = {{ yvar }}, x = share)) +
    geom_col(aes(fill = type), width = 0.5) +
    scale_fill_manual(values = c("Local" = blue, "International" = orange)) +
    geom_label(aes(label = percent(share)),
      size = 8 / .pt,
      position = position_stack(vjust = 0.5)
    ) +
    labs(
      x = NULL, y = NULL,
      title = "FY 21 Local vs. International Budget - All Agencies",
      fill = NULL
    ) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics()
}

lp_plot(df_long, orig)

si_save("Images/lp_remake_base.png")


# sorted ------------------------------------------------------------------

# Sort the data
lp_plot(df_long, ou_long)

si_save("Images/lp_remake_base_sorted_1.png")


# color tinkering ---------------------------------------------------------

# Try it in black and white
lp_plot(df_long, ou_long) +
  scale_fill_manual(values = c("Local" = grey80k, "International" = grey20k))
si_save("Images/lp_remake_base_recolor_2bw.png")

# Add color
lp_plot(df_long, ou_long) +
  scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k))

si_save("Images/lp_remake_base_recolor_2.png")


# declutter ---------------------------------------------------------------

lp_plot_dc <- function(df, yvar) {
  df_long %>%
    ggplot(aes(y = {{ yvar }}, x = share)) +
    geom_col(aes(fill = type), width = 0.85) +
    facet_grid(facet_order ~ ., scales = "free_y", drop = T, space = "free") +
    scale_fill_manual(values = c("Local" = old_rose, "International" = grey10k)) +
    geom_text(
      data = . %>% filter(type == "Local"),
      aes(label = percent(share, 1)), size = 8 / .pt,
      hjust = 0,
      family = "Source Sans Pro"
    ) +
    labs(
      x = NULL, y = NULL,
      title = "FY 21 Local vs. International Budget - All Agencies",
      fill = NULL
    ) +
    scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) +
    coord_cartesian(clip = "off", expand = F) +
    plot_statics() +
    theme(legend.position = "none")
}

lp_plot_dc(df_long, ou_long)
si_save("Images/lp_remake_base_declutter_3.png")


lp_plot_dc(df_long, ou) +
  si_style(facet_space = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous(
    labels = percent, position = "bottom", breaks = seq(.25, 1, 0.25),
    limits = c(0, 1.15)
  )
si_save("Images/lp_remake_base_declutter_4.png")


# recolor  -----------------------------------------------------------------

# Some major work here. First, let's drop the stacked data b/c it makes things a PITA.
# We will put a bar running 0 - 1 in the background then use color splashed to emphasize things
df_long_flt <-
  df_long %>%
  filter(type != "International")


df_long_flt %>%
  ggplot(aes(y = ou, x = share)) +
  annotate("rect", xmin = 0.7, xmax = 0.71, fill = grey10k, alpha = 0.55, ymin = -Inf, ymax = Inf) +
  geom_col(aes(fill = recolor), width = 0.85) +
  geom_vline(xintercept = seq(0, 1, 0.25), size = 0.25, color = "white") +
  geom_vline(xintercept = 0, size = 0.5, color = grey70k) +
  geom_vline(xintercept = 1, size = 0.25, color = grey70k, linetype = "dotted") +
  facet_grid(facet_order ~ ., scales = "free_y", drop = T, space = "free") +
  # scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
  scale_fill_identity() +
  geom_text(aes(label = percent(share, 1)),
    size = 9 / .pt,
    hjust = -0.1,
    family = "Source Sans Pro"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "FY 21 Local vs. International Budget - All Agencies",
    fill = NULL
  ) +
  scale_x_continuous(
    labels = percent, position = "top", breaks = seq(.25, 1, 0.25),
    limits = c(0, 1.15)
  ) +
  coord_cartesian(clip = "off", expand = F) +
  plot_statics() +
  si_style_nolines(facet_space = 0.5) +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  )


si_save("Images/lp_remake_base_recolor_5.png")


# Retitle -----------------------------------------------------------------

achv_goal <-
  df_long_flt %>%
  filter(share >= 0.7) %>%
  tally() %>%
  pull()


achv_goal <-
  df_long_flt %>%
  filter(share < 0.7, ou != "Total") %>%
  tally() %>%
  pull()


df_long_flt %>%
  ggplot(aes(y = ou, x = share)) +
  annotate("rect", xmin = 0.7, xmax = 0.71, fill = grey10k, alpha = 0.55, ymin = -Inf, ymax = Inf) +
  geom_col(aes(fill = recolor), width = 0.85) +
  geom_vline(xintercept = seq(0, 1, 0.25), size = 0.25, color = "white") +
  geom_vline(xintercept = 0, size = 0.5, color = grey70k) +
  geom_vline(xintercept = 1, size = 0.25, color = grey70k, linetype = "dotted") +
  facet_grid(facet_order ~ ., scales = "free_y", drop = T, space = "free") +
  # scale_fill_manual(values = c("Local" = old_rose, "International" = grey20k)) +
  scale_fill_identity() +
  geom_text(aes(label = percent(share, 1)),
    size = 9 / .pt,
    hjust = -0.1,
    family = "Source Sans Pro"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "IN FY21, FOUR OPERATING UNITS HAD ACHIEVED THE LOCAL PARTNER BUDGET SHARE GOAL OF 70%",
    subtitle = "Twenty four operating units are still short of the goal.",
    fill = NULL,
    caption = glue("Source: Local Partner Team | Ref ID: {ref_id}")
    ) +
  scale_x_continuous(
    labels = percent, position = "top", breaks = seq(.25, 1, 0.25),
    limits = c(0, 1.15)
  ) +
  coord_cartesian(clip = "off", expand = F) +
  plot_statics() +
  si_style_nolines(facet_space = 0.5) +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  )

si_save("Images/lp_remake_base_title_6.png")

# SPINDOWN ============================================================================

  # What would a diverging bar graph version look like?
  df_div <- df %>% mutate(International = -International)
  

  df_div %>% 
    mutate(ou = fct_reorder(ou, Local)) %>% 
    ggplot(aes(y = ou)) +
    geom_col(aes(x = Local), fill = scooter, width = 0.5) +
    geom_col(aes(x = International), fill = old_rose, width = 0.5) +
    geom_vline(xintercept = 0, size = 1, color = grey90k) +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(lim = c(-1, 1), labels = percent, 
                       position = "top") 
    
    si_save("Images/lp_viz_remake_diverging.svg")
    