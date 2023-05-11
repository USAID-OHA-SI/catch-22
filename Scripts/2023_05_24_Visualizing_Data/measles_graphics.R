# PROJECT: Data Viz Training for USG Agencies
# PURPOSE: Munge and Analysis measles data for training
# AUTHOR: Tim Essam | SI
# REF ID:   fe9bfecd
# LICENSE: MIT
# DATE: 2023-05-10
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(gagglr)
library(rcartocolor)
library(RColorBrewer)

# REF ID for plots
ref_id <- "fe9bfecd"

# Functions


# LOAD DATA ============================================================================

measles <- read.csv("https://raw.githubusercontent.com/blmoore/blogR/master/data/measles_incidence.csv",
  header = T, skip = 2, stringsAsFactors = F, na.strings = "-"
) %>%
  janitor::clean_names()

df_long <-
  measles %>%
  pivot_longer(alabama:wyoming,
    names_to = "state",
    values_to = "cases"
  ) %>%
  summarise(cases = sum(cases, na.rm = T), .by = c(year, state)) %>%
  mutate(
    cases = ifelse(cases == 0.00, NA_real_, cases),
    state = str_to_title(str_replace_all(state, "_", " "))
  )

summary(df_long$cases)

# TIME SERIES VIZ  ============================================================================

# Highlight states with outbreaks
yrs <- c(1928, 1934, 1936, 1938, 1942, 1943, 1955, 1957, 1958, 1963)

ts_plot1 <- df_long %>%
  summarise(cases = sum(cases, na.rm = T), .by = year) %>%
  ggplot(aes(x = year, y = cases)) +
  geom_area(fill = grey10k, alpha = 0.75) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = label_number_si(), expand = c(0.025, 0)) +
  si_style_ygrid() +
  scale_x_continuous(breaks = seq(1930, 2010, by = 5), expand = c(0.025, 0)) +
  labs(
    x = NULL, y = NULL, title = "MEASLES CASES PER 100K PEOPLE DRASTICALLY DECLINED STARTING IN THE MID 1960s",
    caption = "Source: Weekly Measles Incidence, 1928-2003 | Project Tycho",
    subtitle = " "
  )

si_save("../catch-22/AI/ts_plot1_base.svg")

ts_plot1 +
  geom_vline(xintercept = 1963, linewidth = 1, color = grey90k) +
  annotate("text",
    label = "Vaccine introduced", x = 1963, y = 33e3,
    vjust = 1, hjust = 0
  )

si_save("../catch-22/AI/ts_plot1_base2.svg")


ts_state_plot <- df_long %>%
  mutate(rank = dense_rank(-cases), .by = year) %>%
  mutate(slabel = case_when(
    year %in% yrs & rank == 1 ~ state,
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(y = cases, x = year)) +
  geom_point(color = grey50k) +
  # geom_point(
  #   position = position_jitter(width = .2, seed = 0), alpha = .5, color = grey50k
  # ) +
  # ggrepel::geom_text_repel(aes(label = slabel)) +
  si_style_ygrid() +
  scale_x_continuous(breaks = seq(1930, 2010, by = 5), expand = c(0.025, 0)) +
  scale_y_continuous(labels = label_number_si(), expand = c(0.025, 0)) +
  labs(
    x = NULL, y = NULL, title = "MEASLES CASES 100K PEOPLE DRASTICALLY DECLINED STARTING IN THE MID 1960s",
    caption = "Source: Weekly Measles Incidence, 1928-2003 | Project Tycho",
    subtitle = "Each point represents total cases per state in a given year."
  )

si_save("../catch-22/AI/ts_state_base.svg")

ts_state_plot2 <-
  df_long %>%
  mutate(rank = dense_rank(-cases), .by = year) %>%
  mutate(slabel = case_when(
    year %in% yrs & rank == 1 ~ state,
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(y = cases, x = year)) +
  geom_point(
    data = . %>% filter(is.na(slabel)),
    position = position_jitter(width = .15, seed = 0), alpha = .5, color = grey50k
  ) +
  geom_point(
    data = . %>% filter(!is.na(slabel)),
    position = position_jitter(width = .15, seed = 0),
    color = old_rose
  ) +
  ggrepel::geom_text_repel(aes(label = slabel),
    color = old_rose,
    size = 10 / .pt, family = "Source Sans Pro"
  ) +
  # ggrepel::geom_text_repel(aes(label = slabel)) +
  si_style() +
  scale_x_continuous(breaks = seq(1930, 2010, by = 5), expand = c(0.025, 0)) +
  scale_y_continuous(labels = label_number_si(), expand = c(0.025, 0)) +
  labs(
    x = NULL, y = NULL, title = "MEASLES CASES PER 100K PEOPLE DRASTICALLY DECLINED STARTING IN THE MID 1960s",
    caption = "Source: Weekly Measles Incidence, 1928-2003 | Project Tycho",
    subtitle = "Each point represents total cases per state in a given year."
  )

si_save("../catch-22/AI/ts_state_base2.svg")


ts_state_plot2 +
  geom_vline(xintercept = 1963, linewidth = 1, color = grey90k) +
  annotate("text",
    label = "Vaccine introduced", x = 1963, y = 3e3,
    vjust = 1, hjust = 0
  )
si_save("../catch-22/AI/ts_state_base3.svg")


# HEATMAP ============================================================================


# https://benjaminlmoore.wordpress.com/2015/04/09/recreating-the-vaccination-heatmaps-in-r/

# But maybe more effectively - we can use a heatmap

cols <- c(
  colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e"))(10),
  colorRampPalette(c("#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e"), bias = 2)(90)
)

cols2 <- c(
  colorRampPalette(c("white", grey10k, grey20k, grey30k, grey40k, grey50k))(10),
  colorRampPalette(c(grey50k, grey60k, grey70k, grey80k, grey90k, "black"), bias = 2)(90)
)

prpl <- c(
  colorRampPalette(c("white", "#E9DDFF", "#CFC3FF", "#B5AAF9", "#9E94E0", "#877EC9"))(10),
  colorRampPalette(c("#7069B2", "#5A559B", "#454185", "#454185", "#2F2E6F", "#171D5A", "#000A45"), bias = 2)(90)
)


plot_a <- df_long %>%
  ggplot(aes(y = state, x = year, fill = cases)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = NULL, y = NULL, title = "MEASLES CASES PER 100K PEOPLE DRASTICALLY DECLINED STARTING IN THE MID 1960s",
    caption = "Source: Weekly Measles Incidence, 1928-2003 | Project Tycho",
    subtitle = " ",
    fill = "Measles cases per 100k"
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1930, 2010, by = 10)
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.5, -.13),
    legend.direction = "horizontal",
    legend.text = element_text(colour = grey80k),
    plot.margin = grid::unit(c(.5, .5, 1.5, .5), "cm"),
    axis.text.y = element_text(
      size = 8,
      hjust = 1
    ),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    title = element_text(hjust = -.07, face = "bold", vjust = 1)
  )


plot_a +
  scale_fill_gradientn(
    colours = cols2,
    # na.value = "#f1f3e2",
    na.value = "#f9ebed",
    # na.value = "white",
    breaks = seq(0, 3e3, by = 1e3),
    limits = c(0, 4000),
    labels = c("0k", "1k", "2k", "3k"),
    guide = guide_colourbar(
      ticks = T, nbin = 50,
      barheight = .5, label = T,
      barwidth = 10
    )
  ) +
  geom_vline(xintercept = 1963, linewidth = 0.5, color = grey90k) +
  annotate("text",
    label = "Vaccine introduced", x = 1963, y = 53,
    vjust = 1, hjust = 0, size = I(3)
  ) +
  geom_vline(xintercept = 2000, linewidth = 0.5, color = denim) +
  annotate("text",
    label = "Endemic Measles Eliminated", x = 2000, y = 53,
    vjust = 1, hjust = 0, size = I(3)
  )


si_save("../catch-22/AI/measles_heatmap_base_bw_annotate2.svg")

plot_b <- plot_a +
  geom_vline(xintercept = 1963, linewidth = 0.5, color = grey90k) +
  annotate("text",
    label = "Vaccine introduced", x = 1963, y = 53,
    vjust = 1, hjust = 0, size = I(3)
  )

plot_c <- plot_b +
  geom_vline(xintercept = 2000, linewidth = 0.5, color = old_rose)
