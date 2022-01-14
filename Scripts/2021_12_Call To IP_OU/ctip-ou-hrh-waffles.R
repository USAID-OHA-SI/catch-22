# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | T.Essam | USAID
# PURPOSE:  Call with IP/OU - HRH Waffle Making Time FY21
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED: 2021-12-08
# NOTE: derived from catch-22/Scripts/2021115_Global Planning Meeting/gpm_hrh_waffles.R

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(waffle)

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

plot_title <- ""


# IMPORT ------------------------------------------------------------------
# Create data with updated FY21 numbers from HRH team

hrh_fp <- data.frame(
  name = c("USAID", "PEPFAR"),
  value = c(101, 88)
)

waffle(hrh_fp, rows = 9, flip = T, colors = c("#5bb5d5", "#d67288")) 

si_save("Graphics/HRH_waffle.svg")

hrh_bgt <- data.frame(
  name = c("USAID", "PEPFAR"),
  value = c(39, 57)
)

waffle(hrh_bgt, rows = 10, flip = T, colors = c("#5bb5d5", "#d67288")) 
