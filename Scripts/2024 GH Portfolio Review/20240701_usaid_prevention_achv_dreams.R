# PROJECT:  groundhogday
# AUTHOR:   K.Srikanth | USAID
# PURPOSE:  FY24Q2 prevention indicators target achv
# REF ID:   3adb8fa1 
# LICENSE:  MIT
# DATE:  2024-05-29
# NOTE:     Adapted from rebootTZ FY21Q3 partner revide; Updated for FY23Q4 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(systemfonts)
library(glitr)
library(glamr)
library(gophr)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gisr)
library(sf)
library(googlesheets4)

# DREAMS -----------------------------------------------------------------

#DREAMS PREP
# Google sheet ID for DREAMS DSNU list
dsnu_g_id <- "1oJZyXtvExN9UoQ6krFHERMoXEgCQgsHLf0tVIbmsi_o"

#MSD path
dsnu_msd_path <- si_path() %>% 
  return_latest("PSNU_IM_DREAMS_FY22-24")

#import DREAMS DSNU crosswalk
#recommend adding psnu_uids to this sheet
dsnu_list <- read_sheet(dsnu_g_id) %>% 
  select(OU, `COP22 PSNU`, `DREAMS DSNU`, `COP 22/FY23 Implementing Agency`, `COP 23/FY24 Implementing Agency`)

#rename names
names(dsnu_list)<-c("operatingunit", "psnu", "dsnu", "agencies_FY23", "agencies_FY24")

#import msd and filter to current year
df_msd_dreams <- read_psd(dsnu_msd_path) %>% 
  filter(fiscal_year == metadata$curr_fy)

msd_dsnu_xwalk <- df_msd_dreams %>% 
  count(operatingunit, operatingunituid, psnu, psnuuid, cop22_psnu, cop22_psnuuid, dsnu, dsnuuid) %>% 
  mutate(raised_lvl = ifelse(psnuuid != cop22_psnuuid, TRUE, FALSE)) %>% 
  mutate(dsnu_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnu,
                              TRUE ~ dsnu),
         dsnuuid_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnuuid,
                                 TRUE ~ dsnuuid))

#join the new xwalk back to msd and then join the internal dsnu list to the msd
df_dreams_all <- df_msd_dreams %>% 
  left_join(msd_dsnu_xwalk) %>% 
  left_join(dsnu_list, by=c("operatingunit", "cop22_psnu" = "psnu", "dsnu_new"= "dsnu"))

# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()

ref_id <- "3adb8fa1" #id for adorning to plots, making it easier to find on GH

merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "OU_IM_FY22")

#select indicators
ind_sel <- c("PrEP_NEW", "VMMC_CIRC", "OVC_SERV", "KP_PREV","AGYW_PREV_D")

semi_annual <- c("OVC_SERV", "KP_PREV", "TB_PREV", "AGYW_PREV_D")


#caption info for plotting
metadata<- get_metadata(file_path, caption_note = "US Agency for International Development")

#current FY and quarter
curr_fy <- metadata$curr_fy
curr_qtr <- metadata$curr_qtr

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------

#all other indicators
df <- read_psd(file_path)   

df_achv <- df %>% 
  clean_indicator() %>%
  #rowwise() %>% 
  #mutate(TX_IIT= sum(TX_ML_IIT_less_three_mo, TX_ML_IIT_more_three_mo, na.rm = T)) %>% 
  #ungroup() %>%
  filter(funding_agency == "USAID",
         !operatingunit %in% c("Ukraine"), #exclude TZA and NGA globally for FY23Q4 because of DQA issues 
         fiscal_year == curr_fy,
         indicator %in% ind_sel) %>% 
  resolve_knownissues()


# MUNGE DREAMs --------------------------------------------------------------

df_dreams_achv <- df_dreams_all %>% 
  clean_indicator() %>% 
  mutate(usaid_dsnu_fy24 = ifelse(str_detect(agencies_FY24, "USAID"), "USAID", "non-USAID")) %>% 
  filter(usaid_dsnu_fy24 == "USAID",
         !operatingunit %in% c("Ukraine"), #exclude TZA and NGA globally for FY23Q4 because of DQA issues 
         fiscal_year == curr_fy,
         indicator %in% "AGYW_PREV_D") %>% 
  resolve_knownissues()

#subset to key indicators

## Aggregating results & targets at the global level for each indicator
df_dreams_achv <- df_dreams_achv %>% 
  bind_rows(df_dreams_achv %>% 
              mutate(country = "GLOBAL")) %>% 
  filter(standardizeddisaggregate %in% c("Total Denominator")) %>% 
  group_by(fiscal_year, country, indicator) %>% 
  summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)), 
            .groups = "drop")

#remove data points at Q1 for semi-annual indicators
if(metadata$curr_qtr == 1){
  df_achv <- df_dreams_achv %>%
    dplyr::mutate(cumulative = ifelse(indicator %in% semi_annual, 
                                      NA_real_, cumulative))
}

#calculate achievement and add colors 
df_dreams_achv <- df_dreams_achv %>% 
  adorn_achievement(metadata$curr_qtr)

#viz adjustments
df_dreams_achv_viz <- df_dreams_achv %>% 
  mutate(global_achv = case_when(country == "GLOBAL" ~ achievement),
         achievement = ifelse(country == "GLOBAL", NA, achievement),
         #indicator = factor(indicator, ind_sel),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

df_dreams_achv_viz <- df_dreams_achv_viz %>% 
  mutate(indicator_ss = ifelse(indicator %in% snapshot_ind, paste(indicator, "(SS)"), indicator),
         ind_w_glob_vals = case_when(country == "GLOBAL" & is.na(targets) ~ glue("**{indicator_ss}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
                                     country == "GLOBAL" ~ glue("**{indicator_ss}**<br><span style = 'font-size:11pt;'>{clean_number(cumulative)} / {clean_number(targets)}</span>")),
         indicator = factor(indicator, levels = ind_sel)) %>% 
  group_by(indicator) %>% 
  fill(ind_w_glob_vals, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(indicator) %>% 
  mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals))

# VIZ - ACHIEVEMENT GLOBAL -------------------------------------------------------


#@TO-DO: need to find a way to make this dynamic between quarters
lab_q4<-c("<75%","75-89%","90-110%","+110%")

lab_leg<-case_when(metadata$curr_qtr==1 ~  c("<15%","15-35%",">35%"),
                   metadata$curr_qtr==2 ~  c("<25%","25-40%","40-60%",">60%"),
                   metadata$curr_qtr==3 ~  c("<50%","50-65%","65-85%",">85%"),
                   TRUE ~ c("<75%","75-89%","90-110%","+110%"))%>%
  ## NOTE: Will need to add Q2, Q3, Q4 late,
  # metadata_msd$curr_qtr==3 ~  c("<75%","75-89%","90-110%","+110%"),
  # metadata_msd$curr_qtr==4 ~  c("<75%","75-89%","90-110%","+110%")) %>%
  paste("| SS:",lab_q4)

# VIZ - ACHIEVEMENT BY OU -------------------------------------------------------
df_dreams_achv_viz %>% 
  #rbind(df_dreams_achv_viz) %>% 
  mutate(achv_color = case_when(achv_color == "#e6e6e6" ~ "#697ebc",
                                achv_color == "#ffcaa2" ~ "#fbdc99",
                                achv_color == "#ff939a" ~ "#f8a27e",
                                TRUE ~ achv_color)) %>% 
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
              alpha = .7, size = 3) + 
  geom_point(aes(global_achv), size = 10, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0.3)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0.3),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish, breaks = seq(0, 1.25, .25), label = percent_format(1)) + #capping achievement at 110
  scale_color_identity(guide=guide_legend(direction = "horizontal", title.position = "top",
                                          title.hjust = 0), breaks=c("#f8a27e", "#FBDC99","#5BB5D5","#697EBC"),
                       labels=lab_leg,
                       name="Achievement: Cumulative indicators | Snapshot indicators") +    facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
  labs(x = NULL, y = NULL,
       title = glue("{metadata$curr_pd} Operating Unit achievement, USAID ") %>% toupper,
       subtitle = glue("Global achievement (large, labeled points) with OU achievement reference points <br>"),
       caption = glue("Target achievement capped at 110%
                        Source: {metadata$source} | USAID/OHA/SIEI | Ref ID: {ref_id}")) +
  si_style_nolines() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_markdown(),
    strip.text = element_markdown(),
    panel.spacing.y = unit(0, "lines"),
    legend.position="bottom")

si_save(glue("Graphics/{metadata$curr_pd}_achv_ou_withDREAMS.svg"))
si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_ou.png"))