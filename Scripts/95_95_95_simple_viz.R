# PURPOSE: QC data of Administrator briefer
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-05-14
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(ICPIutilities)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  


# LOAD DATA ============================================================================  

  qc_file <- return_latest(merdata, "OU")
  df <- read_msd(qc_file)
    
  df_sub <- filter(fundingagency == "USAID", 
                   indicator %in% c("HTS_TST", "HTS_TST_POS","TX_CURR", "TX_PVLS"), 
                   standardizeddisaggregate == "Total Numerator")
  
  df_mmd <- 
    df %>% 
    filter(fundingagency == "USAID", 
           indicator == "TX_CURR",
           str_detect(otherdisaggregate, "(3 to 5 months|6 or more months)")) %>% 
    reshape_msd(clean = T) %>% 
    filter(!period %in% c("FY20", "FY19", "FY21", "FY20Q1"), str_detect(period, "FY19", negate = T)) %>% 
    group_by(indicator, period) %>% 
    summarise(tot = sum(value, na.rm = T)) %>% 
    spread(period, tot) %>% 
    mutate(indicator = "TX_MMD",
           total = max(FY20Q2, FY20Q3, FY20Q4, FY21Q1))
 
  
# MUNGE ============================================================================
  
  # Reshape and filter for quaters we need CY20 (FY21Q2)
  #FY20Q2-FY21Q1
  qc <- 
    df %>% 
    reshape_msd(clean = T) %>% 
    filter(!period %in% c("FY20", "FY19", "FY21", "FY20Q1"), str_detect(period, "FY19", negate = T)) %>% 
    group_by(indicator, period) %>% 
    summarise(tot = sum(value, na.rm = T)) %>% 
    spread(period, tot) %>% 
    mutate(total = case_when(
      indicator %in% c("HTS_TST", "HTS_TST_POS") ~ FY20Q2 + FY20Q3 + FY20Q4 + FY21Q1,
      TRUE ~ max(FY20Q2, FY20Q3, FY20Q4, FY21Q1))) 
  print(qc)
  
# VIZ ============================================================================

  #  
  qc %>% 
    bind_rows(df_mmd) %>% 
    select(indicator, total) %>% 
    mutate(type = case_when(
      str_detect(indicator, "HTS") ~ "TESTING",
      str_detect(indicator, "(TX_CURR|TX_MMD)") ~ "TREATMENT",
      TRUE ~ "VIRAL SUPPRESSION"
    ),
    fill = case_when(
      indicator == "HTS_TST_POS" ~ golden_sand,
      indicator == "HTS_TST" ~ golden_sand_light,
      indicator == "TX_CURR" ~ denim_light,
      indicator == "TX_MMD" ~ denim,
      TRUE ~ genoa_light
    )) %>% 
    ggplot(aes(x = type, y = total), ) +
    geom_col(data = . %>% filter(str_detect(indicator, "(POS|MMD)", negate = T)), 
                                 aes(fill = fill), alpha = 0.75) +
    geom_col(data = . %>% filter(str_detect(indicator, "(POS|MMD)")), 
             aes(fill = fill), alpha = 0.75) +
        geom_text(aes(label = 
      case_when(
        indicator == "HTS_TST_POS" ~ paste0(comma(total, suffix = " M", scale = 1e-6), " learned HIV-positive status"),
        indicator == "HTS_TST" ~ paste0(comma(total, suffix = " M", scale = 1e-6), " administered HIV tests "),
        indicator == "TX_CURR" ~ paste0(comma(total, suffix = " M", scale = 1e-6), " on HIV treatment"),
        indicator == "TX_MMD" ~ paste0(comma(total, suffix = " M", scale = 1e-6), " recieved multimonth treatment"),
        TRUE ~ paste0(comma(total, suffix = "M", scale = 1e-6), " are virally supressed")
        )), vjust = -.5) +
    scale_fill_identity() +
    si_style_nolines() +
    facet_wrap(~paste(type, "\n", "\n"), nrow = 1, scales = "free_x")+
    coord_cartesian(expand = F, clip = "off") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_text(face = "bold")) +
    labs(x = NULL, y = NULL, title = "USAID'S SUPPORT ACROSS UNAIDS GOALS\n")
    

# SPINDOWN ============================================================================

