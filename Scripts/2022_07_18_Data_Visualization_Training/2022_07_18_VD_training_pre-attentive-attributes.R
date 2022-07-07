# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Data Visualization Training Summer Series | Pre-attentive attributes
# REF ID:   83b5e54e 
# LICENSE:  MIT
# DATE:     2022-07-07
# UPDATED: 

# PRE-ATTENTIVE ATTRIBUTES ------------------------------------------------
  
  set.seed(42)
  runif(40, 0, 9) %>% 
    floor() %>%  
    paste0( collapse = " ") %>% 
    clipr::write_clip()
  