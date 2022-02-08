plot_trend<- function(df_mal_viz, plot_title, plot_subtitle,plot_source){
  
  trendplot<- df_mal_viz %>% 
    mutate(year = as.numeric(year)) %>% 
    ggplot(aes(year, mean_inc)) + 
    geom_line(aes(y = mean_inc), color = scooter, size = 1) +
    geom_ribbon(aes(ymin = mean_inc-sd_inc, ymax = mean_inc+sd_inc), alpha = 0.2, fill = scooter) +
    geom_point(aes(year, max_inc, color=country_max), na.rm = TRUE,
               shape = 20, size = 4) +
    geom_point(aes(year, min_inc, color=country_min), na.rm = TRUE,
               shape = 20, size = 4) +
    geom_hline(yintercept=goal_malaria,linetype="dashed", color=trolley_grey, size=1) +
    scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         subtitle = plot_subtitle,
         caption = glue("Source: ", plot_source, "\nUS Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    si_style()+
    si_style_ygrid() +
    theme(axis.text.y = element_markdown(),
          plot.title = element_textbox_simple(margin = margin(5.5, 5.5, 5.5, 5.5)),
          plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light",
                                                 margin = margin(5.5, 5.5, 10, 5.5)),
          legend.title=element_blank(),
          legend.position="right")
  
  return(trendplot)
}



  
