

output$sp_playerByCountryPPG    <- renderPlotly({
  
  
  test <- playerGame %>% 
    filter(COUNTRY==input$country&season==input$teamYears)  %>% 
    group_by(PLAYERID,name) %>% 
    select(Gls,Assists,mins) %>% 
    summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
    filter(Points!=0) %>% 
    mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
    ungroup()
  
  test %>% 
    plot_ly() %>% 
    
  add_markers(x = ~Points, y = ~Ppm,  hoverinfo = "text",
          marker=list(size=~Mins/10, sizemode="area"),
          text = ~paste(name,
                       "<br>Goals: ",Goals,
                       "<br>Assists: ",Assists,
                       "<br>Points: ",Points,
                       "<br>Minutes: ",Mins
          )) %>%
    layout(hovermode = "closest",
           title="Points (inc. secondary assists) per 90 mins by Season <br>
           Zoom and Hover points for Details",
           xaxis=list(title="Points in Season"),
           yaxis=list(title="Points per 90 mins",rangemode="tozero"
           )
    )
})

## causing problems 

output$playerByCountryPPG_hl    <- renderPlotly({

  print("inputs arriving")
  print(input$country)
  print(input$teamYears)
  print("that was inputs")

  test <- playerGame %>%
    ungroup() %>% 
    filter(COUNTRY==input$country&season==input$teamYears_ppg)  %>%
    group_by(PLAYERID,name) %>%
    select(Gls,Assists,mins) %>%
    dplyr::summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>%
    filter(Points!=0) %>%
    mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>%
    ungroup()

  test %>% 
    plot_ly() %>% 
  add_markers(x = ~Points, y = ~Ppm,  hoverinfo = "text",
          marker=list(size=~Mins/10, sizemode="area"),
          text = ~paste(name,
                       "<br>Goals: ",Goals,
                       "<br>Assists: ",Assists,
                       "<br>Points: ",Points,
                       "<br>Minutes: ",Mins
          )) %>%
    layout(hovermode = "closest",
           title="Points (inc. secondary assists) per 90 mins by Season <br>
           Zoom and Hover points for Details",
           xaxis=list(title="Points in Season"),
           yaxis=list(title="Points per 90 mins",rangemode="tozero"
           )
    ) %>%
    config(displayModeBar = F)
})