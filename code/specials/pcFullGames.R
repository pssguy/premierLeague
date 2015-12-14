


output$pcFullGames <- renderPlotly({
  
#   print("fg")
#   print(input$pcFullGames) #50
  
  if (input$sp_pcFullGamsTeams=="All Teams") {
  df <-playerGame %>% 
    filter(mins>0) %>% 
    mutate(fullGame=(ifelse(mins==90,1,0))) %>% 
    group_by(PLAYERID,name,POSITION) %>% 
    summarize(count=n(),fullPC=round(100*mean(fullGame),1)) %>% 
    ungroup() %>% 
    filter(count>=input$sp_pcFullGames) 
  } else {
    df <-playerGame %>% 
      filter(mins>0&TEAMNAME==input$sp_pcFullGamsTeams) %>% 
      mutate(fullGame=(ifelse(mins==90,1,0))) %>% 
      group_by(PLAYERID,name,POSITION) %>% 
      summarize(count=n(),fullPC=round(100*mean(fullGame),1)) %>% 
      ungroup() %>% 
      filter(count>=input$sp_pcFullGames) 
  }
  
  
  
  plot_ly(df, x = count, y = fullPC, mode = "markers", hoverinfo = "text",color=POSITION,
          text = paste(name,
                       "<br>Appearances:",count,
                       "<br>Full games:",fullPC,"%")) %>%
    layout(hovermode = "closest",
           xaxis=list(title="Games Played"),
           yaxis=list(title="% Complete Games"
           )
    )
})
