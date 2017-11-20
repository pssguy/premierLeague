## chart that shows for each team- how there manager has performed




## this how managers within team did 
output$teamPPGbyManager <- renderPlotly({
  
  req(input$teamA)
  req(input$managerGames)


  
  teamRecord <- ppgManagerTeamStint  %>% 
    
    select(TEAMNAME,name,ManagerTeam,games,ppg) %>% 
    inner_join(allManagerStints) %>% 
    filter(TEAMNAME==input$teamA&games>=input$managerGames) 
  
 
  teamRecord <- teamRecord %>% 
    mutate(id=row_number())
  
 
  
  
  minY <- min(teamRecord$ppg)-0.1
  maxY <- max(teamRecord$ppg)+0.1
  
  plot_ly(teamRecord, color = I("gray80")) %>%
    add_segments(x = ~Joined, xend = ~Left, y = ~ppg, yend = ~ppg, showlegend = FALSE) %>% 
    add_markers(x = ~Joined, y = ~ppg,  color = I("green"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",name,"<br>From:",Joined,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
    add_markers(x = ~Left, y = ~ppg, color = I("blue"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",name,"<br>To:",Left,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
 
    layout(
           xaxis=list(title=""),
           yaxis=list(title="Points per Game" ))
})



output$liverpool <- renderText({
  
#  if(is.null(input$teamA)) return()
 req(input$teamA)
  if (input$teamA!="Liverpool") {
    
  } else {
    "Houllier was initially a joint manager with Evans and was temporarily
    replaced by Thompson when ill"
  }
  
})



output$managerPPGbyTeam <- renderPlotly({
  
  req(input$manager)
  
 
  
  data <-managerGame %>% 
    filter(name==input$manager) %>% 
    inner_join(allManagerStints)
  
  (glimpse(houllier))
  
  
  df <- data %>% 
    group_by(ManagerTeam,TEAMNAME) %>% 
    summarize(left=Left[1],join=Joined[1],games=n(),ppg=round(sum(points)/games,2)) 
  
  # set chart limits
  minY <- min(df$ppg)-0.1
  maxY <- max(df$ppg)+0.1

  df %>% 
  plot_ly(color = I("gray80")) %>%
    add_segments(x = ~join, xend = ~left, y = ~ppg, yend = ~ppg, showlegend = FALSE) %>% 
    add_markers(x = ~join, y = ~ppg,  color = I("green"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",TEAMNAME,"<br>From:",join,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
    add_markers(x = ~left, y = ~ppg, color = I("blue"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",TEAMNAME,"<br>To:",left,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
    
    layout(
      xaxis=list(title=""),
      yaxis=list(title="Points per Game" )) %>%  config(displayModeBar = F,showLink = F)
      
  
  
})