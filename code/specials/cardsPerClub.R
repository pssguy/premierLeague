

output$sp_cardsClub <- renderPlotly({
  
  req(input$teamYear_cpc)
  cardGames <-playerGame %>% 
    filter(TEAMNAME==input$team_cpc&season==input$teamYear_cpc&CARD>"a") %>% 
    group_by(TEAMMATCHID) %>% 
    tally()  
  
  
  gamesPlayed <- standings %>% 
    filter(team==input$team_cpc&season==input$teamYear_cpc) %>% 
    .$tmYrGameOrder %>% 
    max()
  
  print(input$teamYear_cpc)
  print(str(input$teamYear_cpc))
  
  print("gamesPlayed")
  print(gamesPlayed)
  
  zero <- data.frame(count=0,n=gamesPlayed-nrow(cardGames))
  
  
  df <- cardGames %>% 
    rename(count=n) %>% 
    group_by(count) %>% 
    tally()
  
  theTitle <- "Team's Card count by Game by Season" 
  
  rbind(zero,df) %>% 
    plot_ly(x=count,y=n,type="bar", color="orange",
            hoverinfo = "text",
            text = paste("Cards:",count,"<br> Games:",n)) %>% 
    layout(hovermode = "closest",
           xaxis=list(title="Card Count"),
           yaxis=list(title="Number of Games"),
           title=theTitle            
    )  %>% 
    config(displayModeBar = F,showLink = F)
})