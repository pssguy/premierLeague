

output$sp_cardsClub <- renderPlotly({
  
  
  cardGames <-playerGame %>% 
    filter(TEAMNAME==input$team_cpc&season==input$teamYear_cpc&CARD>"a") %>% 
    group_by(TEAMMATCHID) %>% 
    tally()  
  
  
  zero <- data.frame(count=0,n=36-nrow(cardGames))
  
  
  df <- cardGames %>% 
    rename(count=n) %>% 
    group_by(count) %>% 
    tally()
  
  rbind(zero,df) %>% 
    plot_ly(x=count,y=n,type="bar", color="orange",
            hoverinfo = "text",
            text = paste("Cards:",count,"<br> Games:",n)) %>% 
    layout(hovermode = "closest",
           xaxis=list(title="Card Count"),
           yaxis=list(title="Number of Games"),
           title="Team's Card count by Game by Season"            
    )
})