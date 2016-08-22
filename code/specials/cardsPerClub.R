


output$sp_cardsClub <- renderPlotly({
  req(input$teamYear_cpc)
  cardGames <- playerGame %>%
    filter(TEAMNAME == input$team_cpc &
             season == input$teamYear_cpc & CARD > "a") %>%
    group_by(TEAMMATCHID) %>%
    tally()
  
  
  gamesPlayed <- standings %>%
    filter(team == input$team_cpc & season == input$teamYear_cpc) %>%
    .$tmYrGameOrder %>%
    max()
  
  
  # account for games where no cards given
  zero <- data.frame(count = 0L, n = gamesPlayed - nrow(cardGames))
  
  
  df <- cardGames %>%
    rename(count = n) %>%
    group_by(count) %>%
    tally()
  
  theTitle <- "Team's Card count by Game by Season"
  
  df <- rbind(df, zero)
  
  df %>%
    plot_ly() %>%
    add_bars(
      x =  ~ count,
      y =  ~ n,
      marker = list(color = "orange"),
      hoverinfo = "text",
      text = ~ paste("Cards:", count, "<br> Games:", n)
    ) %>%
    layout(
      hovermode = "closest",
      xaxis = list(title = "Card Count"),
      yaxis = list(title = "Number of Games"),
      title = theTitle
    )  %>%
    config(displayModeBar = F, showLink = F)
})