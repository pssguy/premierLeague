

## goal assists by player chart - originally week 11


assister <- assists %>% 
  left_join(playerGame) %>% 
  select(name,PLAYERID,PLAYER_MATCH_GOAL,gameDate,Opponents,season)



scorer <- goals %>% 
  left_join(playerGame) %>% 
  select(scorer=name,scorerID=PLAYERID,PLAYER_MATCH_GOAL)

output$asstScorer_pl <- renderPlotly({
  
  player <- input$playerA
  
  ## sum for player 
  scorerOrder <- assister %>% 
    filter(PLAYERID==player) %>% 
    left_join(scorer) %>% 
    group_by(scorer) %>% 
    tally() %>% 
    arrange(desc(n))
  
  
  playerName<- assister %>% 
    filter(PLAYERID==player) %>% 
    head(1) %>% 
    pull(name)
  
 
  
  assister %>% 
    filter(PLAYERID==player) %>% 
    arrange(gameDate) %>% 
    mutate(order=row_number()) %>% 
    left_join(scorer) %>% # join individ
    left_join(scorerOrder) %>% #join sum
    plot_ly(x=~order,y=~fct_reorder(scorer, n),
            hoverInfo="text",
            text=~paste0(gameDate,
                         "<br>v ",Opponents)) %>% 
    add_markers(color=~season, size=I(8)) %>% 
    layout(margin=list(l=120),autosize = TRUE,height=900,
           title= glue("{playerName}'s Assists by Scorer"),
           xaxis=list(title="Goal Order"),
           yaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
  
})
  