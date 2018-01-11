

## goal assists by player chart - originally week 11
## look at reiprocal


assister <- assists %>% 
  left_join(playerGame) %>% 
  select(name,PLAYERID,PLAYER_MATCH_GOAL,gameDate,Opponents,season)



scorer <- goals %>% 
  left_join(playerGame) %>% 
  select(scorer=name,scorerID=PLAYERID,PLAYER_MATCH_GOAL,theDate=gameDate)

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


output$scorerAssist_pl <- renderPlotly({
  
  player <- input$playerA
  #player <- "KANEH"
  
  ## sum for player 
  scorerOrder <- scorer %>% 
    filter(scorerID==player) %>% 
    left_join(assister) %>% 
    filter(!is.na(PLAYERID)) %>% #no assists on goal - maybe add unassisted??
    group_by(PLAYERID,name) %>% 
    tally() %>% 
    arrange(desc(n))
  
  
  playerName<- assister %>% 
    filter(PLAYERID==player) %>% 
    head(1) %>% 
    pull(name)
  
  
  ## issue where many scorers not all get shown
  scorer %>% 
    filter(scorerID==player) %>% 
    arrange(theDate) %>% 
    mutate(order=row_number()) %>% 
    left_join(assister) %>% # join individ
    #filter(!is.na(PLAYERID)) %>% 
    left_join(scorerOrder) %>% #join sum
    plot_ly(x=~order,y=~fct_reorder(name, n),
            hoverInfo="text",
            text=~paste0(gameDate,
                         "<br>v ",Opponents)) %>% 
    add_markers(color=~season, size=I(8)) %>% 
    layout(margin=list(l=120),autosize = TRUE,height=900,
           title= glue("{playerName}'s Goals by Assister<br>(excludes Goals without an Assist) "),
           xaxis=list(title="Goal Order"),
           yaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
  
})

  