
## Minutes passed since a player has scored (can add assisted)

# Function to list every minute played
plDroughtFun <- function(df) {
    expand.grid(c(df$on:df$off),df$PLAYER_MATCH)
  }

# need to add other column as are matching on TIME
gls <- goals %>% 
  mutate(scored=1)

output$goalDrought_pl <- renderPlotly({
  
  req(input$playerA)

  
  # games actually played
  games <-playerGame %>% 
    filter(PLAYERID==input$playerA&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(off)) %>% 
    mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  # create list so can apply function above with purrr
  games_list <- split(games, 1:nrow(games))
  mins <-map_df(games_list,plDroughtFun)
  
  
  # rename columns and add gameDate
  names(mins) <- c("TIME","PLAYER_MATCH") 
  mins <-mins %>% 
    left_join(games) %>% 
    select(TIME,PLAYER_MATCH,gameDate)

  

  
  
  # create gaps between goals
  goalData <- mins %>% 
    left_join(gls)  %>% 
    select(PLAYER_MATCH,TIME,gameDate,scored) %>% 
    arrange(gameDate,TIME) %>% 
    mutate(minOrder=row_number(),goal=ifelse(is.na(scored),0,1)) %>% 
    filter(goal==1|minOrder==max(minOrder)) %>% # to take account of current spell
    mutate(minslag=lag(minOrder),gap=minOrder-minslag)
  
  # account for gap at beginning of career
  goalData[1,]$gap <- goalData[1,]$minOrder
  
  
  # combine back to playergame for team data and create plot
  goalData %>% 
    left_join(playerGame) %>% 
    mutate(goalOrder=row_number()) %>% 
    plot_ly(x=~goalOrder, y=~gap) %>% 
    add_bars(hoverinfo="text",
             text=~paste0(gameDate,
                          "<br>",TEAMNAME," v ",Opponents,
                          "<br>",gap," mins")) %>% 
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Goal Order"),
           yaxis=list(title="Minutes between Goals")
    ) %>%  config(displayModeBar = F,showLink = F)
  
  
  
})
  
  
output$goalDrought_pl_front <- renderPlotly({
  
  req(input$playerA)
  
  # games actually played
  games <-playerGame %>% 
    filter(PLAYERID==input$playerA&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(off)) %>% 
    mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  # create list so can apply function above with purrr
  games_list <- split(games, 1:nrow(games))
  mins <-map_df(games_list,plDroughtFun)
  
  
  # rename columns and add gameDate
  names(mins) <- c("TIME","PLAYER_MATCH") 
  mins <-mins %>% 
    left_join(games) %>% 
    select(TIME,PLAYER_MATCH,gameDate)
  
  
  
 
  
  
  # create gaps between goals
  goalData <- mins %>% 
    left_join(gls)  %>% 
    select(PLAYER_MATCH,TIME,gameDate,scored) %>% 
    arrange(gameDate,TIME) %>% 
    mutate(minOrder=row_number(),goal=ifelse(is.na(scored),0,1)) %>% 
    filter(goal==1|minOrder==max(minOrder)) %>% # to take account of current spell
    mutate(minslag=lag(minOrder),gap=minOrder-minslag)
  
  # account for gap at beginning of career
  goalData[1,]$gap <- goalData[1,]$minOrder
  
  
  # combine back to playergame for team data and create plot
  goalData %>% 
    left_join(playerGame) %>% 
    mutate(goalOrder=row_number()) %>% 
    plot_ly(x=~goalOrder, y=~gap) %>% 
    add_bars(hoverinfo="text",
             text=~paste0(gameDate,
                          "<br>",TEAMNAME," v ",Opponents,
                          "<br>",gap," mins")) %>% 
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Goal Order"),
           yaxis=list(title="Minutes between Goals")
    ) %>%  config(displayModeBar = F,showLink = F)
  
  
  
})
