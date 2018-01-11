
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
  print("reg working")
  print(input$playerA)
  print(values$playerID)
  
  # games actually played (need to check for periods where unknown on and off??)
  # games <-playerGame %>% 
  #   filter(PLAYERID==input$playerA&mins>0) %>% 
  #   select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>% 
  #   mutate(on=as.integer(on),off=as.integer(offA)) %>% 
  #   mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(offA),90,offA))
  # 
  games <-playerGame %>% 
    filter(PLAYERID==input$playerA&mins>0) %>% 
  select(PLAYERID,name,PLAYER_MATCH,START,on,offA,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(offA)) %>% 
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
     
    mutate(goalOrder=row_number(),tooltip=ifelse(is.na(scored),paste0("Current Run =",gap," mins"),paste0(gameDate,
                                                                                                          "<br>",TEAMNAME," v ",Opponents,
                                                                                                          "<br>",gap," mins"))) %>% 
    plot_ly(x=~goalOrder, y=~gap, height= 300) %>% 
    add_bars(hoverinfo="text", color=~ season,
             text= ~tooltip) %>% 
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Goal Order"),
           yaxis=list(title="Minutes between Goals")
    ) %>%  config(displayModeBar = F,showLink = F)
  
  
})

output$goalDrought_pl_front <- renderPlotly({

  req(input$playerB)
  print("fp working")
  print(input$playerB)
  print(values$playerID)

  # games actually played
  # games <-playerGame %>%
  #   filter(PLAYERID==input$playerB&mins>0) %>%
  #   select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>%
  #   mutate(on=as.integer(on),off=as.integer(off)) %>%
  #   mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  
  games <-playerGame %>% 
    filter(PLAYERID==input$playerA&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,offA,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(offA)) %>% 
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

  # combine back to playergame for team data and create tooltip taking account of openended last value
  df <- goalData %>%
    left_join(playerGame) %>%
    
    mutate(goalOrder=row_number(),tooltip=ifelse(is.na(scored),paste0("Current Run = ",gap," mins"),paste0(gameDate,
                                                                                                           "<br>",TEAMNAME," v ",Opponents,
                                                                                                           "<br>",gap," mins"))) 
  
  
  
  df %>%
    plot_ly(x=~goalOrder, y=~gap) %>%
    add_bars(hoverinfo="text",
             text=~tooltip) %>%
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Goal Order"),
           yaxis=list(title="Minutes between Goals")
    ) %>%  config(displayModeBar = F,showLink = F)
  



})


#### Assists

## switch this to global

assts <- assists %>% 
  left_join(goals,by="PLAYER_MATCH_GOAL") %>% #duplicate PLAYER_MATCH for assist and goal
  select(PLAYER_MATCH=PLAYER_MATCH.x,TIME,TEAMMATCHID)

# need to add other column as are matching on TIME
assts <- assts %>% 
  mutate(assisted=1)

output$assistDrought_pl <- renderPlotly({
  
  req(input$playerA)
  print("reg working")
  print(input$playerA)
  print(values$playerID)
  
  
  # games actually played
  # games <-playerGame %>% 
  #   filter(PLAYERID==input$playerA&mins>0) %>% 
  #   select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>% 
  #   mutate(on=as.integer(on),off=as.integer(off)) %>% 
  #   mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  # 
  games <-playerGame %>% 
    filter(PLAYERID==input$playerA&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,offA,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(offA)) %>% 
    mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  # create list so can apply function above with purrr
  games_list <- split(games, 1:nrow(games))
  mins <-map_df(games_list,plDroughtFun)
  
  
  # rename columns and add gameDate
  names(mins) <- c("TIME","PLAYER_MATCH") 
  mins <-mins %>% 
    left_join(games) %>% 
    select(TIME,PLAYER_MATCH,gameDate)
  
  
  
  
  
  # create gaps between assists 
  assistData <- mins %>% 
    left_join(assts)  %>% #Joining, by = c("TIME", "PLAYER_MATCH")
    select(PLAYER_MATCH,TIME,gameDate,assisted) %>% 
    arrange(gameDate,TIME) %>% 
    mutate(minOrder=row_number(),assist=ifelse(is.na(assisted),0,1)) %>% 
    filter(assist==1|minOrder==max(minOrder)) %>% # to take account of current spell
    mutate(minslag=lag(minOrder),gap=minOrder-minslag)
  
  # account for gap at beginning of career
  assistData[1,]$gap <- assistData[1,]$minOrder
  
  
  # combine back to playergame for team data and create plot
  assistData %>% 
    left_join(playerGame) %>% 
   
    mutate(assistOrder=row_number(),tooltip=ifelse(is.na(assisted),paste0("Current Run =",gap," mins"),paste0(gameDate,
                                                                                                              "<br>",TEAMNAME," v ",Opponents,
                                                                                                              "<br>",gap," mins"))) %>% 
    plot_ly(x=~assistOrder, y=~gap, height=300) %>% 
    add_bars(hoverinfo="text",color= ~season,
             text= ~tooltip)%>% 
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Assist Order (up to 2 assists allowed per goal)"),
           yaxis=list(title="Minutes between Assists")
    ) %>%  config(displayModeBar = F,showLink = F)
  
  
  
})


output$assistDrought_pl_front <- renderPlotly({
  # 
  # req(input$playerA)
  # print("reg working")
  # print(input$playerA)
  # print(values$playerID)
  # 
  
  # games actually played
  # games <-playerGame %>% 
  #   filter(PLAYERID==input$playerB&mins>0) %>% 
  #   select(PLAYERID,name,PLAYER_MATCH,START,on,off,gameDate,TEAMMATCHID) %>% 
  #   mutate(on=as.integer(on),off=as.integer(off)) %>% 
  #   mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  print("entering front")
  print(input$playerB)
  print("exiting front")
  
  games <-playerGame %>% 
    filter(PLAYERID==input$playerB&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,offA,gameDate,TEAMMATCHID) %>% 
    mutate(on=as.integer(on),off=as.integer(offA)) %>% 
    mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  # create list so can apply function above with purrr
  games_list <- split(games, 1:nrow(games))
  mins <-map_df(games_list,plDroughtFun)
  
  
  # rename columns and add gameDate
  names(mins) <- c("TIME","PLAYER_MATCH") 
  mins <-mins %>% 
    left_join(games) %>% 
    select(TIME,PLAYER_MATCH,gameDate)
  
  
  
  
  
  # create gaps between assists 
  assistData <- mins %>% 
    left_join(assts)  %>% #Joining, by = c("TIME", "PLAYER_MATCH")
    select(PLAYER_MATCH,TIME,gameDate,assisted) %>% 
    arrange(gameDate,TIME) %>% 
    mutate(minOrder=row_number(),assist=ifelse(is.na(assisted),0,1)) %>% 
    filter(assist==1|minOrder==max(minOrder)) %>% # to take account of current spell
    mutate(minslag=lag(minOrder),gap=minOrder-minslag)
  
  # account for gap at beginning of career
  assistData[1,]$gap <- assistData[1,]$minOrder
  
  
  # combine back to playergame for team data and create plot
  assistData %>% 
    left_join(playerGame) %>% 
    mutate(assistOrder=row_number(),tooltip=ifelse(is.na(assisted),paste0("Current Run =",gap," mins"),paste0(gameDate,
                                                                                                              "<br>",TEAMNAME," v ",Opponents,
                                                                                                              "<br>",gap," mins"))) %>% 
    plot_ly(x=~assistOrder, y=~gap) %>% 
    add_bars(hoverinfo="text",
             text= ~tooltip)%>% 
    layout(title="Hover bar for drought-ending game",
           xaxis=list(title= "Assist Order (up to 2 assists allowed per goal)"),
           yaxis=list(title="Minutes between Assists")
    ) %>%  config(displayModeBar = F,showLink = F)
  
  
  
})
