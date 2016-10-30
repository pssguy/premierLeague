


output$playerByTeamPPG <- renderPlotly({
  
 
  
  req(input$teamA)
  req(input$teamYears)
  
  tm <- input$teamA
  yr <- input$teamYears
  
  print(yr)
  print(tm)

  tmRes <-standings %>%
    ungroup() %>%
    filter(team==tm&season==yr) %>%
    select(MATCHID,res,team,season) %>%
    mutate(ppg=(ifelse(res=="Win",3,ifelse(res=="Draw",1,0))))


  tmppg <- playerGame %>%
    ungroup() %>%
    filter(TEAMNAME==tm&season==yr&START>0) %>%
    select(name,MATCHID) %>%
    left_join(tmRes) %>%
    # group_by(name) %>%
    summarize(avPoints=mean(ppg),avStarts=n()/11)

  allPoints <-tmppg$avPoints*tmppg$avStarts  #43

  playerRes <- playerGame %>%
    ungroup() %>%
    filter(TEAMNAME==tm&season==yr&START>0) %>%
    select(name,MATCHID) %>%
    left_join(tmRes) %>%
    group_by(name) %>%
    summarize(stPoints=mean(ppg), starts=n(),allPoints, nonStarts=tmppg$avStarts-starts,nonPoints=(allPoints-(stPoints*starts))/(tmppg$avStarts-starts)) %>%
    ungroup() %>%
    arrange(stPoints,starts) # arranged like this for graph

  ## set mins for line
  minY <- playerRes$name[1]
  maxY <- playerRes$name[nrow(playerRes)]

  ## set title

  theTitle <- paste0("Average Points per Game in Games Started ",tm," - ",yr)

  ## plot_ly
  playerRes %>% 
    arrange(name) %>% 
    plot_ly() %>% 
    
  add_markers(x=~stPoints, y=~name, name="Starter", 
          hoverinfo="text", marker=list(size=~starts,sizemin=2),
          text=~paste("Av Pts:",round(stPoints,2),"<br> Starts:",starts)) %>%
    add_markers(x=~nonPoints,y=~name, name="Non-Starter", 
              hoverinfo="text", marker=list(size=~nonStarts,sizemin=2),
              text=~paste("Av Pts:",round(nonPoints,2),"<br> Non-Starts:",nonStarts))   %>%
    add_lines(x = c(tmppg$avPoints, tmppg$avPoints), y= c(minY, maxY), line = list(color = "green",width=1, dash = "10px"), showlegend = FALSE) %>%
    layout(hovermode = "closest", autosize= F, width=600, height= 700,
           margin=list(l=120),
           xaxis=list(title="Av points per Game"),
           yaxis=list(title=" "),
           title=theTitle, titlefont=list(size=16)
    )

  
})


output$playerByTeamPPG_hl <- renderPlotly({
  
  
  
  req(input$teamA_hl)
  req(input$teamYears_hl)
  
  tm <- input$teamA_hl
  yr <- input$teamYears_hl
  
  print(yr)
  print(tm)
  
  tmRes <-standings %>%
    ungroup() %>%
    filter(team==tm&season==yr) %>%
    select(MATCHID,res,team,season) %>%
    mutate(ppg=(ifelse(res=="Win",3,ifelse(res=="Draw",1,0))))
  
  
  tmppg <- playerGame %>%
    ungroup() %>%
    filter(TEAMNAME==tm&season==yr&START>0) %>%
    select(name,MATCHID) %>%
    left_join(tmRes) %>%
    # group_by(name) %>%
    summarize(avPoints=mean(ppg),avStarts=n()/11)
  
  allPoints <-tmppg$avPoints*tmppg$avStarts  #43
  
  playerRes <- playerGame %>%
    ungroup() %>%
    filter(TEAMNAME==tm&season==yr&START>0) %>%
    select(name,MATCHID) %>%
    left_join(tmRes) %>%
    group_by(name) %>%
    summarize(stPoints=mean(ppg), starts=n(),allPoints, nonStarts=tmppg$avStarts-starts,nonPoints=(allPoints-(stPoints*starts))/(tmppg$avStarts-starts)) %>%
    ungroup() %>%
    arrange(stPoints,starts) # arranged like this for graph
  
  ## set mins for line
  minY <- playerRes$name[1]
  maxY <- playerRes$name[nrow(playerRes)]
  
  ## set title
  
  theTitle <- paste0("Average Points per Game in Games Started ",tm," - ",yr)
  
  ## plot_ly
  
  plot_ly(playerRes , x=stPoints, y=name, name="Starter", mode="markers",
          hoverinfo="text", marker=list(size=starts,sizemin=2),
          text=paste("Av Pts:",round(stPoints,2),"<br> Starts:",starts)) %>%
    add_trace(x=nonPoints,y=name, name="Non-Starter", mode="markers",
              hoverinfo="text", marker=list(size=nonStarts,sizemin=2),
              text=paste("Av Pts:",round(nonPoints,2),"<br> Non-Starts:",nonStarts))   %>%
    add_trace(x = c(tmppg$avPoints, tmppg$avPoints), y= c(minY, maxY), mode = "lines", line = list(color = "green",width=1, dash = "10px"), showlegend = FALSE) %>%
    layout(hovermode = "closest", autosize= F, width=600, height= 700,
           margin=list(l=120),
           xaxis=list(title="Av points per Game"),
           yaxis=list(title=" "),
           title=theTitle, titlefont=list(size=16)
    ) %>% 
  config(displayModeBar = F)
  
  
})