playerSeqData <- reactive ({
  print("psd")
  temp <- playerGame %>%
    filter(PLAYERID==input$player&(START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  
  temp$Scored <- 0
  temp$Scored[temp$Gls>0] <- 1
  #library(doBy)
  run <- subSeq(temp$Scored)
  #   print(run)
  #   print("rundone")
  #   print(temp)
  #   print("tempdun")
  info=list(run=run,temp=temp)
  return(info)
})

output$plWorstSeqGl <- renderDataTable({
  
  bestRun <- playerSeqData()$run %>%
    filter(value==0) %>%
    filter(slength==max(slength)) 
  
  bestGames <- (bestRun$first[nrow(bestRun)]:bestRun$last[nrow(bestRun)])
  
  df <- data.frame(playerSeqData()$temp)
  df$id <- as.integer(row.names(df))
  
  df %>%
    filter(id %in% bestGames)  %>%
    select(TEAMNAME,Opponents,gameDate,Gls)
},options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))

output$plBestSeqGl <- renderDataTable({
  
  bestRun <- playerSeqData()$run %>%
    filter(value==1) %>%
    filter(slength==max(slength)) 
  
  bestGames <- (bestRun$first[nrow(bestRun)]:bestRun$last[nrow(bestRun)])
  
  df <- data.frame(playerSeqData()$temp)
  df$id <- as.integer(row.names(df))
  
  df %>%
    filter(id %in% bestGames)  %>%
    select(TEAMNAME,Opponents,gameDate,Gls)
},options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))


## try an observe with ggvis

observe({
  
  playerSeqData()$run %>%
    filter(value==1) %>% # best are 211 215 and 285 289
    ggvis(~slength) %>%
    layer_histograms(width=0.5) %>%
    bind_shiny('bestRun')
  
})

observe({
  
  playerSeqData()$run %>%
    filter(value==0) %>% # best are 211 215 and 285 289
    ggvis(~slength) %>%
    layer_histograms(width=0.5) %>%
    bind_shiny('worstRun')
  
})


playerSeqAssData <- reactive ({
  print("psd")
  temp <- playerGame %>%
    filter(PLAYERID==input$player&(START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Assists,plGameOrder,TEAMNAME,gameDate,Opponents)
  
  temp$Assisted <- 0
  temp$Assisted[temp$Assists>0] <- 1
  #library(doBy)
  run <- subSeq(temp$Assisted)
  #   print(run)
  #   print("rundone")
  #   print(temp)
  #   print("tempdun")
  info=list(run=run,temp=temp)
  return(info)
})

output$plWorstSeqAss <- renderDataTable({
  
  bestRun <- playerSeqAssData()$run %>%
    filter(value==0) %>%
    filter(slength==max(slength)) 
  
  bestGames <- (bestRun$first[nrow(bestRun)]:bestRun$last[nrow(bestRun)])
  
  df <- data.frame(playerSeqAssData()$temp)
  df$id <- as.integer(row.names(df))
  
  df %>%
    filter(id %in% bestGames)  %>%
    select(TEAMNAME,Opponents,gameDate,Assists)
},options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))

output$plBestSeqAss <- renderDataTable({
  
  bestRun <- playerSeqData()$run %>%
    filter(value==1) %>%
    filter(slength==max(slength)) 
  
  bestGames <- (bestRun$first[nrow(bestRun)]:bestRun$last[nrow(bestRun)])
  
  df <- data.frame(playerSeqDataAss()$temp)
  df$id <- as.integer(row.names(df))
  
  df %>%
    filter(id %in% bestGames)  %>%
    select(TEAMNAME,Opponents,gameDate,Assists)
},options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))


## try an observe with ggvis

observe({
  
  playerSeqAssData()$run %>%
    filter(value==1) %>% # best are 211 215 and 285 289
    ggvis(~slength) %>%
    layer_histograms(width=0.5) %>%
    bind_shiny('bestAssRun')
  
})

observe({
  
  playerSeqAssData()$run %>%
    filter(value==0) %>% # best are 211 215 and 285 289
    ggvis(~slength) %>%
    layer_histograms(width=0.5) %>%
    bind_shiny('worstAssRun')
  
})