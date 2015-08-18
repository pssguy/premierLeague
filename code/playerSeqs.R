playerSeqData <- reactive ({
  print("psd reactive")
  
  if (is.null(input$playerA)) return()
  print("playerid")
  print(input$playerA)
  
  appeared <- playerGame %>%
    filter(PLAYERID==input$playerA&(START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  
  appeared$Scored <- 0
  appeared$Scored[appeared$Gls>0] <- 1
 
  run <- subSeq(appeared$Scored)
 
  info=list(run=run)
  return(info)
})


output$gameNoGoal <- renderPlot({
  if (is.null(playerSeqData()$run)) return()
  
  run <- playerSeqData()$run
  
  gameGoal <- run %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  
  if (tail(run,1)$value==0) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
     theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Not Scored In")
})

output$gameGoal <- renderPlot({
  if (is.null(playerSeqData()$run)) return()
  
  run <- playerSeqData()$run
  
  gameGoal <- run %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  
  if (tail(run,1)$value==1) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Scored In")
})

