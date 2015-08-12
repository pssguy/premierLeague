## probbaly need to have set of selectors a la PFA
## also may want to go to reactive

resData <- eventReactive(input$teamA,{
  
  print("enter teamWins")
  if(is.null(input$teamA)) return()
  W <-standings %>% 
    ungroup() %>% 
    filter(team==input$teamA) %>% 
    arrange(tmGameOrder) %>% 
    select(res,tmGameOrder) %>% 
    mutate(cat=ifelse(res=="Win",1,0)) %>% 
    do(subSeq(.$cat))
  
  D <-standings %>% 
    ungroup() %>% 
    filter(team==input$teamA) %>% 
    arrange(tmGameOrder) %>% 
    select(res,tmGameOrder) %>% 
    mutate(cat=ifelse(res=="Draw",1,0)) %>% 
    do(subSeq(.$cat))
  
  L <-standings %>% 
    ungroup() %>% 
    filter(team==input$teamA) %>% 
    arrange(tmGameOrder) %>% 
    select(res,tmGameOrder) %>% 
    mutate(cat=ifelse(res=="Loss",1,0)) %>% 
    do(subSeq(.$cat))
  
  info=list(W=W,D=D,L=L)
  return(info)
  
})

output$tm_wins <- renderPlot({

  if(is.null(resData)) return()
  W <- resData()$W
  
  Win <- W %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(W,1)$value==1) {
    cond <- Win$slength == tail(W,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Win, aes(x=slength,y=n)) +
    geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Wins")
  
}, height=200)


output$tm_noWins <- renderPlot({
  if(is.null(resData)) return()
  W <- resData()$W
  
  Win <- W %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(W,1)$value==0) {
    cond <- Win$slength == tail(W,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Win, aes(x=slength,y=n)) +
    geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Winless")
  
}, height=200)

output$tm_draws <- renderPlot({
  
  if(is.null(resData)) return()
  D <- resData()$D
  
  Draw <- D %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(D,1)$value==1) {
    cond <- Draw$slength == tail(D,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Draw, aes(x=slength,y=n)) +
    geom_bar(data=subset(Draw,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Draw,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Draws")
  
}, height=200)


output$tm_noDraws <- renderPlot({
  if(is.null(resData)) return()
  if(is.null(resData)) return()
  D <- resData()$D
  
  Draw <- D %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(D,1)$value==0) {
    cond <- Draw$slength == tail(D,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Draw, aes(x=slength,y=n)) +
    geom_bar(data=subset(Draw,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Draw,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("No Draws")
  
}, height=200)


output$tm_losses <- renderPlot({
  
  if(is.null(resData)) return()
  L <- resData()$L
  
  Loss <- L %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(L,1)$value==1) {
    cond <- Loss$slength == tail(L,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Loss, aes(x=slength,y=n)) +
    geom_bar(data=subset(Loss,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Loss,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Defeats")
  
}, height=200)


output$tm_noLosses <- renderPlot({
  
  if(is.null(resData)) return()
  L <- resData()$L
  
  Loss <- L %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(L,1)$value==0) {
    cond <- Loss$slength == tail(L,1)$slength
  } else {
    cond <- FALSE
  }
  ggplot(Loss, aes(x=slength,y=n)) +
    geom_bar(data=subset(Loss,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(Loss,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Undefeated")
  
}, height=200)

output$tmWinSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
 
  # could put in reactive but not sure worthwhile
  long <- resData()$W %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
 
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmNoWinSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
  
  
  long <- resData()$W %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmDrawSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
  
  # could put in reactive but not sure worthwhile
  long <- resData()$D %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmNoDrawSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
  
  
  long <- resData()$D %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})


output$tmLossSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
  
  # could put in reactive but not sure worthwhile
  long <- resData()$L %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmNoLossSeq <- DT::renderDataTable({
  if(is.null(resData())) return()
  
  
  long <- resData()$L %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})