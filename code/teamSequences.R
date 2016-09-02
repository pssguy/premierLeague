## probbaly need to have set of selectors a la PFA
## isssue with error

#resData <- eventReactive(input$teamA,{ this works until another reactive is added eg input$seqVenue
## errro gets shown momentarily if this is first category in teams gone to - bit unlikely
  resData <- reactive({
  
    req(input$teamA)
  # if (is.null(input$teamA)) return()
  # 
  # if (length(input$teamA)<1) return()
  if (input$seqVenue=="All") {
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
  } else if(input$seqVenue=="Home") {
    W <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Win",1,0)) %>% 
      do(subSeq(.$cat))
    
    D <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Draw",1,0)) %>% 
      do(subSeq(.$cat))
    
    L <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Loss",1,0)) %>% 
      do(subSeq(.$cat))
  } else {
    W <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Win",1,0)) %>% 
      do(subSeq(.$cat))
    
    D <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Draw",1,0)) %>% 
      do(subSeq(.$cat))
    
    L <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      select(res,tmGameOrder) %>% 
      mutate(cat=ifelse(res=="Loss",1,0)) %>% 
      do(subSeq(.$cat))
  }
  
  info=list(W=W,D=D,L=L)
  return(info)
  
})

  
output$tm_wins <- renderPlotly({

 
 # if(is.null(resData)) return()
  W <- resData()$W
  
 
  
  Win <- W %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  
 

  
  if (tail(W,1)$value==1) {
    curr <- tail(W,1)$slength 
    Win <-  Win %>% 
      mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Win$opacity <- 0.3
  }

  Win %>% 
    plot_ly() %>% 
    add_bars(data=subset(Win,opacity!=1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    add_bars(data=subset(Win,opacity==1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    layout(hovermode = "closest", title= "Wins",
           xaxis=list(title="Run"),
           yaxis=list(title="Count"
           )) %>% 
    config(displayModeBar = F,showLink = F)

  # plot_ly(Win,x=~slength,
  #         y=~n,
  #         type="bar",
  #         marker=list(color='blue'),
  #         opacity=opacity,
  #         showlegend=F,
  #         group= ~slength, # could equally be slength
  #         hoverinfo="text",
  #          text=~paste("Run:",slength,"<br> Count:",n)) %>% 
  #               layout(hovermode = "closest", title= "Wins",
  #                      xaxis=list(title="Run"),
  #                      yaxis=list(title="Count"
  #                      ))
 

})



output$tm_noWins <- renderPlotly({
  
 # if(is.null(resData)) return()
  W <- resData()$W
  

  
  Win <- W %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  
  
  if (tail(W,1)$value==0) {
    curr <- tail(W,1)$slength 
  Win <-  Win %>% 
    mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Win$opacity <- 0.3
  }
  
  Win %>% 
    plot_ly() %>% 
    add_bars(data=subset(Win,opacity!=1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    add_bars(data=subset(Win,opacity==1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    layout(hovermode = "closest", title= "No Wins",
           xaxis=list(title="Run"),
           yaxis=list(title="Count"
           )) %>% 
    config(displayModeBar = F,showLink = F)
  
#   plot_ly(Win,x=slength,
#           y=n,
#           type="bar",
#           marker=list(color='blue'),
#           opacity=opacity,
#           showlegend=F,
#           group= slength, 
#           hoverinfo="text",
#           text=paste("Run:",slength,"<br> Count:",n)) %>% 
#     layout(hovermode = "closest", title= "No Wins",
#            xaxis=list(title="Run"),
#            yaxis=list(title="Count"
#            ))
#   
 })

output$tm_draws <- renderPlotly({
  
 # if(is.null(resData)) return()
  D <- resData()$D
  
  
  
 Draw <- D %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  
  
  
  
  if (tail(D,1)$value==1) {
    curr <- tail(D,1)$slength 
    Draw <-  Draw %>% 
      mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Draw$opacity <- 0.3
  }
  
 write_csv(Draw,"problemA.csv")
  
  
   plot_ly() %>% 
   add_bars(data=subset(Draw,opacity!=1.0),x=~slength,y=~n,showlegend=F,
            #group= ~slength, # could equally be slength
            hoverinfo="text",
            text=~paste("Run:",slength,"<br> Count:",n)) %>% 
   add_bars(data=subset(Draw,opacity==1.0),x=~slength,y=~n,showlegend=F,
            #group= ~slength, # could equally be slength
            hoverinfo="text",
            text=~paste("Run:",slength,"<br> Count:",n)) %>% 
   layout(hovermode = "closest", title= "Draws",
          xaxis=list(title="Run"),
          yaxis=list(title="Count"
          )) %>% 
   config(displayModeBar = F,showLink = F)
  
  
})

### native plotly

output$tm_noDraws <- renderPlotly({
 # if(is.null(resData)) return()
  D <- resData()$D
  
  
  
  Draw <- D %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  
  
  
  
  if (tail(D,1)$value==0) {
    curr <- tail(D,1)$slength 
    Draw <-  Draw %>% 
      mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Draw$opacity <- 0.3
  }
  
  
  
    plot_ly() %>% 
    add_bars(data=subset(Draw,opacity!=1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    add_bars(data=subset(Draw,opacity==1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    layout(hovermode = "closest", title= "No Draws",
           xaxis=list(title="Run"),
           yaxis=list(title="Count"
           )) %>% 
    config(displayModeBar = F,showLink = F)
  
})


output$tm_losses <- renderPlotly({
  
 # if(is.null(resData)) return()
  L <- resData()$L
  
  
  
  Loss <- L %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  
  
  
  
  if (tail(L,1)$value==1) {
    curr <- tail(L,1)$slength 
    Loss <-  Loss %>% 
      mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Loss$opacity <- 0.3
  }
  
  

    plot_ly() %>% 
    add_bars(data=subset(Loss,opacity!=1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    add_bars(data=subset(Loss,opacity==1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    layout(hovermode = "closest", title= "Losses",
           xaxis=list(title="Run"),
           yaxis=list(title="Count"
           )) %>% 
    config(displayModeBar = F,showLink = F)
  
  
})

### native plotly

output$tm_noLosses <- renderPlotly({
  
 # if(is.null(resData)) return()
  L <- resData()$L
  
  
  
  Loss <- L %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  
  
  
  
  if (tail(L,1)$value==0) {
    curr <- tail(L,1)$slength 
    Loss <-  Loss %>% 
      mutate(opacity=ifelse(slength==curr,1,0.3))
  } else {
    Loss$opacity <- 0.3
  }
  
  
  plot_ly() %>% 
    add_bars(data=subset(Loss,opacity!=1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="text",
             text=~paste("Run:",slength,"<br> Count:",n)) %>% 
    add_bars(data=subset(Loss,opacity==1.0),x=~slength,y=~n,showlegend=F,
             #group= ~slength, # could equally be slength
             hoverinfo="textA",
             textA=~paste("Run:",slength,"<br> Count:",n)) %>% 
    layout(hovermode = "closest", title= "No Losses",
           xaxis=list(title="Run"),
           yaxis=list(title="Count"
           )) %>% 
    config(displayModeBar = F,showLink = F)
  
  
  
  
  
})



output$tmWinSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
 write_csv(resData()$W,"problem.csv")
  # could put in reactive but not sure worthwhile
  long <- resData()$W %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
 
  if (input$seqVenue=="Home") {
tbl <-  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&venue=="H")
 }  else if(input$seqVenue=="Away") {
     tbl <-  standings %>% 
       ungroup() %>% 
       filter(team==input$teamA&venue=="A")  
} else {
  tbl <-  standings %>% 
    ungroup() %>% 
    
    filter(team==input$teamA)
}

 tbl %>% 
#    mutate(gameOrder=row_number()) %>% 
#  filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
   arrange(tmGameOrder) %>% 
   filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmNoWinSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
  
  
  long <- resData()$W %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A")  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      
      filter(team==input$teamA)
  }
  
  tbl %>% 
    # mutate(gameOrder=row_number()) %>% 
    # filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
    arrange(tmGameOrder) %>% 
    filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmDrawSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
  
  # could put in reactive but not sure worthwhile
  long <- resData()$D %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A")  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      
      filter(team==input$teamA)
  }
  
  tbl %>% 
    arrange(tmGameOrder) %>% 
    filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
  
})

output$tmNoDrawSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
  
  
  long <- resData()$D %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A")  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      
      filter(team==input$teamA)
  }
  
  tbl %>% 
    arrange(tmGameOrder) %>% 
    filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
  
})


output$tmLossSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
  
  # could put in reactive but not sure worthwhile
  long <- resData()$L %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A")  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      
      filter(team==input$teamA)
  }
  
  tbl %>% 
    arrange(tmGameOrder) %>% 
    filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
  
})

output$tmNoLossSeq <- DT::renderDataTable({
  #if(is.null(resData())) return()
  
  
  long <- resData()$L %>% 
    filter(value==0) %>%  
    filter(slength==max(slength)) %>% 
    tail(1)
  
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A")  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      
      filter(team==input$teamA)
  }
  
  tbl %>% 
    arrange(tmGameOrder) %>% 
    filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
  
})