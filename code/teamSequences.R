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
  
  write_csv(W,"prob_All.csv")
  
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
    
    write_csv(W,"prob_H.csv")
    
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
    
    write_csv(W,"prob_A.csv")
    
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
  
  W <- resData()$W
  
  Win <- W %>%
    filter(value == 1) %>%
    group_by(slength) %>%
    tally()
 

  if (tail(W, 1)$value == 1) {
    run <- tail(W, 1)$slength
    print(run)
    count <-  Win %>%
      filter(slength == run) %>%
      .$n
    print(Win)
    print(count)
    
    p <- Win %>%
      plot_ly(x =  ~ slength, y =  ~ n) %>%
      add_bars() %>%
      add_bars(x = run,
               y = count,
               color = I("red"))
  } else{
    p <- Win %>%
      plot_ly(x =  ~ slength, y =  ~ n) %>%
      add_bars()
  }
  
  p %>%
    layout(
      hovermode = "closest",
      title = "Wins",
      barmode = "overlay",
      showlegend = FALSE,
      xaxis = list(title = "Run"),
      yaxis = list(title = "Count")
    ) %>%
    config(displayModeBar = F, showLink = F)
  

 

})



output$tm_noWins <- renderPlotly({
  
  W <- resData()$W
  
  Win <- W %>%
    filter(value == 0) %>%
    group_by(slength) %>%
    tally()
  
  
  if (tail(W, 1)$value == 0) {
    run <- tail(W, 1)$slength
    print(run)
    count <-  Win %>%
      filter(slength == run) %>%
      .$n
    print(Win)
    print(count)
    
    p <- Win %>%
      plot_ly(x =  ~ slength, y =  ~ n) %>%
      add_bars() %>%
      add_bars(x = run,
               y = count,
               color = I("red"))
  } else{
    p <- Win %>%
      plot_ly(x =  ~ slength, y =  ~ n) %>%
      add_bars()
  }
  
  p %>%
    layout(
      hovermode = "closest",
      title = "No Wins",
      barmode = "overlay",
      showlegend = FALSE,
      xaxis = list(title = "Run"),
      yaxis = list(title = "Count")
    ) %>%
    config(displayModeBar = F, showLink = F)
  
  

 })

output$tm_draws <- renderPlotly({

D <- resData()$D

Draw <- D %>%
  filter(value == 1) %>%
  group_by(slength) %>%
  tally()


if (tail(D, 1)$value == 1) {
  run <- tail(D, 1)$slength
 
  count <-  Draw %>%
    filter(slength == run) %>%
    .$n
  
  
  p <- Draw %>%
    plot_ly(x =  ~ slength, y =  ~ n) %>%
    add_bars() %>%
    add_bars(x = run,
             y = count,
             color = I("red"))
} else{
  p <- Draw %>%
    plot_ly(x =  ~ slength, y =  ~ n) %>%
    add_bars()
}

p %>%
  layout(
    hovermode = "closest",
    title = "Draws",
    barmode = "overlay",
    showlegend = FALSE,
    xaxis = list(title = "Run"),
    yaxis = list(title = "Count")
  ) %>%
  config(displayModeBar = F, showLink = F)




})
  
  
  
  output$tm_noDraws <- renderPlotly({
    
    D <- resData()$D
    
    Draw <- D %>%
      filter(value == 0) %>%
      group_by(slength) %>%
      tally()
    
    
    if (tail(D, 1)$value == 0) {
      run <- tail(D, 1)$slength
     
      count <-  Draw %>%
        filter(slength == run) %>%
        .$n
      
      
      p <- Draw %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars() %>%
        add_bars(x = run,
                 y = count,
                 color = I("red"))
    } else{
      p <- Draw %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars()
    }
    
    p %>%
      layout(
        hovermode = "closest",
        title = "No Draws",
        barmode = "overlay",
        showlegend = FALSE,
        xaxis = list(title = "Run"),
        yaxis = list(title = "Count")
      ) %>%
      config(displayModeBar = F, showLink = F)
    
    
    
  })
  
  
  output$tm_losses <- renderPlotly({
    
    L <- resData()$L
    
    Loss <- L %>%
      filter(value == 1) %>%
      group_by(slength) %>%
      tally()
    
    
    if (tail(L, 1)$value == 1) {
      run <- tail(L, 1)$slength
      
      count <-  Loss %>%
        filter(slength == run) %>%
        .$n
      
      
      p <- Loss %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars() %>%
        add_bars(x = run,
                 y = count,
                 color = I("red"))
    } else{
      p <- Loss %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars()
    }
    
    p %>%
      layout(
        hovermode = "closest",
        title = "Losses",
        barmode = "overlay",
        showlegend = FALSE,
        xaxis = list(title = "Run"),
        yaxis = list(title = "Count")
      ) %>%
      config(displayModeBar = F, showLink = F)
    
    
    
    
  })
  
  
  
  output$tm_noLosses <- renderPlotly({
    
    L <- resData()$L
    
    Loss <- L %>%
      filter(value == 0) %>%
      group_by(slength) %>%
      tally()
    
    
    if (tail(L, 1)$value == 0) {
      run <- tail(L, 1)$slength
      
      count <- Loss %>%
        filter(slength == run) %>%
        .$n
      
      
      p <- Loss %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars() %>%
        add_bars(x = run,
                 y = count,
                 color = I("red"))
    } else{
      p <- Loss %>%
        plot_ly(x =  ~ slength, y =  ~ n) %>%
        add_bars()
    }
    
    p %>%
      layout(
        hovermode = "closest",
        title = "No Losses",
        barmode = "overlay",
        showlegend = FALSE,
        xaxis = list(title = "Run"),
        yaxis = list(title = "Count")
      ) %>%
      config(displayModeBar = F, showLink = F)
    
    
    
  })



output$tmWinSeq <- DT::renderDataTable({
  
  long <- resData()$W %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})

output$tmNoWinSeq <- DT::renderDataTable({
  
  long <- resData()$W %>% 
    filter(value==0) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})




output$tmDrawSeq <- DT::renderDataTable({
  
  long <- resData()$D %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})


output$tmNoDrawSeq <- DT::renderDataTable({
  
  long <- resData()$D %>% 
    filter(value==0) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})


output$tmLossSeq <- DT::renderDataTable({
  
  long <- resData()$L %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})


output$tmNoLossSeq <- DT::renderDataTable({
  
  long <- resData()$L %>% 
    filter(value==0) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  if (input$seqVenue=="Home") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="H") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number()) 
    
  }  else if(input$seqVenue=="Away") {
    tbl <-  standings %>% 
      ungroup() %>% 
      filter(team==input$teamA&venue=="A") %>% 
      arrange(tmGameOrder) %>% 
      mutate(theOrder=row_number())  
  } else {
    tbl <-  standings %>% 
      ungroup() %>% 
      arrange(tmGameOrder) %>% 
      filter(team==input$teamA) %>% 
      mutate(theOrder=row_number()) 
    
  }
  
  tbl %>% 
    arrange(theOrder) %>% 
    filter(theOrder>=long$first&theOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})




# output$tmWinSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   write_csv(resData()$W,"problem.csv")
#   # could put in reactive but not sure worthwhile
#   long <- resData()$W %>% 
#     filter(value==1) %>%  # need to restrict to wins first
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     #    mutate(gameOrder=row_number()) %>% 
#     #  filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
# })
# 

# output$tmNoWinSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   
#   
#   long <- resData()$W %>% 
#     filter(value==0) %>%  
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     # mutate(gameOrder=row_number()) %>% 
#     # filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
# })

# output$tmDrawSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   
#   # could put in reactive but not sure worthwhile
#   long <- resData()$D %>% 
#     filter(value==1) %>%  # need to restrict to wins first
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
#   
# })

# output$tmNoDrawSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   
#   
#   long <- resData()$D %>% 
#     filter(value==0) %>%  
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
#   
# })
# 
# 
# output$tmLossSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   
#   # could put in reactive but not sure worthwhile
#   long <- resData()$L %>% 
#     filter(value==1) %>%  # need to restrict to wins first
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
#   
# })
# 
# output$tmNoLossSeq <- DT::renderDataTable({
#   #if(is.null(resData())) return()
#   
#   
#   long <- resData()$L %>% 
#     filter(value==0) %>%  
#     filter(slength==max(slength)) %>% 
#     tail(1)
#   
#   
#   if (input$seqVenue=="Home") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="H")
#   }  else if(input$seqVenue=="Away") {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       filter(team==input$teamA&venue=="A")  
#   } else {
#     tbl <-  standings %>% 
#       ungroup() %>% 
#       
#       filter(team==input$teamA)
#   }
#   
#   tbl %>% 
#     arrange(tmGameOrder) %>% 
#     filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
#     mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
#     select(Opponents,Score,Date=gameDate) %>% 
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   
#                   options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
#   
#   
# })