## probbaly need to have set of selectors a la PFA - currently venue / look at opponent/ individual year


#resData <- eventReactive(input$teamA,{ this works until another reactive is added eg input$seqVenue
  goalSeqData <- reactive({
    
    req(input$teamA)
    req(input$seqVenueB)

    GF <-standings %>% 
      ungroup() %>% 
      filter(team==input$teamA) %>% 
      arrange(tmGameOrder) %>% 
      select(GF,tmGameOrder) %>% 
      mutate(cat=ifelse(GF>0,1,0)) %>% 
      do(subSeq(.$cat))
    
  info=list(GF=GF)
  return(info)
  
})

output$tm_goalFor <- renderPlot({

  #if(is.null(goalSeqData())) return()
  GF <- goalSeqData()$GF
  
  For <- GF %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  if (tail(GF,1)$value==1) {
    cond <- For$slength == tail(GF,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(For, aes(x=slength,y=n)) +
    geom_bar(data=subset(For,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(For,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Run of Games Scored In")
  
}, height=300)



 output$tmSeqGF <- DT::renderDataTable({
 # if(is.null(goalSeqData())) return()

  # think need to restrict this - but why no issue with results``   
  long <- goalSeqData()$GF %>% 
    filter(value==1) %>%  # need to restrict to wins first
    filter(slength==max(slength)) %>% 
    tail(1)
  
  print(long)
 
  if (input$seqVenueB=="Home") {
tbl <-  standings %>% 
    ungroup() %>% 
    filter(team==input$teamA&venue=="H")
 }  else if(input$seqVenueB=="Away") {
     tbl <-  standings %>% 
       ungroup() %>% 
       filter(team==input$teamA&venue=="A")  
} else {
  tbl <-  standings %>% 
    ungroup() %>% 
    
    filter(team==input$teamA)
}

 tbl %>% 
    mutate(gameOrder=row_number()) %>% 
  filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
    mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
    select(Opponents,Result=res,Score,Date=gameDate) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  
                  options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = 1))))
  
})
