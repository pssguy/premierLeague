

## issue with fitting on ui - not sure taucharts issue
output$playerTransfers_tau <- renderTaucharts({
  
  if(is.null(input$playerA)) return()
  transfers <-  playerGame %>% 
    filter(PLAYERID==input$playerA&PERMANENT==1) 
  
  print("nrow transfers")
  print(nrow(transfers))
  if (nrow(transfers)==0) {
    write_csv(transfers,"loan.csv")
    return(NULL) # this works but does not replace previous plot
  }
  transfers <-transfers %>% 
    select(name,joined,FEE,TEAMNAME) %>% 
    unique() %>% 
    mutate(Cost=ifelse(FEE==0,0,FEE/1000)) %>% 
    rename(Fee=FEE,Team=TEAMNAME,Date=joined) %>% 
    tauchart() %>% tau_point("Date", "Cost",color="Team") %>% 
        #tau_legend()  %>% 
        tau_tooltip(c("Date","Team","Cost")) %>% 
        tau_guide_x(tick_period='day', tick_format="year", label="") %>% 
        tau_guide_y(label ='Purchase Price (mill)')
  

})


output$playerTransfers_plotly <- renderPlotly({
  
  if(is.null(input$playerA)) return()
  transfers <-  playerGame %>% 
    filter(PLAYERID==input$playerA&PERMANENT==1) 
  
  print("nrow transfers")
  print(nrow(transfers))
  if (nrow(transfers)==0) {
    write_csv(transfers,"loan.csv")
    return(NULL) # this works but does not replace previous plot
  }
  transfers <-transfers %>% 
    select(name,joined,FEE,TEAMNAME) %>% 
    unique() %>% 
    mutate(Cost=ifelse(FEE==0,0,FEE/1000000)) %>% 
    rename(Fee=FEE,Team=TEAMNAME,Date=joined) %>% 
    arrange(Date) %>% 
    plot_ly() %>% 
    add_markers(x=~Date, y=~Fee,color=~as.factor(Team),marker=list(size=10))
  
  
})