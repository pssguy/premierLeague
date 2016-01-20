


output$player_ppg <- renderPlotly({
  
  if(is.null(input$playerA)) return()
  
 
  
  ppgPlayer <- playerGame %>%
    filter(PLAYERID==input$playerA&(START+subOn)>0) %>% 
    mutate(gameOrder=row_number())
  
   games <- nrow(ppgPlayer)
  
  xTitle <- paste0("Appearance Order - Total ",games)

  
  plot_ly(ppgPlayer , x=plGameOrder, y=Gls, name="Goals", type="bar",
          hoverinfo="text",
          text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
    add_trace(x=plGameOrder,y=Assists, name="Assists (inc secondary)")  %>%
    layout(hovermode = "closest", barmode="stack",
         
          xaxis=list(title=xTitle),
           yaxis=list(title="Points"),
           title=" Hover bar for details", titlefont=list(size=16)
    )
  
})


## headline version which defaults to vardy
output$player_ppg_hl <- renderPlotly({
  
  if(is.null(input$playerppg)) return()
  
  
  
  ppgPlayer <- playerGame %>%
    filter(PLAYERID==input$playerppg&(START+subOn)>0) %>% 
    mutate(gameOrder=row_number())
  
  games <- nrow(ppgPlayer)
  
  xTitle <- paste0("Appearance Order - Total ",games)
  
  
  plot_ly(ppgPlayer , x=plGameOrder, y=Gls, name="Goals", type="bar",
          hoverinfo="text",
          text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
    add_trace(x=plGameOrder,y=Assists, name="Assists (inc secondary)")  %>%
    layout(hovermode = "closest", barmode="stack",
           
           xaxis=list(title=xTitle),
           yaxis=list(title="Points"),
           title=" Hover bar for details", titlefont=list(size=16)
    )
  
})