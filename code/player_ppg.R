


output$player_ppg <- renderPlotly({
  
  #if(is.null(input$playerA)) return()
  req(input$playerA)
  
 
  
  ppgPlayer <- playerGame %>%
    filter(PLAYERID==input$playerA&(START+subOn)>0) %>% 
    mutate(gameOrder=row_number()) %>% 
    mutate(Gls=ifelse(Gls+Assists==0,0.02,Gls))
  
   games <- nrow(ppgPlayer)
  
  xTitle <- paste0("Appearance Order - Total ",games)
  
 # write_csv(ppgPlayer,"data/ppgPlayer.csv")

  ppgPlayer %>% 
    
  plot_ly(x=~gameOrder) %>% 
    add_bars(y=~Gls, name="Goals", 
          hoverinfo="text", 
          text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder,"<br>",mins ," mins")) %>%
    add_bars(y=~Assists, name="Assists (inc secondary)", 
              hoverinfo="text", 
              text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder,"<br>",mins ," mins"))  %>%
    layout(hovermode = "closest", barmode="stack",
           
           xaxis=list(title=xTitle),
           yaxis=list(title="Points"),
           title=" Hover bar for details", titlefont=list(size=16)
    )
  
  # plot_ly() %>% 
  #   add_bars(x=~gameOrder, y=~Gls, name="Goals", 
  #            hoverinfo="text", showlegend = FALSE,
  #            text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder,"<br>",mins ," mins")) %>%
  #   add_bars(x=~gameOrder,y=~Assists, name="Assists (inc secondary)", 
  #            hoverinfo="text", 
  #            text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder,"<br>",mins ," mins"))  %>%
  #   layout(hovermode = "closest", barmode="stack",
  #          
  #          xaxis=list(title=xTitle),
  #          yaxis=list(title="Points"),
  #          title=" Hover bar for details", titlefont=list(size=16)
  #   )
  
})


## headline version which defaults to vardy
output$player_ppg_hl <- renderPlotly({
  
 # if(is.null(input$playerppg)) return()
  req(input$playerppg)
  
  
  ppgPlayer <- playerGame %>%
    filter(PLAYERID==input$playerppg&(START+subOn)>0) %>% 
    mutate(gameOrder=row_number())
  
  games <- nrow(ppgPlayer)
  
  xTitle <- paste0("Appearance Order - Total ",games)
  
  ppgPlayer %>% 
    plot_ly() %>% 
  add-bars(x=plGameOrder, y=Gls, name="Goals", 
          hoverinfo="text",
          text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
    add_bars(x=~plGameOrder,y=~Assists, name="Assists (inc secondary)", 
              hoverinfo="text",
              text=~paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder))  %>%
    layout(hovermode = "closest", barmode="stack",
           
           xaxis=list(title=xTitle),
           yaxis=list(title="Points"),
           title=" Hover bar for details", titlefont=list(size=16)
    )
  
})