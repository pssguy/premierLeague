
fullGamesdata <- reactive({
  
  if (input$sp_pcFullGamsTeams=="All Teams") {
    df <-playerGame %>% 
      filter(mins>0) %>% 
      mutate(fullGame=(ifelse(mins==90,1,0))) %>% 
      group_by(PLAYERID,name,POSITION) %>% 
      summarize(count=n(),fullPC=round(100*mean(fullGame),1)) %>% 
      ungroup() %>% 
      filter(count>=input$sp_pcFullGames) 
  } else {
    df <-playerGame %>% 
      filter(mins>0&TEAMNAME==input$sp_pcFullGamsTeams) %>% 
      mutate(fullGame=(ifelse(mins==90,1,0))) %>% 
      group_by(PLAYERID,name,POSITION) %>% 
      summarize(count=n(),fullPC=round(100*mean(fullGame),1)) %>% 
      ungroup() %>% 
      filter(count>=input$sp_pcFullGames) 
  }
  
  info=list(df=df)
  return(info)
})


output$pcFullGames <- renderPlotly({
 
  df <- fullGamesdata()$df
 
  
  plot_ly(df, x = count, y = fullPC, mode = "markers", hoverinfo = "text",color=POSITION,key=PLAYERID,
          text = paste(name,
                       "<br>Appearances:",count,
                       "<br>Full games:",fullPC,"%")) %>%
    layout(hovermode = "closest",
           xaxis=list(title="Games Played"),
           yaxis=list(title="% Complete Games"
           )
    )
}) 


  
  ## crosstalk to get to individual chart
  cv <- crosstalk::ClientValue$new("plotly_click", group = "A")
  
  
  output$selection <- renderPrint({
    s <- cv$get()
    print(s)
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s) # get back x and y values and point numvber which looks like rownumber
    }
  })
  
  output$pcFullGamesDets <- renderPlotly({
    s <- cv$get()
    if (length(s)==0) return()
    
    df <- fullGamesdata()$df

    
  dets <- playerGame %>% 
    filter(PLAYERID==s[["key"]]) %>% 
    select(gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID,name) %>% 
    mutate(points=Goals+Assists)
  
  theTitle <- unique(dets$name)
  
  ## plotly
  
  plot_ly(dets, x = plGameOrder, y = mins,
          mode='markers',
          marker = list(sizeref = 20),
          color=Team,
          #size=points, leave out until sorted in plotly
          hoverinfo = "text",
          text = paste(Team," v ",Opponents,
                       "<br>",gameDate,
                       "<br>On: ",on,
                       "<br>Off: ",off,
                       "<br>Goals ",Goals,
                       "<br>Assists ",Assists)
  ) %>%
    layout( title=theTitle,
      hovermode = "closest",
           xaxis=list(title="Game Order"),
           yaxis=list(title="Minutes Played")
    )
  
})
