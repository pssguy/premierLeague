

output$st_BoxAll <- renderPlotly({
  
  df <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==16) %>% 
    select(team,season,cumPts,tmYrGameOrder)
  
  plot_ly(df,y=cumPts,x=season, type = "box", key=season) %>% 
  layout(hovermode = "closest", autosize= T,
         title = "Points after 16 Games 2015/16",
         xaxis = list(title = "",tickfont=list(size=10)),
         yaxis = list(title="Points")
  )
  
})

## crosstalk to get to table of those results
cv <- crosstalk::ClientValue$new("plotly_click", group = "A")

output$st_BoxSeason <- renderPlotly({
  s <- cv$get()
  
  if (length(s) == 0) {
    return()
  } else {
    yr <- s[["key"]]
  }
  print(yr)
  
  
  df <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==16&season==yr) %>% 
    select(team,season,cumPts,tmYrGameOrder)
  
  plot_ly(df,y=cumPts, type = "box", boxpoints = "all", jitter = 0.3, color=team,
          pointpos = -1.8, hoverinfo="text", text = paste(team,"<br> Points:",cumPts)) %>%
    layout(hovermode = "closest", autosize= F, width=600, height= 700, 
           title = "Points after 16 Games 2015/16",
           xaxis = list( title = ""),
           yaxis = list(title = "",tickfont=list(size=0)),
           legend=list(y=0.95,font=list(size=15))
    )
  
})