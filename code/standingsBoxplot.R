

output$st_BoxAll <- renderPlotly({
  
  df <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$st_boxGames) %>% 
    select(team,season,cumPts,tmYrGameOrder)
  
  plot_ly(df,y=cumPts,x=season, type = "box", key=season) %>% 
  layout(hovermode = "closest", autosize= F, width=800, ## width does not seem to have effect
         title = paste0("Points after ",input$st_boxGames," Games"),
         xaxis = list(title = "",tickfont=list(size=10)),
         yaxis = list(title="")
  )
  
})

## crosstalk to get to table of those results
#cv <- crosstalk::ClientValue$new("plotly_click", group = "A")

output$st_BoxSeason <- renderPlotly({
  #s <- cv$get()
  s <- event_data("plotly_click")
  
  if (length(s) == 0) {
    return()
  } else {
    yr <- s[["x"]]
  }
  print(yr)
  print(s[["x"]])
  print(s[["y"]])
  
  
  df <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$st_boxGames&season==yr) %>% 
    select(team,season,cumPts,tmYrGameOrder) %>% 
    arrange(desc(cumPts))
  
  print(df)
  theTitle <- paste0(yr, " - Points after ",input$st_boxGames," Games")
  
  plot_ly(df,y=cumPts, type = "box", boxpoints = "all", jitter = 0.3, color=team,
          pointpos = -1.8, hoverinfo="text", text = paste(team,"<br> Points:",cumPts)) %>%
    layout(hovermode = "closest", autosize= F, width=800, 
           title = theTitle,
           xaxis = list( title = "",tickfont=list(size=0,color="#fff")),
           yaxis = list(title = ""),
           legend=list(y=1,font=list(size=12))
    )
  
})


### alternative exploding boxplot

output$st_explodingBoxAll <- renderExploding_boxplot({
  
  print("enter exploding")
  
  
  df <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$st_boxGames) %>% 
    select(team,season,cumPts,tmYrGameOrder)  %>% 
    mutate(year=str_sub(season,1,4)) %>% 
    rename(points=cumPts)
  
  print("expl ")
  print(glimpse(df))
  
  exploding_boxplot(
    df,
    y = "points",
    group = "year",
    width= 600,
    label = "team", 
    iqr = 2,
    margin = list(bottom = 30, left = 50, top = 20, right = 20)
    
  )
})

output$test <-  renderExploding_boxplot({
  exploding_boxplot(
    data.frame(
      rowname = rownames(InsectSprays),
      InsectSprays,
      stringsAsFactors = FALSE
    ),
    y = "count",
    group = "spray",
    color = "spray",
    label = "rowname"
  )
  
})