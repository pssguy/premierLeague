

output$sp_yearOnYear <- renderPlotly({


test <-standings %>% 
  ungroup() %>% 
  filter(tmYrGameOrder==input$yronyrRound) %>% 
  select(team,season,cumPts) %>% 
  right_join(allSeasonTeams) %>% 
  arrange(season) %>% 
  
  group_by(team) %>% 
  mutate(prev=lag(cumPts),diff=cumPts-prev) %>% 
  filter(season>"1992/93"&team %in% input$yronyrTeam&!is.na(diff)) %>% 
  ungroup() %>% 
  arrange(team)

test$jitDiff <- jitter(test$diff)


plot_ly(test, x = season, y = jitDiff, mode = "markers", hoverinfo = "text", color=team,height=600,
        text = paste(team,"<br> Season:",season,"<br> Points:",cumPts,"<br> Yr on Yr:",diff)) %>%
  layout(hovermode = "closest", 
         xaxis=list(title="", tickfont=list(size=9)),
         yaxis=list(title="Point Difference")
  )

})
