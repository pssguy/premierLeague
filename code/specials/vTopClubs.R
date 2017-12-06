
output$vTopClubs_front <- renderPlotly({
  
  req(input$player_tc)


temp <-playerGame %>% 
  filter(PLAYERID==input$player_tc&(START+subOn)>0) %>% 
  group_by(Opponents) %>% 
  summarise(apps=n(),mins=sum(mins),goals=sum(Gls),assists=sum(Assists),points=goals+assists,pp90mins=points*90/mins) 



## alternative might be categoryarray / categoryorder ?? orderRule (seasrch in annualCode this file)
temp$Opponents <-
  factor(temp$Opponents, levels = temp$Opponents[order(temp$pp90mins)])


topOpps <- c("Arsenal","Chelsea","Liverpool","Man. City","Man. Utd.","Tottenham H")

top <- temp %>% 
  filter(Opponents %in% topOpps)
rest <- temp %>% 
  filter(!Opponents %in% topOpps)



top %>% 
  plot_ly(x=~pp90mins,y=~Opponents,
          width=0.3,
          height=800) %>% 
  add_bars(name="Top Clubs",
           hoverOver="text",
           text=~paste0("Apps: ",apps,"<br>Goals: ",goals,"<br>Assists: ",assists)) %>% 
  add_bars(data=rest,name="Rest of Pack",opacity=0.5,
           hoverOver="text",
           text=~paste0("Apps: ",apps,"<br>Goals: ",goals,"<br>Assists: ",assists)) %>% 
  layout(
    margin=list(l=120),
    title="Goals+Assists(max 2 per goal) per 90 minutes, by Opposition",
    xaxis=list(title="Per 90 minutes"),
    yaxis=list(title="")
  ) %>% 
  config(displayModeBar = F,showLink = F) 
})