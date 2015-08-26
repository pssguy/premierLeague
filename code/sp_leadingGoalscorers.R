

output$leadingGoalscorers_tau <- renderTaucharts({
  
  print(input$teamE)
  
  if(input$teamE=="All Teams") {
    test <-playerGame %>% 
      group_by(TEAMNAME,season,PLAYERID,name) %>% 
      summarize(goals=sum(Gls),pens=sum(PENS)) %>% 
      # mutate(pointSize=0.5) %>% attempt to make points smaller
      filter(goals>=input$goalA) %>% 
      rename(team=TEAMNAME) %>% 
      ungroup() %>% 
      arrange(season,team)
  } else {
  
  test <-playerGame %>% 
    group_by(TEAMNAME,season,PLAYERID,name) %>% 
    summarize(goals=sum(Gls),pens=sum(PENS)) %>% 
   # mutate(pointSize=0.5) %>% attempt to make points smaller
    filter(goals>=input$goalA&TEAMNAME %in% input$teamE) %>% 
    rename(team=TEAMNAME) %>% 
    ungroup() %>% 
    arrange(season,team)
}
  #str(test)
  
  
  tauchart(test) %>% tau_point("season", "goals",color="team") %>% 
    tau_legend()  %>% 
    tau_tooltip(c("name","team","goals","pens")) %>% 
    #tau_guide_x(tick_period='day', tick_format="year") %>% 
    tau_guide_y(min= 19, max = 40) # ineffective
  
  
})