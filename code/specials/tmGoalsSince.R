

output$tmGoalsSince <- DT::renderDataTable({
  
  if (input$goalsSinceCat=="Goals For") {
  latestTime <-standings %>% 
  ungroup() %>% 
  filter(GF>=input$goalsSinceCount) %>% 
  arrange(desc(tmGameOrder)) %>% 
  group_by(team) %>% 
  slice(1)  %>% 
  ungroup() %>% 
  select(team,latest=tmGameOrder)

firstTime <-standings %>% 
  ungroup() %>% 
  filter(GF>=input$goalsSinceCount) %>%  
  arrange(tmGameOrder) %>% 
  group_by(team) %>% 
  slice(1)  %>% 
  ungroup() %>% 
  select(team,first=tmGameOrder)

allGames <- standings %>% 
  ungroup() %>% 
  arrange(desc(tmGameOrder)) %>% 
  group_by(team) %>% 
  slice(1)  %>% 
  ungroup() %>% 
  select(team,total=tmGameOrder)

standings %>% 
  ungroup() %>% 
  filter(GF>=input$goalsSinceCount) %>%  
  group_by(team) %>% 
  tally() %>% 
  rename(count=n)  %>% 
  inner_join(firstTime) %>% 
  inner_join(latestTime) %>% 
  right_join(allGames) %>% 
  mutate(count=ifelse(is.na(count),0,count)) %>% 
  mutate(since=ifelse(count!=0,total-latest,NA)) %>% 
  select(team,count,total,first,since) %>% 
  DT::datatable(width=350,rownames=FALSE,class='compact stripe hover row-border',options= list(pageLength = 20,paging = TRUE, searching = FALSE,info=FALSE))
  } else if (input$goalsSinceCat=="Goals Ag") {
    
    latestTime <-standings %>% 
      ungroup() %>% 
      filter(GA>=input$goalsSinceCount) %>% 
      arrange(desc(tmGameOrder)) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,latest=tmGameOrder)
    
    firstTime <-standings %>% 
      ungroup() %>% 
      filter(GA>=input$goalsSinceCount) %>%  
      arrange(tmGameOrder) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,first=tmGameOrder)
    
    allGames <- standings %>% 
      ungroup() %>% 
      arrange(desc(tmGameOrder)) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,total=tmGameOrder)
    
    standings %>% 
      ungroup() %>% 
      filter(GA>=input$goalsSinceCount) %>%  
      group_by(team) %>% 
      tally() %>% 
      rename(count=n)  %>% 
      inner_join(firstTime) %>% 
      inner_join(latestTime) %>% 
      right_join(allGames) %>% 
      mutate(count=ifelse(is.na(count),0,count)) %>% 
      mutate(since=ifelse(count!=0,total-latest,NA)) %>% 
      select(team,count,total,first,since) %>% 
      DT::datatable(width=350,rownames=FALSE,class='compact stripe hover row-border',options= list(pageLength = 20,paging = TRUE, searching = FALSE,info=FALSE))
  } else {
    latestTime <-standings %>% 
      ungroup() %>% 
      filter((GF+GA)>=input$goalsSinceCount) %>% 
      arrange(desc(tmGameOrder)) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,latest=tmGameOrder)
    
    firstTime <-standings %>% 
      ungroup() %>% 
      filter((GF+GA)>=input$goalsSinceCount) %>%  
      arrange(tmGameOrder) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,first=tmGameOrder)
    
    allGames <- standings %>% 
      ungroup() %>% 
      arrange(desc(tmGameOrder)) %>% 
      group_by(team) %>% 
      slice(1)  %>% 
      ungroup() %>% 
      select(team,total=tmGameOrder)
    
    standings %>% 
      ungroup() %>% 
      filter((GF+GA)>=input$goalsSinceCount) %>%  
      group_by(team) %>% 
      tally() %>% 
      rename(count=n)  %>% 
      inner_join(firstTime) %>% 
      inner_join(latestTime) %>% 
      right_join(allGames) %>% 
      mutate(count=ifelse(is.na(count),0,count)) %>% 
      mutate(since=ifelse(count!=0,total-latest,NA)) %>% 
      select(team,count,total,first,since) %>% 
      DT::datatable(width=350,rownames=FALSE,class='compact stripe hover row-border',options= list(pageLength = 20,paging = TRUE, searching = FALSE,info=FALSE))
  }
    
})