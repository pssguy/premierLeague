

output$twoClubs <- DT::renderDataTable({
  
  
  #teams <- c("Chelsea","Tottenham H")
  teams <- input$twoTeams
  
  if(input$twoTeamsApp=="Appeared") {
  team1 <- playerGame %>% 
    filter(TEAMNAME==teams[1]&(START+subOn)>0) %>% 
    group_by(PLAYERID,name) %>% 
    tally() %>%
    ungroup() %>% 
    select(-PLAYERID)
  
  team2 <- playerGame %>% 
    filter(TEAMNAME==teams[2]&(START+subOn)>0) %>% 
    group_by(PLAYERID,name) %>% 
    tally() %>%
    ungroup() %>% 
    select(-PLAYERID)
  } else {
    
    team1 <- playerGame %>% 
      filter(TEAMNAME==teams[1]&PLAYERID!="OWNGOAL") %>% 
      group_by(PLAYERID,name) %>% 
      tally() %>%
      ungroup() %>% 
      select(-PLAYERID)
    
    team2 <- playerGame %>% 
      filter(TEAMNAME==teams[2]&PLAYERID!="OWNGOAL") %>% 
      group_by(PLAYERID,name) %>% 
      tally() %>%
      ungroup() %>% 
      select(-PLAYERID)
    
  }
  names(team1)[2] <- teams[1]
  names(team2)[2] <- teams[2]
  
  team1 %>% 
    inner_join(team2) %>% 
    rename(Player=name) %>% 
    DT::datatable(class='compact stripe hover row-border order-column',options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})