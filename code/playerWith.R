

output$playedWith <- DT::renderDataTable({
  
  if (!is.null(input$player)) {
    thePlayer <- input$player
  } else {
    thePlayer=="ROONEYX"
  }
  
  playerGame %>% 
    filter(PLAYERID==thePlayer&(START+subOn)>0)  %>% # 408 to 406
    select(TEAMMATCHID,MATCHID) -> temp
  
  
  playerGame %>% 
    filter(TEAMMATCHID %in% temp$TEAMMATCHID&(START+subOn)>0) %>% 
    select(name,PLAYERID,TEAMNAME) %>% 
    group_by(PLAYERID,name) %>%
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(pc=round(100*n/max(n),1)) %>% 
    select(Player=name,Games=n,Percent=pc)-> allAlongside
  
  DT::datatable(allAlongside,options=list(paging = FALSE, searching = TRUE,info = FALSE))
})


output$playedAgainst <- DT::renderDataTable({
  
  if (!is.null(input$player)) {
    thePlayer <- input$player
  } else {
    thePlayer=="ROONEYX"
  }
  
  playerGame %>% 
    filter(PLAYERID==thePlayer&(START+subOn)>0)  %>% # 408 to 406
    select(TEAMMATCHID,MATCHID) -> temp
  
  
  playerGame %>% 
    filter(MATCHID %in% temp$MATCHID&!(TEAMMATCHID %in% temp$TEAMMATCHID)&(START+subOn)>0) %>% 
    select(name,PLAYERID,TEAMNAME) %>% 
    group_by(PLAYERID,name) %>%
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    select(Player=name,Games=n)-> allAgainst
  
  
  DT::datatable(allAgainst,options=list(paging = TRUE, searching = TRUE,info = TRUE))
})