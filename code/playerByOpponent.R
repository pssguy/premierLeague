

output$playerByOpponent <- DT::renderDataTable({
  
  if(is.null(input$playerA)) return()
  
  player <- playerGame %>% 
    filter(PLAYERID==input$playerA)
  
  
  squad <-player %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(squad=n)
  
  starts <-player %>% 
    filter(START>0) %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(starts=n)
  
  on  <- player %>% 
    filter(subOn>0) %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(on=n)
  
  
  bench  <- player %>% 
    filter(subOn==0&START==0) %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(bench=n)
  
  off  <- player %>% 
    filter(OFF>0) %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(Off=n)
  
  red <- player %>% 
    filter(CARD>"a"&CARD!="Y") %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(red=n)
  
  yellow <- player %>% 
    filter(CARD=="Y") %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(yellow=n)
  
  
  goals <- player %>% 
    group_by(Opponents) %>% 
    summarize(goals=sum(Gls)) 
  
  assists <- player %>% 
    group_by(Opponents) %>% 
    summarize(assists=sum(Assists)) 
  
 
  
  results <- 
    player %>% 
    filter((START+subOn)>0) %>% 
    select(team=TEAMNAME,gameDate,Opponents) %>% 
    inner_join(standings)
  
  wins <- results %>% 
    filter(res=="Win") %>%
    group_by(Opponents) %>% 
    tally() %>% 
    rename(wins=n)
  
  draws <- results %>% 
    filter(res=="Draw") %>%
    group_by(Opponents) %>% 
    tally() %>% 
    rename(draws=n)
  
  losses <- results %>% 
    filter(res=="Loss") %>%
    group_by(Opponents) %>% 
    tally() %>% 
    rename(losses=n)
  
  byOpponent <-
    squad %>% 
    left_join(starts) %>% 
    left_join(on) %>% 
    left_join(off) %>% 
    left_join(bench) %>% 
    left_join(goals) %>% 
    left_join(assists) %>% 
    left_join(red) %>% 
    left_join(yellow) %>% 
    left_join(wins) %>% 
    left_join(draws) %>% 
    left_join(losses) 
  
  byOpponent[is.na(byOpponent)]<-0
  
  byOpponent %>% 
    mutate(apps=starts+on) %>% 
    select(Opponents,apps,starts:losses) %>% 
    DT::datatable(class='compact stripe hover row-border',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
  
})