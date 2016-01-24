

pboData <- reactive({
  
  print("enter reactive")
  
  #if(is.null(input$playerA)) return()
  req(input$playerA)
  
  player <- playerGame %>% 
    filter(PLAYERID==input$playerA)
  #filter(PLAYERID=="ROONEYX")
  
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
  
  R <- player %>% 
    filter(CARD>"a"&CARD!="Y") %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(R=n)
  
  Y <- player %>% 
    filter(CARD=="Y") %>% 
    group_by(Opponents) %>% 
    tally() %>% 
    rename(Y=n)
  
  
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
    rename(W=n)
  
  draws <- results %>% 
    filter(res=="Draw") %>%
    group_by(Opponents) %>% 
    tally() %>% 
    rename(D=n)
  
  losses <- results %>% 
    filter(res=="Loss") %>%
    group_by(Opponents) %>% 
    tally() %>% 
    rename(L=n)
  
  byOpponent <-
    squad %>% 
    left_join(starts) %>% 
    left_join(on) %>% 
    left_join(off) %>% 
    left_join(bench) %>% 
    left_join(goals) %>% 
    left_join(assists) %>% 
    left_join(R) %>% 
    left_join(Y) %>% 
    left_join(wins) %>% 
    left_join(draws) %>% 
    left_join(losses) 
  
  byOpponent[is.na(byOpponent)]<-0
  
 
  
  byOpponent <-  byOpponent %>% 
    mutate(apps=starts+on) %>% 
    select(Opponents,apps,starts:L)
  
  print(glimpse(byOpponent))
  
  info=list(byOpponent=byOpponent)
  return(info)
 
}) 
  
    
    output$playerByOpponent <- DT::renderDataTable({
      # print("enter pbo")
      # if(is.null(pboData())) return()
      print("entered pbo")
      pboData()$byOpponent %>% 
            DT::datatable(selection='single',class='compact stripe hover row-border',
                  colnames = c('Opponents', 'Apps', 'St', 'On', 'Off', 'Bnch', 'Gls','Ass','R','Y','W','D','L'),
                  rownames=FALSE,options = list(
      searching = FALSE,info = FALSE,
      pageLength = 15
    ))
  
  
})

# click to give more info

observeEvent(input$playerByOpponent_rows_selected,{
  s = as.integer(input$playerByOpponent_rows_selected)
  print(pboData()$byOpponent$Opponents[s])
  print(input$playerA)
  values$Opponents <- pboData()$byOpponent$Opponents[s]
#   values$playerID <- teamData()$mostGames$PLAYERID[s]
#   updateTabItems(session, inputId="sbMenu", selected="pl_glance")
})

output$plOpponentSummary <- DT::renderDataTable({
  if (is.null(values$Opponents)) return()
  playerGame %>% 
    filter(PLAYERID==input$playerA&Opponents==values$Opponents) %>% 
    arrange(desc(gameDate)) %>% 
    inner_join(standings,by = c("gameDate" = "gameDate","Opponents"="OppTeam")) %>% 
    mutate(res=paste0(GF,"-",GA)) %>%
    select(gameDate,TEAMNAME,res,st,on,off,Gls,Assists,CARD) %>% 
    DT::datatable(class='compact stripe hover row-border',colnames = c('Date', 'Team','Res ', 'St', 'On', 'Off', 'Gls', 'Ass','Card'),rownames=FALSE,options= list(pageLength = 8, searching = FALSE,info=FALSE))
  
})