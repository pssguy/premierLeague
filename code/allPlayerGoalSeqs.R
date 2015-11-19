

observe({

  
  if(is.null(input$teamC)) {
    theTeam<-"All Teams"
  } else {
    theTeam<-input$teamC
  }

  ## need to have different datasets depending whether all plyers or within team
  
  if (theTeam=="All Teams") {
  bestRun <- goalSeqs %>% 
          
          select(PLAYERID,slength) %>% 
          arrange(PLAYERID,desc(slength)) %>% 
          group_by(PLAYERID) %>% 
          slice(1)
  
  topScorers <- playerGame %>% 
    filter(PLAYERID %in% bestRun$PLAYERID) %>% 
    group_by(name,PLAYERID) %>% 
    
    summarize(totGoals=sum(Gls)) %>% 
    inner_join(bestRun)
  
  } else {
    bestRun <- goalSeqsClub %>% 
      filter(TEAMNAME==theTeam) %>% 
      select(PLAYERID,slength) %>% 
      arrange(PLAYERID,desc(slength)) %>% 
      group_by(PLAYERID) %>% 
      slice(1)
    
    topScorers <- playerGame %>% 
      filter(PLAYERID %in% bestRun$PLAYERID&TEAMNAME==theTeam) %>% 
      group_by(name,PLAYERID) %>% 
      
      summarize(totGoals=sum(Gls)) %>% 
      inner_join(bestRun)
}


  
  
  topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  
  
 
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
    #paste0(names(row),": ", format(row), collapse = "<br />")
    paste0( format(row), collapse = "<br />")
  }
  
  ## slightly different axes dependent on whether team ort not
  
  if (theTeam=="All Teams") {
  topScorers %>% 
    ggvis(~totGoals,~slength,key := ~id) %>% 
    add_axis("y",title="Best Consecutive Scoring Run in PL games", format='d') %>% 
    add_axis("x", title="Career Premier League goals") %>% 
    add_tooltip(all_values,"click") %>% 
    bind_shiny("allPlayerGoalSeqs")
  } else {
    topScorers %>% 
      ggvis(~totGoals,~slength,key := ~id) %>% 
      add_axis("y",title="Best Consecutive Scoring Run in PL games for Club", format='d') %>% 
      add_axis("x", title="Career Premier League goals for Club") %>% 
      add_tooltip(all_values,"click") %>% 
      bind_shiny("allPlayerGoalSeqs")
  }
})