


# appeared <-playerGame %>%
#   filter((START+subOn)>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
# 
# 
# 
# appeared$Scored <- 0
# appeared$Scored[appeared$Gls>0] <- 1
# 
# 
# 
# by_player <- appeared %>% 
#   ungroup() %>% 
#   arrange(PLAYERID,gameDate) %>% 
#   group_by(PLAYERID) # this needs to be here to carry PLAYERID across
# 
# 
# 
# goalSeqs <- do(by_player,subSeq(.$Scored)) %>% 
#   filter(value==1)

bestRun <- goalSeqs %>% 
  #filter(slength>4) %>% 
  select(PLAYERID,slength) %>% 
  arrange(PLAYERID,desc(slength)) %>% 
  group_by(PLAYERID) %>% 
  slice(1)

topScorers <- playerGame %>% 
  filter(PLAYERID %in% bestRun$PLAYERID) %>% 
  group_by(name,PLAYERID) %>% 
  
  summarize(totGoals=sum(Gls)) %>% 
  inner_join(bestRun)


topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
  #paste0(names(row),": ", format(row), collapse = "<br />")
  paste0( format(row), collapse = "<br />")
}

topScorers %>% 
  ggvis(~totGoals,~slength,key := ~id) %>% 
  add_axis("y",title="Best Scoring Run in PL games", format='d') %>% 
  add_axis("x", title="Career Premier League goals") %>% 
  add_tooltip(all_values,"click") %>% 
  bind_shiny("allPlayerGoalSeqs")