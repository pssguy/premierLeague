

## latest App - subsequently switched to specials?? - decid on how names go

 observe({
  if (is.null(input$teamC)) return()
   
   ## easiest to put here
   apps <- summary %>% 
     #filter((St+On)>0) %>% 
     group_by(PLAYERID) %>% 
     summarize(Apps=(sum(St)+sum(On)))
  
  df <-  playerGame %>% 
    filter((START+subOn)>0&TEAMNAME==input$teamC) %>% 
    group_by(gameDate) %>% 
    mutate(youngest=min(age)) %>% 
    filter(age==youngest) %>% 
    ungroup() %>% 
    arrange(gameDate) %>% 
    select(name,age,gameDate,PLAYERID) %>% 
    inner_join(apps)
  
  df$age <- as.numeric(df$age)
  
  df$id <- 1:nrow(df)
  
df <-  df %>% 
    mutate(youngest=cummin(age)) %>% 
    filter(age==youngest) %>% 
    mutate(year=as.integer(str_sub(age,1,2))) %>% 
    mutate(days=floor(365.25*(age-year))) %>% 
    mutate(showAge=paste0(year," yrs ",days," days")) %>% 
    mutate(showApps=paste0(Apps," career Apps"))

print(glimpse(df))
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[x$id == df$id,c("name","showAge","gameDate","showApps") ]
    paste0(format(row), collapse = "<br />")
  }
  
df   %>% 
    ggvis(~gameDate,~youngest, key:= ~id) %>% 
    layer_points(fill =~name) %>% 
    add_legend("fill", title="") %>% 
    add_axis("x", title="") %>% 
    add_axis("y", title="Age") %>% 
    add_tooltip(all_values,"hover") %>% 
  set_options(height = 300) %>% 
    bind_shiny("ageRecord")
  
})



output$teamSeqCurrent <- DT::renderDataTable({
  
  ##
  a <- standings %>% 
    ungroup() %>% 
    filter(season==currentYear) 
  
  teams <- sort(unique(a$team))
  
  scores <- standings %>% 
    ungroup() %>% 
    filter(team %in% teams) %>% 
    group_by(team) %>% 
    arrange(tmGameOrder) %>% 
    select(res,tmGameOrder)
  
 
  Wx <- scores %>% 
    mutate(cat=ifelse(res=="Win",1,0)) %>% 
    do(subSeq(.$cat)) %>% 
    group_by(team) %>% 
    mutate(maxFirst=max(first)) %>% 
    filter(first==maxFirst)
  
  Win <- 
    Wx  %>% filter(value==1) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength),Category="Wins") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  
  noWin <- Wx  %>%
    filter(value==0) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength), Category="No Wins") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  
  Lx <- scores %>% 
    mutate(cat=ifelse(res=="Loss",1,0)) %>% 
    do(subSeq(.$cat)) %>% 
    group_by(team) %>% 
    mutate(maxFirst=max(first)) %>% 
    filter(first==maxFirst)
  
  Loss <- 
    Lx  %>% filter(value==1) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength), Category="Losses") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  #lineupText <- paste(lineup$name,collapse=", ")
  noLoss <- Lx  %>%
    filter(value==0) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength), Category="No Losses") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  
  Dx <- scores %>% 
    mutate(cat=ifelse(res=="Draw",1,0)) %>% 
    do(subSeq(.$cat)) %>% 
    group_by(team) %>% 
    mutate(maxFirst=max(first)) %>% 
    filter(first==maxFirst)
  
  Draw <- 
    Dx  %>% filter(value==1) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength),Category="Draws") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  
  noDraw <- Dx  %>%
    filter(value==0) %>% 
    ungroup() %>% 
    arrange(desc(slength)) %>% 
    mutate(rank=min_rank(-slength), Category="No Draws") %>% 
    filter(rank==1) %>% 
    mutate(teams = paste(team,collapse=", ")) %>% 
    select(Category,teams,count=slength) %>% 
    head(1)
  
  bind_rows(Win,noWin,Draw,noDraw,Loss,noLoss) %>% 
    select(Category,Games=count,Teams=teams) %>% 
    DT::datatable(rownames=FALSE,class='compact stripe hover row-border',options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})

output$teamLeadersCurrent <- DT::renderDataTable({
  
  ## need to set maxDate first - otherwise players starting lter in season might overtake
  maxDate <- standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==currentRound&season==currentYear)
  
  #currentTeams <- unique(maxDate$team)
  maxDate <- max(maxDate$gameDate) #"2015-08-10" looks good
  
  # currentTeams <- df$TEAMNAME may want to use this in below if just 
  
  ty <- data.frame(playerGame %>%
                     filter(gameDate<=maxDate&season==currentYear) %>%
                     group_by(PLAYERID,name,TEAMNAME) %>%
                     summarise(goals=sum(Gls),assists=sum(Assists))) 
  tyGoals <- ty %>%
    arrange(desc(goals)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    select(team=TEAMNAME,goals,name)
  
  tyGoals[tyGoals$goals==0,]$name <- ""
  
  tyAssists <- ty %>%
    arrange(desc(assists)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    select(team=TEAMNAME,assists,name)
  
  tyAssists[tyAssists$assists==0,]$name <- ""
  
  ## for cards
  ty <- playerGame %>%
    ungroup() %>% 
    filter(gameDate<=maxDate&season==currentYear&CARD>1) %>%
    group_by(PLAYERID,name,TEAMNAME) %>%
    tally() 
  
  tyCards <- ty %>%
    arrange(desc(n)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    select(team=TEAMNAME,cards=n,name)
  
  ## cp had no card first day so next is not relevant - need to correct later
  
  
  
  tyAll <- tyGoals %>% 
    inner_join(tyAssists,by=c("team"="team"))
  
  
  tyAll <- tyGoals %>% 
    inner_join(tyAssists,by="team") %>% 
    left_join(tyCards,by="team")
  
  ## need to correct for teams that have no cards
  tyAll[is.na(tyAll$cards)]$name <- ""
  tyAll$cards <-ifelse(is.na(tyAll$cards),0,tyAll$cards)
  tyAll$name <-ifelse(is.na(tyAll$name),"",tyAll$name)
  
  tyAll %>% 
    DT::datatable(class='compact stripe hover row-border',colnames = c('Team', '', 'Goals', 'Assists', '', 'Cards', ''),rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  #DT::datatable(class='compact stripe hover row-border',colnames = c('Player', 'Start', 'On', 'Off', 'Goals', 'Assists', 'Card'),
  
  
})