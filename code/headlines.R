

## latest App - subsequently switched to specials?? - decid on how names go
## ermmber it needs different input names cf those in specials

## youngest player - previous option
#  observe({
#   if (is.null(input$teamC)) return()
#    
#    ## easiest to put here
#    apps <- summary %>% 
#      #filter((St+On)>0) %>% 
#      group_by(PLAYERID) %>% 
#      summarise(Apps=(sum(St)+sum(On)))
#   
#   df <-  playerGame %>% 
#     filter((START+subOn)>0&TEAMNAME==input$teamC) %>% 
#     group_by(gameDate) %>% 
#     mutate(youngest=min(age)) %>% 
#     filter(age==youngest) %>% 
#     ungroup() %>% 
#     arrange(gameDate) %>% 
#     select(name,age,gameDate,PLAYERID) %>% 
#     inner_join(apps)
#   
#   df$age <- as.numeric(df$age)
#   
#   df$id <- 1:nrow(df)
#   
# df <-  df %>% 
#     mutate(youngest=cummin(age)) %>% 
#     filter(age==youngest) %>% 
#     mutate(year=as.integer(str_sub(age,1,2))) %>% 
#     mutate(days=floor(365.25*(age-year))) %>% 
#     mutate(showAge=paste0(year," yrs ",days," days")) %>% 
#     mutate(showApps=paste0(Apps," career Apps"))
# 
# #print(glimpse(df))
#   
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- df[x$id == df$id,c("name","showAge","gameDate","showApps") ]
#     paste0(format(row), collapse = "<br />")
#   }
#   
# df   %>% 
#     ggvis(~gameDate,~youngest, key:= ~id) %>% 
#     layer_points(fill =~name) %>% 
#     add_legend("fill", title="") %>% 
#     add_axis("x", title="") %>% 
#     add_axis("y", title="Age",title_offset=50) %>% 
#     add_tooltip(all_values,"hover") %>% 
#   set_options(height = 300) %>% 
#     bind_shiny("ageRecord")
#   
# })

## causing error but canot find in ui - also thisn structure is need of overhaul
 
 # observeEvent(input$hl_compBtn,{
 #   
 #   # base data.frame
 #   df <-playerGame %>% 
 #     filter((START+subOn>0)&PLAYERID %in% input$hl_playerComps) %>% 
 #     select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
 #     arrange(gameDate) %>% 
 #     group_by(name,PLAYERID) %>% 
 #     mutate(gameOrder=row_number(),points=Assists+Gls,
 #            cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
 #   
 #   # set up plot from radio button choices
 #   
 #   if (input$hl_compCategory=="Points") {
 #     yTitle <-"Cumulative Goals+ Assists (inc. secondary)"
 #     if (input$hl_compTime=="Apps") {
 #       xTitle <-"PL Apps"
 #       plot <- df %>% 
 #         ggvis(~gameOrder,~cumPoints) 
 #     } else if (input$hl_compTime=="Age") {
 #       xTitle <-"Age"
 #       plot <- df %>% 
 #         ggvis(~age,~cumPoints) 
 #     } else if (input$hl_compTime=="Date") {
 #       xTitle <-""
 #       plot <- df %>% 
 #         ggvis(~gameDate,~cumPoints) 
 #     }
 #     
 #   } else if  (input$hl_compCategory=="Goals") {   
 #     yTitle <-"Cumulative Goals"
 #     if (input$hl_compTime=="Apps") {
 #       xTitle <-"PL Apps"
 #       plot <- df %>% 
 #         ggvis(~gameOrder,~cumGoals) 
 #     } else if (input$hl_compTime=="Age") {
 #       xTitle <-"Age"
 #       plot <- df %>% 
 #         ggvis(~age,~cumGoals) 
 #     } else if (input$hl_compTime=="Date") {
 #       xTitle <-""
 #       plot <- df %>% 
 #         ggvis(~gameDate,~cumGoals) 
 #     } 
 #   } else if  (input$hl_compCategory=="Assists") {   
 #     yTitle <-"Cumulative Assists (inc. secondary)"
 #     if (input$hl_compTime=="Apps") {
 #       xTitle <-"PL Apps"
 #       plot <- df %>% 
 #         ggvis(~gameOrder,~cumAssists) 
 #     } else if (input$hl_compTime=="Age") {
 #       xTitle <-"Age"
 #       plot <- df %>% 
 #         ggvis(~age,~cumAssists) 
 #     } else if (input$hl_compTime=="Date") {
 #       xTitle <-""
 #       plot <- df %>% 
 #         ggvis(~gameDate,~cumAssists) 
 #     }
 #   }
 #   
 #   # create plot
 #   plot %>% 
 #     layer_lines(stroke= ~name) %>% 
 #     ggvis::add_axis("x", title=xTitle) %>% 
 #     ggvis::add_axis("y", title=yTitle) %>% 
 #     add_legend("stroke", title="") %>% 
 #     bind_shiny("hl_comparisons")
 #   
 #   
 # })
 # 
 



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
  
 ## think there is a problem here as last filter only results in one tem wheras should be a value for every team as is current - think dplyr not loading last?? 
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
    mutate(maxFirst=max(first)) %>% ## issue is that this isnt done by team but overall max team eg arsenal
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
  
  print(maxDate)
  

  
  ty <- data.frame(playerGame %>%
                     ungroup() %>% 
                     filter(gameDate<=maxDate&season==currentYear) %>%
                     group_by(PLAYERID,name,TEAMNAME) %>%
                     summarise(goals=sum(Gls),assists=sum(Assists)) %>% 
                     mutate(points=goals+assists))
  
  print(glimpse(ty))
  
  tyGoals <- ty %>%
    arrange(desc(goals)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    ungroup() %>%
    select(team=TEAMNAME,goals,name)
  
  
  
  
  
  tyGoals$gname <- ifelse(tyGoals$goals==0,"",tyGoals$name)
  
  tyAssists <- ty %>%
    arrange(desc(assists)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    ungroup() %>%
    select(team=TEAMNAME,assists,name)
  
 
  tyAssists$aname <- ifelse(tyAssists$assists==0,"",tyAssists$name)
 
  tyPoints <- ty %>%
    arrange(desc(points)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    ungroup() %>%
    select(team=TEAMNAME,points,name)
  
  
  tyPoints$pname <- ifelse(tyPoints$points==0,"",tyPoints$name)
  
  ## for cards
  ty <- playerGame %>%
    ungroup() %>% 
    filter(gameDate<=maxDate&season==currentYear&CARD>1) %>%
    group_by(PLAYERID,name,TEAMNAME) %>%
    tally() %>% 
    ungroup()
  
  tyCards <- ty %>%
   
    arrange(desc(n)) %>%
    group_by(TEAMNAME) %>%
    slice(1) %>% 
    ungroup() %>%
    select(team=TEAMNAME,cards=n,cname=name)
  
  ## cp had no card first day so next is not relevant - need to correct later
  
 
  

  
  ## does this cater for where player has assist but no goal??
  tyAll <- tyGoals %>% 
    inner_join(tyAssists,by="team") %>% 
    inner_join(tyPoints,by="team") %>% 
    left_join(tyCards,by="team")
  
  ## need to correct for teams that have no cards

  tyAll$cards <-ifelse(is.na(tyAll$cards),0,tyAll$cards)
#  tyAll$name <-ifelse(is.na(tyAll$name),"",tyAll$name) ??
  
 
  
  tyAll %>% 
    select(-name) %>% 
    select(Team=team,goals,Goals=name.x,assists,Assists=name.y,points,Points=pname,cards,Cards=cname) %>% 
    DT::datatable(class='compact stripe hover row-border',colnames = c('Team', '', 'Goals','', 'Assists','','Points', '', 'Cards'),rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  

  
  
})