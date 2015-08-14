



output$teamSeqCurrent <- DT::renderDataTable({
  
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
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})