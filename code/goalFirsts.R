# first and latest

cats <- c("Head","Right","Left","6_Yd_Box","Pen_Area","Long_Range","Open","Corner","Indirect_FK",
          "Direct_FK","Penalty","Throw")
allCats <- data.frame(category=cats)

output$goalFirsts <- DT::renderDataTable({
  print("enter goalFirsts")
  a <- goals %>%
    left_join(playerGame, by="PLAYER_MATCH") %>%
    filter(PLAYERID==input$player&(START+subOn)>0) %>%
    select(METHOD,PLACE,PLAY,plGameOrderApp)
  
  if (nrow(a)>0) {
    b <- a %>%
      gather(dummy,category,-plGameOrderApp) %>%
      arrange(plGameOrderApp)
    
    
    for (i in 1:length(cats)) {
      
      if (nrow(b %>% filter(category==cats[i])) >0) {
        tempdf <- data.frame(b %>%
                               filter(category==cats[i]) %>%
                               slice(1))
        tempdf <- tempdf[,c("category","plGameOrderApp")]
        
        if (i!=1) {
          df <- rbind(df,tempdf)
        } else {
          df <- tempdf
        }
      }
      allApps <- nrow(playerGame %>% filter(PLAYERID==input$player&(START+subOn)>0))
      
      a$since <- allApps- a$plGameOrderApp
      
      a <- a %>%
        mutate(since=allApps-plGameOrderApp) %>%
        arrange(since)
      
      c <- a %>%
        gather(dummy,category,-c(plGameOrderApp,since))
      #i <- 2
      tempdf <- NULL
      for (i in 1:length(cats)) {
        
        if (nrow(c %>% filter(category==cats[i])) >0) {
          tempdf <- data.frame(c %>%
                                 filter(category==cats[i]) %>%
                                 slice(1))
          tempdf <- tempdf[,c("category","since")]
          
          if (i!=1) {
            dfSince <- rbind(dfSince,tempdf)
          } else {
            dfSince <- tempdf
          }
        }
        
      }
      
      ## also get all goals
      allGoals <- b %>%
        group_by(category) %>%
        summarise(tot=n())
      
      allCats <- data.frame(category=cats)
      
      print("about to join")
      print(allCats)
      print(allGoals)
      print(df)
      print(dfSince)
      
      tbl <- allCats %>%
        left_join(allGoals) %>%
        left_join(df) %>%
        left_join(dfSince) %>%
        rename(Category=category,Tot=tot,First=plGameOrderApp,Since=since)
      
      print("success")
      tbl$Tot <- ifelse(is.na(tbl$Tot),0,tbl$Tot)
      tbl$First <- ifelse(is.na(tbl$First),0,tbl$First)
      tbl$Since <- ifelse(is.na(tbl$Since),0,tbl$Since)
      print(tbl)
      # tbl
    }
  } else {
    tbl <- data.frame(Category=cats,Tot=rep(0,12),First=rep("",12),Since=rep("",12))
  }
  
    DT::datatable(tbl,options= list(paging = FALSE, searching = FALSE, info=FALSE))
                                   
}

)

