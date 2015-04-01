## simplest way may be just have all otions listed
output$currentLeaders <-  DT::renderDataTable({
  print("enter teamYear")
  cat <-input$category_2
  yr <-input$season_2
  
  if(yr!="Record") {
    if (cat=="Goals"&yr=="Current"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==currentYear) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(goals=sum(Gls))) 
      df <- df %>%
        arrange(desc(goals)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    if (cat=="Goals"&yr=="Last Yr"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==lastYear) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(goals=sum(Gls))) 
      df <- df %>%
        arrange(desc(goals)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    if (cat=="Assists"&yr=="Current"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==currentYear) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(assists=sum(Assists))) 
      df <- df %>%
        arrange(desc(assists)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    if (cat=="Assists"&yr=="Last Yr"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==lastYear) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(assists=sum(Assists))) 
      df <- df %>%
        arrange(desc(assists)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    if (cat=="Cards"&yr=="Current"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==currentYear&(CARD>"A")) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(cards=n())) 
      df <- df %>%
        arrange(desc(cards)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    if (cat=="Cards"&yr=="Last Yr"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&season==lastYear&(CARD>"A")) %>%
                         group_by(PLAYERID,name,TEAMNAME) %>%
                         summarise(cards=n())) 
      df <- df %>%
        arrange(desc(cards)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    df <-  data.frame(df[,c(2:4)]) # otherwise stays in roups and cannot unclumn
    colnames(df) <- c("Player","Team","Count")
    DT::datatable(df,rownames= FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE,
                                                  
                                                  order=list(c(2,'desc'))))
  } else {
    
    if (cat=="Goals"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&PLAYERID!="OWNGOAL") %>%
                         group_by(PLAYERID,name,TEAMNAME,season) %>%
                         summarise(goals=sum(Gls))) 
      df <- df %>%
        arrange(desc(goals)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    if (cat=="Assists"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound) %>%
                         group_by(PLAYERID,name,TEAMNAME,season) %>%
                         summarise(assists=sum(Assists))) 
      df <- df %>%
        arrange(desc(assists)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    if (cat=="Cards"){
      df <- data.frame(playerGame %>%
                         filter(plYrTmGameOrder<=currentRound&(CARD>"A")) %>%
                         group_by(PLAYERID,name,TEAMNAME,season) %>%
                         summarise(cards=n())) 
      df <- df %>%
        arrange(desc(cards)) %>%
        group_by(TEAMNAME) %>%
        slice(1) }
    
    
    
    df <-  data.frame(df[,c(2:5)]) # otherwise stays in froups and cannot unclumn
    colnames(df) <- c("Player","Team","Season","Count")
    DT::datatable(df,rownames= FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE,
                                                   
                                                   order=list(c(3,'desc'))))
  }
  })
  
