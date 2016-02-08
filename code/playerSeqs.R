playerSeqData <- reactive ({
  ##print("psd reactive")
  
  # if (is.null(input$playerA)) return()
  # if (is.null(input$seqPlVenue)) return()
  # ##print("playerid")
  # print(input$playerA)
  
  req(input$playerA)
  
  if (input$seqPlVenue=="All") {
  appeared <- playerGame %>%
    filter(PLAYERID==input$playerA&(START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  } else if (input$seqPlVenue=="Home") {
    appeared <- playerGame %>%
      filter(PLAYERID==input$playerA&(START+subOn)>0&venue=="H") %>%  # 380 as sh
      arrange(gameDate) %>%
      select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  } else  {
    appeared <- playerGame %>%
      filter(PLAYERID==input$playerA&(START+subOn)>0&venue=="A") %>%  # 380 as sh
      arrange(gameDate) %>%
      select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  }
    
    
  appeared$Scored <- 0
  appeared$Scored[appeared$Gls>0] <- 1
  
  print(appeared)
  print("just printed appeared")
 
  runApp <- subSeq(appeared$Scored)
  print(runApp)
  print("just printed runApp")
  
  if (input$seqPlVenue=="All") {
    starter <- playerGame %>%
      filter(PLAYERID==input$playerA&START>0) %>%  # 380 as sh
      arrange(gameDate) %>%
      select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  } else if (input$seqPlVenue=="Home") {
    starter <- playerGame %>%
      filter(PLAYERID==input$playerA&START>0&venue=="H") %>%  # 380 as sh
      arrange(gameDate) %>%
      select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  } else  {
    starter <- playerGame %>%
      filter(PLAYERID==input$playerA&START>0&venue=="A") %>%  # 380 as sh
      arrange(gameDate) %>%
      select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  }
  print(nrow(starter))
   
  
 print(nrow(starter))
  
  print(starter)
  print("just printed starter")
  
  if (nrow(starter)!=0) {
    print("started")
    starter$Scored <- 0
    starter$Scored[starter$Gls>0] <- 1
    runStarter <- subSeq(starter$Scored) 
      
    } else {
      print("never started")
    runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value=0)
  }
  
  
  print(runStarter)
  print("just printed runStarter")
  
  print(runApp)
  print(runStarter)
 
  info=list(runApp=runApp,runStarter=runStarter,starter=starter,appeared=appeared)
  return(info)
})


output$gameNoGoal <- renderPlot({
 # if (is.null(playerSeqData()$runApp)) return()
  
  run <- playerSeqData()$runApp
  
  
  gameGoal <- run %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
  
  #if (nrow(gameGoal)==0) return()
  if (nrow(gameGoal)==0){
    gameGoal <- data.frame(slength=0,n=0)
  } 
  
  if (tail(run,1)$value==0) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
     theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Not Scored In")
}, height=250)

output$gameGoal <- renderPlot({
  
  #print("sheehan")
  #print(playerSeqData()$runApp)
  #print("#printed sheehan")  ## need to go back to reactive I think also look at aarons who should have data here
  
#  if (is.null(playerSeqData()$runApp)) return()
  ##print("enter gameGoal")
  run <- playerSeqData()$runApp
  
  #print("run")
  #print(run)
  
  gameGoal <- run %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
 # if (nrow(gameGoal)==0) return()
  if (nrow(gameGoal)==0){
    gameGoal <- data.frame(slength=0,n=0)
  } 
  
  if (tail(run,1)$value==1) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  

  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Scored In")
}, height=250)

output$gameNoGoalStarter <- renderPlot({
 # if (is.null(playerSeqData()$runStarter)) return()
  
  run <- playerSeqData()$runStarter
  ##print("##print run ")
  ##print(glimpse(run))
  ##print("##printed run ")
  
  gameGoal <- run %>% 
    filter(value==0) %>% 
    group_by(slength) %>% 
    tally()
 # if (nrow(gameGoal)==0) return()
  if (nrow(gameGoal)==0){
    gameGoal <- data.frame(slength=0,n=0)
  } 
  
  if (tail(run,1)$value==0) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Not Scored In (Starter)")
}, height=250)

output$gameGoalStarter <- renderPlot({
#   #print("test as starter")
#   #print(playerSeqData()$runStarter)
#  if (is.null(playerSeqData()$runStarter)) return()
  
  run <- playerSeqData()$runStarter
  
  gameGoal <- run %>% 
    filter(value==1) %>% 
    group_by(slength) %>% 
    tally()
  #if (nrow(gameGoal)==0) return() # this works but means no graph is shown
  if (nrow(gameGoal)==0){
    gameGoal <- data.frame(slength=0,n=0)
  }  
  
  if (tail(run,1)$value==1) {
    cond <- gameGoal$slength == tail(run,1)$slength
  } else {
    cond <- FALSE
  }
  
  ggplot(gameGoal, aes(x=slength,y=n)) +
    geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
    geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
    scale_x_discrete(breaks = pretty_breaks()) +
    scale_y_discrete(breaks = pretty_breaks()) +
    theme_bw() +
    xlab("Sequence") +
    ylab("Count") +
    ggtitle("Games Scored In (Starter)")
}, height=250)

# #gameGoalSeq
# observe({
#   #print(input$playerA)
#   
#   if (is.null(input$playerA)) return()
# #  if (input$playerA<"a") return()
#   ##print ("enter gameGoalSeqStarter")
#   ##print(input$playerA)
#   starter <- playerGame %>%
#     filter(PLAYERID==input$playerA&(START+subOn)>0) %>%  # 380 as sh
#     arrange(gameDate) %>%
#     select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>% 
#     mutate(starterOrder=row_number())
#   
#   
#   #print(glimpse(starter))
#   #print("starteransub")
#   #print(nrow(starter))
#   if (nrow(starter)==0) return()
#   
#   if(nrow(starter>0)) {
#     starter$Scored <- "No"
#     starter$Scored[starter$Gls>0] <- "Yes"
#   }
#   #print("OKA")
#   if (nrow(starter)==0) {
#     #print("set up df")
#     runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value="No")
#     runStarter$value <- as.character(runStarter$value)
#     chart <- data.frame(gameDate=as.Date("2000-01-01"),slength=0,Scored="No")
#   } else {
#     runStarter <- subSeq(starter$Scored)
#     chart <-  runStarter %>% 
#       left_join(starter,by=c("last"="starterOrder")) %>% 
#       select(gameDate,slength,Scored) 
#   }
# #   #print("OKB")
# #   #print("starter")
# #   #print("glimpse(runStarter)")
# #   #print(glimpse(runStarter))
# #   
# #   
# # #   starter$Scored <- "No"
# # #   starter$Scored[starter$Gls>0] <- "Yes"
# # #   
# # #   runStarter <- subSeq(starter$Scored)
# #   
# #   #print(glimpse(runStarter))
#   
#   ##print(runStarter)
#   
#   ## can take first,last or midpoint to have run not scoring/scoring
#   
# #   chart <-  runStarter %>% 
# #     left_join(starter,by=c("last"="starterOrder")) %>% 
# #     select(gameDate,slength,Scored) 
#   
#   ##print("chart")
#   #print("chart")
#   #print(glimpse(chart))
#   
#   chart %>%     ggvis(~gameDate,~slength) %>% 
#     layer_points(fill=~ Scored, size=2) %>% 
#     add_axis("x", properties = axis_props(labels = list(
#       angle = 45, align = "left", fontSize = 11
#     )),title = "") %>% 
#     add_axis("y", title="Run of Games") %>% 
#     set_options(width=400, height=400) %>% 
#     bind_shiny("gameGoalSeq")
#   
# })
# 


#gameGoalSeq
observe({
 
  
 # if (is.null(input$playerA)) return()
  req(input$playerA)
  
  starter <- playerGame %>%
    filter(PLAYERID==input$playerA&(START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>% 
    mutate(starterOrder=row_number())
  
  
 
  if (nrow(starter)==0) return()
  
  if(nrow(starter>0)) {
    starter$Scored <- "No"
    starter$Scored[starter$Gls>0] <- "Yes"
  }
  #print("OKA")
  if (nrow(starter)==0) {
    #print("set up df")
    runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value="No")
    runStarter$value <- as.character(runStarter$value)
    chart <- data.frame(gameDate=as.Date("2000-01-01"),slength=0,Scored="No")
  } else {
    runStarter <- subSeq(starter$Scored)
    chart <-  runStarter %>% 
      left_join(starter,by=c("last"="starterOrder")) %>% 
      select(gameDate,slength,Scored) 
  }
  
  
  chart %>%     ggvis(~gameDate,~slength) %>% 
    layer_points(fill=~ Scored, size=2) %>% 
    ggvis::add_axis("x", properties = axis_props(labels = list(
      angle = 45, align = "left", fontSize = 11
    )),title = "") %>% 
    ggvis::add_axis("y", title="Run of Games") %>% 
    set_options(width=400, height=400) %>% 
    bind_shiny("gameGoalSeq")
  
})





#gameGoalSeqStarter
 observe({
  #print("enter observe")
  #print(input$playerA)
  
  #if (is.null(input$playerA)) return()
   req(input$playerA)
 # if (input$playerA<"a") return()
  #if (is.null(playerSeqData()$runStarter)) return()
  ##print ("enter gameGoalSeqStarter")
  ##print(input$playerA)
  starter <- playerGame %>%
    filter(PLAYERID==input$playerA&START>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>% 
    mutate(starterOrder=row_number())
  
  ##print(glimpse(starter))
  #print("nrowstarter")
  #print(nrow(starter))
  if (nrow(starter)==0) return() ## but chart for previous player eg Bent remains
  
  if(nrow(starter>0)) {
   starter$Scored <- "No"
   starter$Scored[starter$Gls>0] <- "Yes"
  }
  #print("OKA")
  if (nrow(starter)==0) {
    #print("set up df")
    runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value="No")
    runStarter$value <- as.character(runStarter$value)
    chart <- data.frame(gameDate=as.Date("2000-01-01"),slength=0,Scored="No")
  } else {
  runStarter <- subSeq(starter$Scored)
  chart <-  runStarter %>% 
    left_join(starter,by=c("last"="starterOrder")) %>% 
    select(gameDate,slength,Scored) 
  }
#   #print("OKB")
#   #print("starter")
#   #print("glimpse(runStarter)")
#   #print(glimpse(runStarter))
  
  ## can take first,last or midpoint to have run not scoring/scoring
  


##print("chart")
##print(chart)
##print("chart done")
    
chart %>%     ggvis(~gameDate,~slength) %>% 
    layer_points(fill=~ Scored, size=2) %>% 
    
  ggvis::add_axis("y", title="Run of Games as Starter") %>% 
  
  ggvis::add_axis("x", properties = axis_props(labels = list(
    angle = 45, align = "left", fontSize = 11
  )),title = "") %>% 
  set_options(width=400, height=400) %>% 
  bind_shiny("gameGoalSeqStarter")
  
})

