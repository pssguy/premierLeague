## chart that shows for each team- how there manager has performed
## start with ggvis - but may change
# managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01') #
# 
# managerGame <-managers %>% 
#   mutate(name=paste(FirstName,Lastname)) %>% 
#   group_by(ManagerID,ManagerTeam) %>% 
#   inner_join(standings,by=c("TEAMNAME"="team")) %>% 
#   select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>% 
#   filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>% 
#   mutate(points=ifelse(res=="Win",3,ifelse(res=="Draw",1,0))) %>% 
#   ungroup()
# 
# 
# 
# ppgManagerTeamStint <- managerGame %>% 
#   group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>% 
#   dplyr::summarise(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2)) %>% 
#   ungroup()
# 
# 
# 
# 
# allManagerStints <- 
#   managerGame %>% 
#   select(name,ManagerTeam,Joined,Left) %>% 
#   unique()
# 
# 
# 
# ## artificial start date for those hired before PL existed
# allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"




output$managerPPGbyTeam <- renderPlotly({
  
  req(input$teamA)
  req(input$managerGames)
  # if(is.null(input$teamA)) return()
  # if(is.null(input$managerGames)) return()
  # print(paste(input$teamA,"managers"))
  # print(input$managerGames) #5 default
  
  teamRecord <- ppgManagerTeamStint  %>% 
    
    select(TEAMNAME,name,ManagerTeam,games,ppg) %>% 
    inner_join(allManagerStints) %>% 
    filter(TEAMNAME==input$teamA&games>=input$managerGames) 
  
  #teamRecord  <- cbind(teamRecord, id = seq_len(nrow(teamRecord)))
  
  teamRecord <- teamRecord %>% 
    mutate(id=row_number())
  
 
  
  
  minY <- min(teamRecord$ppg)-0.1
  maxY <- max(teamRecord$ppg)+0.1
  
  plot_ly(teamRecord, color = I("gray80")) %>%
    add_segments(x = ~Joined, xend = ~Left, y = ~ppg, yend = ~ppg, showlegend = FALSE) %>% 
    add_markers(x = ~Joined, y = ~ppg,  color = I("green"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",name,"<br>From:",Joined,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
    add_markers(x = ~Left, y = ~ppg, color = I("blue"), showlegend = FALSE,
                hoverinfo="text",
                text=~paste0(" ",name,"<br>To:",Left,"<br>Games:",games,
                             "<br>ppg:",ppg)) %>%
    # Causing error 
    #Error in .Call(transpose_impl, .l, .names) : 
    #Incorrect number of arguments (2), expecting 1 for 'transpose_impl'
    # add_annotations(x = teamRecord$Joined,
    #                 y = teamRecord$ppg,
    #                 text = teamRecord$name,
    #                 xref = "x",
    #                 yref = "y",
    #                 showarrow = TRUE,
    #                 arrowhead = 1,
    #                 arrowsize = .3,
    #                 ax = 40,
    #                 ay = -10)  %>% 
    layout(
           xaxis=list(title=""),
           yaxis=list(title="Points per Game" ))
})



output$liverpool <- renderText({
  
#  if(is.null(input$teamA)) return()
 req(input$teamA)
  if (input$teamA!="Liverpool") {
    
  } else {
    "Houllier was initially a joint manager with Evans and was temporarily
    replaced by Thompson when ill"
  }
  
})