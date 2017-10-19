## ata glance like cricket etc

## look at a heardcoded version first

## summary stats/photo/birtplace maybe add transfers in?


teamData <- eventReactive(input$teamA,{
  
  
  test <-standings %>% 
    filter(team==input$teamA) %>% 
    group_by(season) %>% 
    filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
    ungroup()
  
  summary <- test %>% 
    summarise(years=n(),bestPos=min(position),wortsPos=max(position),maxPoints=max(cumPts),minPoints=min(cumPts))
  
  
  mostGames <- playerGame %>% 
    filter(TEAMNAME==input$teamA) %>% 
    group_by(PLAYERID,name) %>% 
    filter((START+subOn)>0) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
  
  
  mostGoals <- playerGame %>% 
    filter(TEAMNAME==input$teamA&name!=" Own Goal") %>% 
    group_by(PLAYERID,name) %>% 
    summarise(sumGoals=sum(Gls)) %>% 
    ungroup() %>% 
    filter(sumGoals>0) %>% 
    arrange(desc(sumGoals))
  
  mostAssists <- playerGame %>% 
    filter(TEAMNAME==input$teamA) %>% 
    
    group_by(PLAYERID,name) %>% 
    summarise(sumAssists=sum(Assists)) %>% 
     ungroup() %>% 
    filter(sumAssists>0) %>% 
    arrange(desc(sumAssists))
  
  
  mostCards <- playerGame %>% 
    filter(TEAMNAME==input$teamA&CARD>"0") %>% 
    group_by(PLAYERID,name) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
  
  info=list(summary=summary,test=test,mostGames=mostGames,
            mostGoals=mostGoals,mostAssists=mostAssists, mostCards=mostCards)
  return(info)
  
})

output$mostGames <- DT::renderDataTable({
  if (is.null(teamData())) return()
  
  teamData()$mostGames %>% 
    select(Player=name,Apps=n) %>% 
    DT::datatable(selection='single',class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})

output$mostGoals <- DT::renderDataTable({
 # if(is.null(teamData())) return()
  
  req(teamData())
  
#   print("mostGoals")
#   print(teamData()$mostGoals)
#   print("mostGoals")
#   write_csv(teamData()$mostGoals,"mostGoals.csv")
#   
#   
#   test <- read_csv("mostGoals.csv")
  
  teamData()$mostGoals   %>% 
    select(Player=name,Goals=sumGoals) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))

# test   %>% 
#   select(Player=name,Goals=sumGoals) %>% 
#   DT::datatable()
  
})

output$mostAssists <- DT::renderDataTable({
 # if(is.null(teamData())) return()
  req(teamData())
  teamData()$mostAssists %>% 
    select(Player=name,Assists=sumAssists) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})


output$mostCards <- DT::renderDataTable({
 # if(is.null(teamData())) return()
  req(teamData())
  teamData()$mostCards %>% 
    select(Player=name,Cards=n) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})


# output$appsBox <- renderInfoBox({
#   infoBox(
#     "Appearances",data()$career$showApps, icon = icon("futbol-o"), #user-times
#     color = "light-blue", subtitle = " Tot - Max"
#   )
# })

output$seasonsBox <- renderInfoBox({
  #print("enterseasonsbox")
  seasons <- as.character(teamData()$summary$years)
  infoBox(
    "Seasons",seasons, icon = icon("futbol-o"), #user-times
    color = "light-blue"
  )
})


output$glanceTest <- renderText({
  #print(teamData()$summary$years)
  input$teamZ
})



output$seasonsHist <- renderPlot({

  if(is.null(teamData())) return
  print("b")
df <-  teamData()$test 
print("a")
# condition for showing bolder color ie current season
cond <- df$season =="2017/18"
#set pretty scales - function does not work
## set p
print("b")
maxSeason <-df %>% 
  group_by(final_Pos) %>% 
  tally()

theMax <- max(maxSeason$n)

#seq(0,8)

write_csv(df,"problem.csv")
p <- ggplot(df, aes(x=final_Pos)) +
  geom_histogram(data=subset(df,cond==FALSE), binwidth=0.5, fill="blue", alpha=0.2) +
  geom_histogram(data=subset(df,cond==TRUE), binwidth=0.5, fill="blue") +
  scale_x_continuous(breaks=df$final_Pos+0.25, labels=df$final_Pos) +
 # scale_y_continuous(breaks=pretty_breaks()) +
  scale_y_discrete(breaks= seq(0,theMax)) +
  theme_bw() +
  xlab("Position (2017/8 in bold)") +
  ylab("Seasons")
print("d")
p

})
