## summary stats/photo/birtplace maybe add transfers in?
#print("playerpataglance") when addressing problem - does not show anyways

glanceData <- reactive({
  
  print("enter ata aglance reactive") # this is not even being called?
  #if(is.null(input$playerA)) return()
  req(input$playerA)

  
  basic <- summary %>%
    filter(PLAYERID==input$playerA)
  
  teams <- length(unique(basic$TEAMNAME)) # 6
  seasons <- length(unique(basic$season))
 
  
bySeason <- summary %>%
  filter(PLAYERID==input$playerA) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  summarise(apps=sum(St+On), goals=sum(StGls+subGls),cards=sum(Y+R),assists=sum(Assists))



max <-bySeason%>%
  summarise(maxApps=max(apps),maxCards=max(cards),maxGoals=max(goals), maxAssists=max(assists))

tot <-bySeason%>%
  summarise(apps=sum(apps),goals=sum(goals),cards=sum(cards),assists=sum(assists)) 
   
career <- cbind(max,tot)  %>% 
  mutate(showApps=paste(apps,"-",maxApps),
         showCards=paste(cards,"-",maxCards),
         showGoals=paste(goals,"-",maxGoals),
                        showAssists=paste(assists,"-",maxAssists))
  




print("career")
print(career)
info=list(teams=teams,seasons=seasons,career=career)
return(info)

})

output$appsBox <- renderInfoBox({
#   if (is.null(input$playerA)) return()
#   if (input$playerA=="") return()
#   print(input$playerA)
#   print("data()$career$showApps")
#   print(glanceData()$career$showApps) #NULL
#   print("that was data()$career$showApps")
  
    infoBox(
    "Appearances",glanceData()$career$showApps, icon = icon("futbol-o"), #user-times
    color = "light-blue", subtitle = " Tot - Max(Year)"
  )
})
output$teamsBox <- renderInfoBox({
  print("enterteamsbox")
  infoBox(
    "Teams",glanceData()$teams, icon = icon("home"),
    color = "light-blue"
  )
})
output$goalsBox <- renderInfoBox({
  infoBox(
    "Goals",glanceData()$career$showGoals, icon = icon("bullseye"),
    color = "green", subtitle = " Tot - Max(Year)"
  )
})
output$assistsBox <- renderInfoBox({
  infoBox(
    "Assists",glanceData()$career$showAssists, icon = icon("heart"),
    color = "green", subtitle = " Tot - Max(Year)"
  )
})
output$cardsBox <- renderInfoBox({
  infoBox(
    "Cards",glanceData()$career$showCards, icon = icon("book"),
    color = "orange", subtitle = " Tot - Max(Year)"
  )
})
output$seasonsBoxPlayer <- renderInfoBox({
 
  infoBox(
    "Seasons",glanceData()$seasons, icon = icon("calendar"),
    color = "light-blue"
  )
})

































































































































































































































































































