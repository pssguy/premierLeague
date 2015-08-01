## summary stats/photo/birtplace maybe add transfers in?


data <- reactive({
  
  if(is.null(input$playerA)) return()
  

  
  basic <- summary %>%
    filter(PLAYERID==input$playerA)
  
  teams <- length(unique(basic$TEAMNAME)) # 6
  seasons <- length(unique(basic$season))
 
  
bySeason <- summary %>%
  filter(PLAYERID==input$playerA) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  summarize(apps=sum(St+On), goals=sum(StGls+subGls),cards=sum(Y+R),assists=sum(Assists))



max <-bySeason%>%
  summarize(maxApps=max(apps),maxCards=max(cards),maxGoals=max(goals), maxAssists=max(assists))

tot <-bySeason%>%
  summarize(apps=sum(apps),goals=sum(goals),cards=sum(cards),assists=sum(assists)) 
   
career <- cbind(max,tot)  %>% 
  mutate(showApps=paste(apps,"-",maxApps),
         showCards=paste(cards,"-",maxCards),
         showGoals=paste(goals,"-",maxGoals),
                        showAssists=paste(assists,"-",maxAssists))
  






info=list(teams=teams,seasons=seasons,career=career)
return(info)

})

output$appsBox <- renderInfoBox({
    infoBox(
    "Appearances",data()$career$showApps, icon = icon("futbol-o"), #user-times
    color = "light-blue", subtitle = " Tot - Max(Year)"
  )
})
output$teamsBox <- renderInfoBox({
  infoBox(
    "Teams",data()$teams, icon = icon("home"),
    color = "light-blue"
  )
})
output$goalsBox <- renderInfoBox({
  infoBox(
    "Goals",data()$career$showGoals, icon = icon("bullseye"),
    color = "green", subtitle = " Tot - Max(Year)"
  )
})
output$assistsBox <- renderInfoBox({
  infoBox(
    "Assists",data()$career$showAssists, icon = icon("heart"),
    color = "green", subtitle = " Tot - Max(Year)"
  )
})
output$cardsBox <- renderInfoBox({
  infoBox(
    "Cards",data()$career$showCards, icon = icon("book"),
    color = "orange", subtitle = " Tot - Max(Year)"
  )
})
output$seasonsBoxPlayer <- renderInfoBox({
 
  infoBox(
    "Seasons",data()$seasons, icon = icon("calendar"),
    color = "light-blue"
  )
})

































































































































































































































































































