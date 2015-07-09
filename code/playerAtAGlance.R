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
  



names(bent)

# teams <- length(unique(bent$TEAMNAME)) # 6
# seasons <- length(unique(bent$season))  #11
# #apps <- sum(bent$St) + sum(bent$On)  #276
# #goals <- sum(bent$StGls)+ sum(bent$subGls) #106
# assists <- sum(bent$Assists) #27
# #cards <- sum(bent$Y) + sum(bent$R) #12
# 
# bent %>% 
#   group_by(season) %>% 
#   mutate(maxApp=)

info=list(teams=teams,seasons=seasons,career=career)
return(info)

})

output$appsBox <- renderInfoBox({
    infoBox(
    "Appearances",data()$career$showApps, icon = icon("futbol-o"), #user-times
    color = "light-blue", subtitle = " Tot - Max"
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
    color = "green", subtitle = " Tot - Max"
  )
})
output$assistsBox <- renderInfoBox({
  infoBox(
    "Assists",data()$career$showAssists, icon = icon("heart"),
    color = "green", subtitle = " Tot - Max"
  )
})
output$cardsBox <- renderInfoBox({
  infoBox(
    "Cards",data()$career$showCards, icon = icon("book"),
    color = "orange", subtitle = " Tot - Max"
  )
})
output$seasonsBox <- renderInfoBox({
  infoBox(
    "Seasons",data()$seasons, icon = icon("calendar"),
    color = "light-blue"
  )
})

































































































































































































































































































