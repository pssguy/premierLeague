## summary stats/photo/birtplace


data <- reactive({
bent <- summary %>%
  filter(PLAYERID=="BENTD")
names(bent)

teams <- length(unique(bent$TEAMNAME)) # 6
seasons <- length(unique(bent$season))  #11
apps <- sum(bent$St) + sum(bent$On)  #276
goals <- sum(bent$StGls)+ sum(bent$subGls) #106
assists <- sum(bent$Assists) #27

info=list(teams=teams,seasons=seasons,apps=apps,goals=goals,assists=assists)
return(info)

})

output$appsBox <- renderInfoBox({
    infoBox(
    "Appearances",data()$apps, icon = icon("thumbs-up"),
    color = "green"
  )
})
output$teamsBox <- renderInfoBox({
  infoBox(
    "Teams",data()$teams, icon = icon("thumbs-up"),
    color = "green"
  )
})
output$goalsBox <- renderInfoBox({
  infoBox(
    "Goals",data()$goals, icon = icon("thumbs-up"),
    color = "green"
  )
})
output$assistsBox <- renderInfoBox({
  infoBox(
    "Assists",data()$assists, icon = icon("thumbs-up"),
    color = "green"
  )
})
output$cardsBox <- renderInfoBox({
  infoBox(
    "Cards",data()$cards, icon = icon("thumbs-down"),
    color = "red"
  )
})
# output$appsBox <- renderInfoBox({
#   infoBox(
#     "Appearances",data()$apps, icon = icon("thumbs-up"),
#     color = "green"
#   )
# })

































































































































































































































































































