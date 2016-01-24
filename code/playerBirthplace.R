## using pgMini means only those who have played
output$playerBirthplace <- renderLeaflet({
  
  # if(is.null(input$playerA)) return()
  # if(input$playerA=="") return()
  req(input$playerA)
 
  theLat <- pgMini[pgMini$PLAYERID==input$playerA,]$lat
  theLon <- pgMini[pgMini$PLAYERID==input$playerA,]$lon
  df <- data.frame(lat=theLat,lon=theLon)
  
  ## could be used as backup if data not yet available for some reason
  if(is.na(theLat)) {
  print("no geo need to get")
  if (!is.na(playerGame[playerGame$PLAYERID==input$playerA,]$city[1])) {
  loc <- paste0(playerGame[playerGame$PLAYERID==input$playerA,]$city[1],", ",playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1])
  } else {
    loc <- playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1]
  }

df <- geocode(loc)

 theLat <- df$lat
 theLon <- df$lon
}

df    %>%
  leaflet() %>%
  addTiles() %>% 
  setView(theLon,theLat, zoom=9) %>% 
  addMarkers()
})