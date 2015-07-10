

output$playerBirthplace <- renderLeaflet({
  
  if(is.null(input$playerA)) return()
  if(input$playerA=="") return()
  
  #print(playerGame[playerGame$PLAYERID=="input$playerA",]$city[1])
  
  if (!is.na(playerGame[playerGame$PLAYERID==input$playerA,]$city[1])) {
  loc <- paste0(playerGame[playerGame$PLAYERID==input$playerA,]$city[1],", ",playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1])
  } else {
    loc <- playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1]
  }
#print(loc)
  
  
 
df <- geocode(loc)

 theLat <- df$lat
 theLon <- df$lon


df    %>%
  leaflet() %>%
  addTiles() %>% 
  setView(theLon,theLat, zoom=9) %>% 
  addMarkers()
})