

output$playerBirthplace <- renderLeaflet({
  
  if(is.null(input$playerA)) return()
  if(input$playerA=="") return()
  
  if (!is.na(playerGame[playerGame$PLAYERID=="input$playerA",]$city[1])) {
  loc <- paste0(playerGame[playerGame$PLAYERID==input$playerA,]$city[1],", ",playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1])
  } else {
    loc <- playerGame[playerGame$PLAYERID==input$playerA,]$COUNTRY[1]
  }
print(loc)
 
df <- geocode(loc)
print(str(df))

theLat <- df$lat
theLon <- df$lng

df    %>%
  leaflet() %>%
 # 
  addTiles() %>% 
#  setView(theLon,theLat, zoom=10) %>% 
  clearBounds() %>% 
  addMarkers()
})