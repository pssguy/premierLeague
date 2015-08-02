

output$teamPic <- renderUI({
 
  if (is.null(input$teamA)) return()
  
  id <- teamCodes[teamCodes$TEAMNAME==input$teamA,]$TEAMID
 
  src1 <- paste0("http://www.premiersoccerstats.com/teamBadges/",id,".png")
  tags$img(src=src1, width=55)

})


output$squadPhoto <- renderUI({
  
  if (is.null(input$teamA)) return()
  
  id <- teamCodes[teamCodes$TEAMNAME==input$teamA,]$TEAMID
  
  src2 <- paste0("http://www.premiersoccerstats.com/squadPhotos/",id,".jpg")
  tags$img(src=src2, width=500)
  
})