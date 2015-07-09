# output$playerPix <- renderText({
# #   if (is.null(input$playerA)) return()
# #   print(input$playerA)
# #   print("enter picture")
# #   
# #   playerName <- pgMini[pgMini$PLAYERID==input$playerA,]$name
# #   
# #   thePlayer <- tolower(str_replace(playerName," ","-"))
# #   ## stil need the id
# #   #src1 <- "http://www.premierleague.com/content/dam/premierleague/shared-images/players/t/toby-alderweireld/55605-lsh.jpg"
# #   ## tags$img(src=src1, width=200)
# #   
# #   # baseURL <- "http://www.premierleague.com"
# #   u <- paste0("http://www.premierleague.com/en-gb/players/profile.career-history.html/",thePlayer)
# #   u.get<- GET(u)
# #   u.content=content(u.get, as="text")
# #   u.html <- htmlParse(u.content)
# #   
# #   picLink <-xpathSApply(u.html, "//*/img[@class='heroimg']/@src")
# #   #unname(picLink)
# #   
# #   src1 <- paste0("http://www.premierleague.com",unname(picLink))
# #   tags$img(src=src1, width=200)
#   "hiya"
# })

output$playerPic <- renderUI({
  if (is.null(input$playerA)) return()
  if (input$playerA=="") return()
  print(input$playerA)
  print("enter picture")
  playerName <- pgMini[pgMini$PLAYERID==input$playerA,]$name
     thePlayer <- tolower(str_replace(playerName," ","-"))
 # print(thePlayer) #"darren-bent"
  u <- paste0("http://www.premierleague.com/en-gb/players/profile.career-history.html/",thePlayer)
 # print(u)
    u.get<- GET(u)
    u.content=content(u.get, as="text")
    u.html <- htmlParse(u.content)
#    print(u.html)
    picLink <-xpathSApply(u.html, "//*/img[@class='heroimg']/@src")
      #unname(picLink)
    ##  print(picLink)
      src1 <- paste0("http://www.premierleague.com",unname(picLink))
    #  print(src1) #1] "http://www.premierleague.com/content/dam/premierleague/shared-images/players/d/darren-bent/10738-lsh.jpg" is a picture
      tags$img(src=src1, width=300)
 # caption <-"test"
 # caption
})