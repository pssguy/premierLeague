
output$playerWiki <- renderUI({
  

  if (is.null(input$playerA)) return()
  print("enter wiki")
  print(input$playerA)
    
 theName <- playerGame[playerGame$PLAYERID==input$playerA,]$name[1]
    theName <- str_replace_all(theName," ","_")
    
    print(theName)
    
    wikiURL <- paste0("http://en.wikipedia.org/wiki/",theName)
  print(wikiURL)
  
  test <- http_status(GET(wikiURL))  #http_status(GET("http://en.wikipedia.org/wiki/Ryan_Bennett"))
  print(test$category)                 # no client error but also length(vcard)
  if (test$category != "client error") {
    print("enter vcard")
  vcard <- html(wikiURL) %>%
    html_nodes(".vcard")
  
  print(vcard)
  
  if (length(vcard) == 0)
    return()
  
  vcardInfo <- vcard[[1]]
  
  HTML(as(vcardInfo,"character"))
  }
})
