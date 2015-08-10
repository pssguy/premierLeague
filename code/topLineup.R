

lineupData <- eventReactive(input$teamA,{
  
  ## allowable players
  teamEver <- playerGame %>% 
    filter(TEAMNAME==input$teamA&START>0) %>% 
    select(PLAYERID,name) %>% 
    unique()
  
  # create a character vector which as ordered by id 
  # can be assessed for how common it is
  test <-playerGame %>% 
    filter(TEAMNAME==input$teamA&START>0) %>% 
    group_by(TEAMMATCHID) %>% 
    arrange(PLAYERID) %>% 
    do(data.frame(x=paste0(.$PLAYERID, collapse="_")))
  
 #print(glimpse(test))
  # count of lineups
  y <- sort(table(test$x))
  
  together <-y[[length(y)]]
#  print(together)
  # now extract names
  eleven <-names(y[length(y)])
  
  z <-str_split_fixed(eleven,"_",n=11)
  v <-data.frame(z)
  
  
  lineup <-gather(v)
  lineup$key <- NULL
  
#  print(glimpse(lineup))
  
  colnames(lineup)[1] <- "PLAYERID"
  
 
  lineup <- lineup %>% 
    left_join(teamEver) %>% 
    select(name) %>% 
    unique()
  print(glimpse(lineup))
  print(str(lineup))
  
  lineupText <- paste(lineup$name,collapse=", ")
  print(lineupText)
  
  info=list(together=together,lineupText=lineupText)
  return(info)
  
})

output$lineupCount <- renderText({
  paste0("This Starting lineup was used ",lineupData()$together," times")
})

output$lineupText <- renderText({
  print("enter lineup")
  print(lineupData()$lineupText)
  lineupData()$lineupText
})

# output$lineup <- DT::renderDataTable({
#  # if(is.null(lineupData())) return()
#   print(str(lineupData()$lineup))
#   lineupData()$lineup %>% 
#     DT::datatable()
# #     DT::datatable(class='compact stripe hover row-border',options= list(
# #       pageLength = 11,lengthChange=FALSE,paging = FALSE , searching = FALSE, info=FALSE,sorting = FALSE))
#   
# })