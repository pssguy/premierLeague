teamLeadersData <- reactive({
  
  print("enter teamleaders")
  
  if (!is.null(input$team_3)) {
    theTeam <- input$team_3
  } else {
    theTeam=="Arsenal"
  }
  print(theTeam)
  print("str")
  print(str(leaders))
  print("?str")
  
  df <- leaders[leaders$TEAMNAME==theTeam,]
  df <- data.frame(df)
  df <- df[,-1]
  print(df)
  info=list(df=df)
  return(info)
})

output$teamLeaders <-  DT::renderDataTable({
  
 df <- teamLeadersData()$df %>%
    select(Season=season,Starts=starts,Sub=sub,Goals=goals,Assists=assists,Points=points,Cards=cards)
 
 DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                orderFixed=list(c(0,'desc'))))
  
}
)
