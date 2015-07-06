teamLeadersData <- reactive({
  

#   df <- leaders[leaders$TEAMNAME==input$teamA,]
#   print(names(df))
#  # df <- data.frame(df)
#   df <- df[,-1]
#   print(df)
  
  # needs to be tbl_df for dply action
  df <- tbl_df(leaders[leaders$TEAMNAME==input$teamA,]) %>% 
    select(-TEAMNAME)
  
  info=list(df=df)
  return(info)
})

output$teamLeaders <-  DT::renderDataTable({
  
 df <- teamLeadersData()$df %>%
    select(Season=season,Starts=starts,Sub=sub,Goals=goals,Assists=assists,Points=points,Cards=cards) %>% 
   arrange(desc(Season)) %>% 
 
 DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
                                orderFixed=list(c(0,'desc'))))
  
}
)
