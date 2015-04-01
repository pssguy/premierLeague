### Team goals 

output$teamGoalsFor <- DT::renderDataTable({
  print("gtf")
  df <- Goals_team_for[Goals_team_for$team==input$team_3,]
  df <- df[,c(-2)]
df <-  df %>%
    arrange(desc(season))
  
#DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)

DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE))

})

