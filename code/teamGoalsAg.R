

output$teamGoalsAg <- DT::renderDataTable({
  #    print("gta")
  #    print(Goals_team_ag)
  df <- Goals_team_ag[Goals_team_ag$opponent==input$team_3,]
  df <- df[,c(-2)]
  print(df)
df <-   df %>%
    arrange(desc(season))
  
  DT::datatable(df,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
                               
  
})

