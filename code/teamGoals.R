##  3 tables

output$teamGoalsFor <- DT::renderDataTable({

  if (input$tmGoals=="For") {
  Goals_team_for[Goals_team_for$team==input$teamA,] %>% 
    select(-team) %>% 
    arrange(desc(season)) %>% 
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
  } else if (input$tmGoals=="Against") {
  Goals_team_ag[Goals_team_ag$opponent==input$teamA,] %>% 
    select(-opponent) %>% 
    arrange(desc(season)) %>% 
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
  } else {
  
    dfFor <- Goals_team_for[Goals_team_for$team==input$teamA,]
    dfAg <- Goals_team_ag[Goals_team_ag$opponent==input$teamA,] 
    df <-dfAg %>%
      rename(team=opponent) %>%
      inner_join(dfFor, by=c("season","team")) %>%
      mutate(Open=Open.y-Open.x,Corner=Corner.y-Corner.x,Throw=Throw.y-Throw.x,IFK=IFK.y-IFK.x,
             DFK=DFK.y-DFK.x,Pen=Pen.y-Pen.x,SixYd=SixYd.y-SixYd.x, PenArea=PenArea.y-PenArea.x,
             LongRange=LongRange.y-LongRange.x,Right=Right.y-Right.x,Left=Left.y-Left.x,Head=Head.y-Head.x,Tot=Tot.y-Tot.x) %>%
      select(season,Open,Corner,Throw,IFK,DFK,Pen,SixYd,PenArea,LongRange,Right,Left,Head,Tot) %>%
      arrange(desc(season)) %>% 
      DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
    
    
    
}
  
})


# output$teamGoalsAg <- DT::renderDataTable({
#   #    print("gta")
#   #    print(Goals_team_ag)
#   df <- Goals_team_ag[Goals_team_ag$opponent==input$team_3,]
#   df <- df[,c(-2)]
#   print(df)
# df <-   df %>%
#     arrange(desc(season))
#   
#   DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
#                                
#   
# })
# 
# output$teamGoalsDiff <- DT::renderDataTable({
#   print("gtf")
#   dfFor <- Goals_team_for[Goals_team_for$team=="Arsenal",]
#   dfAg <- Goals_team_ag[Goals_team_ag$opponent=="Arsenal",] 
#   df <-dfAg %>%
#     rename(team=opponent) %>%
#     inner_join(dfFor, by=c("season","team")) %>%
#     mutate(Open=Open.y-Open.x,Corner=Corner.y-Corner.x,Throw=Throw.y-Throw.x,IFK=IFK.y-IFK.x,
#            DFK=DFK.y-DFK.x,Pen=Pen.y-Pen.x,SixYd=SixYd.y-SixYd.x, PenArea=PenArea.y-PenArea.x,
#            LongRange=LongRange.y-LongRange.x,Right=Right.y-Right.x,Left=Left.y-Left.x,Head=Head.y-Head.x,Tot=Tot.y-Tot.x) %>%
#     select(season,Open,Corner,Throw,IFK,DFK,Pen,SixYd,PenArea,LongRange,Right,Left,Head,Tot)
#   
#   #df <- df[,c(-2)]
#   df <-  df %>%
#     arrange(desc(season))
#   print("df")
#   print(df)
#   DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
#   
# })
# 
