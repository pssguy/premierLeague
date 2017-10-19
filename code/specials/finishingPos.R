finalStandings <-
  standings %>% 
  ungroup() %>% 
  arrange(desc(tmYrGameOrder)) %>% ## act doesnt matter as all games have final_pos in
  group_by(season,team) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(season,final_Pos,team,cumPts,cumGD,cumGF)


output$sp_finishingPos <- renderPlotly({
  
  req(input$teamYear_fp)
  # theTeam <- "Tottenham H"
  # theSeason <- "2015/16"
  
  theTeam <- input$team_fp
  theSeason <- input$teamYear_fp
  
  choice <- finalStandings %>% 
    filter(team==theTeam&season==theSeason) 
  choice$season <- "extra"
  
  seasons <- unique(finalStandings$season)
  seasons <-
    seasons[4:24] 
  
  for (i in seq_along(seasons)) {
    
    tempdf <- finalStandings %>% 
      filter(season==seasons[i]&team!=theTeam) %>% 
      rbind(choice) %>% 
      arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
      mutate(position=as.integer(row.names(.))) %>% ## had bben mutate(position=row_numbers())) but got error in new season
      ungroup() 
    tempdf$trueSeason <- seasons[i]
    if(i!=1) {
      df <- rbind(df,tempdf)
    } else {
      df <- tempdf
    }
    
  }
  
  df %>% 
    filter(season=="extra") %>% 
    plot_ly() %>% 
    add_markers(x=~trueSeason,y=~position) %>%
    layout(margin=list(b=80),
      title = " ",
      xaxis = list(title = " "),
      yaxis = list(title="Position",
                   range=list(20.2,0.8),dtick=1) # the 0.8 is a fudge that ensures that all the point gets shown
    ) %>% 
    config(displayModeBar = F,showLink = F)
  
})


# output$sp_finishingPos_front <- renderPlotly({
#   
#   req(input$teamYear_fp_front)
#   # theTeam <- "Tottenham H"
#   # theSeason <- "2015/16"
#   
#   theTeam <- input$team_fp_front
#   theSeason <- input$teamYear_fp_front
#   
#   choice <- finalStandings %>% 
#     filter(team==theTeam&season==theSeason) 
#   choice$season <- "extra"
#   
#   seasons <- unique(finalStandings$season)
#   seasons <-
#     seasons[4:24] 
#   
#   for (i in seq_along(seasons)) {
#     
#     tempdf <- finalStandings %>% 
#       filter(season==seasons[i]&team!=theTeam) %>% 
#       rbind(choice) %>% 
#       arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
#       mutate(position=as.integer(row.names(.))) %>%  
#       ungroup() 
#     tempdf$trueSeason <- seasons[i]
#     if(i!=1) {
#       df <- rbind(df,tempdf)
#     } else {
#       df <- tempdf
#     }
#     
#   }
#   
#   df %>% 
#     filter(season=="extra") %>% 
#     plot_ly(x=~trueSeason,y=~position,mode="markers") %>%
#     layout(
#       title = " ",
#       xaxis = list(title = " "),
#       yaxis = list(title="Position",
#                    range=list(20.2,0.8),dtick=1) # the 0.8 is a fudge that ensures that all the point gets shown
#     ) %>% 
#     config(displayModeBar = F,showLink = F)
#   
# })
