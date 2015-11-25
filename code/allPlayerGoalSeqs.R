

# observe({
# 
#   
#   if(is.null(input$teamC)) {
#     theTeam<-"All Teams"
#   } else {
#     theTeam<-input$teamC
#   }
# 
#   ## need to have different datasets depending whether all plyers or within team
#   
#   if (theTeam=="All Teams") {
#   bestRun <- goalSeqs %>% 
#           
#           select(PLAYERID,slength) %>% 
#           arrange(PLAYERID,desc(slength)) %>% 
#           group_by(PLAYERID) %>% 
#           slice(1)
#   
#   topScorers <- playerGame %>% 
#     filter(PLAYERID %in% bestRun$PLAYERID) %>% 
#     group_by(name,PLAYERID) %>% 
#     
#     summarize(totGoals=sum(Gls)) %>% 
#     inner_join(bestRun)
#   
#   } else {
#     bestRun <- goalSeqsClub %>% 
#       filter(TEAMNAME==theTeam) %>% 
#       select(PLAYERID,slength) %>% 
#       arrange(PLAYERID,desc(slength)) %>% 
#       group_by(PLAYERID) %>% 
#       slice(1)
#     
#     topScorers <- playerGame %>% 
#       filter(PLAYERID %in% bestRun$PLAYERID&TEAMNAME==theTeam) %>% 
#       group_by(name,PLAYERID) %>% 
#       
#       summarize(totGoals=sum(Gls)) %>% 
#       inner_join(bestRun)
# }
# 
# 
#   
#   # need id for tooltip
#   topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  
#   
#   # need jitter to identify individuals
#   topScorers$jitGoals <- jitter(topScorers$totGoals)
#   topScorers$jitslength <- jitter(topScorers$slength)
#  
#   
# 
#   
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
#     #paste0(names(row),": ", format(row), collapse = "<br />")
#     paste0( format(row), collapse = "<br />")
#   }
#   
#   ## slightly different axes dependent on whether team or not
#   
#   if (theTeam=="All Teams") {
#   topScorers %>% 
#     ggvis(~jitGoals,~jitslength,key := ~id) %>% 
#       layer_points(size=2) %>% 
#     add_axis("y",title="Best Consecutive Scoring Run in PL games", format='d') %>% 
#     add_axis("x", title="Career Premier League goals") %>% 
#     add_tooltip(all_values,"click") %>% 
#     bind_shiny("allPlayerGoalSeqs")
#   } else {
#     topScorers %>% 
#       ggvis(~jitGoals,~jitslength,key := ~id) %>% 
#       layer_points(size=2) %>% 
#       add_axis("y",title="Best Consecutive Scoring Run in PL games for Club", format='d') %>% 
#       add_axis("x", title="Career Premier League goals for Club") %>% 
#       add_tooltip(all_values,"click") %>% 
#       bind_shiny("allPlayerGoalSeqs")
#   }
# })
# 
# 
# ## plotly version - initially all data
# 
# output$allPlayerGoalSeqs_plotly <- renderPlotly({
# bestRun <- goalSeqs %>% 
#   
#   select(PLAYERID,slength) %>% 
#   arrange(PLAYERID,desc(slength)) %>% 
#   group_by(PLAYERID) %>% 
#   slice(1)
# 
# topScorers <- playerGame %>% 
#   filter(PLAYERID %in% bestRun$PLAYERID) %>% 
#   group_by(name,PLAYERID) %>% 
#   
#   summarize(totGoals=sum(Gls)) %>% 
#   inner_join(bestRun)
# 
# # need jitter to identify individuals
# topScorers$jitGoals <- jitter(topScorers$totGoals)
# topScorers$jitslength <- jitter(topScorers$slength)
# 
# plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
#         text = paste("Name:",name,"<br> Goals:",totGoals,"<br> Sequence:",slength)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Career Premier League goals"),
#          yaxis=list(title="Best Consecutive Scoring Run in PL games for Club"
#                    )
#   )
# 
# })

## separate out data and code

data <- reactive({
  
  if(is.null(input$teamC)) {
    theTeam<-"All Teams"
  } else {
    theTeam<-input$teamC
  }
  
  ## need to have different datasets depending whether all plyers or within team
  
  if (theTeam=="All Teams") {
    bestRun <- goalSeqs %>% 
      
      select(PLAYERID,slength) %>% 
      arrange(PLAYERID,desc(slength)) %>% 
      group_by(PLAYERID) %>% 
      slice(1)
    
    topScorers <- playerGame %>% 
      filter(PLAYERID %in% bestRun$PLAYERID) %>% 
      group_by(name,PLAYERID) %>% 
      
      summarize(totGoals=sum(Gls)) %>% 
      inner_join(bestRun)
    
  } else {
    bestRun <- goalSeqsClub %>% 
      filter(TEAMNAME==theTeam) %>% 
      select(PLAYERID,slength) %>% 
      arrange(PLAYERID,desc(slength)) %>% 
      group_by(PLAYERID) %>% 
      slice(1)
    
    topScorers <- playerGame %>% 
      filter(PLAYERID %in% bestRun$PLAYERID&TEAMNAME==theTeam) %>% 
      group_by(name,PLAYERID) %>% 
      
      summarize(totGoals=sum(Gls)) %>% 
      inner_join(bestRun)
  }
  
  
  
  # need id for tooltip
  topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  
  
  # need jitter to identify individuals
  topScorers$jitGoals <- jitter(topScorers$totGoals)
  topScorers$jitslength <- jitter(topScorers$slength)
  
  
  info=list(topScorers=topScorers,theTeam=theTeam)
  return(info)
})


output$allPlayerGoalSeqs_plotly <- renderPlotly({
  
  topScorers <- data()$topScorers
  theTeam <- data()$theTeam
  
  print("topScorers")
  print(glimpse(topScorers))
  
  if (theTeam=="All Teams") {
  plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
          text = paste(name,"<br> Total Goals:",totGoals,"<br> Best Run:",slength)) %>%
    layout(hovermode = "closest",
           xaxis=list(title="Career Premier League goals"),
           yaxis=list(title="Best Consecutive Scoring Run in PL games "
           )
    ) 
  } else {
    plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
            text = paste("Name:",name,"<br> Goals:",totGoals,"<br> Sequence:",slength)) %>%
      layout(hovermode = "closest",
             xaxis=list(title="Career Premier League goals for Club"),
             yaxis=list(title="Best Consecutive Scoring Run in PL games for Club "
             )
      ) 
  }
  
})

# observe({
#   
#   
#   if(is.null(input$teamC)) {
#     theTeam<-"All Teams"
#   } else {
#     theTeam<-input$teamC
#   }
#   
#   ## need to have different datasets depending whether all plyers or within team
#   
#   if (theTeam=="All Teams") {
#     bestRun <- goalSeqs %>% 
#       
#       select(PLAYERID,slength) %>% 
#       arrange(PLAYERID,desc(slength)) %>% 
#       group_by(PLAYERID) %>% 
#       slice(1)
#     
#     topScorers <- playerGame %>% 
#       filter(PLAYERID %in% bestRun$PLAYERID) %>% 
#       group_by(name,PLAYERID) %>% 
#       
#       summarize(totGoals=sum(Gls)) %>% 
#       inner_join(bestRun)
#     
#   } else {
#     bestRun <- goalSeqsClub %>% 
#       filter(TEAMNAME==theTeam) %>% 
#       select(PLAYERID,slength) %>% 
#       arrange(PLAYERID,desc(slength)) %>% 
#       group_by(PLAYERID) %>% 
#       slice(1)
#     
#     topScorers <- playerGame %>% 
#       filter(PLAYERID %in% bestRun$PLAYERID&TEAMNAME==theTeam) %>% 
#       group_by(name,PLAYERID) %>% 
#       
#       summarize(totGoals=sum(Gls)) %>% 
#       inner_join(bestRun)
#   }
#   
#   
#   
#   # need id for tooltip
#   topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  
#   
#   # need jitter to identify individuals
#   topScorers$jitGoals <- jitter(topScorers$totGoals)
#   topScorers$jitslength <- jitter(topScorers$slength)
#   
#   
#   
#   
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
#     #paste0(names(row),": ", format(row), collapse = "<br />")
#     paste0( format(row), collapse = "<br />")
#   }
#   
#   ## slightly different axes dependent on whether team or not
#   
#   if (theTeam=="All Teams") {
#     topScorers %>% 
#       ggvis(~jitGoals,~jitslength,key := ~id) %>% 
#       layer_points(size=2) %>% 
#       add_axis("y",title="Best Consecutive Scoring Run in PL games", format='d') %>% 
#       add_axis("x", title="Career Premier League goals") %>% 
#       add_tooltip(all_values,"click") %>% 
#       bind_shiny("allPlayerGoalSeqs")
#   } else {
#     topScorers %>% 
#       ggvis(~jitGoals,~jitslength,key := ~id) %>% 
#       layer_points(size=2) %>% 
#       add_axis("y",title="Best Consecutive Scoring Run in PL games for Club", format='d') %>% 
#       add_axis("x", title="Career Premier League goals for Club") %>% 
#       add_tooltip(all_values,"click") %>% 
#       bind_shiny("allPlayerGoalSeqs")
#   }
# })
# 
# 
# ## plotly version - initially all data
# 
# output$allPlayerGoalSeqs_plotly <- renderPlotly({
#   bestRun <- goalSeqs %>% 
#     
#     select(PLAYERID,slength) %>% 
#     arrange(PLAYERID,desc(slength)) %>% 
#     group_by(PLAYERID) %>% 
#     slice(1)
#   
#   topScorers <- playerGame %>% 
#     filter(PLAYERID %in% bestRun$PLAYERID) %>% 
#     group_by(name,PLAYERID) %>% 
#     
#     summarize(totGoals=sum(Gls)) %>% 
#     inner_join(bestRun)
#   
#   # need jitter to identify individuals
#   topScorers$jitGoals <- jitter(topScorers$totGoals)
#   topScorers$jitslength <- jitter(topScorers$slength)
#   
#   plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
#           text = paste("Name:",name,"<br> Goals:",totGoals,"<br> Sequence:",slength)) %>%
#     layout(hovermode = "closest",
#            xaxis=list(title="Career Premier League goals"),
#            yaxis=list(title="Best Consecutive Scoring Run in PL games for Club"
#            )
#     )
#   
# })