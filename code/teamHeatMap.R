


output$heatResults <- renderPlotly({
  
  #if (is.null(input$heatTeam)) return()
  req(input$heatTeam)
  
  temp <- standings %>%
    filter(team==input$heatTeam) %>%
   
    mutate(combo=paste0(GF,GA)) %>%
    group_by(combo) %>%
    tally()
  
  
  allCombos <- expand.grid(
    data.frame(GF=0:9,GA=0:9)
  ) %>%
    mutate(combo=paste0(GF,GA)) #still a df with 100vals
  
#   test <- allCombos %>%
#     left_join(temp) # lots of NAs
#   
#   # seems pretty pointless renaming does same
#   test <- test %>%
#     mutate(count=(n))
  
  test <- allCombos %>%
    left_join(temp) %>% 
    select(GF,GA,count=n)
  
  # need to transform
  Games <- t(matrix(test$count, nrow = 10, ncol = 10, byrow = TRUE,
                  dimnames = list(unique(test$GF),
                                  unique(test$GA))))
  
  
  plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = Games, hoverinfo="z",
          colorscale='YIOrRd', reversescale=T,
          type = "heatmap") %>%
    layout(xaxis = list(title = "Goals Against"), 
           yaxis = list(title = "Goals For"))
})

## crosstalk to get to table of those results
#cv <- crosstalk::ClientValue$new("plotly_click", group = "A")




## look to put into reactive so that can use to produce goal scoring table

heatData <- reactive({
  
  print("enter heatData")
  
  # if(is.null(cv$get())) return
  # 
  # s <- cv$get()
  
  if(is.null(event_data("plotly_click"))) return()
 # req(event_data("plotly_selected")) not appropriate use of req apparantly
  
  s <- event_data("plotly_click")
  if (length(s)==0) return()
  
  gFor=s[["y"]]
  gAg =s[["x"]]
 
  
  
  
  if (gFor>gAg) {
 header<-  h4(paste0(gFor,"-",gAg, " victories"))
  } else if (gAg>gFor) {
    header<-    h4(paste0(gFor,"-",gAg, " losses")) 
  } else {
    header<-   h4(paste0(gFor,"-",gAg, " draws"))
  }
  
 df <- standings %>%
    ungroup() %>%
    filter(team == input$heatTeam & GF == gFor & GA == gAg) %>%
    arrange(desc(gameDate)) %>%
    select(
      Opponents = OppTeam,Venue = venue,
      Season = season,Date = gameDate,MATCHID
    ) 
 
 info=list(df=df,header=header)
 return(info)
  
  
})


output$heatHeader <- renderUI({

  if (is.null(heatData())) return()
  #req()
  heatData()$header
  
})


output$heatTable <- DT::renderDataTable({
  if (is.null(heatData())) return()
  
  
  heatData()$df %>%
      DT::datatable(selection='single',
                    class = 'compact stripe hover row-border order-column',rownames = FALSE,options = list(
                      paging = TRUE, searching = FALSE,info = FALSE,
                      columnDefs= list(list(visible=FALSE,targets=list(4)))
                    )
      )
  })
  

output$matchScorers <- renderd3kit_timeline({
  
  if(is.null(values$MATCHID)) return()
  
  allTeams <- teamGames %>% 
    filter(MATCHID == values$MATCHID)
  
  teamMatch <- allTeams$TEAMMATCHID
  
  twGoals <- goals %>% 
    filter(TEAMMATCHID %in% teamMatch)
  
  a <- twGoals %>% 
    left_join(playerGame) %>% 
    select(player=LASTNAME,time=TIME,team=TEAMNAME,venue,MATCHID) %>% 
    mutate(color=ifelse(venue=="H","#0000ff","#458B00")) %>%
    arrange(time)
  
  df <- a %>% 
    filter(MATCHID==values$MATCHID)
  
  print("glimpsedf")
  print(glimpse(df))
  
  write_csv(df,"problem.csv")
  
  colorJS <- htmlwidgets::JS("function(d){return d.color;}")
  
  d3kit_timeline(
    df,
    direction = "down",
    ## having team there takes up too much space was return d.player + ' - ' + d.team;
    textFn = htmlwidgets::JS(
      "
    function(d){
    return d.player;
    }
    "
    ),
    # color probably needs to be treated like the *Fn arguments
    #  for ultimate flexibility
    dotColor = colorJS,
    linkColor = colorJS,
    labelTextColor = "#FFF",
    labelBgColor = colorJS,
    dotRadius = 3,
    labella = list(maxPos = 600),
    layerGap = 10, # distance to axis
    margin = list(left = 20, right = 100, top = 20, bottom = 40),
    scale = htmlwidgets::JS("d3.scale.linear()"),
    domain = c(0,90),
    width = 500,
    height = 150
  )
  
})


# output$heatHeader <- renderUI({
#   s <- cv$get()
#   if (length(s)==0) return()
# #    print(s[["y"]])
# #    print(s[["x"]])
#   gFor=s[["y"]]
#   gAg =s[["x"]]
# #   print(gFor)
# #   print(gAg)
#   
#  
#   
#   if (gFor>gAg) {
#   h4(paste0(gFor,"-",gAg, " victories"))
#   } else if (gAg>gFor) {
#     h4(paste0(gFor,"-",gAg, " losses")) 
#   } else {
#   h4(paste0(gFor,"-",gAg, " draws"))
#   }
# })
# 
# 
# output$heatTable <- DT::renderDataTable({
#   s <- cv$get()
#   
#   if (length(s) == 0) {
#     return()
#   } else {
#     gFor = s[["y"]]
#     gAg = s[["x"]]
#     standings %>%
#       ungroup() %>%
#       filter(team == input$heatTeam & GF == gFor & GA == gAg) %>%
#       arrange(desc(gameDate)) %>%
#       select(
#         Opponents = OppTeam,Venue = venue,
#         Season = season,Date = gameDate
#       ) %>%
#       DT::datatable(selection='single',
#         class = 'compact stripe hover row-border order-column',rownames = FALSE,options = list(
#           paging = TRUE, searching = FALSE,info = FALSE
#         )
#       )
#   }
#   
# })