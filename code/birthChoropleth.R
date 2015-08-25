## for now just do point locations

output$teamLeaflet <- renderLeaflet({
  if(is.null(input$teamA)) return()
  print("enter team birthplaces")
  print(input$teamA)
  theMap <-  summary %>% 
    ungroup() %>% 
    filter(TEAMNAME==input$teamA) %>% 
    mutate(Apps=St+On) %>% 
    select(name,PLAYERID,Apps) %>% 
    group_by(name,PLAYERID) %>% 
    summarize(Apps=sum(Apps)) %>% 
    ungroup()
  
  write_csv(theMap,"theMapProblem.csv")
  
  theMap <-theMap %>% 
    left_join(pgMini) %>% 
    filter(PLAYERID!="OWNGOAL")  
  
  write_csv(theMap,"theMapProblem2.csv") # no obv issue with Barnsley
  
  theMap$lon <-  jitter(theMap$lon, amount=0.5)
  theMap$lat <-  jitter(theMap$lat, amount=0.5)
  
  theMap$popup <-
    sprintf(
      "<table cellpadding='4' style='line-height:1'><tr>
      <th> %1$s  </th></tr>
<tr align='center'><td> %2$s</td></tr>
      
      <tr align='center'><td>Apps: %3$s</td></tr>
        </table>",
      theMap$name,
      theMap$place,
      theMap$Apps
    )
  
  write_csv(theMap,"theMapProblem3.csv")
  binpalEPL <-
    colorBin(c("#FFFF00","#FF8000","#FF0000"), theMap$Apps,  pretty = TRUE)
  
  print(theMap)
  print(str(theMap))
  print("drawing map")
  
  theMap <- data.frame(theMap) #makes no difference
  
  theMap %>%   
    leaflet() %>%
    addTiles() %>%
   # setView(2,49,zoom=3) %>% 
    addCircleMarkers(
      radius = 3,fillOpacity = 0.5,popup =  ~ popup,color = ~ binpalEPL(Apps)
    )
  ## legend not working
  
})

# output$birthChoropleth <- renderPlot({
#   if(is.null(input$teamA)) return()
#   print("enter choropleth")
#   test <-playerGame %>% 
#     filter(TEAMNAME==input$teamA&(START+subOn)>0) %>% 
#     group_by(COUNTRY) %>% 
#     tally() %>% 
#     ungroup() %>% 
#     arrange(desc(n))
#   
#   
#   
#   test[test$COUNTRY=="England",]$COUNTRY <- "united kingdom"
#   test[test$COUNTRY=="Wales",]$COUNTRY <- "united kingdom"
#   test[test$COUNTRY=="Scotland",]$COUNTRY <- "united kingdom"
#   test[test$COUNTRY=="N. Ireland",]$COUNTRY <- "united kingdom"
#   
# chart <-  test %>% 
#     group_by(COUNTRY) %>% 
#     summarize(ct=sum(n)) %>% 
#     mutate(pc=round(100*ct/sum(ct),1),region=tolower(COUNTRY)) %>% 
#     rename(value=pc) %>% 
#     select(region,value) %>% 
#     unique(.) 
# 
# # print(glimpse(chart))
# # write_csv(chart,"problem.csv")
# # df <- read_csv("problem.csv")
# country_choropleth(df,num_colors=1)
#    
# },width=400,height=300)