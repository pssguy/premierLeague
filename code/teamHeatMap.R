


output$heatResults <- renderPlotly({
  
  if (is.null(input$heatTeam)) return()
  
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
cv <- crosstalk::ClientValue$new("plotly_click", group = "A")

#  cv <- ClientValue$new("plotly_click", group = "A") #object 'ClientValue' not found

# output$selection <- renderPrint({
#   s <- cv$get()
#   print(s)
#   if (length(s) == 0) {
#     "Click on a cell in the heatmap to display a scatterplot"
#   } else {
#     cat("You selected: \n\n")
#     as.list(s)
#   }
# })

output$heatHeader <- renderUI({
  s <- cv$get()
  if (length(s)==0) return()
#    print(s[["y"]])
#    print(s[["x"]])
  gFor=s[["y"]]
  gAg =s[["x"]]
#   print(gFor)
#   print(gAg)
  
 
  
  if (gFor>gAg) {
  h4(paste0(gFor,"-",gAg, " victories"))
  } else if (gAg>gFor) {
    h4(paste0(gFor,"-",gAg, " losses")) 
  } else {
  h4(paste0(gFor,"-",gAg, " draws"))
  }
})
output$heatTable <- DT::renderDataTable({
 
  s <- cv$get() 
  
  if (length(s)==0) {
    return()
  } else {

    gFor=s[["y"]]
    gAg =s[["x"]]
    standings %>% 
      ungroup() %>% 
      filter(team==input$heatTeam&GF==gFor&GA==gAg) %>% 
      arrange(desc(gameDate)) %>% 
      select(Opponents=OppTeam,Venue=venue,
             Season=season,Date=gameDate) %>% 
      DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  }
  
})