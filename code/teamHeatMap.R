


output$heatResults <- renderPlotly({
  
  
  temp <- standings %>%
    filter(team=="Man. Utd.") %>%
    mutate(combo=paste0(GF,GA)) %>%
    group_by(combo) %>%
    tally()
  
  
  allCombos <- expand.grid(
    data.frame(GF=0:9,GA=0:9)
  ) %>%
    mutate(combo=paste0(GF,GA)) #still a df with 100vals
  
  test <- allCombos %>%
    left_join(temp) # lots of NAs
  
  # seems pretty pointless renaming does same
  test <- test %>%
    mutate(count=(n))
  
  test <- allCombos %>%
    left_join(temp) %>% 
    select(GF,GA,count=n)
  
  
  Games <- matrix(test$count, nrow = 10, ncol = 10, byrow = TRUE,
                  dimnames = list(unique(test$GF),
                                  unique(test$GA)))
  
  plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = mdat, hoverinfo="z",
          colorscale='YIOrRd', reversescale=T,
          type = "heatmap") %>%
    layout(xaxis = list(title = "Goals For"), 
           yaxis = list(title = "Goals Against"))
})