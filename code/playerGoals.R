observe({
  
  if (input$method=="Method") {
    Method  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID==input$player) %>%
      ggvis(x= ~season,y= ~goals,fill=~category) %>%
      layer_bars() %>%
      bind_shiny('playerGoals')
  } else if (input$method=="Play") {
    
    Play  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID==input$player) %>%
      ggvis(x= ~season,y= ~goals,fill=~category) %>%
      layer_bars() %>%
      bind_shiny('playerGoals')
  } else {
    
    Place  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID==input$player) %>%
      ggvis(x= ~season,y= ~goals,fill=~category) %>%
      layer_bars() %>%
      bind_shiny('playerGoals')
  } 
  
})