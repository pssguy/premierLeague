
# having issue with this when ataglance plauer input is set to "" additional return category seems to help

observe({
 # print("enter goal categories")
  
  if (is.null(input$playerA))
    return()
  if (input$playerA=="")
    return()
 # print(input$playerA)
  
  if (input$method == "Method") {
    Method  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID == input$playerA) %>%
      ggvis(x = ~ season,y = ~ goals,fill =  ~ category) %>%
      layer_bars(width=0.7) %>%
      set_options(width=500) %>% 
      ggvis::add_axis("x", properties = axis_props(labels = list(
        angle = 45, align = "left", fontSize = 11
      )),title = "") %>% 
      ggvis::add_axis("y", title = "Goals", format='d') %>% 
    bind_shiny('playerGoals')
  } else if (input$method == "Play") {
    Play  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID == input$playerA) %>%
      ggvis(x = ~ season,y = ~ goals,fill =  ~ category) %>%
      layer_bars(width=0.7) %>%
      ggvis::add_axis("x", properties = axis_props(labels = list(
        angle = 45, align = "left", fontSize = 11
      )),title = "") %>% 
      ggvis::add_axis("y", title = "Goals", format='d') %>% 
      bind_shiny('playerGoals')
  } else {
    Place  %>%   gather(category,goals,-c(PLAYERID,name,season)) %>%
      filter(PLAYERID == input$playerA) %>%
      ggvis(x = ~ season,y = ~ goals,fill =  ~ category) %>%
      layer_bars(width=0.7) %>%
      ggvis::add_axis("x", properties = axis_props(labels = list(
        angle = 45, align = "left", fontSize = 11
      )),title = "") %>% 
      ggvis::add_axis("y", title = "Goals", format='d') %>% 
      bind_shiny('playerGoals')
  }
  
})