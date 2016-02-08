
observeEvent(input$compBtn,{
  
  # base data.frame
df <-playerGame %>% 
  filter((START+subOn>0)&PLAYERID %in% input$playerComps) %>% 
  select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID) %>% 
  mutate(gameOrder=row_number(),points=Assists+Gls,
         cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
  
# set up plot from radio button choices

  if (input$compCategory=="Points") {
    yTitle <-"Cumulative Goals+ Assists (inc. secondary)"
    if (input$compTime=="Apps") {
      xTitle <-"PL Apps"
plot <- df %>% 
  ggvis(~gameOrder,~cumPoints) 
    } else if (input$compTime=="Age") {
      xTitle <-"Age"
      plot <- df %>% 
        ggvis(~age,~cumPoints) 
    } else if (input$compTime=="Date") {
      xTitle <-""
      plot <- df %>% 
        ggvis(~gameDate,~cumPoints) 
    }
    
  } else if  (input$compCategory=="Goals") {   
    yTitle <-"Cumulative Goals"
    if (input$compTime=="Apps") {
      xTitle <-"PL Apps"
      plot <- df %>% 
        ggvis(~gameOrder,~cumGoals) 
    } else if (input$compTime=="Age") {
      xTitle <-"Age"
      plot <- df %>% 
        ggvis(~age,~cumGoals) 
    } else if (input$compTime=="Date") {
      xTitle <-""
      plot <- df %>% 
        ggvis(~gameDate,~cumGoals) 
    } 
    } else if  (input$compCategory=="Assists") {   
      yTitle <-"Cumulative Assists (inc. secondary)"
      if (input$compTime=="Apps") {
        xTitle <-"PL Apps"
        plot <- df %>% 
          ggvis(~gameOrder,~cumAssists) 
      } else if (input$compTime=="Age") {
        xTitle <-"Age"
        plot <- df %>% 
          ggvis(~age,~cumAssists) 
      } else if (input$compTime=="Date") {
        xTitle <-""
        plot <- df %>% 
          ggvis(~gameDate,~cumAssists) 
      }
    }
    
# create plot
plot %>% 
  layer_lines(stroke= ~name) %>% 
  ggvis::add_axis("x", title=xTitle) %>% 
  ggvis::add_axis("y", title=yTitle) %>% 
  add_legend("stroke", title="") %>% 
  bind_shiny("sp_comparisons")


})
  