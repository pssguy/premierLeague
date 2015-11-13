
observeEvent(input$compBtn,{
df <-playerGame %>% 
  filter((START+subOn>0)&PLAYERID %in% input$playerComps) %>% 
  select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID) %>% 
  mutate(gameOrder=row_number(),points=Assists+Gls,
         cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
  
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
    
    
    
plot %>% 
  layer_lines(stroke= ~name) %>% 
  add_axis("x", title=xTitle) %>% 
  add_axis("y", title=yTitle) %>% 
  add_legend("stroke", title="") %>% 
  bind_shiny("sp_comparisons")
  } else if  (input$compCategory=="Goals") {
   
  df %>% 
    ggvis(~gameOrder,~cumGoals) %>% 
    
    layer_lines(stroke= ~name) %>% 
    add_axis("x", title="PL Appearances") %>% 
    add_axis("y", title="Cumulative Goals") %>% 
    add_legend("stroke", title="") %>% 
    bind_shiny("sp_comparisons")
  } else if  (input$compCategory=="Assists") {
   
    df %>% 
      ggvis(~gameOrder,~cumAssists) %>% 
      
      layer_lines(stroke= ~name) %>% 
      add_axis("x", title="PL Appearances") %>% 
      add_axis("y", title="Cumulative Assists (inc. secondary)") %>% 
      add_legend("stroke", title="") %>% 
      bind_shiny("sp_comparisons")
  }
})
  