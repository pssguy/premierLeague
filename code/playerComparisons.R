
observeEvent(input$compBtn,{
df <-playerGame %>% 
  filter((START+subOn>0)&PLAYERID %in% input$playerComps) %>% 
  select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID) %>% 
  mutate(gameOrder=row_number(),points=Assists+Gls,
         cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
  
  if (input$compCategory=="Points") {
df %>% 
  ggvis(~gameOrder,~cumPoints) %>% 
  
  layer_lines(stroke= ~name) %>% 
  add_axis("x", title="PL Appearances") %>% 
  add_axis("y", title="Cumulative Goals+Assists (inc. secondary") %>% 
  add_legend("stroke", title="") %>% 
  bind_shiny("sp_comparisons")
  }
})
  