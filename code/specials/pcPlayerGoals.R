

output$pcPlayerGoals <- renderPlotly({
  
  print(input$pcPlGoals)
  
minGoals <- Place %>% 
  mutate(goals=(SixYd+PenArea+LongRange)) %>% 
  group_by(PLAYERID) %>% 
  summarize(tot=sum(goals)) %>% 
  filter(tot>=input$pcPlGoals) %>% 
  .$PLAYERID

print(minGoals)

df <- Place %>% 
  filter(PLAYERID %in% minGoals&PLAYERID!="OWNGOAL") %>% 
  mutate(goals=(SixYd+PenArea+LongRange)) %>% 
  group_by(PLAYERID,name) %>% 
  summarize(tot=sum(goals),lr=sum(LongRange),pc=round(100*lr/tot)) %>% 
  ungroup() %>% 
  arrange(desc(pc))  

print(glimpse(df))

write_csv(df,"problem.csv")

plot_ly(df, x = tot, y = pc, mode = "markers", hoverinfo = "text",
        text = paste(name,
                     "<br>Category: ",lr,
                     "<br>Toatal: ",tot,
                     "<br>PerCent: ",pc,"%")) %>%
  layout(hovermode = "closest",
         title="% of Premier League Goals by Category",
         xaxis=list(title="Total Goals"),
         yaxis=list(title="% By category"
         )
  )

})