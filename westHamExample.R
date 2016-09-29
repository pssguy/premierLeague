

sort(names(standings))

standings

unique(standings$team)

df <- standings %>% 
  ungroup() %>% 
  arrange(gameDate) %>% 
  select(res,season,team,tmYrGameOrder) %>% 
  mutate(loss=ifelse(res=="Loss",1,0)) %>% 
  group_by(season,team) %>% 
  mutate(cumLoss=cumsum(loss)) %>% 
  filter(team=="West Ham U"&cumLoss==5) %>% 
  group_by(season) %>% 
  slice(1) %>% 
  plot_ly %>% 
  add_markers(x=~tmYrGameOrder,y=~season) %>% 
  layout(title="West Ham - games played when fifth loss suffered",
         xaxis=list(title="Games Played", rangemode = "tozero"),
         yaxis=list(title=""))

mean(df$tmYrGameOrder)