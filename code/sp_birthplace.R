### may want to replace in different area


temp <-summary %>% 
  ungroup() %>% 
  select(PLAYERID,mins,season,TEAMNAME) %>% 
  left_join(allPlayers) %>% 
  select(season,mins,COUNTRY,TEAMNAME) %>% 
  group_by(season,COUNTRY,TEAMNAME) %>% 
  summarize(totMins=sum(mins)) %>% 
  group_by(season,TEAMNAME) %>% 
  mutate(allMins=sum(totMins),pc=100*totMins/allMins) 


allTemp <-summary %>% 
  ungroup() %>% 
  select(PLAYERID,mins,season) %>% 
  left_join(allPlayers) %>% 
  select(season,mins,COUNTRY) %>% 
  group_by(season,COUNTRY) %>% 
  summarize(totMins=sum(mins)) %>% 
  group_by(season) %>% 
  mutate(allMins=sum(totMins),pc=100*totMins/allMins) ## allMins not same for all clubs ???

allTemp %>% 
  ungroup() %>% 
  filter(COUNTRY=="England") %>% 
  ggvis(~season,~pc) %>% 
  layer_lines(stroke:="coral", strokeWidth:=3) %>% 
  scale_numeric("y",domain=c(0,100)) %>% 
  add_axis("y",title="% Mins played by English Born Players in BPL") %>% 
  add_axis("x", properties = axis_props(labels = list(
    angle = 45, align = "left", fontSize = 10
  )),title = "") %>% 
  bind_shiny("sp_birthplaceChart")



