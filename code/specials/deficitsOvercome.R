

output$relegationOvercome <- renderPlotly({
  
modern <-
  standings %>% 
  filter(season>"1994/95")

modern %>% 
  select(team,cumPts,final_Pos,season,tmYrGameOrder,position)


## look at 17th place which would need to exceed so that number +1
safeSpot <- modern %>% 
  filter(position==17) %>% 
  select(season,tmYrGameOrder,pts=cumPts)


stayUp <- modern %>% 
  left_join(safeSpot) %>% 
  select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
  mutate(diff=cumPts-pts-1) %>% 
  ## only teams that have survived relegation and been below 18th at some point
  filter(final_Pos<18&position>17)  %>% 
  ungroup()


a <- stayUp %>% 
  arrange(diff,desc(tmYrGameOrder)) # most is -11 whu after 29 games in 2006


### look at best by season and round

df <- a %>% 
  group_by(season) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(deficit=-diff) %>% 
  filter(season<"2015/16")

plot_ly(df, x = season, y = deficit, mode = "markers", hoverinfo = "text",color=team,
        text = paste(season,
                     "<br>",team,
                     "<br>Deficit:",deficit,
                     "<br>After:",tmYrGameOrder," games",
                     "<br>Finished:",final_Pos)) %>%
  layout(hovermode = "closest",
         title="Biggest points deficit overcome in 38 game PL season",
         xaxis=list(title="",tickfont=list(size=10),tickcolor="#000",tickangle=45),
         yaxis=list(title="Points in arrears of safety"
         )
  )

})

## when used as headline ( so need to incorporate as a special??)

output$hl_relegationOvercome <- renderPlotly({
  
  modern <-
    standings %>% 
    filter(season>"1994/95")
  
  modern %>% 
    select(team,cumPts,final_Pos,season,tmYrGameOrder,position)
  
  
  ## look at 17th place which would need to exceed so that number +1
  safeSpot <- modern %>% 
    filter(position==17) %>% 
    select(season,tmYrGameOrder,pts=cumPts)
  
  
  stayUp <- modern %>% 
    left_join(safeSpot) %>% 
    select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
    mutate(diff=cumPts-pts-1) %>% 
    ## only teams that have survived relegation and been below 18th at some point
    filter(final_Pos<18&position>17)  %>% 
    ungroup()
  
  
  a <- stayUp %>% 
    arrange(diff,desc(tmYrGameOrder)) # most is -11 whu after 29 games in 2006
  
  
  ### look at best by season and round
  
  df <- a %>% 
    group_by(season) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(deficit=-diff) %>% 
    filter(season<"2015/16")
  
  plot_ly(df, x = season, y = deficit, mode = "markers", hoverinfo = "text",color=team,
          text = paste(season,
                       "<br>",team,
                       "<br>Deficit:",deficit,
                       "<br>After:",tmYrGameOrder," games",
                       "<br>Finished:",final_Pos)) %>%
    layout(hovermode = "closest",
           title="Biggest points deficit overcome in 38 game PL season",
           xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
           yaxis=list(title="Points in arrears of safety"
           )
    )
  
})