diffTeams <-standings %>% 
  ungroup() %>% 
  filter(position==1) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  ungroup() %>% 
  select(season) %>% 
  group_by(season) %>% 
  tally() %>% 
  arrange(desc(n))  %>% 
 # rename(teams=n) for some reason started causing error ##Error in rename(., teams = n) : unused argument (teams = n)
  select(season,teams=n)

topChanges <- standings %>% 
  ungroup() %>% 
  filter(position==1) %>% 
  select(season,team,tmYrGameOrder) %>% 
  arrange(tmYrGameOrder) %>% 
  group_by(season) %>% 
  mutate(lastteam=lag(team)) %>% 
  filter(tmYrGameOrder!=1) %>% 
  mutate(change=ifelse(team==lastteam,0,1)) %>% 
  group_by(season) %>% 
  dplyr::summarise(changes=sum(change)) %>% 
  inner_join(diffTeams)

output$st_topChanges <- renderPlotly({
  

  topChanges %>% 
    plot_ly() %>% 
  add_bars( x = ~season, y = ~changes,  marker=list(color=~teams), showlegend=TRUE, key=~season) %>%
    layout(hovermode = "closest",
           xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
           yaxis=list(title="Changes in Team Leading Table"),
           title=" Click bar for details", titlefont=list(size=12)
    )
})
  ## crosstalk to get to table of those results
#  cv <- crosstalk::ClientValue$new("plotly_click", group = "A")
  
  
  
  output$st_topChangesWeekly <- renderPlotly({
    
   # s <- cv$get() 
    s <- event_data("plotly_click")
    
    if (length(s)==0) {
      return()
    } else {
      
      yr <- s[["key"]]
      
      numeroUnos <- standings %>% 
        ungroup() %>% 
        filter(season==yr&position==1) %>% 
        select(team) %>% 
        unique() 
      
      numeroUnos <- numeroUnos$team
      
      df <-standings %>% 
        ungroup() %>% 
        filter(season==yr&team %in% numeroUnos)
      
      glimpse(df)
      
      
      theTitle<-paste0(yr, " (click Team-name
 in legend to show/no show)")
      
      df %>% 
        plot_ly() %>% 
      
      add_markers(x=~tmYrGameOrder, y=~position,  color=~team,
              hoverinfo = "text",
              text = ~paste(team,
                           #"<br>",Date,
                           "<br> v ",OppTeam," "," ",GF,"-",GA,
                           "<br> Position:",position,
                           "<br> Played:",tmYrGameOrder,
                           "<br> Points:",cumPts))  %>%
        #add_lines(data=df,x=~tmYrGameOrder, y=~position, color=~team) %>% #NB want to come back to
        layout(hovermode = "closest",
               xaxis=list(title="Games Played"),
               yaxis=list(title="League Position", autorange="reversed"),
               
               title=theTitle, titlefont=list(size=12)
        )
    }
      
    
})