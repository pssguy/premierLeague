output$topScorers <- DT::renderDataTable({
  print("topScorers")
  print(input$season_1)
  print(input$teams_1)
  
  if (!is.null(input$season_1)) {
    year <- input$season_1
  } else {
    year <- "2014/15"
  }
  if (!is.null(input$teams_1)) {
    team <- input$teams_1
  } else {
    team <- "All Teams"
  }
  
  print("checks 2")
  print(team)
  print(year)
  print(str(playerGame))
  if (team=="All Teams"){
    if(year != "All Seasons") {
      playerGame %>%
        filter(tmYrGameOrder<=input$games_1) %>%
        group_by(name,season,TEAMNAME,PLAYERID) %>%
        summarise(goals=sum(Gls)) %>%
        filter(season==year) %>%
        group_by(TEAMNAME) %>%
        arrange(desc(goals)) %>%
        slice(1) %>%
        select(name,team = TEAMNAME,goals)  -> df
      
    } else {
      #     updateSelectInput(session, "season_1",
      #                       label = "",
      #                       choices = seasonChoice_2,
      #                       selected = "All Seasons"
      #     )
      seasons <- playerGame %>%
        
        select(name,season,TEAMNAME,PLAYERID) %>%
        do(unique(.)) %>%
        group_by(name,TEAMNAME,PLAYERID) %>%
        summarise(yrs=n())
      
      playerGame %>%
        filter(tmYrGameOrder<=input$games_1) %>%
        group_by(name,TEAMNAME,PLAYERID) %>%
        summarise(goals=sum(Gls)) %>%
        
        group_by(TEAMNAME) %>%
        arrange(desc(goals)) %>%
        slice(1) %>%
        left_join(seasons) %>%
        arrange(desc(seasons)) %>%
        select(team=TEAMNAME,name,goals,seasons=yrs)  -> df
      #     playerGame %>%
      #       group_by(name,TEAMNAME,PLAYERID) %>%
      #       summarise(goals=sum(GOALS)) %>%
      #       group_by(TEAMNAME) %>%
      #       arrange(desc(goals)) %>%
      #       slice(1) %>%
      #       select(team = TEAMNAME,name,goals)
    }
  } else {
    
    playerGame %>%
      filter(tmYrGameOrder<=input$games_1) %>%
      group_by(name,season,TEAMNAME) %>%
      summarise(goals=sum(Gls)) %>%
      filter(TEAMNAME==team) %>%
      group_by(season) %>%
      arrange(desc(goals)) %>%
      slice(1) %>%
      select(season,name,goals) -> df
  }
  
  DT::datatable(df,options = list(paging = FALSE, searching = FALSE, width = 200, 
                                   scrollY=300))
  
  
})

