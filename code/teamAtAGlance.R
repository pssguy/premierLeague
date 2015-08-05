## ata glance like cricket etc

## look at a heardcoded version first

## summary stats/photo/birtplace maybe add transfers in?


teamData <- eventReactive(input$teamA,{
  
  
  test <-standings %>% 
    filter(team==input$teamA) %>% 
    group_by(season) %>% 
    filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
    ungroup()
  
  summary <- test %>% 
    summarize(years=n(),bestPos=min(position),wortsPos=max(position),maxPoints=max(cumPts),minPoints=min(cumPts))
  
  
  mostGames <- playerGame %>% 
    filter(TEAMNAME==input$teamA) %>% 
    group_by(PLAYERID,name) %>% 
    filter((START+subOn)>0) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
  
  
  mostGoals <- playerGame %>% 
    filter(TEAMNAME==input$teamA&name!=" Own Goal") %>% 
    group_by(PLAYERID,name) %>% 
    summarize(sumGoals=sum(Gls)) %>% 
    ungroup() %>% 
    filter(sumGoals>0) %>% 
    arrange(desc(sumGoals))
  
  mostAssists <- playerGame %>% 
    filter(TEAMNAME==input$teamA) %>% 
    
    group_by(PLAYERID,name) %>% 
    summarize(sumAssists=sum(Assists)) %>% 
     ungroup() %>% 
    filter(sumAssists>0) %>% 
    arrange(desc(sumAssists))
  
  
  mostCards <- playerGame %>% 
    filter(TEAMNAME==input$teamA&CARD>"0") %>% 
    group_by(PLAYERID,name) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
  
  info=list(summary=summary,test=test,mostGames=mostGames,
            mostGoals=mostGoals,mostAssists=mostAssists, mostCards=mostCards)
  return(info)
  
})

output$mostGames <- DT::renderDataTable({
  if(is.null(teamData())) return()
  teamData()$mostGames %>% 
    select(Player=name,Apps=n) %>% 
    DT::datatable(selection='single',class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})

output$mostGoals <- DT::renderDataTable({
  if(is.null(teamData())) return()
  teamData()$mostGoals %>% 
    select(Player=name,Goals=sumGoals) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})

output$mostAssists <- DT::renderDataTable({
  if(is.null(teamData())) return()
  teamData()$mostAssists %>% 
    select(Player=name,Assists=sumAssists) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})


output$mostCards <- DT::renderDataTable({
  if(is.null(teamData())) return()
  teamData()$mostCards %>% 
    select(Player=name,Cards=n) %>% 
    DT::datatable(class='compact stripe hover row-border',options= list(
      pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
  
})


# output$appsBox <- renderInfoBox({
#   infoBox(
#     "Appearances",data()$career$showApps, icon = icon("futbol-o"), #user-times
#     color = "light-blue", subtitle = " Tot - Max"
#   )
# })

output$seasonsBox <- renderInfoBox({
  print("enterseasonsbox")
  seasons <- as.character(teamData()$summary$years)
  infoBox(
    "Seasons",seasons, icon = icon("futbol-o"), #user-times
    color = "light-blue"
  )
})


output$glanceTest <- renderText({
  print(teamData()$summary$years)
  input$teamZ
})

observeEvent(teamData(),{
  
  teamData()$test %>% 
    ggvis(~final_Pos) %>% 
    set_options(width=220,height=220) %>% 
    add_axis("y", title="Seasons", format='d') %>% 
    add_axis("x", title="Position", format='d') %>% 
    bind_shiny('seasonsHist')
  
})

