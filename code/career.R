


careerData <- reactive({
  
  # if (is.null(input$playerA)) return()
  req(input$playerA)
#   #print(values$playerId)
   print(input$playerA)
   print("careerData")
  thePlayer <- input$playerA
  
 # thePlayer <- values$playerId
  
 # updateSelectizeInput(session, "playerA", choices = playerChoice, selected = input$playerA)
  
 # if (is.null(values$playerId)) return()
 # #print(values$playerId)
 ## #print(input$playerA)
 # thePlayer <- values$playerId
  
  dfChart <- playerGame %>% 
    filter(PLAYERID==thePlayer) %>% 
    select(date=gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID) %>% 
    mutate(points=Goals+Assists)
  
#  #print(glimpse(dfChart))
  
  
  dfTeamYear <- summary %>%
    filter(PLAYERID==thePlayer) %>%
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>% 
    ungroup() %>% 
    arrange(desc(Season)) %>% 
    select(-(c(name,LASTNAME,PLAYERID,born,left)))
  
  ##print(glimpse(dfTeamYear))
  
  dfTeam <- dfTeamYear %>% 
    group_by(Team) %>%
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  
 # #print(names(dfTeam))
  
  dfCareer <- dfTeamYear %>% 
   
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  dfCareer$Team <- "Career"
#  #print(names(dfCareer))
 
  
  info=list(dfTeamYear=dfTeamYear,dfTeam=dfTeam,dfCareer=dfCareer,dfChart=dfChart)
  return(info)
 
  
  
})

# may come back to if cannot sort colors
# observe({
#   
#  # if(is.null(careerData())) return()
#   #print("enter observe")
#   #print(glimpse(careerData()$dfChart))
#   
#   df <- careerData()$dfChart
#   
#   df <- cbind(df, id = seq_len(nrow(df)))
#   
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- df[df$id == x$id,c("date","Opponents","on","off","Goals","Assists") ]
#     paste0( names(row),": ",format(row), collapse = "<br />") 
#   }
#   
#   
#   df %>% 
#     ggvis(~plGameOrder, ~mins, key := ~id) %>%
#     layer_points(fill = ~Team, size = ~ points) %>% 
#     add_tooltip(all_values,"hover") %>% 
#     
#     ggvis::add_axis("y", title="Minutes Played", format='d') %>% # attempt to enforxe 0 , values=c(0,15,30,45,60,75,90)
#     ggvis::add_axis("x", title="Match Day Squad Game Order", format='d') %>% 
#     hide_legend("size") %>% 
#     bind_shiny("careerChart")
#   
# })

output$careerChart <- renderPlotly({
  
  careerData()$dfChart %>% 
    plot_ly(x=date,y=mins,mode="markers",color=Team,
            hoverinfo = "text",
            text = paste(date,"<br>v ",Opponents,"<br>on:",on,"<br>off:",off,"<br>Goals:",Goals,"<br>Assists:",Assists),
            marker=list(size=points*2+6)) %>% 
    layout(hovermode = "closest",
           title="",
           xaxis=list(title="Match day Squad Game order"),
           yaxis=list(title="Minutes played"
           )
    ) %>% 
    config(displayModeBar = F,showLink = F)
  
})



output$careerYear <- DT::renderDataTable({
  careerData()$dfTeamYear  %>%  
    select(-MP) %>% 
    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    })

output$career <- DT::renderDataTable({
  if (nrow((careerData()$dfTeam))>1) {
    df <- rbind(careerData()$dfTeam,careerData()$dfCareer)
  } else {
    df <- careerData()$dfTeam
  }
 # df <- rbind(careerData()$dfTeam,careerData()$dfCareer)
  df  %>%  
    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE)) %>%  
  formatStyle(
      'Team',
      target = 'row',
      backgroundColor = styleEqual(c('Career'), c('lightgreen'))
     )
})


output$pointsByYearChart <- renderPlotly({
  #if (is.null(input$playerA)) return()
  
  req(input$playerA)
  
  df <-playerGame %>% 
    filter(PLAYERID==input$playerA) %>% 
    group_by(season,PLAYERID,name) %>% 
    select(Gls,Assists,mins) %>% 
    summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
    filter(Points!=0) %>% 
    mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
    ungroup() 
  
#   write_csv(df,"tauproblem.csv")
#   
#   df %>% 
#     tauchart() %>% 
#     tau_point("season","Ppm", size="2") %>% 
#     # tau_line("season","Ppm") %>% 
#     tau_tooltip(c("Goals","Assists")) %>% 
#     tau_guide_x(label="") %>% 
#     tau_guide_y(label ='Points per 90 mins')
  
  plot_ly(df, x = season, y = Ppm, mode = "markers", hoverinfo = "text",
          marker=list(size=Mins/10, sizemode="area"),
          text = paste(
            "<br>Goals: ",Goals,
            "<br>Assists: ",Assists,
            "<br>Points: ",Points,
            "<br>Minutes: ",Mins
          )) %>%
    layout(hovermode = "closest",
           title="Points per 90 mins by Season",
           xaxis=list(title=""),
           yaxis=list(title="Points per 90 mins",rangemode="tozero"
           )
    )
  
})

