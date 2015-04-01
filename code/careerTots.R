# prob should put this and career into a reactive
output$careerTots <- DT::renderDataTable({
  print("enter table")
  if (!is.null(input$player)) {
    thePlayer <- input$player
  } else {
    thePlayer=="ROONEYX"
  }
  
  
  
  # break into team summary? - think this can be done by datata
  
  temp <-  data.frame(summary %>%
                        filter(PLAYERID==thePlayer)) 
  teams <- unique(temp$TEAMNAME)
  
  
  teamTbl <- summary %>%
    filter(PLAYERID==thePlayer) %>%
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(team = TEAMNAME,Apps=apps,St,On,Off,Bench,Goals=Gls,Assists,Points,Pens,Y,R,OG,MP) %>%
    group_by(team,PLAYERID,name) %>%
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Goals=sum(Goals),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP)) 
  
  if (length(teams)>1) {
    career <-   teamTbl %>%
      group_by(PLAYERID,name) %>%
      summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Goals=sum(Goals),
                Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP)) 
    career$team <- "All Teams"
    teamTbl <- rbind(teamTbl,career)
  }
  
  
  teamTbl <- data.frame(teamTbl) %>%
    select(-PLAYERID,-name)
  DT::datatable(teamTbl,options=list(paging = FALSE, searching = FALSE,info = FALSE))
  
  
  
  
}
)



