# output$teamYear <- DT::renderDataTable({
#   print("enter teamYear")
#   if (!is.null(input$team_3)) {
#     theTeam <- input$team_3
#   } else {
#     theTeam <-"Arsenal"
#   }
#   if (!is.null(input$season_3)) {
#     theYear <- input$season_3
#   } else {
#     theYear <-"2014/15"
#   } 
#   
#   if(input$season_3=="All Seasons") {
#     if (input$withClub=="All") {
#       
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam&season==theYear) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
#                    select(PLAYERID,Player=name,pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
#       
#       tbl <- tbl[,-(1:5)]
#       DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       order = list(list(9, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
#     } else {
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam&season==theYear&is.na(left)) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
#                    select(PLAYERID,Player=name,pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
#       
#       tbl <- tbl[,-(1:5)]
#       DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       order = list(list(9, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
#     }
#   } else {
#     if (input$withClub=="All") {
#       
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
#                    group_by(PLAYERID,name,pos) %>%
#                    select(name,pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
#                    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
#                              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
#       
#       
#       DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
#       
#     } else {
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam&is.na(left)) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
#                    group_by(PLAYERID,name,pos) %>%
#                    select(name,pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
#                    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
#                              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
#       
#       DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
#     } 
#     
#     
#   }
#   #   DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#   #     order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
#   
# })  

## first need to choose years for specific team - poss add to sidebar

# output$teamYear_ui <- renderUI({
#   print(input$teamA)
#   yrs <- sort(unique(tmYrs[tmYrs$team==input$teamA,]$season),decreasing = FALSE) # thinka bout + inc all
# 
#   selectInput("teamYears","Season",yrs, selected=yrs[length(yrs)])
# })


# This works but try any simplify by ptting All in seasons selection
output$teamYear <- DT::renderDataTable({
  print("enter teamYear")
#   if (!is.null(input$team_3)) {
#     theTeam <- input$team_3
#   } else {
#     theTeam <-"Arsenal"
#   }
#   if (!is.null(input$season_3)) {
#     theYear <- input$season_3
#   } else {
#     theYear <-"2014/15"
#   } 
  
  theTeam <- input$teamA
  theYear <- input$teamYears
  
  if(input$seasons=="Single") {
 if (input$withClub=="All") {
   
 #  print(glimpse(summary))
   
  data.frame(summary %>%
    filter(TEAMNAME==theTeam&season==theYear) %>%
    
    mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
    select(PLAYERID,Player=name,pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl

  tbl <- tbl[,-(1:5)]
  DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
                                                  order = list(list(9, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
 } else {
   data.frame(summary %>%
                filter(TEAMNAME==theTeam&season==theYear&is.na(left)) %>%
                
                mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
                select(PLAYERID,Player=name,pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
   
   tbl <- tbl[,-(1:5)]
   DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
                                                   order = list(list(9, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
   }
  } else {
    if (input$withClub=="All") {
      
      data.frame(summary %>%
                   filter(TEAMNAME==theTeam) %>%
                   
                   mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
                   group_by(PLAYERID,name,pos) %>%
                   select(name,pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
                   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
                             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
                    
      
      DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
                                                      order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
      
    } else {
      data.frame(summary %>%
                   filter(TEAMNAME==theTeam&is.na(left)) %>%
                   
                   mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
                   group_by(PLAYERID,name,pos) %>%
                   select(name,pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
                   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
                             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
      
      DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
                                                      order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
    } 
   
    
 }
#   DT::datatable(tbl,rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#     order = list(list(8, 'desc')),columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
  
})  
  
