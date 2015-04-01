output$teamYear <- DT::renderDataTable({
  print("enter teamYear")
  if (!is.null(input$team_3)) {
    theTeam <- input$team_3
  } else {
    theTeam <-"Arsenal"
  }
  if (!is.null(input$season_3)) {
    theYear <- input$season_3
  } else {
    theYear <-"2014/15"
  } 
  
  if(input$seasons=="Single") {
 if (input$withClub=="All") {
   
  data.frame(summary %>%
    filter(TEAMNAME==theTeam&season==theYear) %>%
    
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(PLAYERID,Player=name,Age=age,Apps=apps,St,On,Off,Bench,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl

  tbl <- tbl[,-(1:4)]
 } else {
   data.frame(summary %>%
                filter(TEAMNAME==theTeam&season==theYear&is.na(left)) %>%
                
                mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
                select(PLAYERID,Player=name,Age=age,Apps=apps,St,On,Off,Bench,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
   
   tbl <- tbl[,-(1:4)]
   }
  } else {
    if (input$withClub=="All") {
      
      data.frame(summary %>%
                   filter(TEAMNAME==theTeam) %>%
                   
                   mutate(Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
                   group_by(PLAYERID,name) %>%
                   select(name,Apps,St,On,Off,Bench,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
                   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Goals=sum(Goals),
                             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
                    
      
     # tbl <- tbl[,-(1)]
    } else {
      data.frame(summary %>%
                   filter(TEAMNAME==theTeam&is.na(left)) %>%
                   
                   mutate(Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
                   group_by(PLAYERID,name) %>%
                   select(name,Apps,St,On,Off,Bench,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
                   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Goals=sum(Goals),
                             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
    } 
 }
 # print(str(tbl))
  
  DT::datatable(tbl,rownames=FALSE,options=list(paging = FALSE, searching = FALSE,
                                  columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= list(1)))))
  
})  
  
# }
# ,options= list(paging = FALSE, searching = FALSE,
#                #    columnDefs=list(width="20%",columnDefs.targets= 0), # looks good at least once
#                order=list(c(2,'desc'),c(3,'desc'),c(6,'desc'),c(1,'asc')),
#                
#                columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= 0),list(className="rt",targets=list(2,3,4,5,6,7,8,9,10,11,12,13,14,15))))
# ,
# callback="function(table) {
# table.on('click.dt', 'tr', function() {
# $(this).closest('table').find('.selected').each(function(){
# $(this).removeClass('selected');
# });
# $(this).toggleClass('selected');
# Shiny.onInputChange('teamYearRow',
# table.rows('.selected').indexes().toArray());
# });
# }"
# )

# old version reg datatable
# works but probs with width and setting nicer headings
# output$teamYear <- renderDataTable({
#   print("enter table")
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
#   #tbl <- careerData()$summary %>%
#   tbl <- summary %>%
#     filter(TEAMNAME==theTeam&season==theYear) %>%
#     
#     mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
#     select(PLAYERID,Player=name,Age=age,Apps=apps,St,On,Off,Bench,Goals=Gls,Pens,Assists,Points,Y,R,OG,MP)
#   #str(tbl)
#   tbl <- data.frame(tbl)
#   #   print(tbl$apps)
#   #   maxApps <- max(tbl$apps)
#   #   print(maxApps)
#   tbl <- tbl[,-(1:4)]
#   tbl
# }
# ,options= list(paging = FALSE, searching = FALSE,
#                #    columnDefs=list(width="20%",columnDefs.targets= 0), # looks good at least once
#                order=list(c(2,'desc'),c(3,'desc'),c(6,'desc'),c(1,'asc')),
#                
#                columnDefs= list(list(visible=FALSE,targets=list(0)),list(width="20%",columnDefs.targets= 0),list(className="rt",targets=list(2,3,4,5,6,7,8,9,10,11,12,13,14,15))))
# ,
# callback="function(table) {
# table.on('click.dt', 'tr', function() {
# $(this).closest('table').find('.selected').each(function(){
# $(this).removeClass('selected');
# });
# $(this).toggleClass('selected');
# Shiny.onInputChange('teamYearRow',
# table.rows('.selected').indexes().toArray());
# });
# }"
# )
# 
