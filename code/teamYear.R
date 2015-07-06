

## could add some formatting by cell but would be quite long

output$teamYear <- DT::renderDataTable({
 # print("enter teamYear")
 
  
  if (is.null(input$teamYears)) return()
  
  theTeam <- input$teamA
  theYear <- input$teamYears
  
  if(input$seasons=="Single") {
 if (input$withClub=="All") {
   
 
   
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

})  
  
