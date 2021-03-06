

## could add some formatting by cell but would be quite long

output$teamYear <- DT::renderDataTable({
  req(input$teamYears)
  req(input$teamA)
  print("enter teamYear")
 print(paste0(input$teamYears," teamYear"))
 print(input$teamA)
  
  #if (is.null(input$teamYears)) return()
  #req(input$teamYears)
  
  theTeam <- input$teamA
  theYear <- input$teamYears
  
  if(input$seasons=="Single") {
 if (input$withClub=="All") {
   
   tbl <-summary %>%
     ungroup() %>% 
     filter(TEAMNAME==theTeam&season==theYear) %>%
     
     mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear,PPG=ifelse(Points==0,0,round(90*Points/mins,2))) %>%
     select(Player=name,Pos=pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,PPG,Y,R)
 
   tbl %>% 
       DT::datatable(selection='single',rownames = FALSE, class='compact stripe hover row-border',
                   options= list(
                             paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
                             order = list(list(8, 'desc')) # initially set to minutes
                                 )
                  )
   

   
 } else {
   tbl <-summary %>%
     ungroup() %>% 
     filter(TEAMNAME==theTeam&season==theYear&is.na(left)) %>%
     
     mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear,PPG=ifelse(Points==0,0,round(90*Points/mins,2))) %>%
     select(Player=name,Pos=pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,PPG,Y,R)
   
   tbl %>% 
     DT::datatable(selection='single',rownames = FALSE, class='compact stripe hover row-border',
                   options= list(
                     paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
                     order = list(list(8, 'desc')) # initially set to minutes
                   )
     )
   
#    data.frame(summary %>%
#                 filter(TEAMNAME==theTeam&season==theYear&is.na(left)) %>%
#                 
#                 mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
#                 select(PLAYERID,Player=name,Pos=pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
#    
#    tbl <- tbl[,-(1:5)] %>% 
#      arrange(desc(Mins))
#    DT::datatable(tbl,class='compact display',rownames=TRUE,options = list(paging = FALSE, searching = FALSE,
#                                                    #order = list(list(9, 'desc')),
#                                                    columnDefs= list(list(visible=FALSE,targets=list(1)),list(className = 'dt-center', targets = 3),list(width="20%",columnDefs.targets= list(1)))))
   }
  } else {
    if (input$withClub=="All") {
      
      tbl <-summary %>%
        ungroup() %>% 
        filter(TEAMNAME==theTeam) %>%
        
        mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists,PPG=ifelse(Points==0,0,round(90*Points/mins,2))) %>%
        group_by(PLAYERID,name,pos) %>%
        select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,PPG,Y,R,OG,MP) %>%
        summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
                  Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
      
      tbl %>% 
        ungroup() %>%
        select(-PLAYERID) %>% 
        DT::datatable(selection='single',rownames = FALSE, class='compact stripe hover row-border',
                      options= list(
                        paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
                        order = list(list(7, 'desc')) # initially set to minutes
                      )
        )
      
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
#                    group_by(PLAYERID,name,pos) %>%
#                    select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
#                    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
#                              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
#                     
#   tbl    %>% 
#         arrange(desc(Mins)) %>% 
#       DT::datatable('compact display',rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       #order = list(list(8, 'desc')),
#                                                       columnDefs= list(list(visible=FALSE,targets=list(0)),list(className = 'dt-center', targets = 2),list(width="20%",columnDefs.targets= list(1)))))
#       
    } else {
      
      
      
      tbl <-summary %>%
        ungroup() %>% 
        filter(TEAMNAME==theTeam&is.na(left)) %>%
        
        mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists,PPG=ifelse(Points==0,0,round(90*Points/mins,2))) %>%
        group_by(PLAYERID,name,pos) %>%
        select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,PPG,Y,R,OG,MP) %>%
        summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
                  Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
      
      tbl %>% 
        ungroup() %>%
        select(-PLAYERID) %>% 
        DT::datatable(selection='single',rownames = FALSE, class='compact stripe hover row-border',
                      options= list(
                        paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
                        order = list(list(7, 'desc')) # initially set to minutes
                      )
        )
      
#       data.frame(summary %>%
#                    filter(TEAMNAME==theTeam&is.na(left)) %>%
#                    
#                    mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
#                    group_by(PLAYERID,name,pos) %>%
#                    select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
#                    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
#                              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
#    tbl   %>% 
#         arrange(desc(Mins)) %>% 
#       DT::datatable('compact display order-column',rownames=FALSE,options = list(paging = FALSE, searching = FALSE,
#                                                       #order = list(list(8, 'desc')),
 #                                                     columnDefs= list(list(visible=FALSE,targets=list(0)),list(className = 'dt-center', targets = 2),list(width="20%",columnDefs.targets= list(1)))))
    } 
   
    
 }

})  
  
