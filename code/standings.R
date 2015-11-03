output$st_round <- DT::renderDataTable({

#   year <- "2014/15"
#   games <- 38
  
  if(is.null(input$gamesA)) return()
  
  
  df <-  data.frame(standings %>%
                      filter(season==input$seasonA&tmYrGameOrder==input$gamesA) %>%
                      select(Pos=position,Team=team,Pl=tmYrGameOrder,Pts=cumPts,GD=cumGD,GF=cumGF,GA=cumGA,Final=final_Pos)) %>% 
    ungroup() %>% 
    arrange(Pos) %>% 
    select(-season) %>% 
    DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                  options= list(paging = FALSE, searching = FALSE,info=FALSE))
                                                  

}
)


output$st_position <- DT::renderDataTable({
  
  if(is.null(input$gamesB)) return()

  standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$gamesB&position==input$positionA) %>%
    select(Season=season,Team=team,Pts=cumPts,GD=cumGD,GF=cumGF,Final=final_Pos) %>% 
    arrange(desc(Season)) %>% 
DT::datatable(class='compact stripe hover row-border',
              rownames=FALSE,
              options= list(searching = FALSE, info=FALSE))
                               
                               
  
}
)


observe({
  
  if(is.null(input$gamesB)) return()
  

df <-  standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$gamesB&position==input$positionA)
  
df %>% 
  ggvis(~final_Pos) %>% 
  layer_histograms(fill:="lightblue") %>% 
  add_axis("x", title="Final League Position", format='d') %>% 
  add_axis("y", title="Seasons", format='d') %>% 
  bind_shiny("st_position_chart")


  
}
)

output$st_team <- DT::renderDataTable({
  if(is.null(input$gamesC)) return()
  
#   if (!is.null(input$games_4a)) {
#     games <- input$games_4a
#   } else {
#     games <- currentRound 
#   }
#   if (!is.null(input$team_4a)) {
#     theTeam <- input$team_4a
#   } else {
#     theTeam <- "Arsenal"
#   }
  
  
#   df <-  data.frame(standings %>%
#                       filter(tmYrGameOrder==games&team==theTeam) %>%
#                       select(Season=season,Pos=position,Pts=cumPts,GD=cumGD,GF=cumGF,Final=final_Pos)) 
#   
#   DT::datatable(df,options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                  
#                                  order=list(c(0,'desc'))))
  
  
  standings %>% 
    ungroup() %>% 
    filter(tmYrGameOrder==input$gamesC&team==input$teamA) %>%
    select(Season=season,Pos=position,Pts=cumPts,GD=cumGD,GF=cumGF,Final=final_Pos) %>% 
    arrange(desc(Season)) %>% 
    DT::datatable(class='compact stripe hover row-border',
                  rownames=FALSE,
                  options= list(searching = FALSE, info=FALSE))
                                 
}
)

output$currentForm <- DT::renderDataTable({
  print("enter current form")
  
  
  if (!is.null(input$games_5)) {
    games <- input$games_5
  } else {
    games <- 6
  }
  
  current <- standings %>%
    filter(season==currentYear&tmYrGameOrder==currentRound) %>%
    select(team,Pl=tmYrGameOrder,Pts=cumPts,GD=cumGD,GF=cumGF)
  
  past <- standings %>%
    filter(season==currentYear&tmYrGameOrder==(currentRound-games)) %>%
    select(team,oldPl=tmYrGameOrder,oldPts=cumPts,oldGD=cumGD,oldGF=cumGF) %>%
    inner_join(current) %>%
    mutate(Pl=Pl-oldPl,Pts=Pts-oldPts,GD=GD-oldGD,GF=GF-oldGF) %>%
    select(team,Pl,Pts,GD,GF) %>%
    ungroup() %>%
    arrange(desc(Pts),desc(GD),desc(GF),team) %>%
    mutate(position=row_number()) %>%
    select(7,2:6)
  DT::datatable(past,options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))
})



  output$st_dateNow <- DT::renderDataTable({
  
    if(is.null(input$dateA)) return()
    
  theDate <- input$dateA
  
  if (month(theDate)<6) {
    yr <-paste(year(theDate)-1,str_sub(year(theDate),3,4),sep="/")
  }else {
    yr <-paste(year(theDate),as.integer(str_sub(year(theDate),3,4))+1,sep="/")
  }

  table <-standings %>%
    filter(season==yr&gameDate<=theDate)  %>%  #478 loks good
    group_by(team) %>%
    transmute(Pl=n(),Pts=sum(points),GA=sum(GA),GF=sum(GF)) %>%
    mutate(GD=GF-GA) %>% 
    unique(.) %>% 
    ungroup() %>% 
    arrange(desc(Pts),desc(GD),desc(GF),team) %>% 
    select(Team=team,Pl,GD,Pts) %>% 
  DT::datatable(options= list(searching = FALSE, info=FALSE))
                                   
}
)
  
  
  
  output$st_dateLater <- DT::renderDataTable({
    
    if(is.null(input$dateA)) return()
    
    theDate <- input$dateA
    
    if (month(theDate)<6) {
      yr <-paste(year(theDate)-1,str_sub(year(theDate),3,4),sep="/")
    }else {
      yr <-paste(year(theDate),as.integer(str_sub(year(theDate),3,4))+1,sep="/")
    }
    
    table <-standings %>%
      filter(season==yr&gameDate>theDate)  %>%  #478 loks good
      group_by(team) %>%
      transmute(Pl=n(),Pts=sum(points),GA=sum(GA),GF=sum(GF)) %>%
      mutate(GD=GF-GA) %>% 
      unique(.) %>% 
      ungroup() %>% 
      arrange(desc(Pts),desc(GD),desc(GF),team) %>% 
      select(Team=team,Pl,GD,Pts) %>% 
      DT::datatable(options= list(searching = FALSE, info=FALSE))
    
  }
  )
  
  
  output$st_dateSeason <- DT::renderDataTable({
    
    if(is.null(input$dateA)) return()
    
    theDate <- input$dateA
    
    if (month(theDate)<6) {
      yr <-paste(year(theDate)-1,str_sub(year(theDate),3,4),sep="/")
    }else {
      yr <-paste(year(theDate),as.integer(str_sub(year(theDate),3,4))+1,sep="/")
    }
    
    table <-standings %>%
      filter(season==yr)  %>%  #478 loks good
      group_by(team) %>%
      transmute(Pl=n(),Pts=sum(points),GA=sum(GA),GF=sum(GF)) %>%
      mutate(GD=GF-GA) %>% 
      unique(.) %>% 
      ungroup() %>% 
      arrange(desc(Pts),desc(GD),desc(GF),team) %>% 
      select(Team=team,Pl,GD,Pts) %>% 
      DT::datatable(options= list(searching = FALSE, info=FALSE))
    
  }
  )

# output$datetableRest <- DT::renderDataTable({
#   
#   
#   if (!is.null(input$date_1)) {
#     theDate <- input$date_1
#   } else {
#     theDate - Sys.Date()
#   }
#   
#   if (month(theDate)<6) {
#     yr <-paste(year(theDate)-1,str_sub(year(theDate),3,4),sep="/")
#   }else {
#     yr <-paste(year(theDate),as.integer(str_sub(year(theDate),3,4))+1,sep="/")
#   }
#   table <-standings %>%
#     filter(season==yr&gameDate>theDate)  %>%  
#     group_by(team) %>%
#     transmute(Pl=n(),Pts=sum(points),GA=sum(GA),GF=sum(GF)) %>%
#     mutate(GD=GF-GA) 
#   table <- unique(table)
#   
#   table <- data.frame(table)
#   table  %<>% 
#     arrange(desc(Pts),desc(GD),desc(GF),team) %>%
#     mutate(position=row_number()) %>%
#     select(Pos=position,Team=team,Pl,Pts,GD)
#   DT::datatable(table,options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                     order=list(c(3,'desc'),c(4,'desc'),c(1,'asc'))))
# } 
# )

output$datetableYear <- DT::renderDataTable({
  
  
  if (!is.null(input$date_1)) {
    theDate <- input$date_1
  } else {
    theDate - Sys.Date()
  }
  
  if (month(theDate)<6) {
    yr <-paste(year(theDate)-1,str_sub(year(theDate),3,4),sep="/")
  }else {
    yr <-paste(year(theDate),as.integer(str_sub(year(theDate),3,4))+1,sep="/")
  }
  table <-standings %>%
    filter(season==yr)  %>%  
    group_by(team) %>%
    transmute(Pl=n(),Pts=sum(points),GA=sum(GA),GF=sum(GF)) %>%
    mutate(GD=GF-GA) 
  table <- unique(table)
  
  table <- data.frame(table)
table <-  table  %>% 
    arrange(desc(Pts),desc(GD),desc(GF),team) %>%
    mutate(position=row_number()) %>%
    select(Pos=position,Team=team,Pl,Pts,GD)
  
  DT::datatable(table,options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                    order=list(c(3,'desc'),c(4,'desc'),c(1,'asc')))) 
}
)
