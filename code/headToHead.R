# Head to head tab;e abnd graph

## add asparkline

# output$hthTable <- DT::renderDataTable({
#   
#   # calc wins ? could put in global
#   W <- hth %>%
#     group_by(team,OppTeam) %>%
#     filter(res=="Win") %>%
#     summarise(W=n())
#   D <- hth %>%
#     group_by(team,OppTeam) %>%
#     filter(res=="Draw") %>%
#     summarise(D=n())
#   L <- hth %>%
#     group_by(team,OppTeam) %>%
#     filter(res=="Loss") %>%
#     summarise(L=n())
#   
#   table <- data.frame(hth %>%
#                         group_by(team,OppTeam) %>%
#                         summarise(GF=sum(GF),GA=sum(GA),Pts=sum(points),Pl=n()) %>%
#                         mutate(GD=GF-GA,ppg=round(Pts/Pl,2)) %>%
#                         left_join(W) %>%
#                         left_join(D)      %>%
#                         left_join(L))           
#   table[is.na(table)]      <- 0    
#   tbl <-table %>%
#     arrange(desc(ppg),desc(Pl))  %>%
#     filter(team==input$team_3) %>%
#     select(Opponents=OppTeam,Pl,W,D,L,Pts,GF,GA,GD,ppg) 
#   tbl$team <-NULL
#   tbl %>% 
#     DT::datatable(rownames = checkboxRows(., checked=c(1)), escape = -1,options= list(paging = FALSE, searching = FALSE, info=FALSE))
#  # DT::datatable(tbl,rownames = checkboxRows(., checked=c(1)), escape = -1,options= list(paging = FALSE, searching = FALSE, info=FALSE))
#  })

# need to split into reactive as tabe is needed for selection (should change based on row clicked on really)
info <- reactive({
  print("enter htoh reactive")
  #if (is.null(input$teamA)) return()
  req(input$teamA)
  # print("inputteamA")
  # print(input$teamA)
  
  # calc wins ? could put in global
  W <- hth %>%
    group_by(team,OppTeam) %>%
    filter(res=="Win") %>%
    summarise(W=n())
  D <- hth %>%
    group_by(team,OppTeam) %>%
    filter(res=="Draw") %>%
    summarise(D=n())
  L <- hth %>%
    group_by(team,OppTeam) %>%
    filter(res=="Loss") %>%
    summarise(L=n())
  
  table <- data.frame(hth %>%
                        group_by(team,OppTeam) %>%
                        summarise(GF=sum(GF),GA=sum(GA),Pts=sum(points),Pl=n()) %>%
                        mutate(GD=GF-GA,ppg=round(Pts/Pl,2)) %>%
                        left_join(W) %>%
                        left_join(D)      %>%
                        left_join(L)) 
  
 # print(glimpse(table))
  
 table[is.na(table)]      <- 0    
#  print(glimpse(table))
#  
# write_csv(table,"table_test.csv") 
  tbl <-table %>%
    arrange(desc(ppg),desc(Pl))  %>%
    filter(team==input$teamA) %>%
    select(Opponents=OppTeam,Pl,W,D,L,Pts,GF,GA,GD,ppg) 
  
#   print("glimpse tbl")
#   print(glimpse(tbl))
  
  tbl$team <-NULL
  info=list(tbl=tbl)
  return(info)
})


output$hthTable <- DT::renderDataTable({
 # if(is.null(info()$tbl)) return()
 # print("enter hthTable")
  
  print(glimpse(info()$tbl))
  

 print(str(info()$tbl))
  info()$tbl %>% 
    select(-Pts) %>% 
    DT::datatable(rownames=TRUE,selection='single',options= list(paging = TRUE, searching = TRUE,info=FALSE))
  
})


output$hthFixtures <- DT::renderDataTable({
  print("enter hth fixtures")
  if(is.null(input$hthTable_rows_selected)) return()
  
  s = as.integer(input$hthTable_rows_selected)
#   print(input$hthTable_selected) #1 so it is row
#   
#   s = input$hthTable_selected
  print(s)
  print(glimpse(info()$tbl))
  team <- info()$tbl[s,]$Opponents
  print(team)
  
  tm1 <-  teamGames %>% 
    filter(TEAMNAME==input$teamA) %>% 
    select(MATCHID,venue,TEAMNAME,GF=GOALS,gameDate,season)
  
  tm2 <-  teamGames %>% 
    filter(TEAMNAME==team) %>% 
    select(MATCHID,GA=GOALS)
  
  tm1 %>% 
    inner_join(tm2,by=c("MATCHID")) %>% 
    ungroup() %>% 
    arrange(desc(gameDate)) %>% 
    select(season,date=gameDate,venue,GF,GA) -> fixtures
  
  print(glimpse(fixtures))
  
  fixtures %>% 
    DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
                                                                 paging = TRUE, searching = TRUE,info=FALSE))
})


## look tams tebotho add lineups for 

# observe({
#   
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- plot[plot$id == x$id,"OppTeam" ]
#     paste0(names(row), format(row), collapse = "<br />")
#   }
#   
#   plot <- hth %>%
#     filter(team==input$team_3)
#   
#   plot <-data.frame(plot %>%
#                       group_by(team,OppTeam) %>%
#                       summarise(GF=sum(GF),GA=sum(GA),Pts=sum(points),Pl=n()) %>%
#                       mutate(GD=GF-GA,ppg=round(Pts/Pl,2)))
#   plot$ppg <- jitter(plot$ppg,10)
#   plot$Pl <- jitter(plot$Pl,5)
#   
#   print(plot$ppg)
#   
#   plot <- cbind(plot, id = seq_len(nrow(plot)))
#   
#   #plot$ppg <- jitter(plot$ppg,amount=.005) poss but does not look good
#   
#   plot  %>%  
#     ggvis(~Pl,~ppg,key := ~id) %>%
#     add_tooltip(all_values, "hover") %>%
#     add_axis("y",title="Av Points per Game") %>%
#     add_axis("x",title="Games Played") %>%
#     
#     scale_numeric("y", zero = TRUE) %>%
#     bind_shiny('hthGraph')
#   
# })