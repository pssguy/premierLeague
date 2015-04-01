# Head to head tab;e abnd graph

## add asparkline

output$hthTable <- DT::renderDataTable({
  
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
  table[is.na(table)]      <- 0    
  tbl <-table %>%
    arrange(desc(ppg),desc(Pl))  %>%
    filter(team==input$team_3) %>%
    select(Opponents=OppTeam,Pl,W,D,L,Pts,GF,GA,GD,ppg) 
  tbl$team <-NULL
  DT::datatable(tbl,options= list(paging = FALSE, searching = FALSE, info=FALSE))
 })



observe({
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- plot[plot$id == x$id,"OppTeam" ]
    paste0(names(row), format(row), collapse = "<br />")
  }
  
  plot <- hth %>%
    filter(team==input$team_3)
  
  plot <-data.frame(plot %>%
                      group_by(team,OppTeam) %>%
                      summarise(GF=sum(GF),GA=sum(GA),Pts=sum(points),Pl=n()) %>%
                      mutate(GD=GF-GA,ppg=round(Pts/Pl,2)))
  plot$ppg <- jitter(plot$ppg,10)
  plot$Pl <- jitter(plot$Pl,5)
  
  print(plot$ppg)
  
  plot <- cbind(plot, id = seq_len(nrow(plot)))
  
  #plot$ppg <- jitter(plot$ppg,amount=.005) poss but does not look good
  
  plot  %>%  
    ggvis(~Pl,~ppg,key := ~id) %>%
    add_tooltip(all_values, "hover") %>%
    add_axis("y",title="Av Points per Game") %>%
    add_axis("x",title="Games Played") %>%
    
    scale_numeric("y", zero = TRUE) %>%
    bind_shiny('hthGraph')
  
})