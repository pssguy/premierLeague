
scoredOnData <- reactive({
  df <-playerGame %>%
                       filter((Gls)>0) %>%
                       group_by(PLAYERID,name) %>%
                       summarise(teams=n_distinct(Opponents),goals=sum(Gls))%>% 
    ungroup() %>% 
    filter(name!= " Own Goal") %>% 
    mutate(rank=min_rank(-teams)) %>% 
    arrange(desc(teams),desc(goals)) %>%
    filter(name!="Own Goal") %>% 
    select(rank,name,teams,goals,PLAYERID)
  
 print(glimpse(df))
 
 info=list(df=df)
 return(info)
 
})




output$scoredOn <- DT::renderDataTable({
#   a <- data.frame(playerGame %>%
#                     filter((Gls)>0) %>%
#                     group_by(PLAYERID,name) %>%
#                     summarise(teams=n_distinct(Opponents),goals=sum(Gls))) 
#   
#   
#   a %>%
#     arrange(desc(teams)) %>%
#     filter(name!="Own Goal") %>% 
#     select(name,teams,goals) %>% 
  
  print(head( scoredOnData()$df$name))
  
  scoredOnData()$df %>% 
    
    select(- PLAYERID) %>% 
    DT::datatable(
      rownames=TRUE, selection ='single',options=list(
                  columnDefs= list(list(
                                   visible = 'false' , targets = 1 #does not appear to work
                  ))))
                  
  
})
  

scoredOnDetsData <- reactive({
  
 # print("enter scoredOnDetsData")
  #if(is.null(input$scoredOn_rows_selected)) return()
  
  req(input$scoredOn_rows_selected)
  
  s = as.integer(input$scoredOn_rows_selected)
  
 # print(s)
  player <- scoredOnData()$df[s,]$PLAYERID
# print(player)
  
 df <- playerGame %>% 
    filter(PLAYERID==player&(START+subOn)>0) %>% 
    group_by(Opponents) %>% 
    summarise(apps=n(),goals=sum(Gls))
 
 info=list(df=df)
 return(info)
  
})


output$scoredOn_dets <- DT::renderDataTable({
  
 # if (is.null(scoredOnDetsData()$df)) return()
  
req(scoredOnDetsData()$df)
  
  scoredOnDetsData()$df %>% 
    arrange(desc(goals)) %>% 
    DT::datatable(rownames=FALSE)
  

})

observe({
  
 # if (is.null(scoredOnDetsData()$df)) return()
  req(scoredOnDetsData()$df)
  
  df <- scoredOnDetsData()$df
  
  df <- cbind(df, id = seq_len(nrow(df)))
  
  all_values<- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[df$id == x$id,c("Opponents") ]
    paste0( format(row), collapse = "<br />")
  }
  
df   %>% 
    mutate(apps=jitter(apps),goals=jitter(goals)) %>% 
    ggvis(~apps,~goals, key := ~id) %>% 
    layer_points() %>% 
  ggvis::add_axis('x',title="Appearances", format='d') %>% 
  ggvis::add_axis('y',title="Goals Scored", format='d') %>%
  add_tooltip(all_values, "hover") %>%
    bind_shiny("scoredOnChart")
})
