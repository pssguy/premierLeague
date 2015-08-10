


#observeEvent(input$playerA,{ # this works ok but when wanting to type in is an issue - maybe not just this file
observe({
  print("checking not empty")
  print(input$playerA)
  if(is.null(input$playerA)) return()
  if(input$playerA=="")return()
  print("transferfee in")
  print(input$playerA)
  
  transfers <-  playerGame %>% 
    filter(PLAYERID==input$playerA&PERMANENT==1) %>% 
    
    select(name,joined,FEE,TEAMNAME,BIRTHDATE) %>% 
    unique() %>% 
    mutate(Cost=ifelse(FEE==0,0,FEE/1000)) %>% 
    rename(Fee=FEE,Team=TEAMNAME,Date=joined)
  
  print(transfers)
  
  transfers <- cbind(transfers, id = seq_len(nrow(transfers)))
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- transfers[transfers$id == x$id,c("Date","Team","Cost")]
    paste0( names(row),": ",format(row), collapse = "<br />") 
  }
  print("still OK")
  
  write_csv(transfers,"problem.csv")
  
  transfers   %>% 
    ggvis(~Date,~Cost, key:= ~id) %>% 
    layer_points(fill=~Team) %>%  
    scale_numeric("y",domain=c(0,max(transfers$Cost))) %>% 
    scale_datetime("x", nice='year') %>% 
    add_tooltip(all_values,"hover") %>% 
    add_axis("x", properties = axis_props(labels = list(
      angle = 45, align = "left", fontSize = 11
    )),title = "Joined Club",title_offset=50) %>% 
    add_axis("y", title="Fee (million)",title_offset=50) %>% 
    hide_legend("fill") %>% # otherwise takes up too much space
    set_options(width=300,height=200) %>% 
    bind_shiny("playerTransfers")
  
 
})