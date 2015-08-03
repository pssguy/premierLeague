


#observeEvent(input$playerA,{
observe({
  if(is.null(input$playerA)) return()
  
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
    layer_points() %>%  # would like to add fill by team but takes up too much space
    add_tooltip(all_values,"hover") %>% 
    add_axis("x", properties = axis_props(labels = list(
      angle = 45, align = "left", fontSize = 11
    )),title = "Joined Club",title_offset=50) %>% 
    add_axis("y", title="Fee (million)",title_offset=50) %>% 
    set_options(width=300,height=200) %>% 
    bind_shiny("playerTransfers")
  
  
})