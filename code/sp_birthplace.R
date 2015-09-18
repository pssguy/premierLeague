### may want to replace in different area


temp <-summary %>% 
  ungroup() %>% 
  select(PLAYERID,mins,season,TEAMNAME) %>% 
  left_join(allPlayers) %>% 
  select(season,mins,COUNTRY,TEAMNAME) %>% 
  group_by(season,COUNTRY,TEAMNAME) %>% 
  summarize(totMins=sum(mins)) %>% 
  group_by(season,TEAMNAME) %>% 
  mutate(allMins=sum(totMins),pc=100*totMins/allMins) 


allTemp <-summary %>% 
  ungroup() %>% 
  select(PLAYERID,mins,season) %>% 
  left_join(allPlayers) %>% 
  select(season,mins,COUNTRY) %>% 
  group_by(season,COUNTRY) %>% 
  summarize(totMins=sum(mins)) %>% 
  group_by(season) %>% 
  mutate(allMins=sum(totMins),pc=100*totMins/allMins) ## allMins not same for all clubs ???

observe({
  
  
  df <- allTemp %>% 
    ungroup() %>% 
    filter(COUNTRY==input$bp_Country)
  
  df$id <- 1:nrow(df)
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[x$id == df$id,c("season","pc")]
    paste0( names(row),": ",format(row), collapse = "<br />")
  }
  
  # allow 0-100 or more illuminating scale
  if(input$bp_fullScale=="No") {

df %>% 
  ggvis(~season,~pc,key:= ~id) %>% 
  layer_lines(stroke:="coral", strokeWidth:=3) %>% 
      layer_points() %>% 
      add_tooltip(all_values, "click") %>% 
      handle_click(getbpSeason) %>%
  add_axis("y",title="% Mins played by English Born Players in BPL") %>% 
  add_axis("x", properties = axis_props(labels = list(
    angle = 45, align = "left", fontSize = 10
  )),title = "") %>% 
  bind_shiny("sp_birthplaceChart")
  } else {
    
    df %>% 
      ggvis(~season,~pc,key:= ~id) %>% 
      layer_lines(stroke:="coral", strokeWidth:=3) %>% 
      layer_points() %>% 
      add_tooltip(all_values, "click") %>% 
      handle_click(getbpSeason) %>%
       scale_numeric("y",domain=c(0,100)) %>% 
      add_axis("y",title="% Mins played by English Born Players in BPL") %>% 
      add_axis("x", properties = axis_props(labels = list(
        angle = 45, align = "left", fontSize = 10
      )),title = "") %>% 
      bind_shiny("sp_birthplaceChart")
  }
})


# output$sp_bpTeamsbyYear <- DT::renderDataTable({
#   
#   temp %>% 
#     filter(season=="2015/16"&COUNTRY=="England") %>% 
#     mutate(English_pc=round(pc,0)) %>% 
#     ungroup() %>% 
#     select(Team=TEAMNAME,English_pc) %>% 
#     arrange(English_pc) %>% 
#     DT::datatable(class='compact stripe hover row-border order-column',
#                   rownames=FALSE,
#                   colnames = c('Team','% Selected Country'),
#                  # width=200,
#                   options= list( searching = FALSE,info=FALSE))
#   
#   
#   
# })


getbpSeason = function(data,location,session){
  
   if(is.null(data)) return(NULL)

  print(data$season)
  
#   theYear <- data$year
#   theLeague <- data$league
  
  
  session$output$sp_bpTeamsbyYear <- DT::renderDataTable({
    
    temp %>% 
      filter(season==data$season&COUNTRY==input$bp_Country) %>% 
      mutate(English_pc=round(pc,0)) %>% 
      ungroup() %>% 
      select(Team=TEAMNAME,English_pc) %>% 
      arrange(English_pc) %>% 
      DT::datatable(class='compact stripe hover row-border order-column',
                    rownames=FALSE,
                    colnames = c('Team','% Selected Country'),
                    # width=200,
                    options= list( searching = FALSE,info=FALSE))
    
 })
}