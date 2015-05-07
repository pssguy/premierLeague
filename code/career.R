output$career <- DT::renderDataTable({
  
  if (!is.null(input$player)) {
    thePlayer <- input$player
  } else {
    thePlayer<-"ROONEYX"
  }
  print(str(summary))
  print("summary done")
  #tbl <- careerData()$summary %>%
  tbl <- summary %>%
    filter(PLAYERID==thePlayer) %>%
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) 
  #str(tbl)
  tbl <- data.frame(tbl) 
  tbl <- tbl  %>%    arrange(desc(Season))
  print(tbl$Apps)
  maxApps <- max(tbl$Apps, na.rm=T)
  print(maxApps)
  tbl <- tbl[,-(1:5)]
  DT::datatable(tbl,options= list(paging = FALSE, searching = FALSE,info=FALSE))
})


# suggestion in github
## this restricts to one row - can add 
# callback="function(table) {
# table.on('click.dt', 'tr', function() {
# $(this).closest('table').find('.selected').each(function(){
# $(this).removeClass('selected');
# });
# $(this).toggleClass('selected');
# Shiny.onInputChange('row',
# table.rows('.selected').indexes().toArray());
# });
# }"

               
                                