


careerData <- reactive({
  
  if (is.null(input$playerA)) return()
  thePlayer <- input$playerA
  
  dfTeamYear <- summary %>%
    filter(PLAYERID==thePlayer) %>%
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>% 
    ungroup() %>% 
    arrange(desc(Season)) %>% 
    select(-(c(name,LASTNAME,PLAYERID,born,left)))
  
  print(glimpse(dfTeamYear))
  
  dfTeam <- dfTeamYear %>% 
    group_by(Team) %>%
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  
  print(names(dfTeam))
  
  dfCareer <- dfTeamYear %>% 
   
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  dfCareer$Team <- "Career"
  print(names(dfCareer))
 
  
  info=list(dfTeamYear=dfTeamYear,dfTeam=dfTeam,dfCareer=dfCareer)
  return(info)
 
  
  
})


output$careerYear <- DT::renderDataTable({
  careerData()$dfTeamYear  %>%  
    select(-MP) %>% 
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
    })

output$career <- DT::renderDataTable({
  if (nrow((careerData()$dfTeam))>1) {
    df <- rbind(careerData()$dfTeam,careerData()$dfCareer)
  } else {
    df <- careerData()$dfTeam
  }
 # df <- rbind(careerData()$dfTeam,careerData()$dfCareer)
  df  %>%  
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
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

               
                                