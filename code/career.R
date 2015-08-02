


careerData <- reactive({
  
   if (is.null(input$playerA)) return()
#   print(values$playerId)
#   print(input$playerA)
  thePlayer <- input$playerA
  
 # updateSelectizeInput(session, "playerA", choices = playerChoice, selected = input$playerA)
  
 # if (is.null(values$playerId)) return()
 # print(values$playerId)
 ## print(input$playerA)
 # thePlayer <- values$playerId
  
  dfChart <- playerGame %>% 
    filter(PLAYERID==thePlayer) %>% 
    select(date=gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID) %>% 
    mutate(points=Goals+Assists)
  
#  print(glimpse(dfChart))
  
  
  dfTeamYear <- summary %>%
    filter(PLAYERID==thePlayer) %>%
    mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
    select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>% 
    ungroup() %>% 
    arrange(desc(Season)) %>% 
    select(-(c(name,LASTNAME,PLAYERID,born,left)))
  
  #print(glimpse(dfTeamYear))
  
  dfTeam <- dfTeamYear %>% 
    group_by(Team) %>%
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  
 # print(names(dfTeam))
  
  dfCareer <- dfTeamYear %>% 
   
    summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
              Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
  dfCareer$Team <- "Career"
#  print(names(dfCareer))
 
  
  info=list(dfTeamYear=dfTeamYear,dfTeam=dfTeam,dfCareer=dfCareer,dfChart=dfChart)
  return(info)
 
  
  
})


observe({
  
  if(is.null(careerData())) return()
  print("enter observe")
  print(glimpse(careerData()$dfChart))
  
  df <- careerData()$dfChart
  
  df <- cbind(df, id = seq_len(nrow(df)))
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[df$id == x$id,c("date","Opponents","on","off","Goals","Assists") ]
    paste0( names(row),": ",format(row), collapse = "<br />") 
  }
  
  
  df %>% 
    ggvis(~plGameOrder, ~mins, key := ~id) %>%
    layer_points(fill = ~Team, size = ~ points) %>% 
    add_tooltip(all_values,"hover") %>% 
    
    add_axis("y", title="Minutes Played", format='d') %>% # attempt to enforxe 0 , values=c(0,15,30,45,60,75,90)
    add_axis("x", title="Match Day Squad Game Order", format='d') %>% 
    hide_legend("size") %>% 
    bind_shiny("careerChart")
  
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

               
                                