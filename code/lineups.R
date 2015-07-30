# Set up reactive values initially as null
lineupValues <- reactiveValues()

## click on from graph eg team standings

getLineups = function(data,location,session) {
  if (is.null(data))
    return(NULL)
  lineupValues$theGame <- data$tmYrGameOrder
  
}


output$lineup <- DT::renderDataTable({
  if (is.null(lineupValues$theGame))
    return()
 # print(lineupValues$theGame)
  matchDate <-
    standings[standings$team == input$teamA &
                standings$season == input$teamYears &
                standings$tmYrGameOrder == lineupValues$theGame,]$gameDate
  #print(matchDate)
  # lineupValues$theGame <- NULL this stops dtattable rendering not sure why but wnat to reset otherwise automatically calls up game of season even when new inputs
 df <-  playerGame %>%
    filter(TEAMNAME == input$teamA & gameDate == matchDate) %>%
    arrange(desc(gameDate),off,on) %>%
    select(name,st,on,off,Gls,Assists,CARD) 

 ## get rid of 0 for goals and assists
 df[df$Gls==0,]$Gls <- ""
 df[df$Assists==0,]$Assists <- ""
 
df  %>% 
     DT::datatable(class='compact stripe hover row-border',colnames = c('Player', 'Start', 'On', 'Off', 'Goals', 'Assists', 'Card'),
      rownames = FALSE,options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        order = list(list(1, 'desc'), list(2, 'desc')),
        columnDefs = list(list(
          className = 'dt-center', targets = c(1,2,3,4,5,6)
        ))
      )
     )

# looks like only applies to values not character so would need to create artifically?

#     ) %>% 
#      formatStyle(
#        'CARD',
#        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
#        backgroundColor = styleColorBar(c("R","Y"), c('red', 'yellow'))
#        #backgroundColor = styleInterval(0, c('gray', 'yellow'))
#      )

})
