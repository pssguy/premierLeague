
## leaders overall + number of times for each category
# leaders overall might be prob as combing data takes time but number of years certainly poss


teamLeadersData <- reactive({
  
  print("enter teamleaders")
  
  if (!is.null(input$team_3)) {
    theTeam <- input$team_3
  } else {
    theTeam<-"Arsenal"
  }
  print(theTeam)
  print("str")
  print(str(leaders))
  print("?str")
  
  df <- leaders[leaders$TEAMNAME==theTeam,]
  df <- data.frame(df)
  df <- df[,-1]
  print(df)
  info=list(df=df)
  return(info)
})

output$teamLeaders <-  DT::renderDataTable({
  
  df <- teamLeadersData()$df %>%
    select(Season=season,Starts=starts,Sub=sub,Goals=goals,Assists=assists,Points=points,Cards=cards)
  
  DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE,
                                                orderFixed=list(c(0,'desc'))))
  
}
)


## count of seasons

df %>% 
  select(starts) %>% 
  group_by()

## go back to orig

temp <-summary %>%
  select(St) 
temp <- data.frame(temp)
starts <-  temp  %>% 
  arrange(desc(St)) %>%
  group_by(TEAMNAME,season) %>%
  
  slice(1) %>%
  select(n1=name,v1=St) %>%
  mutate(starts=paste(n1,v1)) %>%
  select(TEAMNAME,season,starts)

startsTeamEver <-  temp  %>% 
    group_by(PLAYERID,TEAMNAME,name) %>%
  summarize(St=sum(St)) %>% 
  ungroup() %>% 
  arrange(desc(St)) %>% 
  group_by(TEAMNAME) %>% 
  
  slice(1) %>%
  select(n1=name,v1=St) %>%
  mutate(starts=paste(n1,v1)) %>%
  select(TEAMNAME,starts)


startsTeamLeader <-  temp  %>% 
  arrange(desc(St)) %>%
  group_by(TEAMNAME,season) %>%
  
  slice(1) %>% 
  group_by(TEAMNAME,name,PLAYERID) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  group_by(TEAMNAME) %>% 
  
  slice(1) %>%
  select(TEAMNAME,name,Starts=n)
  
