## extendable to youngest/started/scored ...
## also use as background for time-series / more in depth manager info



managerData <- reactive({
  
  req(input$manager)

stints <- managers %>% 
  filter(name==input$manager) %>% 
  mutate(Left=as_date(ifelse(is.na(Left),Sys.Date(),Left)))


# this gives all player games played for etc...
#i <- 1
for (i in 1:nrow(stints)) {
  
  tempdf <-playerGame %>% 
    filter(gameDate>=stints$Joined[i]&gameDate<=stints$Left[i]&TEAMNAME==stints$TEAMNAME[i])
  
  if (i!=1) {
    df <- bind_rows(df,tempdf)
  } else {
    df <- tempdf
  }
  
}



young<-df %>% 
  filter(mins>0&Gls>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME,LASTNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1) %>% 
  ungroup()  %>% 
  mutate(age = as.numeric(age),cumminAge=cummin(age)) %>% 
  group_by(name,PLAYERID) %>% 
  slice(1) %>% 
  mutate(label=paste0(LASTNAME," ",round(age,2))) %>% 
  rename(Team=TEAMNAME) %>% 
  mutate(alpha=ifelse(age==cumminAge,0.8,0.2)) %>% ## not working yet
  arrange(gameDate) 

info=list(young=young)
return(info)

})

output$managerPlayersAge <- renderPlot({

  print("enter managerPlayersAge")
  
  req(managerData)
  
  print("got Manager data")
title <- paste0("Players age when first scoring for ",input$manager," in Premier League")

print(input$manager)
print(managerData()$young)

write_csv(managerData()$young,"problem.csv")

p <- ggplot(managerData()$young, aes(gameDate, round(age,2), label = label))


p + geom_text_repel(aes(colour = factor(Team))) +
 theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Age")            + 
  ggtitle(title)    +
  #theme(legend.title=element_blank()) + 
  theme_fivethirtyeight() +  
  scale_color_fivethirtyeight() +
  theme(legend.title=element_blank()) 

})


### when front page
# managerData_fp <- reactive({
#   
#   print("enter before req")
#   req(input$manager_fp)
#   print("enter after req")
#   
#   stints <- managers %>% 
#     filter(name==input$manager_fp) %>% 
#     mutate(Left=as_date(ifelse(is.na(Left),Sys.Date(),Left)))
#   
#   
#   # this gives all player games played for etc...
#   #i <- 1
#   for (i in 1:nrow(stints)) {
#     
#     tempdf <-playerGame %>% 
#       filter(gameDate>=stints$Joined[i]&gameDate<=stints$Left[i]&TEAMNAME==stints$TEAMNAME[i])
#     
#     if (i!=1) {
#       df <- bind_rows(df,tempdf)
#     } else {
#       df <- tempdf
#     }
#     
#   }
#   
#   
#   
#   young<-df %>% 
#     filter(mins>0&Gls>0) %>% #2992
#     select(name,PLAYERID,age,gameDate,TEAMNAME,LASTNAME) %>% 
#     arrange(gameDate,age) %>% 
#     group_by(gameDate) %>% 
#     slice(1) %>% 
#     ungroup()  %>% 
#     mutate(age = as.numeric(age),cumminAge=cummin(age)) %>% 
#     group_by(name,PLAYERID) %>% 
#     slice(1) %>% 
#     mutate(label=paste0(LASTNAME," ",round(age,2))) %>% 
#     rename(Team=TEAMNAME) %>% 
#     mutate(alpha=ifelse(age==cumminAge,0.8,0.2)) %>% ## not working yet
#     arrange(gameDate) 
#   
#   print(glimpse(young))
#   
#   info=list(young=young)
#   return(info)
#   
# })
# 
# output$managerPlayersAge_front <- renderPlot({
#   
#   print("managerData_fp")
#   print(input$manager_fp)
#  
#   req(managerData_fp)
#   print("managerData_fp_recd")
#   print(glimpse$managerData_fp)
#   title <- paste0("Players age when first scoring for ",input$manager_fp," in Premier League")
#   
#   p <- ggplot(managerData_fp()$young, aes(gameDate, round(age,2), label = label))
#   
#   
#   p + geom_text_repel(aes(colour = factor(Team))) +
#     theme(axis.title.x = element_blank()) +   # Remove x-axis label
#     ylab("Age")            + 
#     ggtitle(title)    +
#     #theme(legend.title=element_blank()) + 
#     theme_fivethirtyeight() +  
#     scale_color_fivethirtyeight() +
#     theme(legend.title=element_blank()) 
#   
# })
