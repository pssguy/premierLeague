# 
# ## leaders overall + number of times for each category
# # leaders overall might be prob as combing data takes time but number of years certainly poss
# 
# 
# teamLeadersData <- reactive({
#   
#   print("enter teamleaders")
#   
#   if (!is.null(input$team_3)) {
#     theTeam <- input$team_3
#   } else {
#     theTeam<-"Arsenal"
  }
#   print(theTeam)
#   print("str")
#   print(str(leaders))
#   print("?str")
#   
#   df <- leaders[leaders$TEAMNAME==theTeam,]
#   df <- data.frame(df)
#   df <- df[,-1]
#   print(df)
#   info=list(df=df)
#   return(info)
# })
# 
# output$teamLeaders <-  DT::renderDataTable({
#   
#   df <- teamLeadersData()$df %>%
#     select(Season=season,Starts=starts,Sub=sub,Goals=goals,Assists=assists,Points=points,Cards=cards)
#   
#   DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE,
#                                                 orderFixed=list(c(0,'desc'))))
#   
# }
# )
# 
# 
# ## count of seasons
# 
# df %>% 
#   select(starts) %>% 
#   group_by()
# 
# ## go back to orig
# 
# temp <-summary %>%
#   select(St) 
# temp <- data.frame(temp)
# starts <-  temp  %>% 
#   arrange(desc(St)) %>%
#   group_by(TEAMNAME,season) %>%
#   
#   slice(1) %>%
#   select(n1=name,v1=St) %>%
#   mutate(starts=paste(n1,v1)) %>%
#   select(TEAMNAME,season,starts)
# 
# startsTeamEver <-  temp  %>% 
#     group_by(PLAYERID,TEAMNAME,name) %>%
#   summarize(St=sum(St)) %>% 
#   ungroup() %>% 
#   arrange(desc(St)) %>% 
#   group_by(TEAMNAME) %>% 
#   
#   slice(1) %>%
#   select(n1=name,v1=St) %>%
#   mutate(starts=paste(n1,v1)) %>%
#   select(TEAMNAME,starts)
# 
# 
# startsTeamLeader <-  temp  %>% 
#   arrange(desc(St)) %>%
#   group_by(TEAMNAME,season) %>%
#   
#   slice(1) %>% 
#   group_by(TEAMNAME,name,PLAYERID) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) %>% 
#   group_by(TEAMNAME) %>% 
#   
#   slice(1) %>%
#   select(TEAMNAME,name,Starts=n)
#   
# ## look at mins played
# 
# playerGame <- readRDS("playerGame.rds")
# 
# glimpse(playerGame) 
# 
# playerGame <- tbl_df(playerGame)
# 
# playerGame %>% 
#   filter(subOn==99) %>% 
#   select(season) %>% 
#   distinct(.) # last was 1998/99
# 
# playerGame$mins <- 90
# 
# names(playerGame)
# 
# # set all to 90
# playerGame$mins <- 90
# ## never appeared
# playerGame[playerGame$START==0&playerGame$subOn==0,]$mins <- 0
# 
# # came on as sub
# playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 91-playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$subOn
#   
# #mean(playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins) #22 so assume applies at that value to all pre 1999
# playerGame[playerGame$season<="1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 22
# 
# # starter removed
# playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$OFF-1 # to take account of 90 min withdrawals
# playerGame[playerGame$season<="1998/99"&playerGame$OFF>0,]$mins <- 68
# 
# ## sub removed
# playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$OFF-playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$subOn
# #mean(playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins) #34
# playerGame[playerGame$season<="1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- 34
# 
# range(playerGame$mins) #-7 to 92 so some errors to check
# 
# 
# 
# 
# 

#poeition graph
## Problem
# observe({
#   print("enter league pos")
#  
#   print(input$season_3)
#   print(input$team_3)
#   graph <- standings %>%
#     filter(team==input$team_3&season==input$season_3)
#   
#   
#   graph <- cbind(graph, id = seq_len(nrow(graph)))
#   
#   # just need to add a tt and res for fill
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- graph[graph$id == x$id,"tt" ]
#     paste0(names(row), format(row), collapse = "<br />")
#   }
#   
#   pos1 <- standings %>%
#     filter(position==1&season==input$season_3) %>%
#     select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)
#   
#   pos4 <- standings %>%
#     filter(position==4&season==input$season_3) %>%
#     select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)
#   
#   ## need to vary in shiny
#   if (input$season_3 >"1994/95") {
#   pos18 <- standings %>%
#     filter(position==18&season==input$season_3) %>%
#     select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
#   } else if (input$season_3=="1994/95") {
#     pos18 <- standings %>%
#       filter(position==19&season==input$season_3) %>%
#       select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
#   } else {
#     pos18 <- standings %>%
#       filter(position==20&season==input$season_3) %>%
#       select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
#   }
#   #str(pos1)
#   pos1 <- data.frame(pos1) # no good otherwise
#   pos4 <- data.frame(pos4)
#   pos18 <- data.frame(pos18)
#   
#   graph %>%
#     inner_join(pos1) %>%
#     inner_join(pos4) %>%
#     inner_join(pos18) %>%
#     ggvis(~tmYrGameOrder,~cumPts,key := ~id) %>%
#     layer_lines() %>%
#     layer_points(fill = ~res) %>%
#     layer_lines( ~tmYrGameOrder,~lpoints,stroke := "green") %>%
#     layer_lines(~tmYrGameOrder,~epoints,stroke := "blue") %>%
#     layer_lines(~tmYrGameOrder,~rpoints,stroke := "red") %>%
#     add_tooltip(all_values, "hover") %>%
#     add_axis("y",title="Points") %>%
#     add_axis("x",title="Games Played") %>%
#     add_legend("fill",title="") %>%
#     
#            bind_shiny('posGraph')
#   
#   
#   
# })#,suspended = FALSE, autoDestroy = FALSE)


#   graph <- standings %>%
#     filter(team=="Arsenal"&season=="2013/14")
#   
#   
#   graph <- cbind(graph, id = seq_len(nrow(graph)))
# 
#   
#     
#     
#     # just need to add a tt and res for fill
#     all_values <- function(x) {
#       if(is.null(x)) return(NULL)
#       row <- graph[graph$id == x$id,"tt" ]
#       paste0(names(row), format(row), collapse = "<br />")
#     }
#     
#     pos1 <- standings %>%
#       filter(position==1&season=="2013/14") %>%
#       select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)
#     
#     pos4 <- standings %>%
#       filter(position==4&season=="2013/14") %>%
#       select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)
#     
#     pos18 <- standings %>%
#           filter(position==18&season=="2013/14") %>%
#           select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
#     
#       pos1 <- data.frame(pos1) # no good otherwise
#       pos4 <- data.frame(pos4)
#       pos18 <- data.frame(pos18)
#       
#         graph %>%
#           inner_join(pos1) %>%
#           inner_join(pos4) %>%
#           inner_join(pos18) %>%
#           ggvis(~tmYrGameOrder,~cumPts,key := ~id) %>%
#           layer_lines() %>%
#           layer_points(fill = ~res) %>%
#           layer_lines( ~tmYrGameOrder,~lpoints,stroke := "green") %>%
#           layer_lines(~tmYrGameOrder,~epoints,stroke := "blue") %>%
#           layer_lines(~tmYrGameOrder,~rpoints,stroke := "red") %>%
#           add_tooltip(all_values, "hover") %>%
#           add_axis("y",title="Points") %>%
#           add_axis("x",title="Games Played") %>%
#           add_legend("fill",title="")
#   
#       
#       prob with number of values for standings
#       
#       standings %>% 
#         group_by(season,position) %>% 
#         tally() %>% 
#         filter(season=="2014/15")
#       
#       1  2014/15        1  2
#       2  2014/15        2 30
#       3  2014/15        3 29
#       4  2014/15        4 30
#       5  2014/15        5 30
#       6  2014/15        6 30
#       7  2014/15        7 29
#       8  2014/15        8 30
#       9  2014/15        9 30
#       10 2014/15       10 29
#       11 2014/15       11 30
#       12 2014/15       12 27
#       13 2014/15       13 30
#       14 2014/15       14 30
#       15 2014/15       15 29
#       16 2014/15       16 28
#       17 2014/15       17 29
#       18 2014/15       18 27
#       19 2014/15       19 29
#       20 2014/15       20 12
#       
#       ## could be an issue with not same number of games?
#       names(standings) # does not have round but does have tmYrGameOrder
#       
#       data.frame(standings %>% 
#         group_by(season,tmYrGameOrder) %>% 
#         tally() %>% 
#         filter(season=="2014/15"))
#       
#       # n is only 18?
#       
#       
#       data.frame(standings %>% 
#                    group_by(season,tmYrGameOrder) %>% 
#                    tally() %>% 
#                    filter(season=="2013/14")) #38 20
#         
#       
#       data.frame(standings %>% 
#                    group_by(season,team) %>% 
#                    tally() %>% 
#                    filter(season=="2014/15")) # missing Leicester and Chelsea - because they have played a game less
#     
#       standings %>% 
#         filter(season=="2013/14"&team=="Chelsea")
#       
#       both %>% 
#         filter(season=="2014/15"&team=="Chelsea") #29 vals

## most apps before scoring goal

cats <- c("Head","Right","Left","6_Yd_Box","Pen_Area","Long_Range","Open","Corner","Indirect_FK",
          "Direct_FK","Penalty","Throw")
allCats <- data.frame(category=cats)

output$goalFirsts <- DT::renderDataTable({
  print("enter goalFirsts")
  a <- goals %>%
    left_join(playerGame, by="PLAYER_MATCH") %>%
#    filter(PLAYERID==input$player&(START+subOn)>0) %>%
    select(METHOD,PLACE,PLAY,plGameOrderApp)
  
  if (nrow(a)>0) {
    b <- a %>%
      gather(dummy,category,-plGameOrderApp) %>%
      arrange(plGameOrderApp)
    
    
    for (i in 1:length(cats)) {
      
      if (nrow(b %>% filter(category==cats[i])) >0) {
        tempdf <- data.frame(b %>%
                               filter(category==cats[i]) %>%
                               slice(1))
        tempdf <- tempdf[,c("category","plGameOrderApp")]
        
        if (i!=1) {
          df <- rbind(df,tempdf)
        } else {
          df <- tempdf
        }
      }
      allApps <- nrow(playerGame %>% filter(PLAYERID==input$player&(START+subOn)>0))
      
      a$since <- allApps- a$plGameOrderApp
      
      a <- a %>%
        mutate(since=allApps-plGameOrderApp) %>%
        arrange(since)
      
      c <- a %>%
        gather(dummy,category,-c(plGameOrderApp,since))
      #i <- 2
      tempdf <- NULL
      for (i in 1:length(cats)) {
        
        if (nrow(c %>% filter(category==cats[i])) >0) {
          tempdf <- data.frame(c %>%
                                 filter(category==cats[i]) %>%
                                 slice(1))
          tempdf <- tempdf[,c("category","since")]
          
          if (i!=1) {
            dfSince <- rbind(dfSince,tempdf)
          } else {
            dfSince <- tempdf
          }
        }
        
      }
      
      ## also get all goals
      allGoals <- b %>%
        group_by(category) %>%
        summarise(tot=n())
      
      allCats <- data.frame(category=cats)
      
      print("about to join")
      print(allCats)
      print(allGoals)
      print(df)
      print(dfSince)
      
      tbl <- allCats %>%
        left_join(allGoals) %>%
        left_join(df) %>%
        left_join(dfSince) %>%
        rename(Category=category,Tot=tot,First=plGameOrderApp,Since=since)
      
      print("success")
      tbl$Tot <- ifelse(is.na(tbl$Tot),0,tbl$Tot)
      tbl$First <- ifelse(is.na(tbl$First),0,tbl$First)
      tbl$Since <- ifelse(is.na(tbl$Since),0,tbl$Since)
      print(tbl)
      # tbl
    }
  } else {
    tbl <- data.frame(Category=cats,Tot=rep(0,12),First=rep("",12),Since=rep("",12))
  }
  
  DT::datatable(tbl,options= list(paging = FALSE, searching = FALSE, info=FALSE))
  
}

)

names(playerGame)
playerGame %>% 
  filter(PLAYERID=="HILLC2"&(START+subOn)>0) %>% 
  select(name,plGameOrder,Gls)


playerGame %>% 
  filter(PLAYERID=="ROONEYX"&(START+subOn)>0) %>% 
  select(name,plGameOrder,Gls,gameDate) %>% 
  mutate(gameOrder=row_number(plGameOrder)) %>% 
  filter(Gls>0) %>% 
  slice(1)


temp <-playerGame %>% 
  filter((START+subOn)>0) %>% 
  group_by(PLAYERID) %>% 
  select(name,plGameOrder,Gls,gameDate) %>% 
  mutate(gameOrder=row_number(plGameOrder)) %>% 
  filter(Gls>0) %>% 
  group_by(PLAYERID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(gameOrder)) %>% 
  ungroup() %>% 
  mutate(rank=min_rank(-gameOrder)) %>% 
  
  filter(PLAYERID=="ABLETTG")

### probs arising ? DT needed dev shiny

library(shiny)
shinyApp(
  ui = fluidPage(DT::dataTableOutput('tbl')),
  server = function(input, output) {
    output$tbl = DT::renderDataTable({
      DT::datatable(iris, options = list(lengthChange = FALSE))
    })
  }
)

## row selection
library(DT)
iris2 = head(iris, 20)
datatable(
  appendCheckboxes(iris2), escape = -7,
  options = list(pageLength = 15, dom = 'tip')
)
#datatable creates widget# escape . normally defaults to all but want all but the 
# the 7th column i.e the checkbox - still sjows table but...
#You are recommended to escape the table content for security reasons (e.g. XSS attacks) when using this function in Shiny or any other dynamic web applications

# just use checkboxes as the "row names"
datatable(
  iris2, rownames = checkboxRows(iris2), escape = -1,
  options = list(pageLength = 5, dom = 'tip')
)

# http://rstudio.github.io/DT/shiny.html also some stuff on 


### do snippets from here as well
lei never loosing by more than one
glimpse()
  
)
  
  
  ## look at using checkboxes to get head to head data (and extend to other tables)
  ## look at say Arsenal v reading
  glimpse(teamGames)
  
tm1 <-  teamGames %>% 
    filter(TEAMNAME=="Arsenal") %>% 
    select(MATCHID,venue,TEAMNAME,GF=GOALS,gameDate)

tm2 <-  teamGames %>% 
  filter(TEAMNAME=="Reading") %>% 
  select(MATCHID,GA=GOALS)

tm1 %>% 
  inner_join(tm2,by=c("MATCHID")) %>% 
  ungroup() %>% 
  arrange(desc(gameDate)) %>% 
  select(season=season.x,date=gameDate,venue,GF,GA) -> fixtures
  
  