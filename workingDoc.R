#if Rstudio method is not working
library(shinyapps)
servers()
# name                                     url
# 1 beta.rstudioconnect.com https://beta.rstudioconnect.com/__api__
# 2            bookdown.org            https://bookdown.org/__api__
# 3            shinyapps.io             https://api.shinyapps.io/v1
accounts()
# name                  server
# 1       aclark beta.rstudioconnect.com
# 2       aclark            bookdown.org
# 3 mytinyshinys            shinyapps.io
library(shinyapps)
deployApp(account="mytinyshinys")


# #' #
# #' # ## leaders overall + number of times for each category
# #' # # leaders overall might be prob as combing data takes time but number of years certainly poss
# #' #
# #' #
# #' # teamLeadersData <- reactive({
# #' #
# #' #   print("enter teamleaders")
# #' #   (str(leaders))
# #' #   if (!is.null(input$team_3)) {
# #' #     theTeam <- input$team_3
# #' #   } else {
# #' #     theTeam<-"Arsenal"
# #'   }
# #' #   print(theTeam)
# #' #   print("str")
# #' #   print
# #' #   print("?str")
# #' #
# #' #   df <- leaders[leaders$TEAMNAME==theTeam,]
# #' #   df <- data.frame(df)
# #' #   df <- df[,-1]
# #' #   print(df)
# #' #   info=list(df=df)
# #' #   return(info)
# #' # })
# #' #
# #' # output$teamLeaders <-  DT::renderDataTable({
# #' #
# #' #   df <- teamLeadersData()$df %>%
# #' #     select(Season=season,Starts=starts,Sub=sub,Goals=goals,Assists=assists,Points=points,Cards=cards)
# #' #
# #' #   DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE,
# #' #                                                 orderFixed=list(c(0,'desc'))))
# #' #
# #' # }
# #' # )
# #' #
# #' #
# #' # ## count of seasons
# #' #
# #' # df %>%
# #' #   select(starts) %>%
# #' #   group_by()
# #' #
# #' # ## go back to orig
# #' #
# #' # temp <-summary %>%
# #' #   select(St)
# #' # temp <- data.frame(temp)
# #' # starts <-  temp  %>%
# #' #   arrange(desc(St)) %>%
# #' #   group_by(TEAMNAME,season) %>%
# #' #
# #' #   slice(1) %>%
# #' #   select(n1=name,v1=St) %>%
# #' #   mutate(starts=paste(n1,v1)) %>%
# #' #   select(TEAMNAME,season,starts)
# #' #
# #' # startsTeamEver <-  temp  %>%
# #' #     group_by(PLAYERID,TEAMNAME,name) %>%
# #' #   summarize(St=sum(St)) %>%
# #' #   ungroup() %>%
# #' #   arrange(desc(St)) %>%
# #' #   group_by(TEAMNAME) %>%
# #' #
# #' #   slice(1) %>%
# #' #   select(n1=name,v1=St) %>%
# #' #   mutate(starts=paste(n1,v1)) %>%
# #' #   select(TEAMNAME,starts)
# #' #
# #' #
# #' # startsTeamLeader <-  temp  %>%
# #' #   arrange(desc(St)) %>%
# #' #   group_by(TEAMNAME,season) %>%
# #' #
# #' #   slice(1) %>%
# #' #   group_by(TEAMNAME,name,PLAYERID) %>%
# #' #   tally() %>%
# #' #   ungroup() %>%
# #' #   arrange(desc(n)) %>%
# #' #   group_by(TEAMNAME) %>%
# #' #
# #' #   slice(1) %>%
# #' #   select(TEAMNAME,name,Starts=n)
# #' #
# #' # ## look at mins played
# #' #
# #' # playerGame <- readRDS("playerGame.rds")
# #' #
# #' # glimpse(playerGame)
# #' #
# #' # playerGame <- tbl_df(playerGame)
# #' #
# #' # playerGame %>%
# #' #   filter(subOn==99) %>%
# #' #   select(season) %>%
# #' #   distinct(.) # last was 1998/99
# #' #
# #' # playerGame$mins <- 90
# #' #
# #' # names(playerGame)
# #' #
# #' # # set all to 90
# #' # playerGame$mins <- 90
# #' # ## never appeared
# #' # playerGame[playerGame$START==0&playerGame$subOn==0,]$mins <- 0
# #' #
# #' # # came on as sub
# #' # playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 91-playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$subOn
# #' #
# #' # #mean(playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins) #22 so assume applies at that value to all pre 1999
# #' # playerGame[playerGame$season<="1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 22
# #' #
# #' # # starter removed
# #' # playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$OFF-1 # to take account of 90 min withdrawals
# #' # playerGame[playerGame$season<="1998/99"&playerGame$OFF>0,]$mins <- 68
# #' #
# #' # ## sub removed
# #' # playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$OFF-playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$subOn
# #' # #mean(playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins) #34
# #' # playerGame[playerGame$season<="1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- 34
# #' #
# #' # range(playerGame$mins) #-7 to 92 so some errors to check
# #' #
# #' #
# #' #
# #' #
# #' #
# #'
# #' #poeition graph
# #' ## Problem
# #' # observe({
# #' #   print("enter league pos")
# #' #
# #' #   print(input$season_3)
# #' #   print(input$team_3)
# #' #   graph <- standings %>%
# #' #     filter(team==input$team_3&season==input$season_3)
# #' #
# #' #
# #' #   graph <- cbind(graph, id = seq_len(nrow(graph)))
# #' #
# #' #   # just need to add a tt and res for fill
# #' #   all_values <- function(x) {
# #' #     if(is.null(x)) return(NULL)
# #' #     row <- graph[graph$id == x$id,"tt" ]
# #' #     paste0(names(row), format(row), collapse = "<br />")
# #' #   }
# #' #
# #' #   pos1 <- standings %>%
# #' #     filter(position==1&season==input$season_3) %>%
# #' #     select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)
# #' #
# #' #   pos4 <- standings %>%
# #' #     filter(position==4&season==input$season_3) %>%
# #' #     select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)
# #' #
# #' #   ## need to vary in shiny
# #' #   if (input$season_3 >"1994/95") {
# #' #   pos18 <- standings %>%
# #' #     filter(position==18&season==input$season_3) %>%
# #' #     select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
# #' #   } else if (input$season_3=="1994/95") {
# #' #     pos18 <- standings %>%
# #' #       filter(position==19&season==input$season_3) %>%
# #' #       select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
# #' #   } else {
# #' #     pos18 <- standings %>%
# #' #       filter(position==20&season==input$season_3) %>%
# #' #       select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
# #' #   }
# #' #   #str(pos1)
# #' #   pos1 <- data.frame(pos1) # no good otherwise
# #' #   pos4 <- data.frame(pos4)
# #' #   pos18 <- data.frame(pos18)
# #' #
# #' #   graph %>%
# #' #     inner_join(pos1) %>%
# #' #     inner_join(pos4) %>%
# #' #     inner_join(pos18) %>%
# #' #     ggvis(~tmYrGameOrder,~cumPts,key := ~id) %>%
# #' #     layer_lines() %>%
# #' #     layer_points(fill = ~res) %>%
# #' #     layer_lines( ~tmYrGameOrder,~lpoints,stroke := "green") %>%
# #' #     layer_lines(~tmYrGameOrder,~epoints,stroke := "blue") %>%
# #' #     layer_lines(~tmYrGameOrder,~rpoints,stroke := "red") %>%
# #' #     add_tooltip(all_values, "hover") %>%
# #' #     add_axis("y",title="Points") %>%
# #' #     add_axis("x",title="Games Played") %>%
# #' #     add_legend("fill",title="") %>%
# #' #
# #' #            bind_shiny('posGraph')
# #' #
# #' #
# #' #
# #' # })#,suspended = FALSE, autoDestroy = FALSE)
# #'
# #'
# #' #   graph <- standings %>%
# #' #     filter(team=="Arsenal"&season=="2013/14")
# #' #
# #' #
# #' #   graph <- cbind(graph, id = seq_len(nrow(graph)))
# #' #
# #' #
# #' #
# #' #
# #' #     # just need to add a tt and res for fill
# #' #     all_values <- function(x) {
# #' #       if(is.null(x)) return(NULL)
# #' #       row <- graph[graph$id == x$id,"tt" ]
# #' #       paste0(names(row), format(row), collapse = "<br />")
# #' #     }
# #' #
# #' #     pos1 <- standings %>%
# #' #       filter(position==1&season=="2013/14") %>%
# #' #       select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)
# #' #
# #' #     pos4 <- standings %>%
# #' #       filter(position==4&season=="2013/14") %>%
# #' #       select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)
# #' #
# #' #     pos18 <- standings %>%
# #' #           filter(position==18&season=="2013/14") %>%
# #' #           select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
# #' #
# #' #       pos1 <- data.frame(pos1) # no good otherwise
# #' #       pos4 <- data.frame(pos4)
# #' #       pos18 <- data.frame(pos18)
# #' #
# #' #         graph %>%
# #' #           inner_join(pos1) %>%
# #' #           inner_join(pos4) %>%
# #' #           inner_join(pos18) %>%
# #' #           ggvis(~tmYrGameOrder,~cumPts,key := ~id) %>%
# #' #           layer_lines() %>%
# #' #           layer_points(fill = ~res) %>%
# #' #           layer_lines( ~tmYrGameOrder,~lpoints,stroke := "green") %>%
# #' #           layer_lines(~tmYrGameOrder,~epoints,stroke := "blue") %>%
# #' #           layer_lines(~tmYrGameOrder,~rpoints,stroke := "red") %>%
# #' #           add_tooltip(all_values, "hover") %>%
# #' #           add_axis("y",title="Points") %>%
# #' #           add_axis("x",title="Games Played") %>%
# #' #           add_legend("fill",title="")
# #' #
# #' #
# #' #       prob with number of values for standings
# #' #
# #' #       standings %>%
# #' #         group_by(season,position) %>%
# #' #         tally() %>%
# #' #         filter(season=="2014/15")
# #' #
# #' #       1  2014/15        1  2
# #' #       2  2014/15        2 30
# #' #       3  2014/15        3 29
# #' #       4  2014/15        4 30
# #' #       5  2014/15        5 30
# #' #       6  2014/15        6 30
# #' #       7  2014/15        7 29
# #' #       8  2014/15        8 30
# #' #       9  2014/15        9 30
# #' #       10 2014/15       10 29
# #' #       11 2014/15       11 30
# #' #       12 2014/15       12 27
# #' #       13 2014/15       13 30
# #' #       14 2014/15       14 30
# #' #       15 2014/15       15 29
# #' #       16 2014/15       16 28
# #' #       17 2014/15       17 29
# #' #       18 2014/15       18 27
# #' #       19 2014/15       19 29
# #' #       20 2014/15       20 12
# #' #
# #' #       ## could be an issue with not same number of games?
# #' #       names(standings) # does not have round but does have tmYrGameOrder
# #' #
# #' #       data.frame(standings %>%
# #' #         group_by(season,tmYrGameOrder) %>%
# #' #         tally() %>%
# #' #         filter(season=="2014/15"))
# #' #
# #' #       # n is only 18?
# #' #
# #' #
# #' #       data.frame(standings %>%
# #' #                    group_by(season,tmYrGameOrder) %>%
# #' #                    tally() %>%
# #' #                    filter(season=="2013/14")) #38 20
# #' #
# #' #
# #' #       data.frame(standings %>%
# #' #                    group_by(season,team) %>%
# #' #                    tally() %>%
# #' #                    filter(season=="2014/15")) # missing Leicester and Chelsea - because they have played a game less
# #' #
# #' #       standings %>%
# #' #         filter(season=="2013/14"&team=="Chelsea")
# #' #
# #' #       both %>%
# #' #         filter(season=="2014/15"&team=="Chelsea") #29 vals
# #'
# #' ## most apps before scoring goal
# #'
# #' cats <- c("Head","Right","Left","6_Yd_Box","Pen_Area","Long_Range","Open","Corner","Indirect_FK",
# #'           "Direct_FK","Penalty","Throw")
# #' allCats <- data.frame(category=cats)
# #'
# #' output$goalFirsts <- DT::renderDataTable({
# #'   print("enter goalFirsts")
# #'   a <- goals %>%
# #'     left_join(playerGame, by="PLAYER_MATCH") %>%
# #' #    filter(PLAYERID==input$player&(START+subOn)>0) %>%
# #'     select(METHOD,PLACE,PLAY,plGameOrderApp)
# #'
# #'   if (nrow(a)>0) {
# #'     b <- a %>%
# #'       gather(dummy,category,-plGameOrderApp) %>%
# #'       arrange(plGameOrderApp)
# #'
# #'
# #'     for (i in 1:length(cats)) {
# #'
# #'       if (nrow(b %>% filter(category==cats[i])) >0) {
# #'         tempdf <- data.frame(b %>%
# #'                                filter(category==cats[i]) %>%
# #'                                slice(1))
# #'         tempdf <- tempdf[,c("category","plGameOrderApp")]
# #'
# #'         if (i!=1) {
# #'           df <- rbind(df,tempdf)
# #'         } else {
# #'           df <- tempdf
# #'         }
# #'       }
# #'       allApps <- nrow(playerGame %>% filter(PLAYERID==input$player&(START+subOn)>0))
# #'
# #'       a$since <- allApps- a$plGameOrderApp
# #'
# #'       a <- a %>%
# #'         mutate(since=allApps-plGameOrderApp) %>%
# #'         arrange(since)
# #'
# #'       c <- a %>%
# #'         gather(dummy,category,-c(plGameOrderApp,since))
# #'       #i <- 2
# #'       tempdf <- NULL
# #'       for (i in 1:length(cats)) {
# #'
# #'         if (nrow(c %>% filter(category==cats[i])) >0) {
# #'           tempdf <- data.frame(c %>%
# #'                                  filter(category==cats[i]) %>%
# #'                                  slice(1))
# #'           tempdf <- tempdf[,c("category","since")]
# #'
# #'           if (i!=1) {
# #'             dfSince <- rbind(dfSince,tempdf)
# #'           } else {
# #'             dfSince <- tempdf
# #'           }
# #'         }
# #'
# #'       }
# #'
# #'       ## also get all goals
# #'       allGoals <- b %>%
# #'         group_by(category) %>%
# #'         summarise(tot=n())
# #'
# #'       allCats <- data.frame(category=cats)
# #'
# #'       print("about to join")
# #'       print(allCats)
# #'       print(allGoals)
# #'       print(df)
# #'       print(dfSince)
# #'
# #'       tbl <- allCats %>%
# #'         left_join(allGoals) %>%
# #'         left_join(df) %>%
# #'         left_join(dfSince) %>%
# #'         rename(Category=category,Tot=tot,First=plGameOrderApp,Since=since)
# #'
# #'       print("success")
# #'       tbl$Tot <- ifelse(is.na(tbl$Tot),0,tbl$Tot)
# #'       tbl$First <- ifelse(is.na(tbl$First),0,tbl$First)
# #'       tbl$Since <- ifelse(is.na(tbl$Since),0,tbl$Since)
# #'       print(tbl)
# #'       # tbl
# #'     }
# #'   } else {
# #'     tbl <- data.frame(Category=cats,Tot=rep(0,12),First=rep("",12),Since=rep("",12))
# #'   }
# #'
# #'   DT::datatable(tbl,options= list(paging = FALSE, searching = FALSE, info=FALSE))
# #'
# #' }
# #'
# #' )
# #'
# #' names(playerGame)
# #' playerGame %>%
# #'   filter(PLAYERID=="HILLC2"&(START+subOn)>0) %>%
# #'   select(name,plGameOrder,Gls)
# #'
# #'
# #' playerGame %>%
# #'   filter(PLAYERID=="ROONEYX"&(START+subOn)>0) %>%
# #'   select(name,plGameOrder,Gls,gameDate) %>%
# #'   mutate(gameOrder=row_number(plGameOrder)) %>%
# #'   filter(Gls>0) %>%
# #'   slice(1)
# #'
# #'
# #' temp <-playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'   group_by(PLAYERID) %>%
# #'   select(name,plGameOrder,Gls,gameDate) %>%
# #'   mutate(gameOrder=row_number(plGameOrder)) %>%
# #'   filter(Gls>0) %>%
# #'   group_by(PLAYERID) %>%
# #'   slice(1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(gameOrder)) %>%
# #'   ungroup() %>%
# #'   mutate(rank=min_rank(-gameOrder)) %>%
# #'
# #'   filter(PLAYERID=="ABLETTG")
# #'
# #' ### probs arising ? DT needed dev shiny
# #'
# #' library(shiny)
# #' shinyApp(
# #'   ui = fluidPage(DT::dataTableOutput('tbl')),
# #'   server = function(input, output) {
# #'     output$tbl = DT::renderDataTable({
# #'       DT::datatable(iris, options = list(lengthChange = FALSE))
# #'     })
# #'   }
# #' )
# #'
# #' ## row selection
# #' library(DT)
# #' iris2 = head(iris, 20)
# #' datatable(
# #'   appendCheckboxes(iris2), escape = -7,
# #'   options = list(pageLength = 15, dom = 'tip')
# #' )
# #' #datatable creates widget# escape . normally defaults to all but want all but the
# #' # the 7th column i.e the checkbox - still sjows table but...
# #' #You are recommended to escape the table content for security reasons (e.g. XSS attacks) when using this function in Shiny or any other dynamic web applications
# #'
# #' # just use checkboxes as the "row names"
# #' datatable(
# #'   iris2, rownames = checkboxRows(iris2), escape = -1,
# #'   options = list(pageLength = 5, dom = 'tip')
# #' )
# #'
# #' # http://rstudio.github.io/DT/shiny.html also some stuff on
# #'
# #'
# #' ### do snippets from here as well
# #' lei never loosing by more than one
# #' glimpse()
# #'
# #' )
# #'
# #'
# #'   ## look at using checkboxes to get head to head data (and extend to other tables)
# #'   ## look at say Arsenal v reading
# #'   glimpse(teamGames)
# #'
# #' tm1 <-  teamGames %>%
# #'     filter(TEAMNAME=="Arsenal") %>%
# #'     select(MATCHID,venue,TEAMNAME,GF=GOALS,gameDate)
# #'
# #' tm2 <-  teamGames %>%
# #'   filter(TEAMNAME=="Reading") %>%
# #'   select(MATCHID,GA=GOALS)
# #'
# #' tm1 %>%
# #'   inner_join(tm2,by=c("MATCHID")) %>%
# #'   ungroup() %>%
# #'   arrange(desc(gameDate)) %>%
# #'   select(season=season.x,date=gameDate,venue,GF,GA) -> fixtures
# #'
# #'
# #'
# #' #### LOOK AT LINEUPS
# #'
# #' glimpse(playerGame)
# #'
# #' # eg matchid
# #'
# #' ## create a position for order
# #' positions <- data.frame(POSITION=c("G","D","M","F"), posOrder=c(1:4))
# #'
# #' write_csv
# #'
# #' positions <- read_csv("positions.csv")
# #'
# #' home <-playerGame %>%
# #'   filter(MATCHID==8797&venue=="H") %>%
# #'   select(name,LASTNAME,POSITION,st,on,off,Gls,PENS,CARD,Assists) %>%
# #'   left_join(positions) %>%
# #'   arrange(desc(st),posOrder,on,LASTNAME)
# #'
# #' ## split by venue prob easier to have two tables and then just put next to each other
# #' ## could add age (experience to date?) Look at images for cards and on\off
# #'
# #' df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
# #' df %>% spread(x, y) %>% gather(x, y, a:b, na.rm = TRUE)
# #'
# #'
# #'
# #' positions <- read_csv("positions.csv")
# #' library(tidyr)
# #'
# #'
# #'
# #'
# #' ### look at who played alongside
# #'
# #' #e.g rooneyx
# #'
# #' glimpse(playerGame)
# #'
# #' thePlayer <- "ROONEYX"
# #' thePlayer <- "GIGGSR"
# #' thePlayer <- "BENTM"
# #' thePlayer <- "BENTD"
# #'
# #' playerGame %>%
# #'   filter(PLAYERID==thePlayer&(START+subOn)>0)  %>% # 408 to 406
# #'   select(TEAMMATCHID) -> temp
# #'
# #'
# #' playerGame %>%
# #'   filter(TEAMMATCHID %in% temp$TEAMMATCHID&(START+subOn)>0) %>%
# #'   select(name,PLAYERID,TEAMNAME) %>%
# #'   group_by(PLAYERID,name) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n)) %>%
# #'   mutate(pc=round(100*n/max(n),1))-> allAlongside # ie not by team # Rooney has played with 119 players Giggs 137
# #' #bentm 213 games 195 diff players, darren bent 276 games 208 others
# #' ## player with most clubs
# #'
# #' a <- playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'     group_by(PLAYERID) %>%
# #'   select(TEAMMATCHID)
# #'
# #'
# #' by_species <- iris %>% group_by(Species)
# #' by_species %>% summarise_each(funs(length))
# #'
# #' playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'   group_by(PLAYERID) %>%
# #'   summarise_each(funs(length))
# #'
# #' playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'   group_by(PLAYERID,TEAMNAME) %>%
# #'   summarise_each(funs(length))
# #'
# #' playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'   group_by(PLAYERID,TEAMNAME) %>%
# #'   summarise() %>%
# #'   ungroup() %>%
# #'   group_by(PLAYERID) %>%
# #'   summarise(count=length(TEAMNAME)) %>%
# #'   ungroup() %>%
# #'   arrange(desc(count))
# #'
# #' #PLAYERID count
# #' 1     BENTM     8
# #' 2   BELLAMC     7
# #'
# #' ## should add win loss draw (poss add whether TEAMMATCHID is win loss )
# #'
# #' ## for all - just to find out most
# #'
# #' thePlayer <- "GIGGSR"
# #' ## played against the most
# #' playerGame %>%
# #'   filter(PLAYERID==thePlayer&(START+subOn)>0)  %>%
# #'   select(TEAMMATCHID,MATCHID) -> temp
# #'
# #'
# #' playerGame %>%
# #'   filter(MATCHID %in% temp$MATCHID&!(TEAMMATCHID %in% temp$TEAMMATCHID)&(START+subOn)>0) %>%
# #'   select(name,PLAYERID,TEAMNAME,Gls,Assists,CARD) %>%
# #'   group_by(PLAYERID,name) %>%
# #'   mutate(G=sum(Gls),A=sum(Assists)) %>%
# #'   ungroup() %>%
# #'   arrange(desc(n)) -> allAgainst
# #'
# #'
# #' ###
# #'
# #' #look at carousel idea for front page
# #'
# #'
# #' library(loryR)
# #'
# #' flickr_images <- list(
# #'   "https://farm4.staticflickr.com/3133/2288766662_c40c168b76_o.jpg"
# #'   ,"https://farm6.staticflickr.com/5309/5607717791_b030229247_o.jpg"
# #' )
# #'
# #' loryR( flickr_images, width = "50%", images_per_page = 1, options = list(rewind=T))
# #'
# #'
# #' library(shiny)
# #' app <- shinyApp(
# #'   ui = fluidPage(
# #'     numericInput("n", "n", 1),
# #'     plotOutput("plot")
# #'   ),
# #'   server = function(input, output) {
# #'     output$plot <- renderPlot( plot(head(cars, input$n)) )
# #'   }
# #' )
# #'
# #' runApp(app)
# #' }
# #'
# #'   ## this works for external images but not local
# #'   library(shiny)
# #'   library(loryR)
# #'   app <- shinyApp(
# #'     ui = fluidPage(
# #'       loryROutput("test", width = "100%", height = "400px")
# #'     ),
# #'     server = function(input, output) ({
# #'       flickr_images <- list(
# #'         "https://farm4.staticflickr.com/3133/2288766662_c40c168b76_o.jpg"
# #'         ,"https://farm6.staticflickr.com/5309/5607717791_b030229247_o.jpg",
# #'         "jpg1.jpg"
# #'       )
# #'     output$test <-  renderLoryR({
# #'       loryR( flickr_images, width = "50%", images_per_page = 1, options = list(rewind=T) )
# #'
# #'     })
# #'   })
# #'   )
# #'
# #'   runApp(app)
# #'
# #'   ## look at this for local images http://shiny.rstudio.com/articles/images.html
# #'
# #'   app <- shinyApp(
# #'     ui = fluidPage(
# #'       imageOutput("preImage")
# #'     ),
# #'
# #'       server=shinyServer(function(input, output, session) {
# #'         # Send a pre-rendered image, and don't delete the image after sending it
# #'         output$preImage <- renderImage({
# #'
# #'     filename <- normalizePath(file.path('./images','jpg1.jpg'))
# #'
# #'           # Return a list containing the filename and alt text
# #'           list(src = filename,
# #'                width = 200,
# #'                height = 200)
# #'
# #'         }, deleteFile = FALSE)
# #'
# #'   })
# #'   )
# #'
# #'   runApp(app)
# #'
# #'   ## looking at downloading photos from
# #'   http://www.premierleague.com/en-gb/players/index.html
# #'   # not obvious how to get to it
# #'
# #'   library(rvest)
# #'   library(XML)
# #'   url <-"http://www.premierleague.com/en-gb/players/index.html"
# #'
# #'   epl <- html(url)
# #'
# #'   photos are v small
# #'
# #'   http://www.premierleague.com/content/dam/premierleague/shared-images/players/p/patrick-van-aanholt/74230-p.jpg/_jcr_content/renditions/cq5dam.thumbnail.22.28.margin.png
# #'   http://www.premierleague.com/content/dam/premierleague/shared-images/players/p/patrick-van-aanholt/74230-p.jpg
# #'   http://www.premierleague.com/content/dam/premierleague/shared-images/players/p/patrick-van-aanholt/74230-p.jpg/_jcr_content/renditions/cq5dam.png
# #'
# #'
# #'  http://www.premierleague.com/content/dam/premierleague/shared-images/players/p/patrick-van-aanholt/74230-lsh.jpg does work though not head shot
# #'
# #'
# #'  tables = readHTMLTable(url, stringsAsFactors=FALSE)
# #'
# #'  names(tables)
# #'
# #'  tables[[1]] # just first 20 results names - no links
# #'
# #' html(url) %>%
# #'   html_nodes("td:nth-child(1)") %>%
# #'   html_text() #
# #' #[1] "\r\n                        Patrick van Aanholt\r\n       so tables way better for this
# #'
# #' srcs <-html(url) %>%
# #'   html_nodes("#body img") %>%
# #'   html_attr("src")
# #'
# #' # odd 20 but could
# #' [20] "/content/dam/premierleague/shared-images/players/t/toby-alderweireld/55605-p.jpg/_jcr_content/renditions/cq5dam.thumbnail.22.28.margin.png"
# #' [21] "http://adserver.adtech.de/adserv|3.0|327|4806553|0|168|ADTECH;cookie=info;loc=300"
# #'
# #' movie <- html("http://www.imdb.com/title/tt1490017/")
# #' cast <- html_nodes(movie, "#titleCast span.itemprop")
# #'
# #' test <- srcs[20]
# #'
# #' library(stringr)
# #'
# #' var <- str_split(test,"-p.jpg")[[1]][1] #/content/dam/premierleague/shared-images/players/t/toby-alderweireld/55605
# #' #var <- str_replace_all(var,"[0-9]","")
# #'
# #' imageUrl <- paste0("http://www.premierleague.com",var,"-lsh.jpg")
# #'
# #' the example image is 432X299 w h
# #'
# #'
# #'
# #'
# #'
# #' div class="herosection" widget="listToCarousel" wrap="null" visible="1" nextinput=".next-arrow" previnput=".prev-arrow">
# #'   <ul class="jcarousel-skin-hero carousel">
# #'
# #'   <li><img class="heroimg" src="/content/dam/premierleague/shared-images/players/r/rolando-aarons/155513-lsh.jpg" alt="Rolando Aarons"/></li>
# #'
# #'   </ul>
# #'   library(httr)
# #' baseURL <- "http://www.premierleague.com"
# #'   u <- "http://www.premierleague.com/en-gb/players/profile.career-history.html/rolando-aarons"
# #'   u.get<- GET(u)
# #' u.content=content(u.get, as="text")
# #' u.html <- htmlParse(u.content)
# #'   picLink <-xpathSApply(u.html, "//*/td[@class='primary_photo']/a/@href")
# #'   picLink <-xpathSApply(u.html, "//*/img[@class='heroimg']/@src")
# #'   unname(picLink)
# #'
# #'   newLink <- paste0(baseURL,unname(picLink))
# #'
# #'
# #'   #### quick look at Gannt diagrams
# #'
# #'   http://stackoverflow.com/questions/3550341/gantt-charts-with-r?rq=1
# #'
# #'   library(DiagrammeR)
# #'   library(tidyr)
# #'   library(dplyr)
# #'
# #'
# #'   df <- data.frame(task = c("task1", "task2", "task3"),
# #'                    status = c("done", "active", "crit"),
# #'                    pos = c("first_1", "first_2", "first_3"),
# #'                    start = c("2014-01-06", "2014-01-07", "after first_2"),
# #'                    end = c("2014-01-08", "3d", "5d"))
# #'
# #'   mermaid(
# #'     paste0(
# #'       # mermaid "header", each component separated with "\n" (line break)
# #'       "gantt", "\n",
# #'       "dateFormat  YYYY-MM-DD", "\n",
# #'       "title A Very Nice Gantt Diagram", "\n",
# #'       # unite the first two columns (task & status) and separate them with ":"
# #'       # then, unite the other columns and separate them with ","
# #'       # this will create the required mermaid "body"
# #'       paste(df %>%
# #'               unite(i, task, status, sep = ":") %>%
# #'               unite(j, i, pos, start, end, sep = ",") %>%
# #'               .$j,
# #'             collapse = "\n"
# #'       ), "\n"
# #'     )
# #'   )
# #'
# #'   df <- data.frame(task = c("task1", "task2", "task3"),
# #'                    status = c("done", "active", "crit"),
# #'                    pos = c("first_1", "first_2", "first_3"),
# #'                    start = c("2014-01-06", "2014-01-09", "after first_2"),
# #'                    end = c("2014-01-08", "3d", "5d"))
# #'   # this works prob want to zoom in and have interactivity as well if not just a table
# #'
# #'   df <- data.frame(task = c("Giggs", "Beckham", "Cantona"),
# #'                    status = c("done", "active", "crit"),
# #'                    pos = c("first_1", "first_2", "first_3"),
# #'                    start = c("2001-01-06", "2006-01-07", "2010-01-07"),
# #'                    end = c("2014-01-08", "2006-10-07", "2011-01-07"))
# #'
# #'   df <- data.frame(task = c("Giggs", "Beckham", "Cantona"),
# #'                    status = c("done", "active", "crit"),
# #'                    pos = c("1", "2", "3"),
# #'                    start = c("2001-01-06", "2006-01-07", "2010-01-07"),
# #'                    end = c("2014-01-08", "2006-10-07", "2011-01-07"))
# #'
# #'
# #'
# #'   # this does not work
# #'   df <- data.frame(task = c("Giggs", "Beckham", "Vantona"),
# #'                    status = c("Arsenal", "Chelsea", "West Ham"),
# #'                    pos = c("first_1", "first_2", "first_3"),
# #'                    start = c("2001-01-06", "2006-01-07", "2010-01-07"),
# #'                    end = c("2014-01-08", "2006-10-07", "2011-01-07"))
# #'
# #'
# #'
# #'   table <- read_csv("table_test.csv")
# #'   glimpse(table)
# #'
# #'   tbl <-table %>%
# #'     arrange(desc(ppg),desc(Pl))  %>%
# #'     filter(team=="Arsenal") %>%
# #'     select(Opponents=OppTeam,Pl,W,D,L,Pts,GF,GA,GD,ppg)
# #'   names(playerGame)
# #' test <-  playerGame %>%
# #'     filter(PLAYERID=="DEFOEJ"&(START+subOn)>0) %>%
# #'     group_by(Opponents) %>%
# #'     summarise(apps=n(),goals=sum(Gls))
# #'
# #'
# #' ### quick look at dzeko
# #'
# #' # names(summary)
# #' #
# #' # df <- summary %>%
# #' #   filter(season=="2014/15"&POSITION=="F") %>%
# #' #   mutate(goals=(StGls+subGls)) %>%
# #' #   ungroup() %>%
# #' #   select(name,goals,mins)
# #' #
# #' #
# #' #
# #' # df <- cbind(df, id = seq_len(nrow(df)))
# #' #
# #' #   # just need to add a tt and res for fill
# #' #   all_values <- function(x) {
# #' #     if(is.null(x)) return(NULL)
# #' #     row <- df[df$id == x$id,c("name","mins","goals")]
# #' #     paste0(names(row),": ", format(row), collapse = "<br />")
# #' #   }
# #' #
# #' # df  %>%
# #' #     ggvis(~mins,~goals, key:= ~ id) %>%
# #' #     layer_points() %>%
# #' #     add_tooltip(all_values, "click") %>%
# #' #     add_axis("x", title="Minutes played in EPL 2014/5: 160 Forwards")
# #'
# #'
# #' ## player at a glance
# #'
# #' summary %>%
# #'   filter(PLAYERID=="BENTD") %>%
# #'   mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
# #'   select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>%
# #'   ungroup() %>%
# #'   arrange(desc(Season)) %>%
# #'   select(-(c(name,LASTNAME,PLAYERID,born,left)))
# #'
# #'
# #' bent <- summary %>%
# #'   filter(PLAYERID=="BENTD")
# #' names(bent)
# #'
# #' teams <- length(unique(bent$TEAMNAME)) # 6
# #' seasons <- length(unique(bent$season))  #11
# #' apps <- sum(bent$St) + sum(bent$On)  #276
# #' goals <- sum(bent$StGls)+ sum(bent$subGls) #106
# #' assists <- sum(bent$Assists) #27
# #'
# #' ###
# #' issue with goals since
# #'
# #' a <- goals %>%
# #'   left_join(playerGame, by="PLAYER_MATCH") %>%
# #'   filter(PLAYERID=="CHAMAKM"&(START+subOn)>0) %>%
# #'   select(METHOD,PLACE,PLAY,plGameOrderApp)
# #'
# #' trueGames <- playerGame %>%
# #'   filter((START+subOn)>0) %>%
# #'   group_by(PLAYERID) %>%
# #'   mutate(trueGameOrder=row_number())
# #'
# #' a <- goals %>%
# #'   left_join(trueGames, by="PLAYER_MATCH") %>%
# #'   filter(PLAYERID=="CHAMAKM") %>%
# #'    select(METHOD,PLACE,PLAY,trueGameOrder)
# #'
# #'
# #'
# #' playerGame %>%
# #'   filter(PLAYERID=="CHAMAKM") %>%
# #'   select(START,subOn,plGameOrderApp)  %>% #128
# #'   filter((START+subOn)>0) #92  so need to go back to calc plaGameOrderApp calc and
# #' whther has implicatons elsewhere
# #'
# #'
# #' ### look at player timeline - pref with backing by team might be a ggplot
# #'
# #' names(playerGame)
# #'
# #' sprintf("<table cellpadding='4' style='line-height:1'><tr>
# #'                         <th>%1$s%2$s v %3$s </th></tr>
# #'
# #'                           <tr align='center'><td>%4$s</td></tr>
# #'
# #'                           </table>",
# #'         testBatting$Runs,
# #'         testBatting$asterisk,
# #'         testBatting$Opposition,
# #'         testBatting$Start.Date
# #' )
# #' names(playerGame)
# #' df <- playerGame %>%
# #'   mutate(tt= sprintf("<table cellpadding='4' style='line-height:1'><tr>
# #'                         <th>%1$s%2$s v %3$s </th></tr>
# #'
# #'                           <tr align='center'><td>%4$s</td></tr>
# #'
# #'                           </table>",
# #'                      gameDate,
# #'                      Opponents,
# #'                      on,
# #'                      off,
# #'                      Gls,
# #'                      Assists
# #'
# #'
# #'   ) )
# #'
# #' df <- playerGame %>%
# #'     filter(PLAYERID=="WALKERK") %>%
# #'     select(date=gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID) %>%
# #'     mutate(points=Goals+Assists)
# #'
# #' df <- cbind(df, id = seq_len(nrow(df)))
# #'
# #' all_values <- function(x) {
# #'   if(is.null(x)) return(NULL)
# #'   row <- df[df$id == x$id,c("date","Opponents","on","off","Goals","Assists") ]
# #'   paste0( names(row),": ",format(row), collapse = "<br />")
# #' }
# #'
# #'
# #'
# #'
# #' #names(df)
# #'
# #' df %>%
# #'  # filter(PLAYERID=="HENRYT") %>%
# #'   ggvis(~date, ~mins, key := ~id) %>%
# #'   layer_points(fill = ~Team, size = ~ points) %>%
# #'     add_tooltip(all_values,"hover") %>%
# #'
# #'   add_axis("y", title="Minutes Played") %>%
# #'   add_axis("x", title="Game in Squad") %>%
# #'     hide_legend("size")
# #'
# #'
# #'
# #'
# #'   data %>%
# #'     filter(ratings>49&year>=1950)  %>%
# #'     ggvis(~year,~av, key := ~id) %>%
# #'     layer_points(size:=10, opacity:= 0.2) %>%
# #'     add_tooltip(tt_a, "hover") %>%
# #'     add_axis("x", format="####") %>%
# #'     add_axis("y", title="Average User rating")
# #'   df <- cbind(df, id = seq_len(nrow(df)))
# #'
# #'   all_values <- function(x) {
# #'     if(is.null(x)) return(NULL)
# #'     row <- df[df$id == x$id,c("tt") ]
# #'     paste0( format(row), collapse = "<br />")
# #'   }
# #'
# #'
# #'
# #' ## quick look at fees
# #'   ## record could do think like my nba one?
# #'
# #' names(playerGame)
# #'
# #' test <-playerGame %>%
# #'   select(team=TEAMNAME,fee=FEE,joined,name,PLAYERID) %>%
# #'   unique() #6107
# #'
# #' ## restrict to bought in EPL history, and not 0 or 99
# #'
# #' test <-test %>%
# #'   filter(joined>"1992-05-15"&fee!=0&fee!=99) #2908 with no fee, 2627 exc 99s
# #'
# #' need to do something along these lines look at sportsblog
# #' dfMax <-data.frame(df[df$cumStat > cummax(c(-Inf, head(df$cumStat, -1))),])
# #' df[df$value > cummax(c(-Inf, head(df$value, -1))),]
# #' http://stackoverflow.com/questions/27050797/efficient-method-of-obtaining-successive-high-values-of-data-frame-column
# #'
# #'   test %>% arrange(joined)
# #'
# #'   ## actually probably allow pre premiership fees
# #'   prePrem <-playerGame %>%
# #'     select(team=TEAMNAME,fee=FEE,joined,name,PLAYERID) %>%
# #'     unique() %>%
# #'     filter(joined<"1992-08-15") %>%
# #'     arrange(desc(fee)) %>%
# #'      group_by(team) %>%
# #'     slice(1)  ## 33
# #'
# #'   postPrem <- playerGame %>%
# #'     select(team=TEAMNAME,fee=FEE,joined,name,PLAYERID) %>%
# #'     unique() %>%
# #'     filter(joined>"1992-08-15"&fee!=0&fee!=99)
# #'
# #'   allTrans <- rbind(prePrem,postPrem) %>% ungroup()
# #'
# #' ars <-  allTrans %>%
# #'     filter(team=="Arsenal") %>%
# #'     arrange(joined)
# #'
# #'
# #' str(ars)
# #'
# #' ars[ars$fee > cummax(c(-Inf, head(ars$fee, -1))),] %>%
# #'   ggvis(~joined,~fee) %>%
# #'   layer_points()
# #'
# #' df <- allTrans %>%
# #'   arrange(joined) %>%
# #'  filter(fee!=0&fee!=99) %>%
# #'   do(.[.$fee > cummax(c(-Inf, head(.$fee, -1))),])
# #'
# #'
# #' df$id <- 1:nrow(df)
# #'
# #' all_values <- function(x) {
# #'   if(is.null(x)) return(NULL)
# #'   row <- df[x$id == df$id,c("name","fee","joined","team") ]
# #'   paste0( names(row),": ",format(row), collapse = "<br />")
# #' }
# #'
# #'
# #' df %>%
# #'   ggvis(~joined,~fee/1000, key:= ~id) %>%
# #'   layer_points(fill =~team) %>%
# #'   add_tooltip(all_values,"hover") %>%
# #'   add_axis("x",title="Signed") %>%
# #'   add_axis("y", title="Fee (Million Pounds)")
# #'
# #'
# #'  ## for individual cubs
# #'
# #' unique(allTrans$team)
# #'
# #' df <- allTrans %>%
# #'   arrange(joined) %>%
# #'   filter(team=="Crystal P") %>%
# #'   filter(fee!=0&fee!=99) %>%
# #'   do(.[.$fee > cummax(c(-Inf, head(.$fee, -1))),])
# #'
# #'
# #' df$id <- 1:nrow(df)
# #'
# #' all_values <- function(x) {
# #'   if(is.null(x)) return(NULL)
# #'   row <- df[x$id == df$id,c("name","fee","joined") ]
# #'   paste0( names(row),": ",format(row), collapse = "<br />")
# #' }
# #'
# #'
# #' df %>%
# #'   ggvis(~joined,~fee/1000, key:= ~id) %>%
# #'   layer_points() %>%
# #'   add_tooltip(all_values,"hover") %>%
# #'   add_axis("x",title="Signed") %>%
# #'   add_axis("y", title="Fee (Million Pounds)")
# #'
# #'
# #' # poss add others ans but diff color
# #' ## click links to performance and fee out
# #'
# #'
# #' ## age by game with link to lineup
# #' names(playerGame)
# #' names(standings) # has team,gamaDate res
# #' head(playerGame$age)
# #' names(summary)
# #'
# #' temp <-playerGame %>%
# #'   group_by(TEAMNAME,gameDate,TEAMMATCHID,Opponents) %>%
# #'   filter(START>0&TEAMNAME=="Arsenal") %>%
# #'   select(name,age) %>%
# #'   summarize(avAge=extract_numeric(round(mean(age,na.rm=T),1)),max=floor(max(age,na.rm=T)),min=floor(min(age,na.rm=T)))
# #'
# #' ## 886 obs
# #'
# #' standings %>%
# #'   filter(team=="Arsenal") %>%
# #'   select(TEAMNAME=team,gameDate,res,season) %>%
# #'   inner_join(temp)  %>%
# #'   ggvis(~gameDate,~avAge) %>%
# #'   layer_points(size = 1, fill=~res)
# #'
# #' ## could also do boxplots
# #'
# #'
# #' standings %>%
# #'   filter(team=="Arsenal") %>%
# #'   select(TEAMNAME=team,gameDate,res,season) %>%
# #'   inner_join(temp)  %>%
# #'   group_by(season) %>%
# #'   ggvis(~season,~avAge) %>%
# #'   layer_boxplots()
# #'
# #' ## possibly could add a final position
# #'
# #'
# #' ## look at brush
# #'
# #'
# #' mtcars %>%
# #'   ggvis(x = ~wt, y = ~mpg, size.brush := 400) %>%
# #'   layer_points() %>%
# #'   handle_brush(function(items, page_loc, session, ...) {
# #'     show_tooltip(session, page_loc$r + 5, page_loc$t, html = nrow(items))
# #'   })
# #'
# #'
# #'
# #' ### get all lat lons by playerid 2500 only need to do over 2 days initially
# #' # http://www.movable-type.co.uk/scripts/latlong.html
# #' library(geosphere)
# #' names(playerGame)
# #'
# #' players <-
# #'  playerGame %>%
# #'   select(city,COUNTRY,PLAYERID) %>%
# #'   unique()
# #' i <- 1
# #'
# #'
# #' for (i in 25:nrow(players)){
# #'   #for (i in 1:2){
# #'     print(i)
# #'   tempdf <- geocode(paste0(players$city[i],", ",players$COUNTRY[i]))
# #'   tempdf <- cbind(playerID=players$PLAYERID[i],tempdf)
# #'    if (i!=1) {
# #'      df <- rbind(df,tempdf)
# #'    } else {
# #'      df <- tempdf
# #'    }
# #'  # print(head(df,1))
# #' }
# #'
# #'
# #' write_csv(df,'playerGeos.csv')
# #'
# #' ## then jsut link appropriately
# #'
# #' so for example old trafford
# #' 53.463203, -2.291362
# #'
# #' cantona
# #'
# #' 5.36978
# #' 43.29648
# #' library(geosphere)
# #' p1 <- c(-2.291362,53.463203)
# #' p2 <- c(5.36978,43.29648)
# #' distHaversine(p1, p2, r=6378137)/1000 #1264
# #'
# #' distance on google maps is 1591km
# #'
# #' looks good for relative purposes
# #'
# #'
# #' ###
# #'
# #' df <- read_csv("problem.csv")
# #'
# #' df %>%
# #'   select(-Pts) %>%
# #'   DT::datatable(rownames=TRUE,selection='single',options= list(paging = FALSE, searching = FALSE,info=FALSE))
# #'
# #'
# #' ###
# #' ata glance look
# #'
# #' seasons count - most recent
# #'
# #' theTeam <- "Arsenal"
# #' info <-standings %>%
# #'   filter(team==theTeam) %>%
# #'   group_by(season) %>%
# #'   filter(tmYrGameOrder==max(tmYrGameOrder)) %>%
# #'   ungroup()
# #'
# #' summary <- info %>%
# #'   summarize(years=n(),bestPos=min(position),wortsPos=max(position),maxPoints=max(cumPts),minPoints=min(cumPts))
# #'
# #' info %>%
# #'   ggvis(~final_Pos) %>%
# #'
# #'   names(playerGame)
# #'
# #'   leaders
# #' mostGames <- playerGame %>%
# #'   filter(TEAMNAME==theTeam) %>%
# #'   group_by(PLAYERID,name) %>%
# #'   filter((START+subOn)>0) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n))
# #'
# #' PLAYERID             name   n
# #' 1   PARLOUR      Ray Parlour 333
# #'
# #' #nb fore Own Goal
# #' mostGoals <- playerGame %>%
# #'   filter(TEAMNAME==theTeam&name!=" Own Goal") %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(sumGoals=sum(Gls)) %>%
# #'
# #'   ungroup() %>%
# #'   arrange(desc(sumGoals))
# #'
# #' mostAssists <- playerGame %>%
# #'   filter(TEAMNAME==theTeam) %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(sumAssists=sum(Assists)) %>%
# #'
# #'   ungroup() %>%
# #'   arrange(desc(sumAssists))
# #'
# #' mostCards <- playerGame %>%
# #'   filter(TEAMNAME==theTeam&CARD>"0") %>%
# #'   group_by(PLAYERID,name) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n))
# #'
# #'
# #' table(playerGame$CARD)
# #'
# #'
# #' test <- playerGame %>%
# #'   filter(CARD>"0")
# #'
# #' table(test$CARD)
# #'
# #'
# #' ## look at most common lineup
# #'
# #' names(playerGame)
# #' glimpse(playerGame)
# #'
# #' test <-playerGame %>%
# #'   filter(TEAMNAME=="Arsenal"&START>0&TEAMMATCHID==29399) %>%
# #'   arrange(PLAYERID)
# #' combo <- paste0(test$PLAYERID, collapse="_")
# #' "ADAMST_BERGKAD_DIXONL_GRIMANG_KEOWNM_MANNINA_OVERMAM_PARLOUR_SUKERD_VIEIRAP_WINTERN"
# #'
# #'
# #' test <-playerGame %>%
# #'   filter(TEAMNAME=="Arsenal"&START>0&TEAMMATCHID==29399) %>%
# #'   arrange(PLAYERID) %>%
# #'   do(data.frame(x=paste0(.$PLAYERID, collapse="_")))
# #' x
# #' 1 ADAMST_BERGKAD_DIXONL_GRIMANG_KEOWNM_MANNINA_OVERMAM_PARLOUR_SUKERD_VIEIRAP_WINTERN
# #'
# #'
# #' teamEver <- playerGame %>%
# #'   filter(TEAMNAME=="Crystal P"&START>0) %>%
# #'   select(PLAYERID,name) %>%
# #'   unique()
# #'
# #' test <-playerGame %>%
# #'   filter(TEAMNAME=="Crystal P"&START>0) %>%
# #'   group_by(TEAMMATCHID) %>%
# #'   arrange(PLAYERID) %>%
# #'   do(data.frame(x=paste0(.$PLAYERID, collapse="_")))
# #'
# #' ## cool
# #'
# #' df <-data.frame(sort(table(test$x)))
# #' colnames(df) <- c("combo","count")
# #'
# #' y <- sort(table(test$x))
# #'
# #' together <-y[[length(y)]] #10
# #' y[length(y)]
# #' eleven <-names(y[length(y)]) #[1] "ADAMST_ANELKAN_BERGKAD_DIXONL_KEOWNM_OVERMAM_PARLOUR_PETITE_SEAMAND_VIEIRAP_WINTERN"
# #'
# #' library(stringr)
# #' z <-str_split_fixed(eleven,"_",n=11)
# #' v <-data.frame(z)
# #' library(tidyr)
# #'
# #'  w <-gather(v)
# #'  w$key <- NULL
# #'  colnames(w)[2] <- "PLAYERID"
# #'
# #'  ## looks good
# #'  w %>%
# #'    left_join(teamEver) %>%
# #'    select(name) %>%
# #'    unique()
# #'
# #'
# #'
# #' ## histo
# #'
# #' position - little graph ()
# #' longets sequences
# #'
# #' glimpse(standings)
# #'
# #'
# #' ##
# #' look at map / exact and choropleth - start with latter as does not need geocode (have i done this yet
# #'                                                                                  )
# #' names(playerGame)
# #' byCountry <-playerGame %>%
# #'   filter(TEAMNAME=="Arsenal"&(START+subOn)>0) %>%
# #'    group_by(COUNTRY) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n)) %>%
# #'   mutate(pc=round(100*n/sum(n),1))
# #'
# #'
# #' mapData <- readOGR(dsn=".",
# #'                    layer = "ne_50m_admin_0_countries",
# #'                    encoding = "UTF-8",verbose=FALSE)
# #'
# #' names(mapData)
# #' mapData$name
# #' names(byCountry)
# #'
# #' byCountry <- data.frame(byCountry)
# #'
# #'
# #' countries2 <- sp::merge(mapData,
# #'                         byCountry,
# #'                         by.x = "name",
# #'                         by.y = "COUNTRY",
# #'                         sort = FALSE) ## 7 records to sort out
# #'
# #' setdiff(byCountry$COUNTRY,mapData$name)
# #'
# #' [1] "England"        "Ivory Coast"    "Wales"          "Czech Republic" "N. Ireland"     "Scotland"
# #' [7] "South Korea"
# #'
# #' ## will need to look at all countries - also can we split UK down into other countries
# #' ## alt could be
# #' ## also need to sort out all countries initially
# #'
# #' pal <- colorQuantile("Reds", NULL, n = 6)
# #'
# #' leaflet(data = countries2) %>%
# #'   addTiles() %>%
# #'   setView(lng=13,lat=56,zoom= 3) %>%
# #'   addPolygons(fillColor = ~pal(pc),
# #'               fillOpacity = 0.6,
# #'               color = "#BDBDC3",
# #'               weight = 1) %>%
# #'   #                   layerId = ~Country,
# #'   #                   popup = countries2$popUp) %>%
# #'   # addLegend(pal=pal, values = ~densityRange) %>%
# #'   #     addLegend(#colors = c(RColorBrewer::brewer.pal(6, "YlGnBu"), "#808080"),
# #'   #               colors = c(RColorBrewer::brewer.pal(6, "Reds"), "#808080"),
# #'   #               bins = 7,
# #'   #               position = 'bottomright',
# #'   #               title = "Density Range",
# #'   #               labels = labs) %>%
# #'   addLegend(pal=pal,values= ~pc) %>%
# #'   mapOptions(zoomToLimits="first")
# #'
# #' library(rgdal)
# #' ## this works but def want to set
# #'
# #' look alternatuvely at choroplethr::
# #'   library(choroplethr)
# #'
# #' playerGame %>%
# #'   filter(TEAMNAME=="Crystal P"&(START+subOn)>0) %>%
# #'   group_by(COUNTRY) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n)) %>%
# #'   mutate(pc=round(100*n/sum(n),1),region=tolower(COUNTRY)) %>%
# #'   rename(value=pc) %>%
# #'   country_choropleth()
# #'
# #' library(choroplethrMaps)
# #' ?country.map
# #' data(country.map)
# #'
# #' sort(unique(country.map$region)) #"united kingdom"
# #'
# #' test <-playerGame %>%
# #'   filter(TEAMNAME=="Arsenal"&(START+subOn)>0) %>%
# #'   group_by(COUNTRY) %>%
# #'   tally() %>%
# #'   ungroup() %>%
# #'   arrange(desc(n))
# #'
# #' test$COUNTRY
# #'
# #' test[test$COUNTRY=="England",]$COUNTRY <- "united kingdom"
# #' test[test$COUNTRY=="Wales",]$COUNTRY <- "united kingdom"
# #' test[test$COUNTRY=="Scotland",]$COUNTRY <- "united kingdom"
# #' test[test$COUNTRY=="N. Ireland",]$COUNTRY <- "united kingdom"
# #'
# #' test %>%
# #'   group_by(COUNTRY) %>%
# #'   summarize(ct=sum(n)) %>%
# #'   mutate(pc=round(100*ct/sum(ct),1),region=tolower(COUNTRY)) %>%
# #'   rename(value=pc) %>%
# #'   select(region,value) %>%
# #'   unique(.) %>%
# #'   country_choropleth(num_colors=1)
# #'
# #' ## mot that good = should be able to set some qunatiles
# #'
# #' ## works but lumps lots
# #'
# #'
# #' df <- read_csv("problem.csv")
# #' country_choropleth(df,num_colors=1)
# #' ## look for some pngs
# #'
# #' from twitter hastag
# #' http://coolestguidesontheplanet.com/wp-content/uploads/2012/05/arsenal1.png
# #'
# #'
# #' basic <- summary %>%
# #'   filter(PLAYERID=="BENTD")
# #'
# #' teams <- length(unique(basic$TEAMNAME)) # 6
# #' seasons <- length(unique(basic$season)) #11 but this is not being taken across
# #'
# #'
# #' bySeason <- summary %>%
# #'   filter(PLAYERID=="BENTD") %>%
# #'   ungroup() %>%
# #'   group_by(season) %>%
# #'   summarize(apps=sum(St+On), goals=sum(StGls+subGls),cards=sum(Y+R),assists=sum(Assists))
# #'
# #' ## look at a match summary particulary graphs goals
# #'
# #' home <-teamGames %>%
# #'   filter(MATCHID==1319&venue=="H")
# #'
# #' homeId <- home$TEAMMATCHID #20004
# #'
# #' homeGoals <- goals %>%
# #'   filter(TEAMMATCHID==homeId) %>%
# #'   arrange(TIME)
# #'
# #' gameTime <- data.frame(TIME=c(0:90))
# #'
# #' h <-homeGoals %>%
# #'   right_join(gameTime)
# #'
# #' h$gls <- 0
# #' h[!is.na(h$GOALS),]$gls <- 1
# #'
# #' # probably best to do with dygraphs? in final version
# #'
# #' h <-h %>%
# #'   mutate(cumGoals=cumsum(gls))
# #'  h %>%
# #'   ggvis(~TIME,~cumGoals) %>%
# #'   layer_lines()
# #'
# #' ## may need to do some jittering
# #'
# #' away <-teamGames %>%
# #'   filter(MATCHID==1319&venue=="A")
# #'
# #' awayId <- away$TEAMMATCHID #20004
# #'
# #' awayGoals <- goals %>%
# #'   filter(TEAMMATCHID==awayId) %>%
# #'   arrange(TIME)
# #'
# #' gameTime <- data.frame(TIME=c(0:90))
# #'
# #' a <-awayGoals %>%
# #'   right_join(gameTime)
# #'
# #' a$gls <- 0
# #' a[!is.na(a$GOALS),]$gls <- 1
# #'
# #' # probably best to do with dygraphs? in final version
# #'
# #' a <- a %>%
# #'   mutate(cumGoals=cumsum(gls))
# #' a %>%
# #'   ggvis(~TIME,~cumGoals) %>%
# #'   layer_lines()
# #'
# #' h$venue <- "Home"
# #' a$venue <- "Away"
# #'
# #' g <- rbind(h,a)
# #'
# #' g %>%
# #'   group_by(venue) %>%
# #'   ggvis(~TIME,~cumGoals) %>%
# #'   layer_lines(stroke= ~venue) %>%
# #'   add_axis("x",title="Minutes Played") %>%
# #'   add_axis("y",title="Goals Scored", format='d')
# #'
# #' could do a games by date sort of thing
# #'
# #' ## total team record
# #' table(standings$res) #Draw Loss  Win
# #' names(standings)
# #' W <-standings %>%
# #'   ungroup() %>% # grouped by season (might want to look at that)
# #'   filter(team=="Man. Utd."&res=="Win") %>%
# #'   tally() %>%
# #'   .$n
# #'
# #' L <-standings %>%
# #'   ungroup() %>% # grouped by season (might want to look at that)
# #'   filter(team=="Man. Utd."&res=="Loss") %>%
# #'   tally() %>%
# #'   .$n
# #'
# #' D <-standings %>%
# #'   ungroup() %>% # grouped by season (might want to look at that)
# #'   filter(team=="Man. Utd."&res=="Draw") %>%
# #'   tally() %>%
# #'   .$n
# #'
# #' avPts <- round((3*W+D)/(W+D+L),2)
# #'   W
# #'   D
# #'   L
# #' winPC <- round(100*W/(W+D+L),0)  #64
# #'
# #' )
# #'
# #'
# #' ## treansfers for player
# #'
# #'   names(summary)
# #'   sort(names(playerGame))
# #'
# #'   player <- "BENTD"
# #'   glimpse(playerGame)
# #'
# #' transfers <-  playerGame %>%
# #'     filter(PLAYERID==player&PERMANENT==1) %>%
# #'
# #'     select(name,joined,FEE,TEAMNAME,BIRTHDATE) %>%
# #'     unique() %>%
# #'     mutate(Cost=ifelse(FEE==0,0,FEE/1000)) %>%
# #'     rename(Fee=FEE,Team=TEAMNAME,Date=joined)
# #'
# #' transfers <- cbind(transfers, id = seq_len(nrow(transfers)))
# #'
# #'
# #'
# #' # looking at problem with crash
# #' transfers <- read_csv("problem.csv")
# #'
# #' transfers <- data.frame(transfers)
# #'
# #' all_values <- function(x) {
# #'   if(is.null(x)) return(NULL)
# #'   row <- transfers[transfers$id == x$id,c("Date","Team","Cost")]
# #'   paste0( names(row),": ",format(row), collapse = "<br />")
# #' }
# #'
# #' transfers   %>%
# #'     ggvis(~Date,~Cost, key:= ~id) %>%
# #'     layer_points() %>%
# #'     add_tooltip(all_values,"hover") %>%
# #'     add_axis("x",title="Joined Club") %>%
# #'     add_axis("y", title="Fee (million)")
# #'
# #' transfers   %>%
# #'   ggvis(~Date,~Cost, key:= ~id) %>%
# #'   layer_points() %>%
# #'   add_tooltip(all_values,"hover") %>%
# #'   add_axis("x", properties = axis_props(labels = list(
# #'     angle = 45, align = "left", fontSize = 11
# #'   )),title = "Joined Club",title_offset=50)
# #' add_axis("y", title="Fee (million)") %>%
# #'   # set_options(width=300,height=200) %>%
# #'   bind_shiny("playerTransfers")
# #'
# #' glimpse(transfers)
# #' str(transfers)
# #' df <- data.frame(date=as.Date("2014-07-10"),Cost=30)
# #' str(df)
# #' df %>%
# #'   ggvis(~date,~Cost) %>%
# #'   layer_points()
# #'
# #' df <- data.frame(date=as.Date(c("2014-07-10","2014-08-10")),Cost=c(30,40))
# #'
# #' df <- data.frame(date=as.Date("2014-07-10"),Cost=30)
# #' df %>%
# #'   ggvis(~date,~Cost) %>%
# #'   layer_points()  %>%
# #'   scale_numeric("y",domain=c(0,max(df$Cost))) %>%
# #'   scale_datetime("x", nice='year')
# #' df <- data.frame(date=as.POSIXlt("2014-07-10"),Cost=30)
# #'
# #' df %>%
# #'   ggplot(aes(date,Cost)) +
# #'   geom_bar(stat="identity")
# #'
# #'
# #'
# #' set.seed(2934)
# #' dat <- data.frame(
# #'   time = as.Date("2013-07-01") + 1:100,
# #'   value = seq(1, 10, length.out = 100) + rnorm(100)
# #' )
# #' p <- dat %>% ggvis(~time, ~value) %>% layer_points()
# #'
# #' # Start and end on month boundaries
# #' p %>% scale_datetime("x", nice = "month")
# #'
# #' p %>% scale_datetime("x")
# #' dist <- data.frame(times = as.POSIXct("2013-07-01", tz = "GMT") +
# #'                      rnorm(200) * 60 * 60 * 24 * 7)
# #' p <- dist %>% ggvis(x = ~times) %>% layer_histograms()
# #' p
# #'
# #' # Start and end on month boundaries
# #' p %>% scale_datetime("x", nice = "month")
# #'
# #' p %>% scale_datetime("x", utc = TRUE)
# #'
# #'
# #' ### another look at latlons - using and updating
# #'
# #' playerGeos <- read_csv("playerGeos.csv")  #4041
# #' names(playerGeos) #[1] "playerID" "lon"      "lat"
# #'
# #' #pgMini was originally done on a blog The
# #'
# #' pgMini <- playerGame %>%  ## so wil only show those that have made an appearance - but that is prob ok
# #'     select(playerID=PLAYERID,name) %>%
# #'   unique() %>%
# #'   left_join(playerGeos)
# #'
# #'
# #' ## do a team one - as well as existing choropleth - perhaps change by year as well
# #' ## weight according to games played or current
# #'
# #'
# #' summary %>%
# #'   filter(TEAMNAME==theTeam&is.na(left)) %>%
# #'
# #'   mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
# #'   group_by(PLAYERID,name,pos) %>%
# #'   select(name,pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
# #'   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
# #'             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))) -> tbl
# #'
# #'  theTeam <- "Arsenal"
# #' temp <-  summary %>%
# #'     ungroup() %>%
# #'     filter(TEAMNAME==theTeam) %>%
# #'     mutate(Apps=St+On) %>%
# #'     select(name,PLAYERID,Apps) %>%
# #'     group_by(name,PLAYERID) %>%
# #'     summarize(Apps=sum(Apps)) %>%
# #'     ungroup()
# #'
# #' #temp <- data.frame(temp)
# #'
# #'
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), temp$Apps,  pretty = TRUE)
# #'
# #' binpal <- colorNumeric("Reds", c(0,max(temp$Apps)))
# #'
# #' binpal <- colorNumeric("Reds", c(0,sqrt(max(temp$Apps))))# 0-18
# #'
# #' ## this is what I used in climate locations
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), df$operational,  pretty = TRUE)
# #'
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), temp$Apps,  pretty = TRUE)
# #'
# #' #binpal <- colorNumeric("Blues", c(0,max(temp$Apps)/100)) 3.33
# #'
# #' temp$popup <-
# #'   sprintf(
# #'     "<table cellpadding='4' style='line-height:1'><tr>
# #'       <th> %1$s  </th></tr>
# #'
# #'       <tr align='center'><td>Apps: %2$s</td></tr>
# #'
# #'
# #'
# #'       </table>",
# #'     temp$name,
# #'     temp$Apps
# #'   )
# #'
# #' # not showing colors prop
# #' ## from locations
# #' df <- read_csv("problem.csv")
# #' str(df) #tbl_df inc popup chr,operational int - with attributes lat and lon, numbers
# #' temp <-temp %>%
# #'   left_join(pgMini)
# #' str(temp) #tbl_df, popup chr,Aps num, lon and lat numbers
# #'
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), df$operational,  pretty = TRUE)
# #'
# #' # write_csv(df,"problem.csv")
# #'
# #' df %>%    leaflet() %>%
# #'   addTiles() %>%
# #'   addCircleMarkers(
# #'     radius = 4,fillOpacity = 0.5,popup =  ~ popup,layerId =  ~ stationId,color = ~ binpal(operational)
# #'   )  %>%
# #'
# #'   addLegend(
# #'     pal = binpal,values = ~ operational, position = 'bottomleft',title = "Years Operational"
# #'   ) # here does look diff with most if not all 0-50 (maybe had 0-100 in climate )
# #'
# #'
# #' str(a)
# #' temp <- temp %>%
# #'     left_join(pgMini)
# #' temp %>%
# #'     leaflet() %>%
# #'     addTiles() %>%
# #'    # setView(theLon,theLat, zoom=9) %>%
# #'     addCircleMarkers(radius=4, fill = ~ binpal(Apps), popup= ~popup, fillOpacity = 0.5) %>%
# #' addLegend(
# #'   pal = binpal,values = ~ Apps, position = 'bottomleft',title = "Apps"
# #' )
# #'
# #'
# #' need to readdress
# #'
# #'   ##? legend not appearing
# #'
# #'   binpal <-
# #'     colorBin(c("#FFFF00","#FF8000","#FF0000"), rawData$mag,  pretty = TRUE)
# #'
# #'   # binpal$bins
# #'
# #'
# #'   addTiles() %>%
# #'     addCircles(
# #'       radius = 5,popup =  ~ popup,color = ~ binpal(mag)
# #'     ) %>%
# #'     addLegend(
# #'       pal = binpal,values = ~ mag, position = 'bottomleft',title = "Magnitude"
# #'     )
# #'
# #'
# #'   names(standings)
# #' wk1 <-  standings %>%
# #'     filter(tmYrGameOrder==1) %>%
# #'     group_by(team) %>%
# #'     summarize(pts=sum(points), games=n(),ppg=round(pts/games,2)) %>%
# #'     ungroup() %>%
# #'     arrange(desc(pts))
# #'
# #' wk1a <-standings %>%
# #'   filter(tmYrGameOrder==1&final_Pos==1) %>%
# #'   select(season,team,res)
# #'   ##
# #'
# #'   glimpse(standings)
# #'
# #'
# #'   ## graph of goals for each season
# #'
# #' For <-  Goals_team_for %>%
# #'     filter(team=="Arsenal") %>%
# #'     select(team,season,For=Tot)
# #'
# #' Ag <-  Goals_team_ag %>%
# #'   filter(opponent=="Arsenal") %>%
# #'   select(team=opponent,season,Ag=Tot) %>%
# #'   inner_join(For)
# #'
# #' # should be tidier
# #' Both <- Ag %>%
# #'   gather(type,goals,-season) %>%
# #'   filter(type!="team")
# #'
# #' Both$type <- as.character(Both$type)
# #' Both$goals <- as.integer(Both$goals)
# #'
# #' str(Both)
# #'
# #' Both %>%
# #'   group_by(type) %>%
# #'   ggvis(~season,~goals, stroke= ~ type) %>%
# #'   layer_lines()
# #'
# #'
# #' Both %>%
# #'   #group_by(type) %>%
# #'   ggplot(aes(season,goals, group=type, fill=type))+ geom_area(alpha=0.5, position="identity")  #geom_area(stat="identity")
# #'
# #' ## final pos after win away
# #' names(standings)
# #' standings %>%
# #'   filter(res=="Win"&tmYrGameOrder==1&venue=="A") %>%
# #'   ungroup() %>%
# #'   arrange(desc(final_Pos)) %>%
# #'   ggvis(~final_Pos)
# #'
# #' ## or loss at home
# #' standings %>%
# #'   filter(res=="Loss"&tmYrGameOrder==1&venue=="H") %>%
# #'   ungroup() %>%
# #'   arrange(desc(final_Pos)) %>%
# #'   ggvis(~final_Pos)
# #'
# #' ## could look at av as well
# #'
# #'
# #' ## revist the compare player comp
# #'
# #'
# #' ```{r, echo=FALSE,warning=FALSE,message=FALSE,fig.width = 18, fig.height = 16}
# #'
# #' inputPanel(
# #'   selectizeInput("players_b","Select two or more players",playerChoice,selected=c("GERRARS","LAMPARF"),multiple=TRUE, options=list(maxOptions=7600)),
# #'   radioButtons("category_b","Category",choices=c("Goals","Assists","Points")),
# #'   radioButtons("time_b","x-axis",choices=c("Apps","Age","Date"))
# #' )
# #'
# #'
# #' players <- c("PUNCHEJ","LALLANA")
# #' category <- "Points"
# #' time <- "Apps"
# #'
# #'
# #'
# #' data_b <- reactive({
# #'
# #'   if(is.null(input$category_b)) {
# #'     cat <- "Goals"
# #'   } else {
# #'     cat <- input$category_b
# #'   }
# #'
# #'   if(is.null(input$time_b)) {
# #'     time <- "Apps"
# #'   } else {
# #'     time <- input$time_b
# #'   }
# #'
# #'   if (is.null(input$players_b)) {
# #'     players <- c("GERRARS","LAMPARF")
# #'   } else {
# #'     players <- input$players_b
# #'   }
# #'
# #'   print(players)
# #'
# #'   temp <-playerGame %>%
# #'     filter(PLAYERID %in% players &(START+subOn)>0) %>%
# #'     select(gameDate,PLAYERID,name,plGameOrder,age,Gls,Assts=Assists) %>%
# #'     group_by(PLAYERID)  %>%
# #'     mutate(cumGoals=cumsum(Gls),cumAssists=cumsum(Assts),Points=cumGoals+cumAssists)
# #'   temp <- tbl_df(temp)
# #'   info=list(temp=temp,cat=cat,time=time)
# #'   return(info)
# #'
# #' })
# #' # this works but then got warning and bottom of graph cut off works when clicking dial
# #' observe({
# #'   df <- data.frame(data_b()$temp) %>%
# #'     rename(Apps=plGameOrder,Goals=cumGoals,Age=age,Date=gameDate,Assists=cumAssists)
# #'   print("enter observe")
# #'   print(glimpse(df))
# #'   print(nrow(df))
# #'   cat <- data_b()$cat
# #'   time <- data_b()$time
# #'   # works but need to check when publishing as sometimes bottom of cghart missing
# #'   # think neeed aes_string(paste("~",input$category3,",") type here or as.name(col)
# #'   # see snippetWD throws probs still
# #'   # For now
# #'   if(time=="Apps"){
# #'     if (cat=="Points") {
# #'       df %>%
# #'         ggvis(~Apps,~Points) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else if(cat=="Goals") {
# #'       df %>%
# #'         ggvis(~Apps,~Goals) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else {
# #'       df %>%
# #'         ggvis(~Apps,~Assists) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'     }
# #'   } else if(time=="Age"){
# #'     if (cat=="Points") {
# #'       df %>%
# #'         ggvis(~Age,~Points) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else if(cat=="Goals") {
# #'       df %>%
# #'         ggvis(~Age,~Goals) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else {
# #'       df %>%
# #'         ggvis(~Age,~Assists) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'     }
# #'
# #'   } else {
# #'
# #'     if (cat=="Points") {
# #'       df %>%
# #'         ggvis(~Date,~Points) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else if(cat=="Goals") {
# #'       df %>%
# #'         ggvis(~Date,~Goals) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'
# #'     } else {
# #'       df %>%
# #'         ggvis(~Date,~Assists) %>%
# #'         group_by(name) %>%
# #'         layer_lines(stroke=~name) %>%
# #'         set_options(width = "auto", height = 400, resizable=FALSE) %>%
# #'         bind_shiny('stars')
# #'     }
# #'
# #'   }
# #'
# #' })
# #' ggvisOutput('stars')
# #'
# #'
# #'
# #' players <- c("PUNCHEJ","LALLANA")
# #' category <- "Points"
# #' time <- "Apps"
# #'
# #'
# #' temp <-playerGame %>%
# #'   filter(PLAYERID %in% players &(START+subOn)>0) %>%
# #'   select(gameDate,PLAYERID,name,plGameOrder,age,Gls,Assts=Assists) %>%
# #'   group_by(PLAYERID)  %>%
# #'   mutate(AppOrder = row_number(),cumGoals=cumsum(Gls),cumAssists=cumsum(Assts),Points=cumGoals+cumAssists)
# #'
# #'
# #' temp %>%
# #'   ggvis(~AppOrder,~Points) %>%
# #'   group_by(name) %>%
# #'   layer_lines(stroke=~name) %>%
# #'
# #'
# #'  # look at diff for transfer cost
# #'
# #'   cost <- read_csv("problem.csv")
# #'
# #' devtools::install_github("hrbrmstr/taucharts")
# #'
# #' library(taucharts)
# #'
# #' # current verison
# #' packageVersion("taucharts")
# #' #> [1] '0.3.3'
# #'
# #' library(taucharts)
# #' library(testthat)
# #'
# #' date()
# #' #> [1] "Thu Aug  6 18:01:57 2015"
# #'
# #' test_dir("tests/")
# #' names(cost)
# #'
# #' min(cost$Date)
# #' tauchart(cost) %>%
# #'
# #'   tau_point("Date", "Cost", color="Team") %>%
# #'  # tau_legend() %>%
# #'   tau_tooltip(fields=c("Team","Date","Cost")) %>%
# #'   tau_guide_y(label="Fee (million)")  %>%
# #'   tau_guide_x(label="Purchase Date",tick_format="%Y", min=min(cost$Date))
# #'
# #' data(cars_data)
# #' tauchart(cars_data) %>%
# #'   tau_point("milespergallon", c("class", "price"), color="class") %>%
# #'   tau_tooltip(c("vehicle", "year", "class", "price", "milespergallon"))
# #'
# #'
# #' how_good <- structure(list(
# #'   Ranking = structure(
# #'     1:5, .Label = c("Bottom", "Bellow Average",
# #'                     "Average", "Above Average", "Top"), class = "factor"
# #'   ), Prob = c(30L, 10L, 15L, 40L, 5L)/100
# #' ), .Names = c("Ranking", "Prob"), row.names = c(NA,-5L), class = "data.frame")
# #'
# #' # NOTE my division by 100 in Prob above
# #'
# #' tauchart(how_good) %>%
# #'   tau_bar(x="Ranking", y="Prob") %>%
# #'   tau_guide_y(tick_format="m")
# #'
# #' ## back to teams players by location
# #'
# #' names(pgMini)
# #' [1] "PLAYERID" "name"     "lon"      "lat"
# #' pgMini
# #'
# #' theTeam <- "Arsenal"
# #' temp <-  summary %>%
# #'   ungroup() %>%
# #'   filter(TEAMNAME==theTeam) %>%
# #'   mutate(Apps=St+On) %>%
# #'   select(name,PLAYERID,Apps) %>%
# #'   group_by(name,PLAYERID) %>%
# #'   summarize(Apps=sum(Apps)) %>%
# #'   ungroup()
# #'
# #' names(temp) #[1] "name"     "PLAYERID" "Apps"
# #'
# #' temp <-temp %>%
# #'   left_join(pgMini)
# #'
# #'   mutate(lon=jitter(lon),lat=jitter(lat))
# #'
# #' temp$lon <-  jitter(temp$lon, amount=0.5)
# #' temp$lat <-  jitter(temp$lat, amount=0.5)
# #' str(temp) #tbl_df, popup chr,Aps num, lon and lat numbers
# #'
# #' temp$popup <-
# #'   sprintf(
# #'     "<table cellpadding='4' style='line-height:1'><tr>
# #'       <th> %1$s  </th></tr>
# #' <tr align='center'><td> %2$s</td></tr>
# #'
# #'       <tr align='center'><td>Apps: %3$s</td></tr>
# #'
# #'
# #'
# #'       </table>",
# #'     temp$name,
# #'     temp$place,
# #'     temp$Apps
# #'   )
# #'
# #' binpalEPL <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), temp$Apps,  pretty = TRUE)
# #'
# #'
# #' ## this is what I used in climate locations
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), df$operational,  pretty = TRUE)
# #'
# #' alternatives are something like
# #' binpalEPL <- colorNumeric("Reds", c(0,max(temp$Apps)))
# #'
# #' binpalEPL <- colorNumeric("Reds", c(0,sqrt(max(temp$Apps))))# 0-1
# #'
# #' df %>%    leaflet() %>%
# #'   addTiles() %>%
# #'   addCircleMarkers(
# #'     radius = 4,fillOpacity = 0.5,popup =  ~ popup,layerId =  ~ stationId,color = ~ binpal(operational)
# #'   )  %>%
# #'
# #'   addLegend(
# #'     pal = binpal,values = ~ operational, position = 'bottomleft',title = "Years Operational"
# #'   ) # here does look diff with most if not all 0-50 (maybe had 0-100 in climate )
# #'
# #'
# #' str(a)
# #' temp <- temp %>%
# #'   left_join(pgMini)
# #' temp %>%
# #'   leaflet() %>%
# #'   addTiles() %>%
# #'   # setView(theLon,theLat, zoom=9) %>%
# #'   addCircleMarkers(radius=4, fill = ~ binpal(Apps), popup= ~popup, fillOpacity = 0.5) %>%
# #'   addLegend(
# #'     pal = binpal,values = ~ Apps, position = 'bottomleft',title = "Apps"
# #'   )
# #'
# #'
# #' climate <- read_csv("climateProblem.csv")
# #' problems()
# #' Error in withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")) :
# #'   argument "x" is missing, with no default
# #'
# #' str(climate) $operational is an integer
# #'
# #' binpal <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), climate$operational,  pretty = TRUE)
# #'
# #'
# #' climate %>%    leaflet() %>%
# #'   addTiles() %>%
# #'   addCircleMarkers(
# #'     radius = 4,fillOpacity = 0.5,popup =  ~ popup,layerId =  ~ stationId,color = ~ binpal(operational)
# #'   )  %>%
# #'
# #'   addLegend(
# #'     pal = binpal,values = ~ operational, position = 'bottomleft',title = "Years Operational"
# #'   ) # inc nice legend( though maybe as limited)
# #'
# #' #paris 48.8567° N, 2.3508° E
# #' temp %>%    leaflet() %>%
# #'   addTiles() %>%
# #'   setView(2,49,zoom=3) %>%
# #'   addCircleMarkers(
# #'     radius = 3,fillOpacity = 0.5,popup =  ~ popup,color = ~ binpalEPL(Apps)
# #'   ) %>%
# #'   ## legend does not appear??
# #'   addLegend(
# #'     pal = binpalEPL,values = ~ Apps, position = 'bottomleft',title = "Apps"
# #'   )
# #'
# #'
# #' #### issue with lineups
# #'
# #' teamEver <- playerGame %>%
# #'   filter(TEAMNAME=="Bradford C"&START>0) %>%
# #'   select(PLAYERID,name) %>%
# #'   unique()
# #'
# #' ## has quite a few with PLAYERIDs in lowercase is that it??
# #'
# #' # create a character vector which as ordered by id
# #' # can be assessed for how common it is
# #' test <-playerGame %>%
# #'   filter(TEAMNAME=="Bradford C"&START>0) %>%
# #'   group_by(TEAMMATCHID) %>%
# #'   arrange(PLAYERID) %>%
# #'   do(data.frame(x=paste0(.$PLAYERID, collapse="_")))
# #'
# #' head(test,1) #BEAGRIP_REDFEAN_dreyerj_jacobsw_obriena_whalleg_windasd  ## only 7 players teammatchid 29304
# #' playerGame %>%
# #'   filter(TEAMMATCHID==29304) # only 9 with 2 not starting WTF
# #'
# #' in access has 16 inc 11 starters
# #' in local sql
# #'
# #'
# #' SELECT        soccer.tblPlayers.FIRSTNAME, soccer.tblPlayers.LASTNAME, soccer.tblPlayers.BIRTHDATE, soccer.tblPlayers.COUNTRY, soccer.tblPlayers.PLACE, soccer.tblPlayers.POSITION, soccer.tblPlayerClub.PLAYERID,
# #' soccer.tblPlayerClub.FEE, soccer.tblPlayerClub.[PERMANENT], soccer.tblPlayerClub.[LEFT], soccer.tblPlayerClub.JOINED, soccer.tblPlayer_Match.START, soccer.tblPlayer_Match.[OFF],
# #' soccer.tblPlayer_Match.[ON], soccer.tblPlayer_Match.GOALS, soccer.tblPlayer_Match.PENS, soccer.tblPlayer_Match.CARD, soccer.tblPlayer_Match.OwnGoal, soccer.tblPlayer_Match.MissedPenalty,
# #' soccer.tblPlayer_Match.PLAYER_MATCH, soccer.tblMatchTeam.TEAMMATCHID, soccer.tblMatchTeam.[HOME/AWAY], soccer.tblMatch.DATE, soccer.tblMatch.MATCHID
# #' FROM            soccer.tblPlayers INNER JOIN
# #' soccer.tblPlayerClub ON soccer.tblPlayers.PLAYERID = soccer.tblPlayerClub.PLAYERID INNER JOIN
# #' soccer.tblPlayer_Match ON soccer.tblPlayerClub.PLAYER_TEAM = soccer.tblPlayer_Match.PLAYER_TEAM INNER JOIN
# #' soccer.tblMatchTeam ON soccer.tblPlayer_Match.TEAMMATCHID = soccer.tblMatchTeam.TEAMMATCHID INNER JOIN
# #' soccer.tblMatch ON soccer.tblMatchTeam.MATCHID = soccer.tblMatch.MATCHID
# #'
# #'
# #' print(glimpse(test))
# #' # count of lineups
# #' y <- sort(table(test$x))
# #'
# #' together <-y[[length(y)]]
# #' print(together)
# #' # now extract names
# #' eleven <-names(y[length(y)])
# #'
# #' z <-str_split_fixed(eleven,"_",n=11)
# #' v <-data.frame(z)
# #'
# #'
# #' lineup <-gather(v)
# #' lineup$key <- NULL
# #'
# #' print(glimpse(lineup))
# #'
# #' colnames(lineup)[1] <- "PLAYERID"
# #'
# #'
# #' lineup <- lineup %>%
# #'   left_join(teamEver) %>%
# #'   select(name) %>%
# #'   unique()
# #' print(glimpse(lineup))
# #' print(str(lineup))
# #'
# #' lineupText <- paste(lineup$name,collapse=", ")
# #' print(lineupText)
# #'
# #'
# #'
# #'
# #'
# #' SELECT        soccer.tblPlayers.FIRSTNAME, soccer.tblPlayers.LASTNAME, soccer.tblPlayers.BIRTHDATE, soccer.tblPlayers.COUNTRY, soccer.tblPlayers.PLACE, soccer.tblPlayers.POSITION, soccer.tblPlayerClub.PLAYERID,
# #' soccer.tblPlayerClub.FEE, soccer.tblPlayerClub.[PERMANENT], soccer.tblPlayerClub.[LEFT], soccer.tblPlayerClub.JOINED, soccer.tblPlayer_Match.START, soccer.tblPlayer_Match.[OFF],
# #' soccer.tblPlayer_Match.[ON], soccer.tblPlayer_Match.GOALS, soccer.tblPlayer_Match.PENS, soccer.tblPlayer_Match.CARD, soccer.tblPlayer_Match.OwnGoal, soccer.tblPlayer_Match.MissedPenalty,
# #' soccer.tblPlayer_Match.PLAYER_MATCH, soccer.tblMatchTeam.TEAMMATCHID, soccer.tblMatchTeam.[HOME/AWAY], soccer.tblMatch.DATE, soccer.tblMatch.MATCHID, soccer.tblTeam_Names.TEAMNAME
# #' FROM            soccer.tblPlayers INNER JOIN
# #' soccer.tblPlayerClub ON soccer.tblPlayers.PLAYERID = soccer.tblPlayerClub.PLAYERID INNER JOIN
# #' soccer.tblPlayer_Match ON soccer.tblPlayerClub.PLAYER_TEAM = soccer.tblPlayer_Match.PLAYER_TEAM INNER JOIN
# #' soccer.tblMatchTeam ON soccer.tblPlayer_Match.TEAMMATCHID = soccer.tblMatchTeam.TEAMMATCHID INNER JOIN
# #' soccer.tblMatch ON soccer.tblMatchTeam.MATCHID = soccer.tblMatch.MATCHID INNER JOIN
# #' soccer.tblTeam_Names ON soccer.tblMatchTeam.TEAMID = soccer.tblTeam_Names.TEAMID
# #'
# #' executes with 289403 rows ## tblPlayer_Match has 289474
# #'
# #'  with  WHERE soccer.tblPlayer_Match.TEAMMATCHID = 29304 has 16
# #'
# #'  confirmed in updatingaql playerGameTest that only 9 came across so need to work backwards
# #'  ?? is there a common insertSource
# #'
# #' probs <- playerGame %>%
# #'    filter(START>0) %>%
# #'    group_by(TEAMMATCHID) %>%
# #'    tally() %>%
# #'    filter(n!=11)
# #'
# #' ouch 1535 occurrences confirms 29304 as 7
# #'
# #' just playermatch
# #' START PLAYER_TEAM
# #' 1      6        3678
# #' 2      6        3679
# #' 3      6        3674
# #' 4      6        3680
# #' 5      6        3668
# #' 6      6        3676
# #' 7      6        3681
# #' 8      0        3651
# #' 9      6        3672
# #' 10     6        3671
# #' 11     6        3673
# #' 12     6        3657
# #' 13     0        3669
# #' 14     0        3677
# #' 15     0        3670
# #' 16     0        3675
# #'
# #'
# #' players.PLAYERID=playerClub.PLAYERID is issue
# #'
# #'       6        3680
# #' 2     6        3668
# #' 3     6        3676
# #' 4     6        3681
# #' 5     0        3651
# #' 6     6        3671
# #' 7     6        3673
# #' 8     6        3657
# #' 9     0        3669
# #'
# #' so 3678,3679,3674,3672,3677,3670,3675
# #'
# #' walshg
# #' wetherd
# #' millsl
# #' halleg
# #' toddl
# #' clarkem
# #' MOORED but he is moored in tblPlayers
# #'
# #'
# #'    ## season hist do ggplot2 so that current season can be shown
# #'
# #' observeEvent(teamData(),{
# #'
# #'   df <-  teamData()$test %>%
# #'     filter(season!="2015/16")
# #'   print(glimpse(df))
# #'
# #'   df  %>%
# #'
# #'     ggvis(~final_Pos) %>%
# #'     set_options(width=220,height=220) %>%
# #'     add_axis("y", title="Seasons", format='d') %>%
# #'     add_axis("x", title="Position", format='d') %>%
# #'     bind_shiny('seasonsHist')
# #'
# #' })
# #'
# #' ## like old epl sequences - which may want to dos anyways
# #'
# #' df <- read_csv("problem.csv")
# #'
# #' df  %>%
# #'
# #'   ggvis(~final_Pos) %>%
# #'   set_options(width=220,height=220) %>%
# #'   add_axis("y", title="Seasons", format='d') %>%
# #'   add_axis("x", title="Position", format='d')
# #'
# #' df %>%
# #'   ggplot(aes(final_Pos)) +
# #'   geom_histogram
# #'
# #' http://stackoverflow.com/questions/13372523/altering-the-color-of-one-value-in-a-ggplot-histogram
# #' names(df)
# #' cond <- df$season =="2014/15"
# #'
# #' maxSeason <-df %>%
# #'   group_by(final_Pos) %>%
# #'   tally()
# #'
# #' theMax <- max(maxSeason$n)
# #'
# #' sessionInfo()
# #' ggplot(df, aes(x=final_Pos)) +
# #'   geom_histogram(data=subset(df,cond==FALSE), binwidth=0.5, fill="blue", alpha=0.5) +
# #'   geom_histogram(data=subset(df,cond==TRUE), binwidth=0.5, fill="blue") +
# #'   scale_x_continuous(breaks=df$final_Pos+0.25, labels=df$final_Pos) +
# #'   #scale_y_continuous(breaks=pretty_breaks()) + ## could not find pretty_breaks though scales is loaded
# #'   # needs to be first even so did not work
# #'   scale_y_discrete(breaks= c(0,theMax)) +
# #'   theme_bw() +
# #'   xlab("Position (2015/6 in bold)") +
# #'   ylab("Seasons")
# #'
# #' df <- data.frame(wins=c(1,1,3,1,1,2,11,2,11,15,1,1,3))
# #' cond <- df$wins == tail(df,1)$wins #[1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
# #' ggplot(df, aes(x=wins)) +
# #'   geom_histogram(data=subset(df,cond==FALSE), binwidth=0.5, fill="red") +
# #'   geom_histogram(data=subset(df,cond==TRUE), binwidth=0.5, fill="blue") +
# #'   scale_x_continuous(breaks=df$wins+0.25, labels=df$wins)
# #'
# #' ## back looking at lineup also for say barnsley the map isnt working+
# #' ##need tp get some new images
# #' # problem is _ is sometimes in playerid eg de_ZEEA substitutin / worked
# #' test <- read_csv("testproblem.csv")
# #' lineup <- read_csv("lineupproblem.csv")
# #' lineup2 <- read_csv("lineup2problem.csv")
# #'
# #' p1 <- read_csv("tempProblem.csv")
# #'
# #' p2 <- read_csv("tempProblem2.csv")
# #' lineup$x
# #'
# #' names(standings)
# #'
# #' v <-standings %>%
# #'   group_by(season,team) %>%
# #'   filter(tmGameOrder==max(tmGameOrder)) %>%
# #'   ungroup() %>%
# #'   select(team,cumPts,games=tmGameOrder) %>%
# #'   group_by(team) %>%
# #'  # filter(team=="Arsenal") %>%
# #'   summarize(totPts=sum(cumPts),totGames=max(games),avPoints=mean(totPts/totGames))
# #'
# #' v %>%
# #'   filter(avPoints>1.3) %>%
# #'   ggvis(~totGames,~avPoints) %>%
# #'   layer_points(fill= ~team) %>%
# #'   add_axis("y", title="Points per Game") %>%
# #'   add_axis("x", title="Games Played")
# #' %>%
# #'   %hide_legend("fill")
# #'
# #'
# #' v %>%
# #'   arrange(desc(avPoints))
# #'
# #' 1033/698
# #'
# #'
# #' ### sequences - start withteams
# #' ## wins/no losses/etc
# #'
# #' names(standings)
# #'
# #' W <-standings %>%
# #'   filter(team=="Arsenal") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #' cond <- df$season =="2014/15"
# #'
# #' ## max(W$slength) 13 I have 14 in access confirm on soccerbase 13 at end of 2001/2 and first of 2002/3 so should be 14
# #' ## looking at W has season in as well as standings is grouped
# #'
# #' glimpse(standings)
# #' str(standings) inc attr season
# #'
# #' W <-standings %>%
# #'   ungroup() %>%
# #'   filter(team=="Crystal P") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #' cond <- df$wins == tail(df,1)$wins
# #'
# #' cond <- tail(W,1)$slength
# #'
# #' cond <- W$slength == tail(W,1)$slength
# #'
# #' Win <- W %>%
# #'   filter(value==1)
# #' %>%
# #'   ggvis(~slength)
# #'
# #' ggplot(Win, aes(x=slength)) +
# #'   #geom_histogram()
# #'   geom_histogram(data=subset(Win,cond==FALSE), binwidth=0.5, fill="red") +
# #'   geom_histogram(data=subset(Win,cond==TRUE), binwidth=0.5, fill="blue") +
# #'   scale_x_continuous(breaks=df$wins+0.25, labels=df$wins)
# #'
# #'
# #' ## alt may need to summarize first
# #'
# #' Win <- W %>%
# #'   filter(value==1) %>%
# #'   group_by(slength) %>%
# #'   tally()
# #' if (tail(W,1)$value==1) {
# #' cond <- Win$slength == tail(W,1)$slength
# #' } else {
# #' cond <- FALSE
# #' }
# #' ggplot(Win, aes(x=slength,y=n)) +
# #'   geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# #'   geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# #'   theme_bw() +
# #'   xlab("Sequence") +
# #'   ylab("Count")
# #'
# #'
# #'
# #' W_Season <-standings %>%
# #'   ungroup() %>%
# #'   group_by(season) %>%
# #'   filter(team=="Arsenal") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #' Win_Season <- W_Season %>%
# #'   filter(value==1) %>%
# #'   ggvis(~slength)
# #'
# #' W_Year <-standings %>%
# #'   ungroup() %>%
# #'   group_by(season) %>%
# #'   filter(team=="Arsenal"&season=="2003/04") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #' Win_Year <- W_Year %>%
# #'   filter(value==1) %>%
# #'   ggvis(~slength)
# #'
# #' need to extract most recent such run
# #'
# #' longW <- W %>%
# #'   filter(slength==max(slength)) %>%
# #'   tail(1)
# #'
# #' standings %>%
# #'   ungroup() %>%
# #'   filter(team=="Arsenal"&tmGameOrder>=longW$first&tmGameOrder<=longW$last) %>%
# #'   mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@",OppTeam),paste0("v",OppTeam))) %>%
# #'   select(Opponents,Score,Date=gameDate) %>%
# #'   datatable()
# #'
# #'  ####
# #' W <-standings %>%
# #'   ungroup() %>%
# #'   filter(team=="Crystal P"&venue=="H") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #' longest is 3 91-93
# #'
# #'
# #'
# #' Win <- W %>%
# #'   filter(value==1) %>%
# #'   group_by(slength) %>%
# #'   tally()
# #' if (tail(W,1)$value==1) {
# #'   cond <- Win$slength == tail(W,1)$slength
# #' } else {
# #'   cond <- FALSE
# #' }
# #' ggplot(Win, aes(x=slength,y=n)) +
# #'   geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# #'   geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# #'   theme_bw() +
# #'   xlab("Sequence") +
# #'   ylab("Count") +
# #'   ggtitle("Wins")
# #'
# #' long <- W %>%
# #'   filter(value==1) %>%  # need to restrict to wins first
# #'   filter(slength==max(slength)) %>%
# #'   tail(1)
# #'
# #'
# #' standings %>%
# #'   ungroup() %>%
# #'   filter(team==input$teamA&tmGameOrder>=long$first&tmGameOrder<=long$last) %>%
# #'   mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>%
# #'   select(Opponents,Score,Date=gameDate) %>%
# #'   DT::datatable(class='compact stripe hover row-border',
# #'                 rownames=FALSE,
# #'
# #'                 options= list(paging = FALSE, searching = FALSE, info=FALSE,
# #'                               columnDefs = list(list(className = 'dt-center', targets = 1))))
# #'
# #' ### goals
# #' names(standings)
# #'
# #'
# #' GF <-standings %>%
# #'   ungroup() %>%
# #'   filter(team=="Crystal P") %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(GF,tmGameOrder) %>%
# #'   mutate(cat=ifelse(GF>0,1,0)) %>%
# #'   do(subSeq(.$cat))
# #'
# #'
# #'
# #'
# #' For <- GF %>%
# #'   filter(value==1) %>%
# #'   group_by(slength) %>%
# #'   tally()
# #' if (tail(GF,1)$value==1) {
# #'   cond <- For$slength == tail(GF,1)$slength
# #' } else {
# #'   cond <- FALSE
# #' }
# #'
# #' ggplot(For, aes(x=slength,y=n)) +
# #'   geom_bar(data=subset(For,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# #'   geom_bar(data=subset(For,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# #'   theme_bw() +
# #'   xlab("Sequence") +
# #'   ylab("Count") +
# #'   ggtitle("Run of Games Scored In")
# #'
# #'
# #' ## longest consec runs - see if it is team record
# #'
# #' a <- standings %>%
# #'   ungroup() %>%
# #'   filter(season=="2015/16")
# #'
# #' teams <- sort(unique(a$team))
# #'
# #'
# #' Win <-standings %>%
# #'   ungroup() %>%
# #'   filter(team %in% teams) %>%
# #'   group_by(team) %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat)) %>%
# #'   group_by(team) %>%
# #'   mutate(maxFirst=max(first)) %>%
# #'   filter(first==maxFirst) %>%
# #'   filter(value==1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength)) %>%
# #'   filter(rank==1) %>%
# #'   select(team,wins=slength)
# #'
# #' #team wins
# #' #1 Man. City    7 # may want to see if team record
# #'
# #' NoWin <-standings %>%
# #'   ungroup() %>%
# #'   filter(team %in% teams) %>%
# #'   group_by(team) %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder) %>%  # prob split here
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat)) %>%
# #'   group_by(team) %>%
# #'   mutate(maxFirst=max(first)) %>%
# #'   filter(first==maxFirst) %>%  # prob split here
# #'   filter(value==0) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength)) %>%
# #'   filter(rank==1) %>%
# #'   select(team,noWins=slength)
# #' ### sploit version
# #' scores <- standings %>%
# #'   ungroup() %>%
# #'   filter(team %in% teams) %>%
# #'   group_by(team) %>%
# #'   arrange(tmGameOrder) %>%
# #'   select(res,tmGameOrder)
# #'
# #' W <- scores %>%
# #'   mutate(cat=ifelse(res=="Win",1,0)) %>%
# #'   do(subSeq(.$cat)) %>%
# #'   group_by(team) %>%
# #'   mutate(maxFirst=max(first)) %>%
# #'   filter(first==maxFirst)
# #'
# #' Win <-
# #'   W  %>% filter(value==1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength),Category="Wins") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #'
# #' noWin <- W  %>%
# #'   filter(value==0) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength), Category="No Wins") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #'
# #' L <- scores %>%
# #'   mutate(cat=ifelse(res=="Loss",1,0)) %>%
# #'   do(subSeq(.$cat)) %>%
# #'   group_by(team) %>%
# #'   mutate(maxFirst=max(first)) %>%
# #'   filter(first==maxFirst)
# #'
# #' Loss <-
# #'   L  %>% filter(value==1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength), Category="Losses") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #' #lineupText <- paste(lineup$name,collapse=", ")
# #' noLoss <- L  %>%
# #'   filter(value==0) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength), Category="No Losses") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #'
# #' D <- scores %>%
# #'   mutate(cat=ifelse(res=="Draw",1,0)) %>%
# #'   do(subSeq(.$cat)) %>%
# #'   group_by(team) %>%
# #'   mutate(maxFirst=max(first)) %>%
# #'   filter(first==maxFirst)
# #'
# #' Draw <-
# #'   D  %>% filter(value==1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength),Category="Draws") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #'
# #' noDraw <- D  %>%
# #'   filter(value==0) %>%
# #'   ungroup() %>%
# #'   arrange(desc(slength)) %>%
# #'   mutate(rank=min_rank(-slength), Category="No Draws") %>%
# #'   filter(rank==1) %>%
# #'   mutate(teams = paste(team,collapse=", ")) %>%
# #'   select(Category,teams,count=slength) %>%
# #'   head(1)
# #'
# #' tbl <- bind_rows(Win,noWin,Draw,noDraw,Loss,noLoss) %>%
# #'   select(Category,count,teams)
# #'
# #'
# #' ### current leaders from layouttests
# #'
# #' output$currentLeaders <-  DT::renderDataTable({
# #'   print("enter teamYear")
# #'   cat <-input$category_2
# #'   yr <-input$season_2
# #'
# #'   if(yr!="Record") {
# #'     if (cat=="Goals"&yr=="Current"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==currentYear) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(goals=sum(Gls)))
# #'       df <- df %>%
# #'         arrange(desc(goals)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     if (cat=="Goals"&yr=="Last Yr"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==lastYear) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(goals=sum(Gls)))
# #'       df <- df %>%
# #'         arrange(desc(goals)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'     if (cat=="Assists"&yr=="Current"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==currentYear) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(assists=sum(Assists)))
# #'       df <- df %>%
# #'         arrange(desc(assists)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     if (cat=="Assists"&yr=="Last Yr"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==lastYear) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(assists=sum(Assists)))
# #'       df <- df %>%
# #'         arrange(desc(assists)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     if (cat=="Cards"&yr=="Current"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==currentYear&(CARD>"A")) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(cards=n()))
# #'       df <- df %>%
# #'         arrange(desc(cards)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     if (cat=="Cards"&yr=="Last Yr"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&season==lastYear&(CARD>"A")) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME) %>%
# #'                          summarise(cards=n()))
# #'       df <- df %>%
# #'         arrange(desc(cards)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     df <-  data.frame(df[,c(2:4)]) # otherwise stays in froups and cannot unclumn
# #'     colnames(df) <- c("Player","Team","Count")
# #'     DT::datatable(df)
# #'   } else {
# #'
# #'     if (cat=="Goals"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&PLAYERID!="OWNGOAL") %>%
# #'                          group_by(PLAYERID,name,TEAMNAME,season) %>%
# #'                          summarise(goals=sum(Gls)))
# #'       df <- df %>%
# #'         arrange(desc(goals)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'     if (cat=="Assists"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME,season) %>%
# #'                          summarise(assists=sum(Assists)))
# #'       df <- df %>%
# #'         arrange(desc(assists)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'     if (cat=="Cards"){
# #'       df <- data.frame(playerGame %>%
# #'                          filter(plYrTmGameOrder<=currentRound&(CARD>"A")) %>%
# #'                          group_by(PLAYERID,name,TEAMNAME,season) %>%
# #'                          summarise(cards=n()))
# #'       df <- df %>%
# #'         arrange(desc(cards)) %>%
# #'         group_by(TEAMNAME) %>%
# #'         slice(1) }
# #'
# #'
# #'
# #'     df <-  data.frame(df[,c(2:5)]) # otherwise stays in froups and cannot unclumn
# #'     colnames(df) <- c("Player","Team","Season","Count")
# #'     DT::datatable(df)
# #'   }
# #' })
# #'
# #' names(playerGame)
# #' ## think issue is that needs to be based on player game whic is nt same as game order
# #' names(standings) has gameDate and tmGameOrder so need to go via that
# #'
# #' maxDate <- standings %>%
# #'   ungroup() %>%
# #'   filter(tmYrGameOrder==currentRound&season==currentYear)
# #'
# #' maxDate <- max(maxDate$gameDate) #"2014-08-18" looks good
# #'
# #' ly <- data.frame(playerGame %>%
# #'                    filter(gameDate<=maxDate&season==lastYear&TEAMNAME %in% currentTeams) %>%
# #'                    group_by(PLAYERID,name,TEAMNAME) %>%
# #'                    summarise(goals=sum(Gls)))
# #' ly <- ly %>%
# #'   arrange(desc(goals)) %>%
# #'   group_by(TEAMNAME) %>%
# #'   slice(1)
# #'
# #' ly[ly$goals==0,]$name <- ""
# #'
# #'
# #'
# #' ## look at goals first
# #' maxDate <- standings %>%
# #'   ungroup() %>%
# #'   filter(tmYrGameOrder==currentRound&season==currentYear)
# #'
# #' maxDate <- max(maxDate$gameDate) #"2015-08-10" looks good
# #'
# #' ty <- data.frame(playerGame %>%
# #'                    filter(gameDate<=maxDate&season==currentYear&TEAMNAME %in% currentTeams) %>%
# #'                    group_by(PLAYERID,name,TEAMNAME) %>%
# #'                    summarise(goals=sum(Gls),assists=sum(Assists)))
# #' tyGoals <- ty %>%
# #'   arrange(desc(goals)) %>%
# #'   group_by(TEAMNAME) %>%
# #'   slice(1) %>%
# #'   select(team=TEAMNAME,goals,name)
# #'
# #' tyGoals[tyGoals$goals==0,]$name <- ""
# #'
# #' tyAssists <- ty %>%
# #'   arrange(desc(assists)) %>%
# #'   group_by(TEAMNAME) %>%
# #'   slice(1) %>%
# #'   select(team=TEAMNAME,assists,name)
# #'
# #' tyAssists[tyAssists$assists==0,]$name <- ""
# #'
# #' ## for cards
# #' ty <- playerGame %>%
# #'                    ungroup() %>%
# #'                    filter(gameDate<=maxDate&season==currentYear&TEAMNAME %in% currentTeams&CARD>1) %>%
# #'                    group_by(PLAYERID,name,TEAMNAME) %>%
# #'                    tally()
# #'
# #' tyCards <- ty %>%
# #'   arrange(desc(n)) %>%
# #'   group_by(TEAMNAME) %>%
# #'   slice(1) %>%
# #'   select(team=TEAMNAME,cards=n,name)
# #' ## cp had no card first day so next is not relevant - need to correct later
# #'
# #' #tyCards[tyCards$cards==0,]$name <- ""
# #'
# #' tyAll <- tyGoals %>%
# #'   inner_join(tyAssists,by=c("team"="team"))
# #'
# #'
# #' tyAll <- tyGoals %>%
# #'   inner_join(tyAssists,by="team") %>%
# #'   left_join(tyCards,by="team")
# #'
# #' tyAll[is.na(tyAll$cards)]$name <- ""
# #' tyAll$cards <-ifelse(is.na(tyAll$cards),0,tyAll$cards)
# #' tyAll$name <-ifelse(is.na(tyAll$name),"",tyAll$name)
# #'
# #' ## lastyear
# #' currentTeams <- df$TEAMNAME # if only want to look at current - poss have as radio button
# #'
# #'
# #'
# #' maxDate <- standings %>%
# #'   ungroup() %>%
# #'   filter(tmYrGameOrder==currentRound&season==lastYear)
# #'
# #' maxDate <- max(maxDate$gameDate) #"2014-08-18" looks good
# #'
# #' ly <- data.frame(playerGame %>%
# #'                    filter(gameDate<=maxDate&season==lastYear&TEAMNAME %in% currentTeams) %>%
# #'                    group_by(PLAYERID,name,TEAMNAME) %>%
# #'                    summarise(goals=sum(Gls)))
# #' ly <- ly %>%
# #'
# #'   ## for all years - need to do some date manipulation
# #'   arrange(desc(goals)) %>%
# #'   group_by(TEAMNAME) %>%
# #'   slice(1)
# #'
# #' ly[ly$goals==0,]$name <- ""
# #'
# #' ## most subbed players all time
# #' names(playerGame)
# #' playerGame %>%
# #'   group_by(PLAYERID,name) %>%
# #'   mutate(pc=100*off)
# #'
# #'
# #' names(summary)
# #' str(summary)
# #' summary <- data.frame(summary)
# #' summary %>%
# #'  ungroup() %>%
# #'   filter(PLAYERID=="AGUEROS") %>%
# #'   select(St,Off,name) %>%
# #'   summarize(allSt=sum(St),allOff=sum(Off),pc=allOff/allSt)
# #'   select
# #'
# #'
# #'   summary %>%
# #'     ungroup() %>%
# #'
# #'     select(St,Off,name,PLAYERID) %>%
# #'     group_by(PLAYERID,name) %>%
# #'     summarize(allSt=sum(St),allOff=sum(Off),pc=round(100*allOff/allSt,1)) %>%
# #'  select(name,allSt,pc) %>%
# #'     names(playerGame)
# #'   temp <-     playerGame %>%
# #'     filter(PLAYERID=="AGUEROS") %>%
# #'     select(off,Gls)
# #'     filter(allSt>99) %>%
# #'     ungroup() %>%
# #'     arrange(desc(pc)) %>%
# #'
# #'
# #'
# #'
# #' dfTeamYear <- summary %>%
# #'   filter(PLAYERID==thePlayer) %>%
# #'   mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
# #'   select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>%
# #'   ungroup() %>%
# #'   arrange(desc(Season)) %>%
# #'   select(-(c(name,LASTNAME,PLAYERID,born,left)))
# #'
# #' #print(glimpse(dfTeamYear))
# #'
# #' dfTeam <- dfTeamYear %>%
# #'   group_by(Team) %>%
# #'   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
# #'             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
# #'
# #' # print(names(dfTeam))
# #'
# #' dfCareer <- dfTeamYear %>%
# #'
# #'   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(Mins),Goals=sum(Gls),
# #'             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
# #' dfCareer$Team <- "Career"
# #'
# #' names(standings)
# #' standings %>%
# #'   ungroup() %>%
# #'   filter(tmYrGameOrder ==2) %>%
# #'   arrange(desc(cumGA)) %>%
# #'   select(season,cumGA,team,final_Pos)
# #'
# #'
# #' # try out the difference package for
# #' library(daff)
# #'
# #' x1 <- data.frame(x=c(1,2,3),y=c("a","b","c"))
# #' y1 <- data.frame(x=c(3,2,4),y=c("a","b","c"))
# #'
# #' diff_data(x1,y1)
# #'
# #'
# #' library(daff)
# #' x <- iris
# #' x[1,1] <- 10
# #' diff_data(x, iris)
# #' #' @@,Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species
# #' #' ->,10->5.1,3.5,1.4,0.2,setosa
# #' #' ,4.9,3,1.4,0.2,setosa
# #'
# #' dd <- diff_data(x, iris)
# #' str(dd) #List of 11
# #' write_diff(dd, "diff.csv")
# #'
# #' res <- read_csv("diff.csv") #str(res) #‘tbl_df’, ‘tbl’ and 'data.frame':
# #' @@ Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# #' 1  ->      10->5.1         3.5          1.4         0.2  setosa
# #' 2              4.9           3          1.4         0.2  setosa
# #' colnames(res)[1]<- "Diff"
# #' res %>%
# #'   filter(Diff=="->") # so far so good
# #'
# #'
# #' matched.indices <- which(L[,"1"] > 58)
# #'
# #' # prob best to check for specific values eg Goals
# #' names(playerGame)
# #'
# #' tw <- #4,119
# #'   playerGame %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(Goals=sum(Gls)) # inc BALOTEM
# #' #Mario Balotelli
# #' #21
# #'
# #' lw <- #4059 (can never be more)
# #'   playerGame %>%
# #'   filter(gameDate<"2015-08-14") %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(Goals=sum(Gls)) # BALOTEM Mario Balotelli 21
# #'
# #'
# #'
# #' dd <- diff_data(tw,lw) #str is Tableview
# #' # this is no issue
# #' write_diff(dd, "diff.csv")
# #'
# #' library(tidyr)
# #'
# #'
# #'
# #' res <- read_csv("diff.csv") #Balotell still just 21
# #'
# #'
# #' # df <- data.frame(x = c("a.b", "a.d", "b.c"))
# #' # df %>% separate(x, c("A", "B"))
# #'
# #' df <- separate(data=res,col= Goals,into=c("New","Old"),sep="->", extra="drop")
# #'
# #' # remove all na rows
# #' df <- df[!is.na(df$Old),] # now down to 23 who scored
# #'
# #' df$Old <- as.integer(ifelse(is.na(df$Old),0,df$Old))
# #' df$New <- as.integer(df$New)
# #' str(df)
# #' ## could also set it to ten presulably even if 11
# #' debGoals <- df %>%
# #'   filter(New>0&Old<=0)
# #' tenGoals <-df %>%
# #'   filter(New>9&Old<=9)
# #' twentyFiveGoals <-df %>%
# #'   filter(New>24&Old<=24)
# #' fiftyGoals <-df %>%
# #'   filter(New>49&Old<=9)
# #' hundredGoals <-df %>%
# #'   filter(New>99&Old<=99)
# #'
# #' milestoneGoals <-bind_rows(debGoals,tenGoals,twentyFiveGoals,fiftyGoals,hundredGoals) %>%
# #'   arrange(desc(New)) %>%
# #'   mutate(category="Goals") %>%
# #'   select(category,name,New)
# #'
# #' assists names(playerGame)
# #'
# #' tw_assists <- #4,119
# #'   playerGame %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(Assts=sum(Assists)) # inc BALOTEM
# #' #Mario Balotelli
# #' #21
# #'
# #' lw_assists <- #4059 (can never be more)
# #'   playerGame %>%
# #'   filter(gameDate<"2015-08-14") %>%
# #'   group_by(PLAYERID,name) %>%
# #'   summarize(Assts=sum(Assists)) # BALOTEM Mario Balotelli 21
# #'
# #'
# #' rd <- render_diff(dd)
# #' dd <- diff_data(tw_assists,lw_assists) #s
# #'
# #' write_diff(dd, "diff.csv")
# #'
# #' library(tidyr)
# #'
# #'
# #'
# #' res <- read_csv("diff.csv") #Balotell still just 21
# #'
# #'
# #' # df <- data.frame(x = c("a.b", "a.d", "b.c"))
# #' # df %>% separate(x, c("A", "B"))
# #'
# #' df <- separate(data=res,col= Assts,into=c("New","Old"),sep="->", extra="drop")
# #'
# #' ## look again at player sequences
# #' ## add starts and minutes played also histo v point
# #'
# #'
# #' temp <- playerGame %>%
# #'   filter(PLAYERID=="ROONEYX"&(START+subOn)>0) %>%  # 380 as sh
# #'   arrange(gameDate) %>%
# #'   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
# #'
# #' temp$Scored <- 0
# #' temp$Scored[temp$Gls>0] <- 1
# #'
# #' run <- subSeq(temp$Scored)
# #'
# #'
# #' gameGoal <- run %>%
# #'   filter(value==1) %>%
# #'   group_by(slength) %>%
# #'   tally()
# #'
# #' ### games not scord in rooney 17-29 need to check this is the right run  - partic if showing these
# #' names(playerGame)
# #' test <- playerGame %>%
# #'   filter(PLAYERID=="ROONEYX"&START>0) %>%
# #'   select(gameDate,TEAMNAME,START,subOn,Gls) #cna then match bu gameDate,TEAMNAME to get oppo
# #'
# #'
# #' if (tail(run,1)$value==1) {
# #'   cond <- gameGoal$slength == tail(run,1)$slength
# #' } else {
# #'   cond <- FALSE
# #' }
# #'
# #'
# #'
# #'
# #' ggplot(gameGoal, aes(x=slength,y=n)) +
# #'   geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# #'   geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# #'   theme_bw() +
# #'   xlab("Sequence") +
# #'   ylab("Count") +
# #'   ggtitle("Games Scored In")
# #'
# #' # remove all na rows
# #' df <- df[!is.na(df$Old),] # now down to 23 who scored
# #'
# #' df$Old <- as.integer(ifelse(is.na(df$Old),0,df$Old))
# #' df$New <- as.integer(df$New)
# #' str(df)
# #' ## could also set it to ten presulably even if 11
# #' debGoals <- df %>%
# #'   filter(New>0&Old<=0)
# #' tenGoals <-df %>%
# #'   filter(New>9&Old<=9)
# #' twentyFiveGoals <-df %>%
# #'   filter(New>24&Old<=24)
# #' fiftyGoals <-df %>%
# #'   filter(New>49&Old<=9)
# #' hundredGoals <-df %>%
# #'   filter(New>99&Old<=99)
# #'
# #' milestoneAssists <-bind_rows(debGoals,tenGoals,twentyFiveGoals,fiftyGoals,hundredGoals) %>%
# #'   arrange(desc(New)) %>%
# #'   mutate(category="Assists") %>%
# #'   select(category,name,New)
# #'
# #' milestones <- bind_rows(milestoneGoals,milestoneAssists) %>%
# #'               arrange(desc(New)) %>%
# #'               head(6) %>%
# #'               rename(Count=New)
# #'
# #' ### goal distribution
# #'
# #' output$goalDistribution <- renderPlot({
# #'   goals %>%
# #'     left_join(playerGame, by="PLAYER_MATCH") %>%
# #'     filter(PLAYERID=="ROONEYX") %>%
# #'     select(METHOD,PLACE,PLAY,plGameOrderApp)  %>%
# #'     ggplot(aes(x=METHOD,y=PLACE))+
# #'     geom_point()+
# #'     geom_jitter(aes(colour=PLAY),position=position_jitter(width=0.25, height=0.25))+
# #'     scale_y_discrete(limits=c("6_Yd_Box","Pen_Area","Long_Range"),labels=c("6yd Box","Pen Area","Long Range"))+
# #'     scale_colour_brewer(palette="Set1")+
# #'     ggtitle("Method, Place and Play of Goals Scored") +
# #'     xlab("Method") +
# #'     ylab("Play") +
# #'     labs(colour="Place") +
# #'     theme_bw()
# #'
# #' })
# #'
# #'
# #' starter <- playerGame %>%
# #'   filter(PLAYERID=="ROONEYX"&START>0) %>%  # 380 as sh
# #'   arrange(gameDate) %>%
# #'   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
# #'   mutate(starterOrder=row_number())
# #'
# #' starter$Scored <- "No"
# #' starter$Scored[starter$Gls>0] <- "Yes"
# #'
# #' runStarter <- subSeq(starter$Scored)
# #' tail(starter)
# #'
# #' ## can take first,last or midpoint to have run not scoring/scoring
# #'
# #' runStarter %>%
# #'   left_join(starter,by=c("last"="starterOrder")) %>%
# #'   select(gameDate,slength,Scored) %>%
# #'   #mutate(cat="no") %>%
# #'   ggvis(~gameDate,~slength) %>%
# #'   layer_points(fill=~ Scored, size=2) %>%
# #'   add_axis("x",title="") %>%
# #'   add_axis("y", title="Run of Games as Starter")
# #'
# #'
# #' ## look at the minutes thingee
# #'
# #' names(playerGame)
# #' playerGame %>%
# #'   filter(PLAYERID=="ROONEYX") %>%
# #'   arrange(gameDate) %>%
# #'   select(gameDate,TEAMMATCHID,START,OFF,subOn) %>%
# #'
# #'
# #'
# #'
# #'
# #'
# #'
# #'   starter <- playerGame %>%
# #'   filter(PLAYERID=="ROONEYX"&(START+subOn)>0) %>%  # 380 as sh
# #'   arrange(gameDate) %>%
# #'   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
# #'   mutate(starterOrder=row_number())
# #'
# #'
# #' print(glimpse(starter))
# #' print("starter")
# #'
# #' starter$Scored <- "No"
# #' starter$Scored[starter$Gls>0] <- "Yes"
# #'
# #' runStarter <- subSeq(starter$Scored)
# #'
# #' print(runStarter)
# #'
# #' ## can take first,last or midpoint to have run not scoring/scoring
# #'
# #' chart <-  runStarter %>%
# #'   left_join(starter,by=c("last"="starterOrder")) %>%
# #'   select(gameDate,slength,Scored)
# #'
# #' print("chart")
# #' print(chart)
# #' print("chart done")
# #'
# #' chart %>%     ggvis(~gameDate,~slength) %>%
# #'   layer_points(fill=~ Scored, size=2) %>%
# #'   add_axis("x",title="") %>%
# #'   add_axis("y", title="Run of Games")
# #'
# #'
# #' #### player by opposition
# #'
# #'
# #' dfTeamYear <- summary %>%
# #'   filter(PLAYERID==thePlayer) %>%
# #'   mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
# #'   select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>%
# #'   ungroup() %>%
# #'   arrange(desc(Season)) %>%
# #'   select(-(c(name,LASTNAME,PLAYERID,born,left)))
# #'
# #' ## kick off with all appearances
# #'
# #' player <- playerGame %>%
# #'   filter(PLAYERID=="ROONEYX")
# #' names(player)
# #'
# #' squad <-player %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(squad=n)
# #'
# #' starts <-player %>%
# #'   filter(START>0) %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(starts=n)
# #'
# #' on  <- player %>%
# #'   filter(subOn>0) %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(on=n)
# #'
# #'
# #' bench  <- player %>%
# #'   filter(subOn==0&START==0) %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(bench=n)
# #'
# #' off  <- player %>%
# #'   filter(OFF>0) %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(Off=n)
# #'
# #' red <- player %>%
# #'   filter(CARD>"a"&CARD!="Y") %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(red=n)
# #'
# #' yellow <- player %>%
# #'   filter(CARD=="Y") %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(yellow=n)
# #'
# #'
# #' goals <- player %>%
# #'   group_by(Opponents) %>%
# #'   summarize(goals=sum(Gls))
# #'
# #' assists <- player %>%
# #'   group_by(Opponents) %>%
# #'   summarize(assists=sum(Assists))
# #'
# #' # # Rows do need to match when column-binding
# #' # bind_cols(data.frame(x = 1), data.frame(y = 1:2))
# #' #
# #' # bind_cols(on,goals) # Error: incompatible number of rows (37, expecting 28 so will need to left_bind
# #' #
# #' # sort(names(player))
# #' # names(standings)
# #' # player
# #'
# #' results <-
# #'   player %>%
# #'   filter((START+subOn)>0) %>%
# #'   select(team=TEAMNAME,gameDate,Opponents) %>%
# #'   inner_join(standings)
# #'
# #' wins <- results %>%
# #'   filter(res=="Win") %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(wins=n)
# #'
# #' draws <- results %>%
# #'   filter(res=="Draw") %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(draws=n)
# #'
# #' losses <- results %>%
# #'   filter(res=="Loss") %>%
# #'   group_by(Opponents) %>%
# #'   tally() %>%
# #'   rename(losses=n)
# #'
# #' byOpponent <-
# #' squad %>%
# #'   left_join(starts) %>%
# #'   left_join(on) %>%
# #'   left_join(off) %>%
# #'   left_join(bench) %>%
# #'   left_join(goals) %>%
# #'   left_join(assists) %>%
# #'   left_join(red) %>%
# #'   left_join(yellow) %>%
# #'   left_join(wins) %>%
# #'   left_join(draws) %>%
# #'   left_join(losses)
# #'
# #' byOpponent[is.na(byOpponent)]<-0
# #'
# #' byOpponent %>%
# #'   mutate(apps=starts+on) %>%
# #'   select(Opponents,apps,starts:losses)
# #'
# #'
# #' ### look at summary of player v opponent
# #'
# #' names(playerGame)
# #'
# #' ## something similar to lineups
# #'
# #' df <-  playerGame %>%
# #'   filter(TEAMNAME == input$teamA & gameDate == matchDate) %>%
# #'   arrange(desc(gameDate),off,on) %>%
# #'   select(name,st,on,off,Gls,Assists,CARD)
# #'
# #'
# #' names(playerGame)
# #' names(standings)
# #' playerGame %>%
# #'   filter(PLAYERID=="BENTD"&Opponents=="Everton") %>%
# #'   arrange(desc(gameDate)) %>%
# #'   inner_join(standings,by = c("gameDate" = "gameDate","Opponents"="OppTeam")) %>%
# #'   mutate(res=paste0(GF,"-",GA)) %>%
# #'   select(gameDate,TEAMNAME,res,st,on,off,Gls,Assists,CARD) %>%
# #'   DT::datatable(class='compact stripe hover row-border',colnames = c('Date', 'Team','Res ', 'St', 'On', 'Off', 'Gls', 'Ass','Card'),rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# #'
# #' ### issue with team maps not appearing
# #'
# #' temp <- read_csv("tempProblem3.csv")
# #'
# #'
# #' binpalEPL <-
# #'   colorBin(c("#FFFF00","#FF8000","#FF0000"), temp$Apps,  pretty = TRUE)
# #'
# #' temp %>%    leaflet() %>%
# #'   addTiles() %>%
# #'   #setView(2,49,zoom=3) %>%
# #'   addCircleMarkers(
# #'     radius = 3,fillOpacity = 0.5,popup =  ~ popup,color = ~ binpalEPL(Apps)
# #'   )
# #'
# #'
# #' data.frame(summary %>%
# #'              filter(TEAMNAME==theTeam&season==theYear) %>%
# #'
# #'              mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
# #'              select(PLAYERID,Player=name,Pos=pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
# #'
# #' #   tbl <- tbl[,-(1:5)] %>%
# #' #     arrange(desc(Mins)) %>%
# #' #   DT::datatable(class='compact stripe hover row-border',rownames=TRUE,options = list(paging = FALSE, searching = FALSE,
# #' #                                                   #order = list(list(9, 'desc')),
# #' #     columnDefs= list(list(visible=FALSE,targets=list(1)),list(className = 'dt-center', targets = 3),list(width="20%",columnDefs.targets= list(1)),list(width="1%",columnDefs.targets= list(0)))))
# #' # cf teamataglnce
# #' tbl <-   tbl %>%
# #'   select(-(1:6)) #%>%
# #' #   arrange(desc(Mins))
# #' print(str(tbl)) #data.frame ( no reason to be issue I would have thought)
# #'
# #'
# #' tbl <-summary %>%
# #'   ungroup() %>%
# #'              filter(TEAMNAME=="Arsenal"&season=="2015/16") %>%
# #'
# #'              mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
# #'              select(Player=name,Pos=pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)
# #'
# #' str(tbl) #Classes ‘tbl_df’, ‘tbl’ and 'data.frame':
# #'
# #'
# #'
# #'
# #'   tbl <- tbl[,-(1:5)] %>%
# #'     arrange(desc(Mins)) %>%
# #'   DT::datatable(class='compact stripe hover row-border',rownames=TRUE,options = list(paging = FALSE, searching = FALSE,
# #'                                                   #order = list(list(9, 'desc')),
# #'     columnDefs= list(list(visible=FALSE,targets=list(1)),list(className = 'dt-center', targets = 3),list(width="20%",columnDefs.targets= list(1)),list(width="1%",columnDefs.targets= list(0)))))
# #' cf teamataglnce
# #'
# #'
# #'
# #' tbl <-summary %>%
# #'   ungroup() %>%
# #'   filter(TEAMNAME=="Arsenal") %>%
# #'
# #'   mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
# #'   group_by(PLAYERID,name,pos) %>%
# #'   select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
# #'   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
# #'             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
# #'
# #'
# #' tbl <-summary %>%
# #'   ungroup() %>%
# #'   filter(TEAMNAME=="Arsenal") %>%
# #'
# #'   mutate(pos=str_sub(POSITION,1,1),Apps=St+On,Goals=StGls+subGls,Pens=startPens+subPens,Points=Goals+Assists) %>%
# #'   group_by(PLAYERID,name,pos) %>%
# #'   select(Player=name,Pos=pos,Apps,St,On,Off,Bench,mins,Goals,Pens,Assists,Points,Y,R,OG,MP) %>%
# #'   summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Mins=sum(mins),Goals=sum(Goals),
# #'             Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP))
# #' str(table)
# #' tbl %>%
# #'   ungroup() %>%
# #'   select(-PLAYERID) %>%
# #'   DT::datatable(selection='single',rownames = FALSE, class='compact stripe hover row-border',
# #'                 options= list(
# #'                   paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE,
# #'
# #' ### updates of geos
# #'
# #'
# #' playerGeos <- read_csv("playerGeos.csv") #4041
# #'
# #' toGet <- playerGeos %>%
# #'   filter(is.na(lat))
# #' toGet$playerID
# #'
# #' # [1] "ANDRADB" "BANGAB"  "BERTIL"  "ESPINOG" "GOSSJ"   "JORONEJ" "KNARVIT" "LAURSEM"
# #' # [9] "LEITES"  "PARKC"   "SONGR"   "SPIEGER" "SUNUG"
# #'
# #' sorted <- just have those with not na
# #' ## but have recently done a city  update  one so prob should do all again
# #' playerGame %>%
# #'   filter(PLAYERID=="NJIEC") # does not include those who havent played yet
# #' there are 4236 in access currently
# #'
# #' allPlayers <- readRDS("allPlayers.rds") #4232 upped to 4236
# #'
# #' allPlayers %>%
# #'   filter(PLAYERID=="NJIEC")
# #'
# #' names(playerGame)
# #'
# #' players <-
# #'  playerGame %>%
# #'   select(city,COUNTRY,PLAYERID) %>%
# #'   unique() #4126
# #' i <- 1
# #'
# #' players %>%
# #'   anti_join(ply)
# #'
# #' # might 2 days looks like some missing so need to keep previous version
# #' names(allPlayers)
# #' for (i in 3:nrow(allPlayers)){
# #'  # for (i in 1:2){
# #'     print(i)
# #'   tempdf <- geocode(paste0(players$PLACE[i],", ",players$COUNTRY[i]))
# #'   tempdf <- cbind(playerID=players$PLAYERID[i],tempdf)
# #'    if (i!=1) {
# #'      df <- rbind(df,tempdf)
# #'    } else {
# #'      df <- tempdf
# #'    }
# #'  # print(head(df,1))
# #' }
# #' # 1: geocode failed with status INVALID_REQUEST, location = "Reykjavík,+Iceland"
# #' # 2: geocode failed with status INVALID_REQUEST, location = "Križovany+nad+Dudváhom,+Slovakia"
# #' # 3: geocode failed with status INVALID_REQUEST, location = "Bagnols-sur-Cèze,+France"
# #' # 4: geocode failed with status INVALID_REQUEST, location = "	São+Je+do+Rio+Preto,+Brazil"
# #' # 5: geocode failed with status ZERO_RESULTS, location = "Charguacayo,+Ecuador"
# #' # 6: geocode failed with status ZERO_RESULTS, location = "Oekolia,+Cyprus"
# #' # 7: geocode failed with status INVALID_REQUEST, location = "Rautjärvi,+Finland"
# #' # 8: geocode failed with status ZERO_RESULTS, location = "Skjerjard,+Norway"
# #' # 9: geocode failed with status ZERO_RESULTS, location = "Farvoug,+Denmark"
# #' # 10: geocode failed with status ZERO_RESULTS, location = "Nkenglicock,+Cameroon"
# #' # 11: geocode failed with status ZERO_RESULTS, location = "Ruttrnrn,+Switzerland"
# #' # 12: geocode failed with status INVALID_REQUEST, location = "Châteauroux,+France"
# #'
# #' write_csv(df,'playerGeosNew.csv')
# #'
# #' write_csv(df,'playerGeos.csv')
# #'                   order = list(list(7, 'desc')) # initially set to minutes
# #'                 )
# #'   )
# #'
# #' geocodes <- read_csv('playerGeosNew.csv') #4236
# #'
# #' ###
# #' ### http://www.11v11.com/teams/manchester-united/tab/matches/season/2015
# #' can lead to interesting facts about game
# #' use .sortable a
# #' eg http://www.11v11.com/matches/west-bromwich-albion-v-manchester-united-20-october-2014-310558/
# #'   .comments .away
# #'
# #' also gives time of cards
# #'
# #'
# #' ### look at highligting row
# #'
# #' library(DT)
# #' options(DT.options = list(pageLength = 5))
# #' df = as.data.frame(cbind(matrix(round(rnorm(50), 3), 10), sample(0:1, 10, TRUE)))
# #' # style V6 based on values of V6
# #' datatable(df) %>% formatStyle(
# #'   'V6',
# #'   backgroundColor = styleEqual(c(1), c('yellow'))
# #' )
# #'
# #' datatable(df,options = list(
# #'   columnDefs = list(list(targets = 6, visible = FALSE)))) %>%
# #'   formatStyle(
# #'   'V6',
# #'   target = 'row',
# #'   backgroundColor = styleEqual(c(1), c('yellow'))
# #' )
# #'
# #' ## goals per min rgeough season
# #' ## also use lag to get time between goals
# #'
# #' names(standings) team.gamedate,tmyrgameorder/season
# #' names(goals) $TEAMMMATCHUD.TIME
# #'
# #' names(teamGames)
# #'
# #' teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   filter(TEAMNAME=="Crystal P"&season=="2014/15") %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'   ggvis(~cumTime,~cumGoals) %>%
# #'   layer_lines()
# #'
# #'
# #' teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'    mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'   filter(TEAMNAME=="Crystal P"&season=="2014/15") %>%
# #'   ggvis(~cumTime,~cumGoals)
# #'
# #' # bit messy - not best for ggvis
# #' teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'   filter(TEAMNAME=="Man. Utd."&cumTime<=270) %>%
# #'   ggvis(~cumTime,~cumGoals) %>%
# #'   layer_lines(stroke=~season)
# #'
# #'
# #' test <-teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'   filter(TEAMNAME=="Man. Utd."&cumGoals==3)
# #'
# #' test %>%
# #'   ggvis(~cumTime,~season) %>%
# #'   layer_points()
# #'
# #' years <- sort(test$season)
# #'
# #' df = data.frame(x = c(10, 50), y = c(150, 150))
# #' df %>% ggvis(x = ~x, y = ~y) %>% layer_lines(stroke := "red") %>%
# #'   layer_points(data = mtcars, x = ~mpg, y = ~hp)
# #'
# #'
# #' df = data.frame(y = years, x = rep(270,23))
# #'
# #' df %>% ggvis(x = ~x, y = ~y) %>% layer_lines(stroke := "red") %>%
# #'   layer_points(data = test, x = ~cumTime, y = ~season) %>%
# #'   add_axis("y", title="") %>%
# #'   add_axis("x", title= "Cumulative Minutes (assuming 90 mins per game)")
# #'
# #' ## look at the lag or cumdiff
# #'
# #'
# #' test <- teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'   filter(TEAMNAME=="Man. Utd."&season=="2014/15") %>%
# #'   mutate(diff = cumTime - lag(cumTime, default = 0)) %>%
# #'   select(tmYrGameOrder,TIME,cumTime,diff)
# #'
# #' test %>%
# #'   ggvis(~diff)
# #'
# #' # worst was over 300 mins - could then work back to see games
# #' ## also get max for each team by season
# #'
# #' # laets do latter (could add to latest sequencess)
# #'
# #' allSeasonDiffs <- teamGames %>%
# #'   select(season,tmYrGameOrder,TEAMMATCHID) %>%
# #'   right_join(goals) %>%
# #'   arrange(tmYrGameOrder,TIME) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   mutate(cumTime=90*(tmYrGameOrder-1)+TIME,cumGoals=row_number()) %>%
# #'  # filter(TEAMNAME=="Man. Utd."&season=="2014/15") %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   mutate(diff = cumTime - lag(cumTime, default = 0)) %>%
# #'   select(tmYrGameOrder,TIME,cumTime,diff)  %>%
# #'   ungroup() %>%
# #'   arrange(desc(diff)) %>%
# #'   group_by(TEAMNAME,season) %>%
# #'   slice(1) %>%
# #'   ungroup() %>%
# #'   arrange(desc(diff))
# #'
# #' allSeasonDiffs%>%
# #'   filter(season=="2014/15")
# #'
# #' View(allSeasonDiffs%>%
# #'   filter(TEAMNAME=="Man. Utd."))
# #'
# #' ## something looks wrong here
# #' View(allSeasonDiffs%>%
# #'        filter(TEAMNAME=="Chelsea"))
# #'
# #'
# #' # look at alternative to ggvis for transfer eg taucharts
# #'
# #' permanent <- read_csv("permanent.csv")
# #'
# #' loan <- read_csv("loan.csv")
# #' rror: You have 42 column names, but 0 columns dioene before sunmmarize
# #'
# #'
# #' permanent   %>%
# #'   ggvis(~Date,~Cost) %>%
# #'   layer_points(fill=~Team) %>%
# #'   scale_numeric("y",domain=c(0,max(permanent$Cost))) %>%
# #'   scale_datetime("x", nice='year') %>%
# #'   #add_tooltip(all_values,"hover") %>%
# #'   add_axis("x", properties = axis_props(labels = list(
# #'     angle = 45, align = "left", fontSize = 11
# #'   )),title = "Joined Club",title_offset=50) %>%
# #'   add_axis("y", title="Fee (million)",title_offset=50) %>%
# #'   hide_legend("fill") %>% # otherwise takes up too much space
# #'   set_options(width=300,height=200) %>%
# #'   bind_shiny("playerTransfers")
# #'
# #' tauchart(mtcars) %>% tau_point("mpg", "wt")
# #'
# #' tauchart(permanent) %>% tau_point("Date", "Cost",color="Team") %>%
# #'   tau_legend()  %>%
# #'   tau_tooltip(c("Date","Team","Cost")) %>%
# #'   tau_guide_x(tick_period='day', tick_format="year") %>%
# #'   tau_guide_y(label ='Purchase Price (mill)')
# #'
# #'
# #' temp <- data.frame(Date=character(),Cost=numeric(),Team=character())
# #'
# #' tauchart(temp) %>% tau_point("Date", "Cost",color="Team")
# #'
# #'
# #' %>%
# #'   tau_legend()  %>%
# #'   tau_tooltip(c("Date","Team","Cost")) %>%
# #'   tau_guide_x(tick_period='day', tick_format="year") %>%
# #'   tau_guide_y(label ='Purchase Price (mill)')
# #'
# #' ## possible uses
# #' library(tidyr)
# #' df <- data.frame(a = c(1, 2, 5), b = c(3, 5, 3), c = c(1, 2, 3)) # 3 rows
# #' expand(df) #18 rows
# #' expand(df, a, b) # each 6 rows
# #' expand(df, a, c)
# #' expand(df, b, c)
# #'
#   ## add graph for teams
#
#
#
#
#   standings %>%
#     ungroup() %>%
#     filter(tmYrGameOrder==4&position==2) %>%
#     select(Season=season,Team=team,Pts=cumPts,GD=cumGD,GF=cumGF,Final=final_Pos) %>%
#     arrange(desc(Season)) %>%
#     DT::datatable(class='compact stripe hover row-border',
#                   rownames=FALSE,
#                   options= list(searching = FALSE, info=FALSE))
#
# standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==4&position==2) %>%
#   ggvis(~final_Pos) %>%
#   add_axis("y", breaks=pretty_breaks())
#
# theMin <- min(standings$final_Pos)
# theMax <- max(standings$final_Pos)
# standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==4&position==2) %>%
#
#   ggplot(aes(final_Pos)) +
#   geom_histogram(fill="blue",binwidth = 0.5, alpha=0.5) +
#   # scale_x_continuous(breaks=df$final_Pos+0.2, labels=df$final_Pos)
#  # scale_x_continuous(breaks=standings$final_Pos+0.2, labels=standings$final_Pos)
#   scale_x_discrete(breaks=c(theMin:theMax)) +
#   theme_bw() +
#   xlab("Final Position") +
#   ylab("Seasons")

# if probs with deployment ------------------------------------------------

library(shinyapps)
deployApp(appName="premierLeague", account="mytinyshinys")

# look at timevis - say for Man U purchases since ferguson left -----------


#
# ## graph of goals assist per 90 mins
#
#
# summary %>%
#   filter(PLAYERID=="ROONEYX") %>%
#   mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
#   select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>%
#   ungroup() %>%
#   arrange(desc(Season)) %>%
#   select(-(c(name,LASTNAME,PLAYERID,born,left))) %>%
#   mutate()
#
# ## add tooltip
#
#
# df <-playerGame %>%
#   filter(PLAYERID=="ROONEYX") %>%
#   group_by(season,PLAYERID,name) %>%
#   select(Gls,Assists,mins) %>%
#   summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>%
#   filter(Points!=0) %>%
#   mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>%
#   ungroup()
#
#
# ## maybe do a tauchart
# library(taucharts)
#
# df %>%
#   tauchart() %>%
#   tau_point("season","Ppm", size="2") %>%
#  # tau_line("season","Ppm") %>%
# tau_tooltip(c("Goals","Assists")) %>%
#   tau_guide_x(label="") %>%
#   tau_guide_y(label ='Points per 90 mins')
#
# df <- cbind(df, id = seq_len(nrow(df)))
#
# all_values <- function(x) {
#   if(is.null(x)) return(NULL)
#   row <- df[df$id == x$id,c("Goals","Assists") ]
#
#
#   ### issues with Most Goals asn Most Cards
#
#   test <- read_csv("mostGoals.csv")
#
#
#   test %>%
#     select(Player=name,Goals=sumGoals) %>%
#     DT::datatable(class='compact stripe hover row-border',options= list(
#       pageLength = 5,lengthChange=FALSE,paging = TRUE, searching = FALSE, info=FALSE,sorting = FALSE))
#   paste0( names(row),": ",format(row), collapse = "<br />")
# }
#
# df %>%
#   ggvis(~season,~Ppm,  key := ~id) %>%
# #  layer_lines() %>%
#   layer_points(size= ~Points, fill:="red") %>%
#   add_tooltip(all_values,"hover") %>%
#   hide_legend("size") %>%
# add_axis("x", properties = axis_props(labels = list(
#   angle = 45, align = "left", fontSize = 11
# )),title = "") %>%
#   add_axis("y", title="Points per 90 mins")
#
# observe({
#
#   if(is.null(careerData())) return()
#   #print("enter observe")
#   #print(glimpse(careerData()$dfChart))
#
#   df <- careerData()$dfChart
#
#   df <- cbind(df, id = seq_len(nrow(df)))
#
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- df[df$id == x$id,c("date","Opponents","on","off","Goals","Assists") ]
#     paste0( names(row),": ",format(row), collapse = "<br />")
#   }
#
#
#   df %>%
#     ggvis(~plGameOrder, ~mins, key := ~id) %>%
#     layer_points(fill = ~Team, size = ~ points) %>%
#     add_tooltip(all_values,"hover") %>%
#
#     add_axis("y", title="Minutes Played", format='d') %>% # attempt to enforxe 0 , values=c(0,15,30,45,60,75,90)
#     add_axis("x", title="Match Day Squad Game Order", format='d') %>%
#     hide_legend("size") %>%
#     bind_shiny("careerChart")
#
# })



# starter <- playerGame %>%
#   filter(PLAYERID=="BAMFORP"&START>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
#   mutate(starterOrder=row_number())
#
#
#
# ### create reg update of player geos
# currently in global
#
# pgMini <- playerGame %>%  ## so wil only show those that have made an appearance - but that is prob ok
#   select(PLAYERID,name,city,COUNTRY) %>%
#   unique() %>%
#
#   left_join(playerGeos,by=c("PLAYERID"="playerID")) %>%
#   filter(PLAYERID!="OWNGOAL") %>%
#   mutate(place=ifelse(is.na(city),COUNTRY,paste0(city," ",COUNTRY)))
#
# ## nothing missing? maybe because updated playerGeos from when
# but playerGeos was done 20th Aug no there are NAs just dont show up easily in RStudio
# http://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/ # more robust
#
# toGet <- playerGame %>%  ## so wil only show those that have made an appearance - but that is prob ok
#   filter(is.na(Lat)) %>%
#   unique()
#
# toGet <- pgMini %>% #27
#   filter(is.na(lat))
#
# for (i in 1:nrow(toGet)){
#
#       print(i)
#     tempdf <- geocode(paste0(toGet$city[i],", ",toGet$COUNTRY[i]), source="google")
#     tempdf <- cbind(playerID=toGet$PLAYERID[i],tempdf)
#      if (i!=1) {
#        df <- rbind(df,tempdf)
#      } else {
#        df <- tempdf
#      }
# }
#
# warnings() ## looks like lots of them are due to accents
# geocode failed with status INVALID_REQUEST, location = "Reykjavík, Iceland"
# 2: geocode failed with status INVALID_REQUEST, location = "Križovany nad Dudváhom, Slovakia"
#
# ## so need to look at those separately
#
# oldGeos <- read_csv("playerGeos.csv")
#
# ## do a check
# oldGeos %>%
#   filter(playerID=="SONGR") # yep NA NA #4236 df is 27
#
# newGeos <-rbind(oldGeos,df) %>% unique() #4251
#
# # this is from ggmap
# ## try other source dsk no need to put source
# toGet <- newGeos %>%
#   filter(is.na(lat)) %>%
#   left_join(pgMini,by=c("playerID"="PLAYERID"))# %>%
#   #select(PLAYE#12
#
# for (i in 1:nrow(toGet)){
#
#   print(i)
#   tempdf <- geocode(paste0(toGet$city[i],", ",toGet$COUNTRY[i]))
#   tempdf <- cbind(playerID=toGet$playerID[i],tempdf) # NB now playerID
#   if (i!=1) {
#     df <- rbind(df,tempdf)
#   } else {
#     df <- tempdf
#   }
# }
# ## didnt help
#
# write_csv(toGet,"problemGeos.csv")
# # temporarily just use counntry
#
# for (i in 1:nrow(toGet)){
#
#   print(i)
#   tempdf <- geocode(toGet$COUNTRY[i], source="google")
#   tempdf <- cbind(playerID=toGet$playerID[i],tempdf)
#   if (i!=1) {
#     df <- rbind(df,tempdf)
#   } else {
#     df <- tempdf
#   }
# }
#
# newGeos <-rbind(newGeos,df) %>% unique() #4263
#
# write_csv(newGeos,"playerGeos.csv")
#
# ## checkout problemgeos
#
# problemGeos <- read_csv("problemGeos.csv")
#
# geocode("Farvang, Denmark", source="google") #google maps vannot find
# geocode("Farvang, Denmark")
#
# CORRECTED IN ACCESS - sometimes town changed ore removed
#
# ## look at fixture previews - see if can find elsewhere
#
# test <-data.frame(standings %>%
#                     arrange(gameDate) %>%
#                     filter(team=="Arsenal"&season=="2013/14"))
#
# standings$resCode <- 1
#
# standings[standings$res=="Draw",]$resCode <- 0
# standings[standings$res=="Loss",]$resCode <- -1
#
# library(sparkline)
# ars <- standings %>%
#   ungroup() %>%
#   mutate(GD=GF-GA) %>%
#   filter(team=="Arsenal"&season=="2013/14")
#
#
# sparkline(ars$GD,, type='bar')
#
# looks good but no detailed results
#
# unique()
#
# homePrem <-standings %>%
#   ungroup() %>%
#   arrange(gameDate) %>%
#
#   filter(team=="Crystal P"&OppTeam=="Man. City"&venue=="H") %>%
#   mutate(GD=GF-GA)
#
# sparkline(homePrem$GD, type='bar')
#
#
# prem <-standings %>%
#   ungroup() %>%
#   arrange(gameDate) %>%
#
#   filter(team=="Crystal P"&OppTeam=="Man. City") %>%
#   mutate(GD=GF-GA)
#
# sparkline(homePrem$GD, type='bar')
#
# sparkline(prem$GD, type='bar')
#
#
# ###
#
# for tooltip i had raised as issue
#
# Thinks for the example you give above, but I encounter some problems when running your code ,This is the code :
#   df <- data.frame(
#     season = rep(1992:1993, each=5),
#     result = c(1,0,1,-1,0,0,1,1,0,-1),
#     goals = c(2,0,1,0,3,0,2,3,1,0)
#   )
#
# library(dplyr)
# x = df %>%
#   group_by(season) %>%
#   summarize(
#     result = paste(result, collapse = ","),
#     goals = paste(goals, collapse = ",")
#   )
#
# library(sparkline)
# library(DT)
# columnDefs = list(list(
#   targets = c(1, 2),
#   render = JS("function(data, type, full){
#               return '' + data + ''
#
#               }")
# ))
#
# fnDrawCallback = JS("function (oSettings, json) {
#                     $('.spark:not(:has(canvas))').sparkline('html', {
#                     type: 'bar',
#                     highlightColor: 'orange'
#                     });
# }")
#
# d1 <- datatable(x, options = list(
# columnDefs = columnDefs,
# fnDrawCallback = fnDrawCallback
# ))
# d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency('sparkline'))
# d1 # works but is data no spakleine
#
#
# ### sequence issues player goals  eg frankie bennett never scored as starter
# first last slength midpoint value
# 1     1    5       5        3     0
#
# run <- data.frame(first=1,last=5,slength=5,midpoint=3,value=0)
#
# gameGoal <- run %>%
#   filter(value==1) %>%
#   group_by(slength) %>%
#   tally()
#
# nrow(gameGoal)
#
# gameGoal <- data.frame(slength=0,n=0)
#
# cond <- FALSE
#
# ggplot(gameGoal, aes(x=slength,y=n)) +
#   geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
#   geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
#   scale_x_discrete(breaks = pretty_breaks()) +
#   scale_y_discrete(breaks = pretty_breaks()) +
#   theme_bw() +
#   xlab("Sequence") +
#   ylab("Count") +
#   ggtitle("Games Scored In (Starter)")
#
# # this shows up instead of graph
# Error in matrix(value, n, p) :
#   'data' must be of a vector type, was 'NULL'
#
# ## player pix
#
# eg Josh Sheehan comes back with
# http://www.premierleague.com/content/dam/premierleague/shared-images/site-furniture/players/-lsh.jpg
# which is great
# however if has not appeared (eg bench only there is a crash)
# starter <- playerGame %>%
#   filter(PLAYERID=="SHEEHAJ"&(START+subOn)>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
#   mutate(starterOrder=row_number())
#
# runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value="No")
# runStarter$value <- as.character(runStarter$value)
#
# # first last slength midpoint value
# # 1     0    0       0        0    No
#
# chart <- data.frame(gameDate=as.Date("2000-01-01"),slength=0,Scored="No")
# gameDate slength Scored
# 1 2000-01-01       0     No
#
# chart %>%     ggvis(~gameDate,~slength) %>%
#   layer_points(fill=~ Scored, size=2) %>%
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 11
#   )),title = "") %>%
#   add_axis("y", title="Run of Games")
#
# ## ggplot wuld show but needs work on - need to show 0 date etc
#
#
# starter <- playerGame %>%
#   filter(PLAYERID=="SHEEANJ"&(START+subOn)>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
#   mutate(starterOrder=row_number())
#
#
# print(glimpse(starter))
# print("starteransub")
# print(nrow(starter))
# # if (nrow(starter)==0) return()
#
# if(nrow(starter>0)) {
#   starter$Scored <- "No"
#   starter$Scored[starter$Gls>0] <- "Yes"
# }
# print("OKA")
# if (nrow(starter)==0) {
#   print("set up df")
#   runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value="No")
#   runStarter$value <- as.character(runStarter$value)
#   chart <- data.frame(gameDate=as.Date("2000-01-01"),slength=0,Scored="No")
# } else {
#   runStarter <- subSeq(starter$Scored)
#   chart <-  runStarter %>%
#     left_join(starter,by=c("last"="starterOrder")) %>%
#     select(gameDate,slength,Scored)
# }
# #   print("OKB")
# #   print("starter")
# #   print("glimpse(runStarter)")
# #   print(glimpse(runStarter))
# #
# #
# # #   starter$Scored <- "No"
# # #   starter$Scored[starter$Gls>0] <- "Yes"
# # #
# # #   runStarter <- subSeq(starter$Scored)
# #
# #   print(glimpse(runStarter))
#
# #print(runStarter)
#
# ## can take first,last or midpoint to have run not scoring/scoring
#
# #   chart <-  runStarter %>%
# #     left_join(starter,by=c("last"="starterOrder")) %>%
# #     select(gameDate,slength,Scored)
#
# #print("chart")
# #print(chart)
# #print("chart done")
#
# chart %>%     ggvis(~gameDate,~slength) %>%
#   layer_points(fill=~ Scored, size=2) %>%
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 11
#   )),title = "") %>%
#   add_axis("y", title="Run of Games") %>%
#   set_options(width=400, height=400)
#
#
#
#
#
#
# chart %>%
#   ggplot(aes(x=gameDate,y=slength)) +geom_point()
#
#
# ## joe hart runapp/runstarter
#
# first last slength midpoint value
# 1     1  271     271      136     0
# first last slength midpoint value
# 1     1  271     271      136     0
#
# AARONSR no data
#
# appeared <- playerGame %>%
#   filter(PLAYERID=="AARONSR"&(START+subOn)>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
#
# # PLAYERID   Gls plGameOrder    TEAMNAME   gameDate Opponents
# # (chr) (int)       (int)       (chr)     (date)     (chr)
# # 1  AARONSR     0           1 Newcastle U 2014-08-17 Man. City
# # 2  AARONSR     1           2 Newcastle U 2014-08-30 Crystal P
# # 3  AARONSR     0           3 Newcastle U 2014-11-01 Liverpool
# # 4  AARONSR     0           5 Newcastle U 2015-05-16       QPR
# # 5  AARONSR     0           7 Newcastle U 2015-08-15   Swansea
#
# appeared$Scored <- 0
# appeared$Scored[appeared$Gls>0] <- 1
#
# runApp <- subSeq(appeared$Scored)
#
# first last slength midpoint value
# 1     1    1       1        1     0
# 2     2    2       1        2     1
# 3     3    5       3        4     0
#
#
# as sytatrer
#
# starter <- playerGame %>%
#   filter(PLAYERID=="BENTD"&START>0) %>%  # 380 as sh
#   arrange(gameDate) %>%
#   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
#
# print(nrow(starter))
#
# starter <- as.data.frame(starter)
#
# if(nrow(starter)==0)) {
#   print("started")
#   starter$Scored <- 0
#   starter$Scored[starter$Gls>0] <- 1
#   runStarter <- subSeq(starter$Scored)
#
# } else {
#   print("never started")
#   runStarter <- data.frame(first=0,last=0,slength=0,midpoint=0,value=0)
# }
#
#
# ## checking out error
#
# Error in eval(substitute(expr), envir, enclos) :
#   corrupt 'grouped_df', contains 20 rows, and 477 rows in groups
#
# maxDate <- standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==5&season=="2015/16")
# maxDate <- max(maxDate$gameDate) #"2015-08-10" looks good
#
# print(maxDate)
#
# # currentTeams <- df$TEAMNAME may want to use this in below if just
#
# ty <- data.frame(playerGame %>%
#                    filter(gameDate<=maxDate&season==currentYear) %>%
#                    group_by(PLAYERID,name,TEAMNAME) %>%
#                    summarise(goals=sum(Gls),assists=sum(Assists)))
#
# print(glimpse(ty))
#
# tyGoals <- ty %>%
#   arrange(desc(goals)) %>%
#   group_by(TEAMNAME) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(team=TEAMNAME,goals,name)
#
# print("good to here")
#
# #tyGoals[tyGoals$goals==0,]$name <- ""
#
# tyGoals$name <- ifelse(tyGoals$goals==0,"",tyGoals$name)
#
# tyAssists <- ty %>%
#   arrange(desc(assists)) %>%
#   group_by(TEAMNAME) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(team=TEAMNAME,assists,name)
#
# #tyAssists[tyAssists$assists==0,]$name <- ""
# tyGoals$name <- ifelse(tyAssists$assists==0,"",tyGoals$name)
#
#
# ## for cards
# ty <- playerGame %>%
#   ungroup() %>%
#   filter(gameDate<=maxDate&season==currentYear&CARD>1) %>%
#   group_by(PLAYERID,name,TEAMNAME) %>%
#   tally()
#
# tyCards <- ty %>%
#   arrange(desc(n)) %>%
#   group_by(TEAMNAME) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(team=TEAMNAME,cards=n,name)
#
# ## cp had no card first day so next is not relevant - need to correct later
#
#
#
# tyAll <- tyGoals %>%
#   inner_join(tyAssists,by=c("team"="team"))
#
#
# tyAll <- tyGoals %>%
#   inner_join(tyAssists,by="team") %>%
#   left_join(tyCards,by="team")
#
# ## need to correct for teams that have no cards
# #  tyAll[is.na(tyAll$cards)]$name <- ""
# tyAll$cards <-ifelse(is.na(tyAll$cards),0,tyAll$cards)
# tyAll$name <-ifelse(is.na(tyAll$name),"",tyAll$name)
#
# tyAll %>%
#   DT::datatable(class='compact stripe hover row-border',colnames = c('Team', '', 'Goals', 'Assists', '', 'Cards', ''),rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# #DT::datatable(class='compact stripe hover row-border',colnames = c('Player', 'Start', 'On', 'Off', 'Goals', 'Assists', 'Card'),
#
#
# song billong 3 teams?
#
# ## look at some overall say leading goalscorers byt team how do they rank
#
# ## mins played by country
# sort(names(summary))
# sort(names(playerGame))
#
# allPlayers <- playerGame %>%
#   select(name,PLAYERID,COUNTRY) %>%
#   unique() #4158
#
# # by team
# temp <-summary %>%
#   ungroup() %>%
#   select(PLAYERID,mins,season,TEAMNAME) %>%
#   left_join(allPlayers) %>%
#   select(season,mins,COUNTRY,TEAMNAME) %>%
#   group_by(season,COUNTRY,TEAMNAME) %>%
#   summarize(totMins=sum(mins)) %>%
#   group_by(season,TEAMNAME) %>%
#   mutate(allMins=sum(totMins),pc=100*totMins/allMins) ## allMins not same for all clubs ???
#
# allSeasons <- data.frame(season=unique(summary$season))
#
# temp %>%
#   ungroup() %>%
#   filter(COUNTRY=="England"&TEAMNAME=="Man. City") %>%
#   right_join(allSeasons) %>%
#   mutate(pc=ifelse(is.na(pc),0,pc)) %>%
#   ggvis(~season,~pc) %>%
#   layer_points()
#
# ## allteams
allTemp <-Sys.info()

#
# allTemp %>%
#      ungroup() %>%
#      filter(COUNTRY=="England") %>%
#      ggvis(~season,~pc) %>%
#      layer_lines(stroke:="coral", strokeWidth:=3) %>%
#      scale_numeric("y",domain=c(0,100)) %>%
#      add_axis("y",title="% Mins played by English Born Players in BPL") %>%
#        add_axis("x", properties = axis_props(labels = list(
#            angle = 45, align = "left", fontSize = 10
#          )),title = "")
#
#
#
#
# ## table
#
# temp %>%
#   filter(season=="2015/16"&COUNTRY=="England") %>%
#   mutate(English_pc=round(pc,0)) %>%
#   ungroup() %>%
#   select(Team=TEAMNAME,English_pc) %>%
#   arrange(English_pc) %>%
#   DT::datatable(class='compact stripe hover row-border order-column',
#                 rownames=FALSE,
#                 colnames = c('Team','% English'),
#                 width=200,
#                 options= list(paging = FALSE, searching = FALSE,info=FALSE))
#
# temp %>%
#   ungroup() %>%
#   filter(COUNTRY=="England"&TEAMNAME=="Crystal P") %>%
#
#   ggvis(~season,~pc) %>%
#   layer_points()
#
#
# ## for shiny need to get the blank seasons in especially if line
#
#
# allTemp %>%
#   ungroup() %>%
#   filter(COUNTRY=="England") %>%
#   ggvis(~season,~pc) %>%
#   layer_lines(stroke:="coral", strokeWidth:=3) %>%
#   layer_points() %>%
#   handle_click(getSeason) %>%
#   add_axis("y",title="% Mins played by English Born Players in BPL") %>%
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 10
#   )),title = "")
#
# ## do mins for selected team/year/country
#
# playerClubSeason <-summary %>%
#   ungroup() %>%
#   select(name,PLAYERID,mins,season,TEAMNAME) %>%
#   left_join(allPlayers)
#
#
# %>%
#   select(name,PLAYERID,season,mins,COUNTRY,TEAMNAME) %>%
#   group_by(season,COUNTRY,TEAMNAME) %>%
#   summarize(totMins=sum(mins)) %>%
#   group_by(season,TEAMNAME) %>%
#   mutate(allMins=sum(totMins),pc=round(100*totMins/allMins))
#
# ### take a look at mangers - timelines/sequences pardew/head to head murinho/wenger
#
# names(managers)
# #[1] "Lastname"  "FirstName" "ManagerID" "TeamID"    "Joined"    "Left"      "TEAMNAME"  "Caretaker"
# glimpse(managers)
# str(managers)
# ## result ppg win%
#
# sort(names(standings))
#
# ifelse(is.na(managers$Left),as.Date(Sys.Date(), origin= '1970-01-01'),as.Date(managers$Left, origin= '1970-01-01'))
#
# is.na(managers$Left) <-
#
#
#  managers <- readRDS("managers.rds")
# #   added to updating sql
# #   managers$Left <- as.Date(managers$Left/(60*60*24), origin= '1970-01-01')
# #   managers$Joined <- as.Date(managers$Joined/(60*60*24), origin= '1970-01-01')
# #   managers$Left <- as.Date(managers$Left, origin= '1970-01-01')
#  managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01')
# 
# 
# #   this looks good
# #  pardew <-managers %>%
# #    mutate(Left=ifelse(is.na(managers$Left),as.Date(Sys.Date(), origin= '1970-01-01'),as.Date(managers$Left, origin= '1970-01-01'))) %>%
# #    filter(ManagerID=="PardewA") %>%
# #    inner_join(standings,by=c("TEAMNAME"="team")) %>%
# #    select(Lastname,FirstName,ManagerID,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>%
# #    filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left))
# #
#  managerGame <-managers %>%
#    mutate(name=paste(FirstName,Lastname)) %>%
#    group_by(ManagerID,ManagerTeam) %>%
#    inner_join(standings,by=c("TEAMNAME"="team")) %>%
#    select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>%
#    filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>%
#    mutate(points=ifelse(res=="Win",3,ifelse(res=="Draw",1,0)))
# 
# #  all stints at team
#  ppgManagerTeam <- managerGame %>%
#    group_by(TEAMNAME,ManagerID,name) %>%
#    summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2))
# 
# #  separate stints at team eg Ball at Sunderland
#  ppgManagerTeamStint <- managerGame %>%
#    group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>%
#    summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2))
# 
#  ppgManagerTeam  %>%
#    ungroup() %>%
#    filter(TEAMNAME=="Tottenham H") %>%
#    arrange(desc(ppg))
# 
# # now combine to get start end date for chart
# 
#  allManagerStints <-
#    managerGame %>%
#    select(name,ManagerTeam,Joined,Left) %>%
#    unique()
# 
# # allManagerStints$Joined <- as.Date("1992-08-15")
# 
#  allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"

# #  may need to change joined date
#   glimpse(standings) earlies t"1992-08-15"
#  ppgManagerTeamStint  %>%
#
#    select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
#    inner_join(allManagerStints) %>%
#    mutate(startDate=ifelse(ppgManagerTeamStint$Joined<=as.Date("1992-08-15"),as.Date("1992-08-15"),ppgManagerTeamStint$Joined)) %>%
#    mutate(startDate=ifelse(Joined<=as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',Joined)) %>%
#    filter(TEAMNAME=="Arsenal") %>%
#    ggvis(~Joined,~ppg) %>%
#    layer_points()
#
#   looks Ok just need to wait for timeline htmlswidget and extend to left
#



#  (60*60*24), origin= '1970-01-01'
#
#  managerGame %>%
#    tally(group_by(TEAMNAME,ManagerID))%>%
#    summarize(sumPoints=sum(points),games=n())
#
#  as.POSIXct.Date(Sys.Date()) [1] "2015-09-19 17:00:00 PDT"
#
#   think about time ahead
#
#  names(goals)
#
#   All Managers at club
#  ppgManagerTeamStint  %>%
#
#    select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
#    inner_join(allManagerStints) %>%
#    mutate(startDate=ifelse(ppgManagerTeamStint$Joined<=as.Date("1992-08-15"),as.Date("1992-08-15"),ppgManagerTeamStint$Joined)) %>%
#     mutate(startDate=ifelse(Joined<=as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',Joined)) %>%
#    filter(TEAMNAME=="Liverpool"&games>5) %>%
#
#    ggvis(x =~ Joined,y=~ppg+0.01,fill = "687a97") %>%
#    layer_rects(x2=~Left,y2=~ppg-0.01) %>%
#    layer_text(text:=~name, stroke:="red") %>%
#    scale_numeric("y",domain=c(0,3)) %>%
#    add_axis("x", title="") %>%
#    add_axis("y", title="Av Points per Game")

#  Sam Allardyce
#
#
#   Managers  byclub
#
#   add line then add %>%layer_paths(~x,~meanvalue,stroke:=black)

 ppgManagerTeamStint  %>%

   select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
   inner_join(allManagerStints) %>%
   filter(name=="Sam Allardyce"&games>5) %>%
   mutate(ppgRequired=1.233) %>%

   ggvis(x =~ Joined,y=~ppg+0.01,fill = "687a97") %>%
   layer_rects(x2=~Left,y2=~ppg-0.01) %>%
   layer_text(text:=~TEAMNAME, stroke:="red") %>%
   scale_numeric("y",domain=c(1,1.5)) %>%
   add_axis("x", title="") %>%
   add_axis("y", title="Av Points per Game") %>%
   layer_paths(x=2000,y=1.233)



# ## ages per game
# sort(names(playerGame))
#
# temp <-playerGame %>%
#   filter(START>0) %>%
#   group_by(TEAMMATCHID) %>%
#   arrange(age) %>%
#   mutate(ageOrder=row_number()) %>%
#   select(PLAYERID,name,gameDate,age,ageOrder,team=TEAMNAME)
#
# temp %>%
#   filter(PLAYERID=="ROONEYX") %>%
#   ggvis(~gameDate,~ageOrder) %>%
#   layer_points(fill= ~ team, size=2) %>%
#   add_axis("x", title="") %>%
#   add_axis("y", title="Age Rank of Starters")
#
# playerTemp <- temp %>%
#   filter(PLAYERID=="KEANER")
#
# temp %>%
#   filter(TEAMMATCHID==34533) %>%
#   select(name,age,ageOrder)
#
# temp %>%
#   filter(PLAYERID=="OSHEAJ") %>%
#   ggvis(~gameDate,~ageOrder) %>%
#   layer_points(fill= ~ team, size=2) %>%
#   add_axis("x", title="") %>%
#   add_axis("y", title="Age Rank of Starters")
#
# write_csv(temp,"ageRank.csv")
#
#
#
#
# # team minutes ahead ------------------------------------------------------
#
# ## shold be able to do other stuff with this
# ## have got something with minutes not scoring need to look at that
#
# names(standings)
#
# sort(names(teamGames))
#
# teamGames %>%
#   ungroup() %>%
#   filter(team=="Chelsea") %>%
#   select(team,tmGameOrder,MATCHID,TEAMMATCHID,season)
#
#
#
#
# goalsByTeam %>%
#
#
#   # fixture facts -----------------------------------------------------------
#
# ## team matchups
# ## results, most played, etc
#
#
# ## average starter age
# sort(names(playerGame))
# sort(names(standings))
# p <-  playerGame %>%
#   filter(START>0&!is.na(age)) %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team) %>%
#   summarize(avAge=mean(age)) %>%
#   inner_join(standings) %>%
#   filter(team=="Chelsea") %>%
#   group_by(res) %>%
#   ggvis(~gameDate,~avAge) %>%
#   layer_points(fill= ~res)
#
# playerGame %>%
#   filter(START>0&!is.na(age)) %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team) %>%
#   summarize(avAge=mean(age)) %>%
#   inner_join(standings) %>%
#   #filter(team=="Chelsea") %>%
#   group_by(res) %>%
#   ggvis(~gameDate,~avAge) %>%
#   layer_points(fill=~res) # cannot do boxplots takes a while in
#
#
# # look at fee changes
#
# p <-  playerGame %>%
#   filter(START>0&FEE!=99)
# str(p)
# sort(table(p$FEE)) ??
#
#
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#
#   filter(team=="Crystal P") %>%
#
#   ggvis(~season,~avFEE) %>%
#   #layer_points() ## not in correct order
#   layer_boxplots() ## better
#
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#
#   filter(season=="2015/16"&avFEE<5000) %>%
#
#   ggvis(~team,~avFEE) %>%
#   #layer_points() ## not in correct order
#   layer_boxplots()
#
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#
#   filter(season=="2015/16") %>%
#
#   ggvis(~team,~avFEE) %>%
#   #layer_points() ## not in correct order
#   layer_boxplots() %>%
#   add_axis("x", title="",properties = axis_props(labels = list(
#            angle = 45, align = "left", fontSize = 11
#         ))) %>%
#   add_axis("y",title="Av Fee of Starters by Game",title_offset=50)
#
#
# ## CHECK MAX
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(TEAMMATCHID,gameDate,team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#
#   #filter(season=="2015/16") %>%
#   ungroup() %>%
#   arrange(desc(avFEE)) # 41727
#
# playerGame %>%
#   filter(TEAMMATCHID==41727&START>0) %>%
#   select(name,FEE) %>%
#   arrange(desc(FEE)) %>%
#   summarize(av=mean(FEE)) #27231.82
#
# ## all time most expensive
#
# mostCostly <- playerGame %>%
#   select(name,FEE) %>%
#   unique()
#
# p %>%
#   group_by(TEAMNAME,gameDate,TEAMMATCHID) %>%
#   summarize(totFee=sum(FEE)) %>%
#   filter(totFee>27231) %>%
#   ungroup() %>%
#   arrange(gameDate) #14281
#
# playerGame %>%
#   filter(TEAMMATCHID==14281&START>0) %>%
#   select(name,FEE) %>%
#   arrange(desc(FEE))
#
#
#
#
#
# ## look at spend and league positon
# names(standings)
# test <-  p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#   inner_join(standings) %>%
#   filter(season=="1992/93") %>%
#   group_by(team) %>%
#   ggvis(~avFEE, ~final_Pos, fill=~team)
#
# #def might make a good trevis
#
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#   inner_join(standings) %>%
#   filter(team=="Chelsea") %>%
#   group_by(final_Pos) %>%
#   ggvis(~avFEE, ~season, fill=~final_Pos) %>%
#   layer_points()
#
# ## nice to have a color ramp ideally
# p %>%
#   rename(team=TEAMNAME) %>%
#   group_by(team,season) %>%
#   summarize(avFEE=mean(FEE, na.rm=T)) %>%
#   inner_join(standings) %>%
#   filter(team=="Chelsea") %>%
#   group_by(as.factor(final_Pos)) %>%
#   ggvis(~avFEE, ~season, fill=~as.factor(final_Pos)) %>%
#   layer_points()
#
#
# ## could do record fees like ages
#
# ## interesting couple over 32 and one v low at just 20 ( it was a loss)
#
# ## could add result/link to team/manager or season as bakdrop/plotly??
#
# ggvis()
#


# # trying out qtlcharts ----------------------------------------------------
# # more for use in calculating trelliscopes for genetic info?
#
# #http://kbroman.org/qtlcharts/assets/vignettes/userGuide.html
#
#
# #You first need to load the package.
#
# library(qtlcharts)
# #Let’s begin by considering the function iplotCorr, which creates a heatmap of a correlation matrix, linked to scatterplots of the underlying variables.
#
# #We’ll first load the geneExpr dataset, included with the R/qtlcharts package.
#
# data(geneExpr) # shows as promise in global environment
#
#
# #This is a list with two components. The first component, geneExpr$expr, is a 491 × 100 matrix of gene expression data; the second component, geneExpr$genotype, is a vector of genotypes (of length 491) at a QTL that influences those 100 genes’ expression values. (The genes were selected from a larger expression genetics study, on that basis: that they are all influenced by this QTL.)
#
# #Let’s pull out those two components of geneExpr as separate objects, expr and geno.
#
# expr <- geneExpr$expr
# class(expr) #matrix
# geno <- geneExpr$genotype
# class(geno) #numeric
# #The simplest use of iplotCorr is with a numeric matrix, as with the expr dataset. For example:
#
#   iplotCorr(expr, reorder=TRUE)
# #This will open an interactive figure in a web browser, with a heat map of the correlation matrix of the genes on the left linked to the underlying scatterplots. With the argument reorder=TRUE, the genes are reordered (by hierarchical clustering with the R function hclust) to bring genes with similar expression patterns next to each other.
# # try with expr as data.frame
#   df <- data.frame(expr) #491 obs of 100 variables
#   glimpse(df)
#   iplotCorr(df, reorder=TRUE) # appears same as the matrix
#
#
#
#
# # look at plotly ----------------------------------------------------------
#
#   bestRun <- goalSeqs %>%
#
#     select(PLAYERID,slength) %>%
#     arrange(PLAYERID,desc(slength)) %>%
#     group_by(PLAYERID) %>%
#     slice(1)
#
#   topScorers <- playerGame %>%
#     filter(PLAYERID %in% bestRun$PLAYERID) %>%
#     group_by(name,PLAYERID) %>%
#
#     summarize(totGoals=sum(Gls)) %>%
#     inner_join(bestRun)
#
#   topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))
#
#   # need jitter to identify individuals
#   topScorers$jitGoals <- jitter(topScorers$totGoals)
#   topScorers$jitslength <- jitter(topScorers$slength)
#
#
#
#
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
#     #paste0(names(row),": ", format(row), collapse = "<br />")
#     paste0( format(row), collapse = "<br />")
#   }
#
#     topScorers %>%
#       ggvis(~jitGoals,~jitslength,key := ~id) %>%
#       layer_points(size=2) %>%
#       add_axis("y",title="Best Consecutive Scoring Run in PL games", format='d') %>%
#       add_axis("x", title="Career Premier League goals") %>%
#       add_tooltip(all_values,"click") %>%
#       bind_shiny("allPlayerGoalSeqs")
#
#     ## so look at ggplot alternative
#     library(ggplot2)
#
#  p <-   topScorers %>%
#       ggplot(aes(x=jitGoals,y=jitslength)) +
#       geom_point()
#  library(plotly)
#  ggplotly(p)
#
#  ## add more info
#
#  glimpse(topScorers)
#
#
#  p <- ggplot(data = dsamp, aes(x = carat, y = price)) +
#    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
#    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)
#
#  p <-   topScorers %>%
#    ggplot(aes(x=jitGoals,y=jitslength)) +
#    geom_point(aes(text = paste(name,"<br> Goals:",totGoals,"<br> Sequence:",slength)), size = 4) +
#    ylab("Best Scoring Sequence - jittered to show all Players") +
#    xlab("Career Goals")
#
#  ggplotly(p)
#
#  # vignette example
#
#  library(ggplot2)
#  library(plotly)
#  library(dplyr)
#
#  df <- data.frame(Goals=c(1,1,4),jitGoals=c(1.1,0.9,4.2),Seq=c(1,1,2),jitSeq=c(0.9,1,2.1),name=c("A","B","C"))
#
#
#  p <-   df %>%
#    ggplot(aes(x=jitGoals,y=jitSeq)) +
#    geom_point(aes(text = paste("Name:",name,"<br> Goals:",Goals,"<br> Sequence:",Seq)), size = 4) +
#    ylab("Best Scoring Sequence - jittered to show all Players") +
#    xlab("Career Goals")
#
#
#  ggplotly(p)


# switch career chart to plotly (zooming) -------------------------------------------

# dfChart <- playerGame %>%
#   filter(PLAYERID=="BENTD") %>%
#   select(date=gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID) %>%
#   mutate(points=(Goals+Assists)) # +1 to pointsthis would not be so bad but lists the points on text
#
#
# #   df <- careerData()$dfChart
# #
# #   df <- cbind(df, id = seq_len(nrow(df)))
# #
# #   all_values <- function(x) {
# #     if(is.null(x)) return(NULL)
# #     row <- df[df$id == x$id,c("date","Opponents","on","off","Goals","Assists") ]
# #     paste0( names(row),": ",format(row), collapse = "<br />")
# #   }
#
#
# plot_ly(dfChart, x = plGameOrder, y = mins, mode = "markers", hoverinfo = "text",
#         color=Team,size=points,
#         text = paste("v ",Opponents,"<br> ",date,"<br> Goals:",Goals,"<br> Assists:",Assists)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Match Day Squad Game Order"),
#          yaxis=list(title="Minutes Played "
#          )
#   )
#
# ## try ggplot
#
# p <- ggplot(dfChart,aes(x = plGameOrder, y = mins, color=Team, size=points))+
#      geom_point(text = paste("v ",Opponents,"<br> ",date,"<br> Goals:",Goals,"<br> Assists:",Assists)) # not really working
#
# ggplotly(p)
#
#
# plotly(p,hoverinfo="text",text="test") Error in plotly(p, hoverinfo = "text", text = "test") :
#   unused arguments (hoverinfo = "text", text = "test")
#
#
#
# # points cf previous year -------------------------------------------------
#
# sort(names(standings))
#
# thisYear <- standings %>%
#   ungroup() %>%
#   filter(season=="2015/16"&tmYrGameOrder==14) %>%
#   select(team,TY=cumPts)
#
#
# # diff <-standings %>%
# #   ungroup() %>%
# #   filter(season=="2014/15"&tmYrGameOrder==13) %>%
# #   select(team,LY=cumPts,final_Pos) %>%
# #   inner_join(thisYear) %>%
# #   mutate(Difference=TY-LY) %>%
# #   arrange(desc(Difference))
# #
# # diff %>%
# #   ggvis(~Difference,~team, group=final_Pos) %>%
# #   layer_points(fill=final_Pos)
#
#
#
# diff <-standings %>%
#   ungroup() %>%
#   filter(season=="2014/15"&tmYrGameOrder==14) %>%
#   select(team,LY=cumPts,final_Pos) %>%
#   inner_join(thisYear) %>%
#   mutate(Difference=TY-LY) %>%
#   arrange(Difference)
#
#  ## used for tweet
#           ## not getting rid of legend  is it due to nbeing markers and not trace scatter??
# plot_ly(diff, x = Difference, y = team, mode = "markers", hoverinfo = "text",
#         color=final_Pos, showlegend = FALSE,
#         text = paste("Last Year Finish: ",final_Pos,"<br> Current Diff: ", Difference," Points")) %>%
#   layout(hovermode = "closest", title= "Point Change compared with Last Season after 14 games",
#          xaxis=list(title="Point Difference"),
#          yaxis=list(title=""),
#                     margin = list(l = 100)
#
#          ) %>%
#   dplyr::filter(Difference == min(Difference)) %>%
#   layout(annotations = list(x = Difference, y = team, text = "Crap season",font=list(color="red"), showarrow = T, ax=20, arrowcolor="red")) # prob having 2?
#
# # extend to position, gf gd ga
# do some lag for any season
#
#
# sort(names(standings))
#
# test <-standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==14) %>%
#   select(team,season,cumPts) %>%
#   arrange(desc(season)) %>%
#   filter(team=="Chelsea") %>%
#   mutate(prev=lead(cumPts),diff=cumPts-prev)
#
#
# test <-standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==14) %>%
#   select(team,season,cumPts) %>%
#   arrange(season) %>%
#   filter(team=="Chelsea") %>%
#   mutate(prev=lag(cumPts),diff=cumPts-prev)
#
# test %>%
#   filter(!is.na(diff)) %>%
#   ggvis(~season,~diff) %>%
#   layer_points()
#
#
# test %>%
#   filter(!is.na(diff)) %>%
#   ggvis(~diff) %>%
#   layer_histograms()
#
#
#
# ## look at annotation
# p <- plot_ly(economics, x = date, y = uempmed)
#
# p %>%
#   #add_trace(y = fitted(loess(uempmed ~ as.numeric(date)))) %>%
#   layout(title = "Median duration of unemployment (in weeks)",
#          showlegend = FALSE) %>%  # here legend refers to two traces
#   dplyr::filter(uempmed == max(uempmed)) %>%
#   layout(annotations = list(x = date, y = uempmed, text = "Peak", showarrow = T))
#
#
# plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
#         text = paste(name,"<br> Total Goals:",totGoals,"<br> Best Run:",slength)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Career Premier League goals"),
#          yaxis=list(title="Best Consecutive Scoring Run in PL games "
#          )
#   )
#   df %>%
#     ggvis(~plGameOrder, ~mins, key := ~id) %>%
#     layer_points(fill = ~Team, size = ~ points) %>%
#     add_tooltip(all_values,"hover") %>%
#
#     add_axis("y", title="Minutes Played", format='d') %>% # attempt to enforxe 0 , values=c(0,15,30,45,60,75,90)
#     add_axis("x", title="Match Day Squad Game Order", format='d') %>%
#     hide_legend("size") %>%
#     bind_shiny("careerChart")
#
# })
#
# ## cretae the full dataframe
#
# yrs <- unique(standings$season)
# teams <- unique(standings$team)
#
#
#
# allSeasonTeams <- data.frame(season=rep(yrs, length(teams)),team=rep(teams, length(yrs))) ##1128
#
# ## looks good
# test <-standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==14) %>%
#   select(team,season,cumPts) %>%
#   right_join(allSeasonTeams) %>%
#   arrange(season) %>%
#  # filter(team=="Chelsea") %>%
#   group_by(team) %>%
#   mutate(prev=lag(cumPts),diff=cumPts-prev) %>%
#   filter(season>"1992/93") %>%
#   arrange(team)
#
# test %>%
#   filter(!is.na(diff)&team=='Chelsea') %>%
#   ggvis(~season,~diff) %>%
#   layer_points()  %>%
#   scale_numeric("y",zero=TRUE) # otherwise do not show for those where same eg cp
#
#
# ## takena look at all data
#
# test %>%
#   filter(!is.na(diff)) %>%
#   ggvis(~season,~diff) %>%
#   layer_boxplots()
# # or points if jittered
#
# look at plotly ( could just have one lot showing )
#
#
#
# names(test)
#
# plot_ly(test, x = season, y = diff, mode = "markers",color=team) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Career Premier League goals"),
#          yaxis=list(title="Best Consecutive Scoring Run in PL games "
#          )
#   )
#
# [1] "team"   "season" "cumPts" "prev"   "diff"
#
# # need to set jitter
# test$jitDiff <- jitter(test$diff)
#
# # this works
# plot_ly(test, x = season, y = jitDiff, mode = "markers", hoverinfo = "text", color=team,height=600,
#         text = paste(team,"<br> Season:",season,"<br> Points:",cumPts,"<br> Yr on Yr:",diff)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title=""),
#          yaxis=list(title="Point Difference"
#          )
#   )
#
# #experment
# plot_ly(test, x = season, y = jitDiff,  mode="markers", color=team,height=600,
#         text = paste(team,"<br> Season:",season,"<br> Points:",cumPts,"<br> Yr on Yr:",diff)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title=""),
#          yaxis=list(title="Point Difference"
#          )
#   )
#
#
#
# ## extend to shiny app
#
# teams <- c("Arsenal","Chelsea")
# rounds <- 12
#
# test <-standings %>%
#   ungroup() %>%
#   filter(tmYrGameOrder==rounds) %>%
#   select(team,season,cumPts) %>%
#   right_join(allSeasonTeams) %>%
#   arrange(season) %>%
#   # filter(team=="Chelsea") %>%
#   group_by(team) %>%
#   mutate(prev=lag(cumPts),diff=cumPts-prev) %>%
#   filter(season>"1992/93"&team %in% teams) %>%
#   ungroup() %>%
#   arrange(team)
#
# test$jitDiff <- jitter(test$diff)
#
#
# plot_ly(test, x = season, y = jitDiff, mode = "markers", hoverinfo = "text", color=team,height=600,
#         text = paste(team,"<br> Season:",season,"<br> Points:",cumPts,"<br> Yr on Yr:",diff)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="", tickfont=list(size=9)),
#          yaxis=list(title="Point Difference")
#          )
#
#
# ## need to do work on
#     library(RColorBrewer)
#
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#
# plot_ly(test, x = season, y = jitDiff, mode = "markers", hoverinfo = "text", color=team,marker=list(color = brewer.pal(6, "Paired")),height=600,
#         text = paste(team,"<br> Season:",season,"<br> Points:",cumPts,"<br> Yr on Yr:",diff)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="", tickfont=list(size=9)),
#          yaxis=list(title="Point Difference")
#   )
#
# test %>%
#   filter(!is.na(diff)&team=='Crystal P') %>%
#   ggvis(~season,~diff) %>%
#   layer_points() %>%
#   scale_numeric("y",zero=TRUE) # otherwise do not show
#
# test %>%
#   filter(!is.na(diff)) %>%
#   ggvis(~diff) %>%
#   layer_histograms()

# cp <- standings %>%
#     ungroup %>%
#     arrange(tmGameOrder) %>%
#     filter(team=="Crystal P")

# standings %>%
#   ungroup() %>%
#   filter(GF>5&team=="Chelsea")



# crosstalk - heatmap and games -------------------------------------------

# sort(names(standings))
#
# standings %>%
#   filter(team=="Man. Utd.")
# ## from backup folders premier league
#
# temp <- standings %>%
#   filter(team=="Man. Utd.") %>%
#   mutate(combo=paste0(GF,GA)) %>%
#   group_by(combo) %>%
#   tally()
#
#
# allCombos <- expand.grid(
#   data.frame(GF=0:9,GA=0:9)
# ) %>%
#   mutate(combo=paste0(GF,GA)) #still a df with 100vals
#
# test <- allCombos %>%
#   left_join(temp) # lots of NAs
#
# # seems pretty pointless renaming does same
# test <- test %>%
#   mutate(count=(n))
#
# test <- allCombos %>%
#   left_join(temp) %>%
#   select(GF,GA,count=n)
#
# library(tidyr)
#
# tmp <- data.frame(x=gl(2,3, labels=letters[24:25]),
#                   y=gl(3,1,6, labels=letters[1:3]),
#                   z=c(1,2,3,3,3,2))
#
# #   x y z
# # 1 x a 1
# # 2 x b 2
# # 3 x c 3
# # 4 y a 3
# # 5 y b 3
# # 6 y c 2
#
# class(spread(tmp, y, z)) #"data.frame"
#  t <-spread(tmp, y, z) t$x
#  t_mat <- as.matrix(t)
#
# #   x a b c
# # 1 x 1 2 3
# # 2 y 3 3 2
#
#  plot_ly(x = t$x, y = t$y, z = t, key = t,
#          type = "heatmap") %>%
#    layout(xaxis = list(title = ""),
#           yaxis = list(title = ""))
#
# #  Error in getLevels(domain, NULL, levels, ordered) :
# #    argument "domain" is missing, with no default
# #  In addition: Warning message:
# #    In RColorBrewer::brewer.pal(N, "Set2") :
# #    minimal value for n is 3, returning requested palette with 3 different levels
# #
#
# spread(test, GF, GA)
# #   x a b c
# # 1 x 1 2 3
# # 2 y 3 3 2
#
# library(rCharts)
#
#
# d1 <-  dPlot(
#   GA ~ GF
#   ,data = test
#   ,type = "bar"
#   ,yAxis = list( type = "addCategoryAxis" )
#   ,colorAxis = list(
#     type = "addColorAxis"
#     ,colorSeries = 'n'
#     ,palette = c('white','red')
#     ,outputFormat = "0.01%"
#   )
#   ,height = 400
#   ,width = 400
#   ,barGap = 0
# )
# d1 # does work but hacky heatmap
#
# is.matrix(as.matrix(1:10))
# !is.matrix(warpbreaks)  # data.frame, NOT matrix! is df with fields breaks wool tension
# warpbreaks[1:10,]
# as.matrix(warpbreaks[1:10,])  # using as.matrix.data.frame(.) method - but mkes values characters
#
# ## Example of setting row and column names
# mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
#                dimnames = list(c("row1", "row2"),
#                                c("C.1", "C.2", "C.3")))
# mdat
#
# class(test$count) #integer
#
# class(c(1,2,3, 11,12,13)) #numeric
# class(test$GF) # integer - may need to make charracter
#
# Games <- matrix(test$count, nrow = 10, ncol = 10, byrow = TRUE,
#                dimnames = list(unique(test$GF),
#                                unique(test$GA)))
# mdat
#
# nrow(test) #100
#
# r<- c("row1", "row2")
# c <- c("C.1", "C.2", "C.3")
#
# # C.1 C.2 C.3
# # row1   1   2   3
# # row2  11  12  13
#
# ## this works (not hoverinfo)
# plot_ly(x = unique(test$GF), y = unique(test$GF), z = mdat, key = mdat, hoverinfo="text",
#         text=paste0("Games:", test$count),
#         type = "heatmap") %>%
#   layout(xaxis = list(title = ""),
#          yaxis = list(title = ""))
#
# # even Games does not come up
# plot_ly(x = unique(test$GF), y = unique(test$GF), z = mdat, key = mdat, hoverinfo="text",
#         text=paste0("Games:"),
#         type = "heatmap") %>%
#   layout(xaxis = list(title = ""),
#          yaxis = list(title = ""))
#
# plot_ly(x = unique(test$GF), y = unique(test$GF), z = mdat, key = mdat, hoverinfo="text",
#         text=paste0("Games:"),
#         type = "heatmap") %>%
#   layout(xaxis = list(title = ""),
#          yaxis = list(title = ""))
#
# plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = mdat, hoverinfo="z",
#        colorscale='YIOrRd', reversescale=T,
#         type = "heatmap") %>%
#   layout(xaxis = list(title = "Goals For"),
#          yaxis = list(title = "Goals Against"))
#
#
#
# https://plot.ly/python/heatmaps-contours-and-2dhistograms-tutorial/
#
# #https://plot.ly/r/heatmaps/
# m <- matrix(rnorm(9), nrow = 3, ncol = 3)
# plot_ly(z = m,
#         x = c("a", "b", "c"), y = c("d", "e", "f"),
#         type = "heatmap",
#         hoverinfo="z"),
#         text=paste0("Count:",z))
#
#
# m <- round(cor(mtcars), 3) #class(m) #matrix
#
#        mpg    cyl   disp     hp   drat     wt   qsec     vs     am   gear   carb
# mpg   1.000 -0.852 -0.848 -0.776  0.681 -0.868  0.419  0.664  0.600  0.480 -0.551
# cyl  -0.852  1.000  0.902  0.832 -0.700  0.782 -0.591 -0.811 -0.523 -0.493  0.527
# disp -0.848  0.902  1.000  0.791 -0.710  0.888 -0.434 -0.710 -0.591 -0.556  0.395
# hp   -0.776  0.832  0.791  1.000 -0.449  0.659 -0.708 -0.723 -0.243 -0.126  0.750
# drat  0.681 -0.700 -0.710 -0.449  1.000 -0.712  0.091  0.440  0.713  0.700 -0.091
# wt   -0.868  0.782  0.888  0.659 -0.712  1.000 -0.175 -0.555 -0.692 -0.583  0.428
# qsec  0.419 -0.591 -0.434 -0.708  0.091 -0.175  1.000  0.745 -0.230 -0.213 -0.656
# vs    0.664 -0.811 -0.710 -0.723  0.440 -0.555  0.745  1.000  0.168  0.206 -0.570
# am    0.600 -0.523 -0.591 -0.243  0.713 -0.692 -0.230  0.168  1.000  0.794  0.058
# gear  0.480 -0.493 -0.556 -0.126  0.700 -0.583 -0.213  0.206  0.794  1.000  0.274
# carb -0.551  0.527  0.395  0.750 -0.091  0.428 -0.656 -0.570  0.058  0.274  1.000
# nms <- names(mtcars)
#
#
# plot_ly(x = nms, y = nms, z = m, key = m,
#         type = "heatmap") %>%
#   layout(xaxis = list(title = ""),
#          yaxis = list(title = ""))
#
#
# ## look at results
# sort(names(standings))
# df <- standings %>%
#   ungroup() %>%
#  filter(team=="Man. Utd."&GF==3&GA==1) %>%
#   arrange(desc(gameDate)) %>%
#   select(Opponents=OppTeam,Venue=venue,
#          Season=season) %>%
#   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# change the player apps chart to a plotly --------------------------------


# sort(names(playerGame))
# df <- playerGame %>%
#   filter(PLAYERID=="BENTD") %>%
#   select(gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID,name) %>%
#   mutate(points=Goals+Assists)
#
# ## ggvis version without tooltip
#
# sort(names(df))
#
# df %>%
#   ggvis(~plGameOrder, ~mins) %>%
#   layer_points(fill = ~Team, size = ~ points)
#
# ## plotly
#
# plot_ly(df, x = plGameOrder, y = mins,
#         mode='markers',
#         marker = list(sizeref = 20),
#         color=Team,
#         size=points,
#         hoverinfo = "text",
#         text = paste(Team," v ",Opponents,
#                      "<br>",gameDate,
#                      "<br>On: ",on,
#                      "<br>Off: ",off,
#                      "<br>Goals ",Goals,
#                      "<br>Assists ",Assists)
#         ) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="Minutes Played")
#          )
#
#
# plot_ly(mtcars, x = mpg, y = disp, mode = "markers", size = cyl, marker = list(sizeref = 5,sizemin=5))
# plot_ly(mtcars, x = mpg, y = disp, mode = "markers", size = disp, marker = list(sizeref = 10))
#
# plot_ly(df, x = plGameOrder, y = mins, type='scatter'
#         color= Team, size=points,
#         hoverinfo = "text",
#         text = paste0(Team," v ",Opponents,
#                      "<br>",gameDate,
#                      "<br>On: ",on) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="Minutes Played"
#          )
#   )
#
#
#
#
# # another look at manager records -----------------------------------------
#
#            managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01')
#            managers$Left
#
#                 this looks good
#              pardew <-managers %>%
#                 mutate(Left=ifelse(is.na(managers$Left),as.Date(Sys.Date(), origin= '1970-01-01'),as.Date(managers$Left, origin= '1970-01-01'))) %>%
#                filter(ManagerID=="PardewA") %>%
#                inner_join(standings,by=c("TEAMNAME"="team")) %>%
#                select(Lastname,FirstName,ManagerID,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>%
#                filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left))
#
#            managerGame <-managers %>%
#              mutate(name=paste(FirstName,Lastname)) %>%
#              group_by(ManagerID,ManagerTeam) %>%
#              inner_join(standings,by=c("TEAMNAME"="team")) %>%
#              select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>%
#              filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>%
#              mutate(points=ifelse(res=="Win",3,ifelse(res=="Draw",1,0)))
#
#               all stints at team
#            ppgManagerTeam <- managerGame %>%
#              group_by(TEAMNAME,ManagerID,name) %>%
#              summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2))
#
#               separate stints at team eg Ball at Sunderland
#            ppgManagerTeamStint <- managerGame %>%
#              group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>%
#              summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2))
#
#            ppgManagerTeam  %>%
#              filter(TEAMNAME=="Tottenham H") %>%
#              ungroup() %>%
#              arrange(desc(ppg)) %>%
#              select(manager=name,games,ppg) %>%
#              DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
#
#
#               now combine to get start end date for chart
#
#            allManagerStints <-
#              managerGame %>%
#              select(name,ManagerTeam,Joined,Left) %>%
#              unique()
#
#             allManagerStints$Joined <- as.Date("1992-08-15")
#
#            allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"
#
#              #may need to change joined date
#               glimpse(standings) earlies t"1992-08-15"
#
# #            ppgManagerTeamStint  %>%
# #                 select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
# #              inner_join(allManagerStints) %>%
# #               mutate(startDate=ifelse(ppgManagerTeamStint$Joined<=as.Date("1992-08-15"),as.Date("1992-08-15"),ppgManagerTeamStint$Joined)) %>%
# #                mutate(startDate=ifelse(Joined<=as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',Joined)) %>%
# #              filter(TEAMNAME=="Arsenal") %>%
# #              ggvis(~Joined,~ppg) %>%
# #              layer_points()
#
#            ## def some issues here
#               ppgManagerTeamStint <-     ppgManagerTeamStint  %>%
#              select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
#              inner_join(allManagerStints)
#
#               glimpse(ppgManagerTeamStint)
#
#            ppgManagerTeamStint  %>%
#            #  mutate(startDate=ifelse(Joined<=as.Date("1992-08-15"),as.Date("1992-08-15"),Joined)) %>%
#              mutate(startDate=ifelse(Joined<=as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',Joined)) %>%
#              filter(TEAMNAME=="Arsenal") %>%
#              ggvis(~Joined,~ppg) %>%
#              layer_points()
#
#           test <- ppgManagerTeamStint  %>%
#
#              select(TEAMNAME,name,ManagerTeam,games,ppg) %>%
#              inner_join(allManagerStints) %>%
#               mutate(startDate=ifelse(ppgManagerTeamStint$Joined<=as.Date("1992-08-15"),as.Date("1992-08-15"),ppgManagerTeamStint$Joined)) %>%
#                mutate(startDate=ifelse(Joined<=as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',as.Date("1992-08-15")/(60*60*24), origin= '1970-01-01',Joined)) %>%
#              filter(TEAMNAME=="Everton"&games>5) %>%
#
#             ggvis(x =~ Joined,y=~ppg+0.01,fill = " 687a97") %>%
#             layer_rects(x2=~Left,y2=~ppg-0.01) %>%
#             layer_text(text:=~name, stroke:="red") %>%
#             scale_numeric("y",domain=c(0,3))
#
#
#
#
#
#
#
#
#


# minutes between goals teams/team v team/players type of goal-------------------------

# start with teams

# sort(names(goals))
#
# [1] "GOALS"             "METHOD"            "PLACE"             "PLAY"              "PLAYER_MATCH"      "PLAYER_MATCH_GOAL" "TEAMMATCHID"
# [8] "TIME"
#
# sort(names(standings)) # lacks TEAMMATCHID - bit of prob putting in standings via updatingsql so
#
# [1] "allGames"      "cumGA"         "cumGD"         "cumGF"         "cumPts"        "final_Pos"     "GA"            "gameDate"      "GF"
# [10] "MATCHID"       "OppTeam"       "points"        "position"      "res"           "season"        "team"          "tmGameOrder"   "tmYrGameOrder"
# [19] "tt"
#
# sort(names(teamGames))
#
# [1] "CROWD"         "gameDate"      "GOALS"         "MATCHID"       "REFEREE"       "season"        "TEAMID"        "TEAMMATCHID"   "TEAMNAME"      "tmGameOrder"
# [11] "tmYrGameOrder" "venue"
# >
#
#   ## prob could just do this in update
#   standings %>%
#   inner_join(teamGames)
#
# # this only works where one goal scored
#  test<-teamGames %>%
#   ungroup() %>%
#   left_join(goals,by="TEAMMATCHID") %>%
#   filter(TEAMNAME=="Arsenal"&season=="2015/16") %>%
#   select(TEAMNAME,tmGameOrder,TIME) %>%
#   arrange(tmGameOrder,TIME)
#
# # test <-test %>%
# #   group_by(tmGameOrder) %>%
# #   mutate(x=lag(TIME)) %>%
# #   mutate(y=ifelse(is.na(x),0,x)) %>%
# #   mutate(z=ifelse(is.na(TIME),90,))
#
#
# test <-test %>%
#   group_by(tmGameOrder) %>%
#   mutate(x=lag(TIME)) %>%
#   mutate(y=ifelse(is.na(x),90,x)) %>%
#   filter(!is.na(TIME))
#
# glimpse(test)
#
# test %>%
#
#   mutate(TIME=ifelse(is.na(TIME),90,TIME)) %>%
#   mutate(cumTime=cumsum(TIME)) %>%
#   mutate(timediff=lag(cumTime))
#
# ## http://stackoverflow.com/questions/34337000/how-to-use-r-to-calculate-time-gaps-in-scoring-goals-in-soccer
# df <- data.frame(game=c(1,2,3,4,5,6,6,6,7),goaltime=c(NA,35,51,NA,NA,2,81,90,15))
# diff(c(0,setdiff(90*(df$game-1)+df$goaltime,NA),90*max(df$game)))
#
# diff(c(0,setdiff(90*(test$tmGameOrder-1)+test$TIME,NA),90*max(test$tmGameOrder)))
#
# test <-test %>%
#   mutate(game=tmGameOrder-min(tmGameOrder)+1)
# test %>%
#   mutate(timeGap=diff(c(0,setdiff(90*(game-1)+TIME,NA),90*max(game)))) #Error: wrong result size (30), expected 32 or 1
#
#
# test %>%
#   summarize(timeGap=diff(c(0,setdiff(90*(game-1)+TIME,NA),90*max(game)))) #Error: expecting result of length one, got : 30
#
# length(diff(c(0,setdiff(90*(test$game-1)+test$TIME,NA),90*max(test$game)))) #30
#
# ## cannot add to dat.frame it is just a vector
#
# test %>%
#   do(diff(c(0,setdiff(90*(test$game-1)+test$TIME,NA),90*max(test$game)))) #Error: Result must be a data frame
#
# ## look at more than one team
#
test<-teamGames %>%
  ungroup() %>%
  left_join(goals,by="TEAMMATCHID") %>%
  filter(season=="2015/16") %>%
  select(TEAMNAME,tmGameOrder,TIME) %>%
  arrange(TEAMNAME,tmGameOrder,TIME) %>%
  group_by(TEAMNAME) %>%
  mutate(game=tmGameOrder-min(tmGameOrder)+1)
#
# diff(c(0,setdiff(90*(test$game-1)+test$TIME,NA),90*max(test$game)))
#
# require(stats)
# names(warpbreaks) #"breaks"  "wool"    "tension"
# by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary) #Error in FUN(X[[i]], ...) : could not find function "FUN"
# by(warpbreaks[, 1],   warpbreaks[, -1],       summary) #Error in FUN(X[[i]], ...) : could not find function "FUN"
# by(warpbreaks, warpbreaks[,"tension"],
#    function(x) lm(breaks ~ wool, data = x))
#
# names(test) #[1] "TEAMNAME"    "tmGameOrder" "TIME"        "game"
# by(test, test[,"TEAMNAME"],
#    function(x) diff(c(0,setdiff(90*(game-1)+TIME,NA),90*max(game))))
#
# by(test, "TEAMNAME",
#    function(x) diff(c(0,setdiff(90*(game-1)+TIME,NA),90*max(game))))
#
#
# tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)
#
# tapply(test$game, test[, 4, drop = FALSE], nrow)
#
#
# by_cyl <- group_by(mtcars, cyl)
# do(by_cyl, head(., 2))
#
# by_team <- group_by(test,TEAMNAME)
#
#
# person = c( 'Grace', 'Grace', 'Grace', 'Rob', 'Rob', 'Rob' )
# foods   = c( 'apple', 'banana', 'cucumber', 'spaghetti', 'cucumber', 'banana' )
# eaten <- data.frame(person, foods, stringsAsFactors = FALSE)
# str(eaten)
#   eaten  %>%
#   group_by(person) %>%
#     do(combn(.$foods, m = 2))
#
#   test %>%
#     group_by(TEAMNAME) %>%
#     do(diff(c(0,setdiff(90*(.$game-1)+.$TIME,NA),90*max(.$game))))
#
#             diff(c(0,setdiff(90*(test$game-1)+test$TIME,NA),90*max(test$game)))
# do(by_team,diff(c(0,setdiff(90*($game-1)+$TIME,NA),90*max($game))))
#
# mtcars %>%
#   group_by(vs) %>%
#   do(. %>%
#        arrange(desc(mpg)) %>%
#        head(3))
#
# mtcars %>%
#   group_by(vs) %>%
#   do(head(arrange(., desc(mpg)), 3))
#
#
# test %>%
#   group_by(TEAMNAME) %>%
#   do(diff(c(0,setdiff(90*(.$game-1)+.$TIME,NA),90*max(.$game))))
#
# glimpse(test)
#
# tms <- unique(test$TEAMNAME)
# i <- 1
# for(i in 1:length(tms)) {
#  print(i)
#   subtest <- subset(test,TEAMNAME==tms[i])
#  tempdf <- data.frame(timegaps=diff(c(0,setdiff(90*(subtest$game-1)+subtest$TIME,NA),90*max(subtest$game))))
#  tempdf$team <- tms[i]
#  if (i != 1) {
#    df <- rbind(df,tempdf)
#  } else{
#    df <- tempdf
#  }
# }
# #
# # # current run
# current <- df %>%
#   group_by(team) %>%
#   filter(row_number()==n()) %>%
#   rename(current=timegaps)
# 
# max <- df %>%
#   group_by(team) %>%
#   filter(timegaps==max(timegaps)) %>%
#   rename(max=timegaps)
# 
# mean <- df %>%
#   group_by(team) %>%
#   summarize(meangaps=round(mean(timegaps),0))
# 
# tyFor <- current %>%
#   inner_join(max) %>%
#   inner_join(mean)
# 
# #
# #
# # ## all time
# 
# alltime<-teamGames %>%
#   ungroup() %>%
#   left_join(goals,by="TEAMMATCHID") %>%
#   select(TEAMNAME,tmGameOrder,TIME) %>%
#   arrange(TEAMNAME,tmGameOrder,TIME) %>%
#   group_by(TEAMNAME) %>%
#   mutate(game=tmGameOrder-min(tmGameOrder)+1)
# 
# 
# tms <- unique(alltime$TEAMNAME)
# i <- 1
# for(i in 1:length(tms)) {
#   print(i)
#   subtest <- subset(alltime,TEAMNAME==tms[i])
#   tempdf <- data.frame(timegaps=diff(c(0,setdiff(90*(subtest$game-1)+subtest$TIME,NA),90*max(subtest$game))))
#   tempdf$team <- tms[i]
#   if (i != 1) {
#     dfall <- rbind(dfall,tempdf)
#   } else{
#     dfall <- tempdf
#   }
# }
# 
# currentall <- dfall %>%
#   group_by(team) %>%
#   filter(row_number()==n()) %>%
#   rename(current=timegaps)
# 
# maxall <- dfall %>%
#   group_by(team) %>%
#   filter(timegaps==max(timegaps)) %>%
#   rename(maxever=timegaps)
# 
# meanall <- dfall %>%
#   group_by(team) %>%
#   summarize(meanever=round(mean(timegaps),0))
# 
# # just thi season
#  tyForall<- currentall %>%
#   inner_join(maxall) %>%
#   inner_join(meanall) %>%
#   inner_join(max)
# 
#  names(tyForall)
# 
#  maxRange <- max(tyForall$maxever)
# 
#  roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
#    if(length(x) != 1) stop("'x' must be of length 1")
#    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
#  }
#  maxRange <- roundUpNice(max(tyForall$maxever))
# 
#  # latest plotly needs modesand ys added unless ???
#    plot_ly(tyForall, x = max, y = team, mode = "markers", name='Max 2015/16',marker = list(color = "blue",size=12)) %>%
#      add_trace(x = current, y = team, name = "Current", marker = list(color = "red",size=8), mode="markers") %>%
#      add_trace(x = maxever, y = team,name = "All Premier", marker = list(color = "orange",size=10), mode="markers") %>%
#      add_trace(x = c(90, 90), y= c("Arsenal", "West Ham U"), mode = "lines", line = list(color = "gray",width=1, dash = "10px"), showlegend = FALSE) %>% 
#      add_trace(x = c(180, 180), y= c("Arsenal", "West Ham U"), mode = "lines", line = list(color = "gray",width=1, dash = "10px"), showlegend = FALSE) %>% 
#      add_trace(x = c(270, 270), y= c("Arsenal", "West Ham U"), mode = "lines", line = list(color = "gray",width=1, dash = "10px"), showlegend = FALSE) %>% 
#      layout(hovermode = "closest",
#             xaxis=list(title="Minutes without Goal",range=list(0,maxRange)),
#             yaxis=list(title="", autorange="reversed"),
#             margin = list(l = 100, pad=5)
#             )

#  ## have a look at say dead ball goal
#
#  thisyear<-teamGames %>%
#    ungroup() %>%
#    filter(season=="2015/16") %>%
#    left_join(subset(goals,PLAY!="Open"),by="TEAMMATCHID") %>% # more thanhalf??
#    select(TEAMNAME,tmGameOrder,TIME) %>%
#    arrange(TEAMNAME,tmGameOrder,TIME) %>%
#    group_by(TEAMNAME) %>%
#    mutate(game=tmGameOrder-min(tmGameOrder)+1)
#
#
#  tms <- unique(thisyear$TEAMNAME)
#  i <- 1
#  for(i in 1:length(tms)) {
#    print(i)
#    subtest <- subset(thisyear,TEAMNAME==tms[i])
#    tempdf <- data.frame(timegaps=diff(c(0,setdiff(90*(subtest$game-1)+subtest$TIME,NA),90*max(subtest$game))))
#    tempdf$team <- tms[i]
#    if (i != 1) {
#      df <- rbind(df,tempdf)
#    } else{
#      df <- tempdf
#    }
#  }
#
#
#
#  max<- df %>%
#    group_by(team) %>%
#    filter(timegaps==max(timegaps)) %>%
#    rename(max=timegaps)
#
#
#  alltime<-teamGames %>%
#    ungroup() %>%
#    left_join(subset(goals,PLAY!="Open"),by="TEAMMATCHID") %>% # more thanhalf??
#    select(TEAMNAME,tmGameOrder,TIME) %>%
#    arrange(TEAMNAME,tmGameOrder,TIME) %>%
#    group_by(TEAMNAME) %>%
#    mutate(game=tmGameOrder-min(tmGameOrder)+1)
#
#  tms <- unique(alltime$TEAMNAME)
#  i <- 1
#  for(i in 1:length(tms)) {
#    print(i)
#    subtest <- subset(alltime,TEAMNAME==tms[i])
#    tempdf <- data.frame(timegaps=diff(c(0,setdiff(90*(subtest$game-1)+subtest$TIME,NA),90*max(subtest$game))))
#    tempdf$team <- tms[i]
#    if (i != 1) {
#      dfall <- rbind(dfall,tempdf)
#    } else{
#      dfall <- tempdf
#    }
#  }
#
#  currentall <- dfall %>%
#    group_by(team) %>%
#    filter(row_number()==n()) %>%
#    rename(current=timegaps)
#
#  maxall <- dfall %>%
#    group_by(team) %>%
#    filter(timegaps==max(timegaps)) %>%
#    rename(maxever=timegaps)
#
#  meanall <- dfall %>%
#    group_by(team) %>%
#    summarize(meanever=round(mean(timegaps),0))
#
#  # just thi season
#  tyForall<- currentall %>%
#    inner_join(maxall) %>%
#    inner_join(meanall) %>%
#    inner_join(max)
#
#  names(tyForall)
#
#  maxRange <- max(tyForall$maxever)
#
#  roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
#    if(length(x) != 1) stop("'x' must be of length 1")
#    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
#  }
#  maxRange <- roundUpNice(max(tyForall$maxever))
#
 plot_ly(tyForall, x = max, y = team, mode = "markers", name='Max 2015/16',marker = list(color = "blue",size=10)) %>%
   add_trace(x = current, name = "Current", marker = list(color = "green")) %>%
   add_trace(x = maxever, name = "All Premier", marker = list(color = "red")) %>%
   layout(hovermode = "closest",
          xaxis=list(title="Minutes without Goal",range=list(0,maxRange)),
          yaxis=list(title="", autorange="reversed"),
          margin = list(l = 100, pad=5)
   )
#
#
#
# # games to be played by premier teams -------------------------------------
#
# library(rvest)
#
# page <-read_html("http://www.soccerbase.com/teams/team.sd?team_id=142&comp_id=1&teamTabs=results") #List of 2
#
# #<table class="soccerGrid" id="tgc">
# page %>%
#   html_node("table.soccerGrid") %>% # no matches
#   html_table(header = FALSE)
#
# devtools::install_github("cpsievert/rdom")
# library(rdom)
# library(rvest)
#
# # not good enough to have in R directory needs to be in project
# rpage <-rdom(u)
#
# rpage %>%
#   html_node("table.soccerGrid")
#
#
# stars <- "http://www.techstars.com/companies/stats/"
# # doesn't work
# read_html(stars) %>% html_node(".table75") %>% html_table()
# # should work
# rdom(stars) %>% html_node(".table75") %>% html_table()
# # more efficient
# stars %>% rdom(".table75") %>% html_table()
#
# s
#
#
#
# library(XML)
# u <- "http://www.soccerbase.com/teams/team.sd?team_id=142&comp_id=1&teamTabs=results"
#
# tables = readHTMLTable(u)
# names(tables)
#
# df <- tables[["tgc"]] # only community shield
#
# length(tables)
#
# tables[[4]]
#
# tdist <- read_html("http://en.wikipedia.org/wiki/Student%27s_t-distribution") #List of 2
# # <table class="infobox bordered" style="width:22em;width:325px; max-width:325px; font-size:95%;">
# tdist %>%
#   html_node("table.infobox") %>%
#   html_table(header = FALSE)

## gretzky Most goals in a season: 92
## Most assists in a season: 163

# sort(names(playerGame)) # would need to link via MATCHID how many games team played
#
# test <- playerGame %>%
#   select(name,PLAYERID,season,plYrTmGameOrder,Assists) %>%
#   arrange(plYrTmGameOrder) %>%
#   group_by(PLAYERID,season) %>%
#   mutate(cumAss=cumsum(Assists)) %>%
#   ungroup() %>%
# #arrange(desc(cumAss)) %>%
#  # group_by(PLAYERID,season) %>%
#   arrange(desc(cumAss),desc(plYrTmGameOrder))
#
# a <- test %>%
#   group_by(name,PLAYERID,season) %>%
#   slice(1) %>%
#   ungroup()
#
#
# 1    Thierry Henry   HENRYT 2002/03              37       3     35
# 2    David Beckham  BECKHAD 1999/00              32       1     33
# 3  Darren Anderton  ANDERTD 1994/95              37       0     27
# 4        Juan Mata    MATAJ 2012/13              35       0     27
# 5      Samir Nasri   NASRIS 2013/14              34       1     27
#
# th <- test %>%
#   filter(PLAYERID=="HENRYT"&season=="2002/03") %>%
#   ggvis(~plYrTmGameOrder,~cumAss)
#
#
# b <- a %>%
#   arrange(desc(cumAss)) %>%
#    head(5)
# ozil <- test %>%
#   filter(PLAYERID=="OZILM"&season=="2015/16") %>%
# group_by(name,PLAYERID,season) %>%
#   slice(1) %>%
#   ungroup()
#
# b <- rbind(b,ozil)
#
#
# b %>%
#   select(PLAYERID,season,name) %>%
#   left_join(test) %>%
#   ggvis(~plYrTmGameOrder,~cumAss,stroke=~name) %>%
#   layer_lines()


# timeline of goals scored ------------------------------------------------

# library(timelineR)
#
# df <- data.frame(time=c(3,17,88),player=c("Aguero","Bolasie","Zaha"),team=c("MNC","CRP","CRP"),
#                  color=c("#FFB612","#0C371D","#0C371D"))
#
# ## need to construct similar df (maybe just last name)
#
#
# sort(names(standings))
#
# latest <- standings %>%
#   ungroup() %>%
#   filter(season=="2015/16"&tmYrGameOrder==19&venue=="H")
#
# matches <- latest$MATCHID
#
# allTeams <- teamGames %>%
#   filter(MATCHID %in% matches)
#
# teamMatch <- allTeams$TEAMMATCHID
#
# twGoals <- goals %>%
#   filter(TEAMMATCHID %in% teamMatch)
#
# a <- twGoals %>%
#   left_join(playerGame) %>%
#   select(player=LASTNAME,time=TIME,team=TEAMNAME,venue,MATCHID) %>%
#  # mutate(color=ifelse(venue=="H","#FFB612","#0C371D")) %>%
#   mutate(color=ifelse(venue=="H","#0000ff","#458B00")) %>%
#   arrange(time)
#
# df <- a %>%
#   filter(MATCHID==9359)
#
# glimpse(df)
# sort(names(a))
#
#
#
# colorJS <- htmlwidgets::JS("function(d){return d.color;}")
#
# ## this is in his github example http://kristw.github.io/d3kit-timeline/
#
#
# function color5(d){
#   return colorScale(d.team);
# }
#
# color5 <- htmlwidgets::JS("function(d){return d.team;}")
#
# var chart5 = new d3Kit.Timeline('#timeline5', {
#   direction: 'up',
#   initialWidth: 804,
#   initialHeight: 120,
#   scale: d3.scale.linear(),
#   domain: [0,90],
#   margin: {left: 20, right: 20, top: 20, bottom: 30},
#   textFn: function(d){return d.name;},
#   layerGap: 40,
#   dotColor: colorJS,
#   labelBgColor: colorJS,
#   linkColor: colorJS,
#   labella: {
#     maxPos: 764,
#     algorithm: 'simple'
#   }
# });
# chart5.axis.tickFormat(function(d){return d+'\'';});
# chart5.data([
#   {time: 1,  name: 'MÜller',  team: 'GER'},
#   {time: 23, name: 'Klose',   team: 'GER'},
#   {time: 24, name: 'Kroos',   team: 'GER'},
#   {time: 26, name: 'Kroos',   team: 'GER'},
#   {time: 29, name: 'Khedira', team: 'GER'},
#   {time: 69, name: 'SchÜrrle', team: 'GER'},
#   {time: 79, name: 'SchÜrrle', team: 'GER'},
#   {time: 90, name: 'Oscar', team: 'BRA'},
#   ]);
#
# function color5(df){
#   return colorScale(df.team);
# }
#
# color5 <- htmlwidgets::JS("function(d){return d.color;}")
# d3kit_timeline(
#   df,
#   direction = "up",
#   ## having team there takes up too much space was return d.player + ' - ' + d.team;
#   textFn = htmlwidgets::JS(
#     "
#     function(d){
#     return d.player;
#     }
#     "
#   ),
#   # color probably needs to be treated like the *Fn arguments
#   #  for ultimate flexibility
#   dotColor = colorJS,
#   linkColor = colorJS,
#   labelTextColor = "#FFF",
#   labelBgColor = colorJS,
#   dotRadius = 3,
#   labella = list(maxPos = 600),
#   layerGap = 10, # distance to axis
#   margin = list(left = 20, right = 100, top = 20, bottom = 40),
#   scale = htmlwidgets::JS("d3.scale.linear()"),
#   domain = c(0,90),
#   width = 500,
#   height = 250
# ) #%>>%
#  # add_axis( ticks = 7 )
#
# #didnt work in shiny but timelyportfolio fixed
#
# #was
# library(shiny)
# library(timelineR)
#
# app <- shinyApp(
#   ui = fluidPage(
#     timelineOutput("timeline")
#   ),
#   server = function(input, output) {
#     output$timeline <- renderTimeline({
#
#       df <- data.frame(time = 1:3)
#       d3kit_timeline(
#         df,
#         textFn = ~time,
#         margin = list(left=100,right=20,bottom=20,top=20),
#         scale = htmlwidgets::JS("d3.scale.linear()"),
#         domain = c(0,5)
#       )
#
#     })
#   }
# )
# runApp(app)
#
# #now
# library(shiny)
# library(timelineR)
#
# app <- shinyApp(
#   ui = fluidPage(
#     d3kit_timelineOutput("timeline")
#   ),
#   server = function(input, output) {
#     output$timeline <- renderd3kit_timeline({
#
#       df <- data.frame(time = 1:3)
#       d3kit_timeline(
#         df,
#         textFn = ~time,
#         margin = list(left=100,right=20,bottom=20,top=20),
#         scale = htmlwidgets::JS("d3.scale.linear()"),
#         domain = c(0,5)
#       )
#
#     })
#   }
# )
# runApp(app)
#
#
# # scraping bbc site - but how far back ------------------------------------
#
# #https://statsandsnakeoil.wordpress.com/2015/03/06/ceci-nest-pas-opta-data-scraping-bbc-match-reports-with-r-part-1-getting-the-reports/
#
#
# library(rvest)
#
# library(magrittr)
# url <- "http://www.bbc.co.uk/sport/0/football/30860467"
# webPage <- read_html(url)
# liveText <- webPage %>%
#   html_nodes("#live-text-commentary-wrapper .blq-clearfix p") %>%
#   html_text() # 113 e.g. Offside, Manchester City. James Milner tries a through ball, but JesÃºs Navas is caught offside.
#
# minsText <- webPage %>%
#   html_nodes(".live-text span") %>%
#   html_text() # eg " 45:00  +1:48 Half time "  , "Substitution" " 90:00  +5:08 Full time "
#
#
# ## first half extra time
# library(stringr)
#  a <- minsText[which(!is.na(str_match(minsText,"45:00  +")))]
# ht_extra <- a[which(!is.na(str_match(a, "Half time")))] %>%
#   str_replace("45:00","") %>%
#   extract_numeric() #148 - so need to split into minutes and secons say rounding up
#
# a <- minsText[which(!is.na(str_match(minsText,"90:00  +")))]
# ft_extra <- a[which(!is.na(str_match(a, "Full time")))] %>%
#   str_replace("90:00","") %>%
#   extract_numeric() #508
#
# liveText <- liveText[!grepl("Match ends", liveText, ignore.case=TRUE)] # down by 1
#
# minsNum <- minsText %>%
#   gsub("[\\sa-zA-Z!]","",.) %>%
#   gsub("(:).*",";",.)
#
# #didnt seem to work
# # minsNum <- minsNum[grepl("[0-9]",minsNum) & !grepl("[+]",minsNum)] %>%
# #   as.numeric()
#
# library(tidyr)
# minsNum <- minsNum[grepl("[0-9]",minsNum) & !grepl("[+]",minsNum)] %>%
#   extract_numeric()
#
# matchInfo <- data.frame(url, minsNum, liveText, stringsAsFactors=FALSE) - ##could use in timeliner
#
# # We can improve the functionality further by writing it as a function. This can easily be done, by adding 1 line before all the code I’ve included above, and removing the url <- “http…”
#
# 1
# 2
# 3
# 4
# 5
# myFunction <- function(url) {
#
#   #This is where the rest of the code goes
#
# }
#
#
#
# ## so could get them all for premiership clubs
# ## looks like only 2015
#
#   url="http://www.bbc.co.uk/sport/football/teams/middlesbrough/results"
#   webPage <- read_html(url)
#   linkPrefix <- "http://www.bbc.co.uk"
#
#   linkSuffix <- webPage %>%
#     html_nodes(".status .report") %>%
#     html_attr("href")
#
#   urls <- paste0(linkPrefix,linkSuffix)
#
#   dateString <- webPage %>%
#     html_nodes(".match-date") %>%
#     html_text(trim=T)
#
#   dateString <- dateString[!grepl("Date",dateString)]
#
#   homeTeam <- webPage %>%
#     html_nodes(".team-home") %>%
#     html_text(trim=T)
#
#   awayTeam <- webPage %>%
#     html_nodes(".team-away") %>%
#     html_text(trim=T)
#
#   scoreString <- webPage %>%
#     html_nodes("#results-data abbr") %>%
#     html_text(trim=T) #58 need to remove a Vs and prob in data.frame the P-P
#   scoreString <- scoreString[!grepl("Vs",scoreString)]
#
#   competition <- webPage %>%
#     html_nodes(".match-competition") %>%
#     html_text(trim=T)
#
#   competition <- competition[!grepl("Competition",competition)]
#
#   # cater for P-P game
#   linksList <- data.frame(competition, homeTeam, awayTeam, scoreString, dateString, stringsAsFactors = F)
#   library(dplyr)
#   glimpse(linksList)
#
#   linksList <-linksList %>%
#     filter(scoreString!="P-P") %>%
#     cbind(urls)
#
#
#
#
#
#
# getReportUrls()





# player transfers --------------------------------------------------------
# sort(names(playerClub))
#
#  [1] "FEE"         "FEEOUT"      "JOINED"      "LEFT"        "PERMANENT"   "PLAYER_TEAM" "PLAYERID"    "TEAMID"
#
#  ## need to join for team and player name but no issue
#
#  sort(names(players))
#  sort(names(playerGame))
#
# ## combine fee in and out and joined and left
#
#  transferIn <- playerClub %>%
#    select(-FEEOUT,-LEFT) %>%
#    rename(date=JOINED) %>%
#    mutate(transfer="In", color="red")
#
#  transferOut <- playerClub %>%
#    select(-FEE,-JOINED) %>%
#    rename(date=LEFT,FEE=FEEOUT) %>%
#    mutate(FEE=-FEE) %>%
#    mutate(transfer="Out", color="blue")
#
#  transfers <- rbind(transferIn,transferOut) %>%
#    arrange(date)
#
#  sort(names(transfers))
#
#  # takes a moment - would want to do outside reactive
#  playerName <- playerGame %>%
#    select(PLAYERID,name) %>%
#    unique()
#
#  transfers <- transfers %>%
#    left_join(teams) %>%
#    left_join(playerName)
#
#  ## highlight missing dates 19 for correction
#
#  errors <- transfers %>%
#    filter(is.na(date)&!is.na(FEE))
#
#  ## could also look at those with NA/99
#
#  ## might also want to filter out 0 who have never played and initially no lonas and zero fees(impacts look of graph)
#
#  sort(names(transfers))
#
# liv <- transfers %>%
#    filter(TEAMNAME=="Liverpool"&!is.na(date)&PERMANENT==1&FEE!=0)
#
# names(liv) #[1] "PLAYERID"    "TEAMID"      "date"        "PERMANENT"   "FEE"         "PLAYER_TEAM" "transfer"
#
#
# ## issue where 2 players on samew dat eg suarez and carroll
#
# #Error in jitter(date) : 'x' must be numeric - this works - issue is where transactions occur on same date
# plot_ly(liv, x = date, y = FEE/1000, type = "bar", hoverinfo="text",marker = list(
#   color=color
# ),
#         text = paste(name,"<br>
#                      ",date,"<br>",abs(FEE/1000),"mill.")) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
#          yaxis=list(title="Transaction Fee - Million Pounds"),
#          title=" Transfer Transactionns - Liverpool", titlefont=list(size=12)
#   )
#
#
# plot_ly(liv, x = date, y = FEE/1000, mode="markers", hoverinfo="text",marker = list(
#   color=color
# ),
# text = paste(name,"<br>
#              ",date,"<br>",abs(FEE/1000),"mill.")) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
#          yaxis=list(title="Transaction Fee - Million Pounds"),
#          title=" Transfer Transactionns - Liverpool", titlefont=list(size=12)
#   )
#
# , marker=list(color=teams), showlegend=TRUE, key=season) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
#          yaxis=list(title="Changes in Team Leading Table"),
#          title=" Click bar for details", titlefont=list(size=12)
#   )
#
# plot_ly(topChanges, x = season, y = changes, type = "bar", marker=list(color=teams), showlegend=TRUE, key=season) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="",tickfont=list(size=9),tickcolor="#000",tickangle=45),
#          yaxis=list(title="Changes in Team Leading Table"),
#          title=" Click bar for details", titlefont=list(size=12)
#   )
#
# ## my stackoverflow
#
# library(plotly)
# df <- data.frame(x=c(1,2,3),y=c(6,3,5),opacity=c(1,0.2,1), color=c("red","blue","red"))
#
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#         group=x,
#         opacity=opacity,
#         marker = list(
#           color='#5a22e3'
#         )
# )
#
# # color needs to be character or factor
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#        # opacity=opacity,
#         color=as.factor(x)
# )
#
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#         # opacity=opacity,
#         color=color
# )
#
#
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#         # opacity=opacity,
#         color=as.factor(opacity)
# )
 
 
 
 
# ppg when playing v not playing could also show if scored etc.------------------------------------------

# sort(names(playerGame))
#  sort(names(standings))
#  unique(playerGame$TEAMNAME)
#  
#  sort(names(teamGames)) # no result
#  
#  ## aguero
#  
#  ## team results
#  
#  yr <- "2015/16"
#  tm <- "Man. City"
#  
#  tmRes <-standings %>% 
#    ungroup() %>% 
#    filter(team==tm&season==yr) %>% 
#    select(MATCHID,res,team,season) %>% 
#    mutate(ppg=(ifelse(res=="Win",3,ifelse(res=="Draw",1,0))))
#  
#  
#  tmppg <- playerGame %>% 
#    ungroup() %>% 
#    filter(TEAMNAME==tm&season==yr&START>0) %>% 
#    select(name,MATCHID) %>% 
#    left_join(tmRes) %>% 
#    # group_by(name) %>% 
#    summarize(avPoints=mean(ppg),avStarts=n()/11)
#  
#  allPoints <-tmppg$avPoints*tmppg$avStarts  #43
#  
#  playerRes <- playerGame %>% 
#    ungroup() %>% 
#    filter(TEAMNAME==tm&season==yr&START>0) %>% 
#    select(name,MATCHID) %>% 
#    left_join(tmRes) %>% 
#    group_by(name) %>% 
#    summarize(stPoints=mean(ppg), starts=n(),allPoints, nonStarts=tmppg$avStarts-starts,nonPoints=(allPoints-(stPoints*starts))/(tmppg$avStarts-starts)) %>% 
#    ungroup() %>% 
#    arrange(stPoints,starts) # arranged like this for graph
#  
#  ## set mins for line
#  minY <- playerRes$name[1]
#  maxY <- playerRes$name[nrow(playerRes)]
#  
#  ## set title
#  
#  theTitle <- paste0("Average Points per Game in Games Started ",tm," - ",yr)
#  
#  ## plot_ly
#  
#  plot_ly(playerRes , x=stPoints, y=name, name="Starter", mode="markers", height= "1000px"),
#          hoverinfo="text", marker=list(size=starts,sizemin=2),
#          text=paste("Av Pts:",round(stPoints,2),"<br> Starts:",starts)) %>%
#    add_trace(x=nonPoints,y=name, name="Non-Starter", mode="markers",
#              hoverinfo="text", marker=list(size=nonStarts,sizemin=2),
#              text=paste("Av Pts:",round(nonPoints,2),"<br> Non-Starts:",nonStarts))   %>%
#    add_trace(x = c(tmppg$avPoints, tmppg$avPoints), y= c(minY, maxY), mode = "lines", line = list(color = "green",width=1, dash = "10px"), showlegend = FALSE) %>% 
#    layout(hovermode = "closest", 
#           margin=list(l=120),
#           xaxis=list(title="Av points per Game"),
#           yaxis=list(title=" "),
#           title=theTitle, titlefont=list(size=16)
#    )
#  
#  
 
#  test <- playerGame %>% 
#    filter(PLAYERID=="GAYLED") %>%  
#    arrange(desc(gameDate))%>%
#                          DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
#  
#  
#  
#  
#  
# # Current teams for premierinjuries ---------------------------------------
# 
# currentTeams <-teamGames %>% 
#    filter(season=="2015/16") %>% 
#    select(team=TEAMNAME) %>% 
#    unique() %>% 
#    arrange(team) %>% 
#    data.frame(stringsAsFactors=F)
#  
#  write_csv(currentTeams,"data/currentTeams.csv")
#  
#  
#  
#  
# # try to reduce loading time ----------------------------------------------
# 
# lastName <- playerGame %>% 
#         select(LASTNAME)
#  
#  saveRDS(lastName,"lastName.rds")
#  
#  
#  
#  
#  
# # extend standingsexploding boxplot to certain number of teams gam --------
#  
#  devtools::install_github("ramnathv/htmlwidgets")
#  devtools::install_github("timelyportfolio/explodingboxplotR")
#  
#  library(explodingboxplotR)
# 
#  sort(names(standings))
#  
#  df <- standings %>% 
#    ungroup() %>% 
#    filter(tmYrGameOrder==27&position>=1&position<=7) %>% 
#    select(team,season,cumPts,tmYrGameOrder)  %>% 
#    mutate(year=str_sub(season,1,4)) %>% 
#    rename(points=cumPts)
#  
#  # print("expl ")
#  # print(glimpse(df))
#  
#  exploding_boxplot(
#    df,
#    y = "points",
#    group = "year",
#    width= 600,
#    label = "team", 
#    iqr = 2,
#    margin = list(bottom = 30, left = 50, top = 20, right = 20)
#  )
 
 
 
 
# does ozil go missing v big clubs ----------------------------------------

df <- read_csv("byOpponent.csv")
 
 glimpse(df)
 
 df %>% 
   arrange(desc(goals+assists)) %>% 
   plot_ly(y=goals,x=Opponents,type="bar",name="Goals",
           hoverinfo="text",
           text=paste0("<br>Opp: ",Opponents,
                       "<br>Games: ",apps,
                       "<br>Goals: ",goals,
                       "<br>Assists: ",assists)) %>% 
   add_trace(y=assists,x=Opponents,type="bar",name="Assists",
             hoverinfo="text",
             text=paste0("<br>Opp: ",Opponents,
                         "<br>Games: ",apps,
                         "<br>Goals: ",goals,
                         "<br>Assists: ",assists)) %>% 
   layout(barmode = "stack",
          xaxis=list(title=""),
          yaxis=list(title="Goals and Assists"),
          margin=list(b=70),
          title="Ozil - Goals+Assists (inc. secondary) by PL Opponent")
 
 
 
 
# most assists ------------------------------------------------------------

playerGame %>% 
   group_by(PLAYERID) %>% 
 summarize(tot=sum(Assists)) %>% 
   ungroup() %>% 
   arrange(desc(tot))
 
 
 
 
 
# Look at times between goals scored --------------------------------------

# For teams
#  http://stackoverflow.com/questions/34337000/how-to-use-r-to-calculate-time-gaps-in-scoring-goals-in-soccer
#  diff(c(0,setdiff(90*(df$game-1)+df$goaltime,NA),90*max(df$game)))
#  tmGoalsSince
 
 sort(names(playerGame))
 
 wickham <- playerGame %>% 
   filter(PLAYERID=="WICKHAC"&(START+subOn)>0) %>% 
   select(TEAMMATCHID,gameDate,START,mins,subOn,OFF,TEAMNAME,PLAYER_MATCH)
 
 wickhamGameID <- wickham$PLAYER_MATCH
 
 sort(names(goals))
 
 wickham_goals<-
   goals %>% 
   filter(PLAYER_MATCH %in% wickhamGameID) %>% 
   arrange(PLAYER_MATCH,TIME)
 
test <- wickham %>% 
   left_join(wickham_goals) %>% 
   mutate(timeNoGl=ifelse(is.na(TIME),mins,mins-TIME))  ## only if one goal



# Get players bought under each manager -----------------------------------

#poss should be in weekly update

sort(names(playerClub))
sort(names(managers))

glimpse(playerClub)
glimpse(managers)

# create a mangerTeam which has manager ID for every date

## earliest min(managers$Joined)
"1975-01-06"
i <- 1
for (i in 1:nrow(managers)) {
  if(!is.na(managers$Left[i])) {
  tempdf <- data.frame(theDate=seq(from = as.Date(managers$Joined[i]), 
               to = as.Date(managers$Left[i]),"days"),ManagerID=managers$ManagerID[i], TEAMID=managers$TeamID[i],stringsAsFactors = FALSE)
  } else {
    tempdf <- data.frame(theDate=seq(from = as.Date(managers$Joined[i]), 
                 to = Sys.Date(),"days"),ManagerID=managers$ManagerID[i], TEAMID=managers$TeamID[i],stringsAsFactors = FALSE) 
  }
  if (i!=1) {
    managerTeam <- rbind(df,tempdf)
  } else {
    df <- tempdf
  }
}
 
saveRDS(df,"managerTeam.rds")
managerTeam <- readRDS("managerTeam.rds")

glimpse(playerClub)
glimpse(managers)
glimpse(managerTeam)

## issue that playerClub JOINED and LEFT are time rather than dates
## joined value is shown but all as NA
playerClub$JOINED <- as.Date(playerClub$JOINED)
playerClub$LEFT <- as.Date(playerClub$LEFT)

test <-playerClub %>% 
  left_join(managerTeam,by=c("JOINED"="date","TEAMID"="TeamID"))

# changed to theDate rather than date in case - also tried changing to just data.frame
test <-playerClub %>% 
  rename(theDate=JOINED,TeamID=TEAMID) %>% 
  left_join(managerTeam)


str(playerClub)
str(managerTeam)

dong <- head(playerClub,1)

dong <- dong %>% 
     select(PLAYERID,TeamID=TEAMID,theDate=JOINED)

glimpse(dong)

dong %>% 
  dplyr::left_join(managerTeam) # still does not work

merge(dong,managerTeam) #works


test <-playerClub %>% 
  rename(theDate=JOINED,TeamID=TEAMID) %>% 
  merge(managerTeam) %>% 
  rename()

playerClub <-playerClub %>% 
  rename(theDate=JOINED) %>% 
  merge(managerTeam) %>% 
  rename(JOINED=theDate,bought=ManagerID) %>% 
  rename(theDate=LEFT) %>% 
  merge(managerTeam) %>% 
  rename(LEFT=theDate,sold=ManagerID)

# look at plotly gannt for manager ppg ------------------------------------

## chart that shows for each team- how there manager has performed
## start with ggvis - but may change
managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01')

managerGame <-managers %>% 
  mutate(name=paste(FirstName,Lastname)) %>% 
  group_by(ManagerID,ManagerTeam) %>% 
  inner_join(standings,by=c("TEAMNAME"="team")) %>% 
  select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>% 
  filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>% 
  mutate(points=ifelse(res=="Win",3,ifelse(res=="Draw",1,0))) %>% 
  ungroup()



ppgManagerTeamStint <- managerGame %>% 
  group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>% 
  dplyr::summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2)) %>% 
  ungroup()




allManagerStints <- 
  managerGame %>% 
  select(name,ManagerTeam,Joined,Left) %>% 
  unique()



## artificial start date for those hired before PL existed
allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"



  
  teamRecord <- ppgManagerTeamStint  %>% 
    
    select(TEAMNAME,ManagerName=name,ManagerTeam,games,ppg) %>% 
    inner_join(allManagerStints) %>% 
    filter(TEAMNAME=="Arsenal"&games>=5) 
    
  
  names(teamRecord)
  
  # Choose colors based on number of resources
  cols <- RColorBrewer::brewer.pal(length(unique(teamRecord$ManagerName)), name = "Set2")
  teamRecord$color <- factor(teamRecord$ManagerName, labels = cols)

  p <- plot_ly()
  
  for(i in 1:(nrow(teamRecord) - 1)){
  add_trace(p,
            x = c(teamRecord$Joined[i],teamRecord$Left[i]),  # x0, x1
            y = c(teamRecord$ppg[i], teamRecord$ppg[i]),  # y0, y1
            mode = "lines",
            #line = list(color = df$color[i], width = 20),
            showlegend = F,
            hoverinfo = "text")
  }
  p %>% 
    add_trace(x=c(3,5),y=c(1.6,1.6),mode="lines")
  
  p %>% 
    add_trace(x=c( "1992-08-15","1995-02-21"),y=c(1.45,1.45),mode="lines") %>% 
    add_trace(x=c( "1995-02-21","1995-06-08"),y=c(1.21,1.21),mode="lines") %>% 
    add_trace(x=c( "1996-08-12","1996-09-12"),y=c(1.6,1.6),mode="lines") %>% 
    add_trace(x=c( "1995-06-08","1996-08-12"),y=c(1.66,1.66),mode="lines")
  
  for(i in 1:(nrow(teamRecord) - 1)){
    add_trace(x=c( "1992-08-15","1995-02-21"),y=c(1.45,1.45),mode="lines")
    
  }
  
  ### 
  
  library(plotly)
  
  # Read in data
  df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", stringsAsFactors = F)
  
  # Convert to dates
  df$Start <- as.Date(df$Start, format = "%m/%d/%Y")
  
  # Sample client name
  client = "Sample Client"
  
  # Choose colors based on number of resources
  cols <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
  df$color <- factor(df$Resource, labels = cols)
  
  # Initialize empty plot
  p <- plot_ly()
  
  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically
  
  for(i in 1:(nrow(df) - 1)){
    p <- add_trace(p,
                   x = c(df$Start[i], df$Start[i] + df$Duration[i]),  # x0, x1
                   y = c(i, i),  # y0, y1
                   mode = "lines",
    
                   
                   line = list(color = df$color[i], width = 20),
                   showlegend = F,
                   hoverinfo = "text",
                   
                   # Create custom hover text
                   
                   text = paste("Task: ", df$Task[i], "<br>",
                                "Duration: ", df$Duration[i], "days<br>",
                                "Resource: ", df$Resource[i]),
                   
                   evaluate = T  # needed to avoid lazy loading
    )
  }
  p
  
  
  p <- plot_ly()
  for(i in 1:(nrow(teamRecord))){
    p <- add_trace(p,
                   x = c(teamRecord$Joined[i], teamRecord$Left[i]),  # x0, x1
                   y = c(teamRecord$ppg[i], teamRecord$ppg[i]),  # y0, y1
                   mode = "lines",
                   
                   
                   line = list(color = teamRecord$color[i], width = 10),
                   showlegend = F,
                  # name= teamRecord$ManagerName[i],
                   hoverinfo = "text",

                   # Create custom hover text

                   text = paste(teamRecord$ManagerName[i]),
                   
                   evaluate = T  # needed to avoid lazy loading
    )
  } 
    p <- layout(p, hovermode = "closest", 
                      xaxis=list(title=""),
               yaxis=list(title="Average points per Game"),
                     title=""
               )
  p
  # Add information to plot and make the chart more presentable
  
  p <- layout(p,
              
              # Axis options:
              # 1. Remove gridlines
              # 2. Customize y-axis tick labels and show task names instead of numbers
              
              xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
              
              yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                           tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                           domain = c(0, 0.9)),
              
              # Annotations
              
              annotations = list(
                # Add total duration and total resources used
                # x and y coordinates are based on a domain of [0,1] and not
                # actual x-axis and y-axis values
                
                list(xref = "paper", yref = "paper",
                     x = 0.80, y = 0.1,
                     text = paste0("Total Duration: ", sum(df$Duration), " days<br>",
                                   "Total Resources: ", length(unique(df$Resource)), "<br>"),
                     font = list(color = "#ffff66", size = 12),
                     ax = 0, ay = 0,
                     align = "left"),
                
                # Add client name and title on top
                
                list(xref = "paper", yref = "paper",
                     x = 0.1, y = 1, xanchor = "left",
                     text = paste0("Gantt Chart: ", client),
                     font = list(color = "#f2f2f2", size = 20, family = "Times New Roman"),
                     ax = 0, ay = 0,
                     align = "left")
              ),
              
              plot_bgcolor = "#333333",  # Chart area color
              paper_bgcolor = "#333333")  # Axis area color
  
  p
  
# testeing read_rds -------------------------------------------------------
Sys.time()
playerGame <- readRDS("playerGame.rds")
Sys.time()
library(readr)
Sys.time()
playerGame <- read_rds("playerGame.rds")
Sys.time()

# no obvious improvementy

library(feather)
write_feather(playerGame, "playerGame.feather")
Sys.time()
playerGame <- read_feather("playerGame.feather")
Sys.time()

system.time(playerGame <- readRDS("playerGame.rds")) #1.50
system.time(playerGame <- read_rds("playerGame.rds")) #1.53
system.time(playerGame <- read_feather("playerGame.feather")) #0.26


# setting career summaries to plotly --------------------------------------



dfChart <- playerGame %>% 
  filter(PLAYERID=="BENTD") %>% 
  select(date=gameDate,Opponents,on,off,Goals=Gls,Assists,Team=TEAMNAME,mins,plGameOrder,PLAYERID) %>% 
  mutate(points=Goals+Assists)

library(RColorBrewer)

dfChart %>% 
  plot_ly(x=date,y=mins,mode="markers",color=Team,opacity=2,
hoverinfo = "text",
text = paste(date,"<br>v ",Opponents,"<br>on:",on,"<br>off:",off,"<br>Goals:",Goals,"<br>Assists:",Assists),
marker=list(size=points*2+6)) %>% 
  layout(hovermode = "closest",
         title="",
         xaxis=list(title="Match day Squad Game order"),
         yaxis=list(title="Minutes played"
         )
  )


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
  
  ggvis::add_axis("y", title="Minutes Played", format='d') %>% # attempt to enforxe 0 , values=c(0,15,30,45,60,75,90)
  ggvis::add_axis("x", title="Match Day Squad Game Order", format='d') %>% 
  hide_legend("size") %>% 
  bind_shiny("careerChart")


# timevis -----------------------------------------------------------------


# this needs to  be changed - should really have a playerclub - linked to players and geos table ( and now do)

library(timevis)

sort(names(playerGame))
glimpse(playerGame)

df <- playerGame %>% 
  filter(TEAMNAME=="Man. Utd."& joined>"2013-06-01") %>% 
  mutate(content=paste0(name,"<br> Fee(m): ",FEE/1000)) %>% 
  select(content,start=joined) %>% 
  unique()


players <- playerGame %>% 
  filter(TEAMNAME=="Man. Utd."& joined>"2013-06-01") %>% 
 # group_by(PLAYERID) %>% 
 # mutate(content=paste0(name,"<br> Fee(m): ",FEE/1000)) %>% 
  select(PLAYERID,start=joined,FEE,name) %>% 
  unique()


starts <- playerGame %>% 
  filter(TEAMNAME=="Man. Utd."& joined>"2013-06-01"&START>0) %>% 
  group_by(PLAYERID) %>%
  tally()

df <- players %>% 
  left_join(starts)
df[is.na(df$n),]$n <- 0

df %>% 
  mutate(FEE=ifelse(FEE==99,NA,FEE)) %>% 
  mutate(style=ifelse(start>"2014-04-22","background: orange;","background: lightblue;")) %>% 
  mutate(content=paste0(name,
                        "<br> Fee(m): ",FEE/1000,
                         " Starts: ",n)) %>%
  timevis()

## above works well and was tweeted. however lookin at adjusting color by appearances or fee and adding background for manager - under 10/10-30 and over 

starts first

df %>% 
  mutate(FEE=ifelse(FEE==99,NA,FEE)) %>% 
  mutate(style=ifelse(n<10,"background: #dceddfff;",ifelse(n>30,"background: #78ed8aff;","background: #aef2b9ff;"))) %>% 
 
  mutate(content=paste0(name,
                        "<br> Fee(m): ",FEE/1000,
                        " Starts: ",n)) %>%
  timevis()
  
# x <- 1:50
# case_when(
#   x %% 35 == 0 ~ "fizz buzz",
#   x %% 5 == 0 ~ "fizz",
#   x %% 7 == 0 ~ "buzz",
#   TRUE ~ as.character(x)
# )
# 
# df$style <- "background: LemonChiffon"
# case_when(
#  df$n <10 ~ df$style =  "background: #dceddfff;"
#  df$n >29 ~ df$style =  "background: #78ed8aff;"
# )
# 
# df$style <-
#   case_when(
#     df$n < 10 ~  "background: #dceddfff;"
#     df$n > 29 ~ "background: #78ed8aff;"
#   )

# couldnt get to work

df[df$n < 10,]$style <- "background: Ivory"
df[df$n >29,]$style <- "background: Khaki"

df %>% 
  mutate(FEE=ifelse(FEE==99,NA,FEE)) %>% 
  mutate(content=paste0(name,
                        "<br> Fee(m): ",FEE/1000,
                        " Starts: ",n)) %>%
  timevis()

looks ok
df$type='box' for existing then add background for managers

names(df)
managers$PLAYERID <- NA
managers$start

df
mgers <- data.frame(PLAYERID=c(NA,NA),
                       start=c("2013-05-19","2014-05-19"),
                     #  end=c("2013-05-19","2014-05-19"),
                       FEE=c(NA,NA),
                       name=c("Moyes","van Gaal"),
                       n=c(NA,NA),
                       style=c("background: lightblue","background: orange"),
                       type=(c('background','background'))
)
mgers$start <- as.Date(mgers$start)

df <- bind_rows(df,mgers)

# this causes error
df %>% 
 # mutate(FEE=ifelse(FEE==99,NA,FEE)) %>% 
  # mutate(content=paste0(name,
  #                       "<br> Fee(m): ",FEE/1000,
  #                       " Starts: ",n)) %>%
  timevis()


df %>% 
  
  mutate(content=name) %>% 
  select(start,style,content,type) %>% # problems with type
  
  timevis()

## revisit now have done more in reg updating



names(playerClub)

library(timevis)


## this basis of requirement

# players <- playerGame %>% 
#   filter(TEAMNAME=="Man. Utd."& joined>"2013-06-01") %>% 
#   # group_by(PLAYERID) %>% 
#   # mutate(content=paste0(name,"<br> Fee(m): ",FEE/1000)) %>% 
#   select(PLAYERID,start=joined,FEE,name) %>% ## problem is player name is taken fromplayerGame and some will not have appeared if just bought
#   unique()

starts <- playerGame %>% 
  filter(TEAMNAME=="Man. Utd."& joined>"2013-06-01"&START>0) %>% 
  group_by(PLAYERID) %>%
  tally()  #23

## may want to show those with no starts as well?? especially early season
names(playerClub)
test <-playerClub %>% 
  left_join(teamCodes) %>% 
  filter(TEAMNAME=="Man. Utd."& JOINED>"2013-06-01") %>% #34
  
  #names(test)
  
  left_join(starts) %>% #to get starts info
  #left_join(players) %>% # adds name - but does not for new players
  left_join(allPlayers) %>% # adds name - but does not for new players
  mutate(name=ifelse(is.na(FIRSTNAME),LASTNAME,paste0(FIRSTNAME," ",LASTNAME))) %>% 
  mutate(starts=ifelse(is.na(n),0,n)) %>% 
  mutate(FEE=ifelse(FEE==99,NA,FEE)) %>% 
  mutate(content=paste0(name,
                        "<br> Fee(m): ",FEE/1000,
                        "<br> Starts: ",starts)) %>%
  #mutate(style=ifelse(starts<10,"background: #dceddfff;",ifelse(starts>30,"background: rgba(250,128,15)f;","background: rgba(185,18,18);"))) %>% 
  mutate(style=ifelse(starts<10,"background: Ivory;",ifelse(starts>30,"background: Khaki;","background: #FFFACD;"))) # %>% 
  #rename(start=JOINED) 
  
  glimpse(test)
test$content
 p <-  test %>% 
      select(start=JOINED,style,content) %>% 
      mutate(type='box')
 p$end <- NA
# p %>% 
#   timevis()
# glimpse(p)
# ## DA suggests having start and end as characters

p$start <- as.character(p$start)
p$end <- as.character(p$end)

p <- p %>% 
  select(start,style,content,end,type)
library(timevis)
 p %>% 
  timevis()
glimpse(p)

  ## now look at adding managers
  names(managers)
  
  today <- as.character(Sys.Date()) #class(today)
  
  glimpse(managers)
  managers$Joined <- as.character(managers$Joined)
  managers$Left <- as.character(managers$Left)
  
  
m <-  managers %>% 
    filter(TEAMNAME=="Man. Utd."&(Left>="2013-06-01"|is.na(Left)))
m[is.na(m$Left),]$Left <- today

## look at colors

library(RColorBrewer)
colors <- brewer.pal(12,"Set3")
colors[1] <- "#FFFFFF" #set 1 to white to see if blue lines still show



m <-m %>% 
    #mutate(type='background',style='background: #FFFFB3;') %>% 
  mutate(type='background') #%>% 
           #rename(content=Lastname,start=Joined,end=Left) 
m$style <- ""
for(i in 1:nrow(m)) {
  m$style[i]=paste0("background: ",colors[i],";")
  #m$style[i]=paste0('background: #BEBADA;')
}

m <- m %>% 
    select(content=Lastname,start=Joined,end=Left,type,style) 
glimpse(m)
 m %>% 
   timevis(showZoom= FALSE)
 
 m[m$content=="Moyes",]$style <- "background: rgba(17,14,225,0.5);"
 m[m$content=="van Gaal",]$style <- "background: rgba(250,128,15,0.7);"
 m[m$content=="Mourinho",]$style <- "background: rgba(225,56,14,0.7);"
 m[m$content=="Giggs",]$style <- "background: rgba(14,225,17,0.7);"
 
 ## add a title for managers to show giggs mainly
 
 m$title<- m$content
 

all <- rbind(p,m)

all %>% 
    timevis(showZoom= FALSE,fit=TRUE)

## change managers backgrounds

glimpse(m)

m %>% 
  timevis()
p %>% 
  timevis()
  
all <- rbind(p,m)
all <- rbind(m,p)

glimpse(m)
glimpse(p)
glimpse(all)

write_csv(m,"m.csv")
write_csv(p,"p.csv")

all %>% 
  timevis() ## works but cannot see lines joining player and time currently under review

## could look at brazilians in kleague inc range - perhaps just first club??



## try reading in again (problem with types - better just use raw anyways)

# m <- read_csv("m.csv")
# p <- read_csv("p.csv")
# 
# 
# # toy example

glimpse(all)

partdf <- all[35:36,]

dput(partdf)

df <- data.frame(start=c("2016/01/01","2016/03/01"),
                 end=c("2016/074/01",NA),
                 content=c("a","b"),
                 style=c("background: #BEBADA;","background: #FFFACD;"),
                 type=c("background","box"), stringsAsFactors=FALSE)

timevis(partdf)


y <- structure(list(start = c("2013-07-01", "2013-05-19"), style = c("background: Ivory;",
                                                                     "background: rgba(255,0,0,0.5);"), content = c("Reece James<br> Fee(m): 0<br> Starts: 0",
                                                                                                          "Moyes"), type = c("box", "background"), end = c(NA, "2014-04-22"
                                                                                                          )), .Names = c("start", "style", "content", "type", "end"), row.names = 35:36, class = "data.frame")

timevis(y)

#  ------------------------------------------------------------------------


# add ggplotly to manages (actually ggvis anways) -------------------------------------------------



# look at an all players to join playerclub and get updated geos ----------

## actually there is an allPlayers rds

library(readr)
allPlayers <- readRDS("allPlayers.rds") #4359 (with some since end of season to add at least 20)
playerGeos <- read_csv("playerGeos.csv") #4263

sort(names(allPlayers))
names(playerGeos)

#NB error with some library so needed to resstart and load dplyr
library(dplyr)
newGuys <- allPlayers %>% 
  rename(playerID=PLAYERID) %>% 
  anti_join(playerGeos,by="playerID")  #109 - not quite correct but hey!!

names(newGuys)

library(ggmap)
i <- 1
for (i in 1:nrow(newGuys)){
  
  print(i)
  tempdf <- geocode(paste0(newGuys$PLACE[i],", ",newGuys$COUNTRY[i]))
  tempdf <- cbind(playerID=newGuys$playerID[i],tempdf) # NB now playerID
  if (i!=1) {
    df <- rbind(df,tempdf)
  } else {
    df <- tempdf
  }
}

for (i in 1:nrow(toGet)){

  print(i)
  tempdf <- geocode(paste0(toGet$city[i],", ",toGet$COUNTRY[i]))
  tempdf <- cbind(playerID=toGet$playerID[i],tempdf) # NB now playerID
  if (i!=1) {
    df <- rbind(df,tempdf)
  } else {
    df <- tempdf
  }
}

playerGeos <- rbind(playerGeos,df)

saveRDS(playerGeos,"playerGeos.rds")

## some of them are still very generic eg England


# look at new sparklines --------------------------------------------------

library(sparkline)

names(standings)

sort(names(standings))

ars92 <- standings %>% 
  filter(team=="Arsenal"&season=="1992/93") %>% 
  select(tmYrGameOrder,position=-position,GF,GA) %>% 
  mutate(GD=GF-GA) %>% 
  arrange(tmYrGameOrder)


sl1 <- sparkline(
  ars92$GD,
  type='tristate',
  #barColor="#aaf",
  chartRangeMin=min(ars92$GD),
  chartRangeMax=max(ars92$GD),
  # set an id that will make it easier to refer
  #  in the next sparkline
  elementId="sparkline-for-composite"
)

sl2 <- sparkline(
  -ars92$position,
  type="line",
  fillColor = FALSE,
  lineColor ='blue',
  chartRangeMin = 1,
  chartRangeMax = 22
)

# add sparkline as a composite
addComposite(sl1, sl2)

sl2 <- sparkline(
  c(4,1,5,7,9,9,8,7,6,6,4,7,8,4,3,2,2,5,6,7),
  type="line",
  fillColor = FALSE,
  lineColor ='red',
  chartRangeMin = -5,
  chartRangeMax = 10
)

## do table 

df <- standings %>% 
 
  filter(team=="Arsenal"&season=="1992/93") %>% 
  select(tmYrGameOrder,position=-position,GF,GA) %>% 
  mutate(GD=GF-GA) %>% 
  arrange(tmYrGameOrder)


df <- standings %>% 
  ungroup() %>% 
  filter(season=="2015/16") %>% 
  select(final_Pos,team) %>% 
  arrange(final_Pos)

teams <-unique(df$team)

for (i in seq_along(teams)){
  
  df.team <- df %>% 
    filter(team==teams[i])
  
  sl[i] <- 
     sparkline(
      df.team$GD,
      type='tristate',
      #barColor="#aaf",
      chartRangeMin=min(df.team$GD),
      chartRangeMax=max(df.team$GD),
      # set an id that will make it easier to refer
      #  in the next sparkline
      elementId="sparkline-for-composite"
    )
  
}

library(ggplot2) 
library(gridExtra) 

p <- qplot(mpg, disp, data = mtcars) 
q <- qplot(hp, wt, data = mtcars) 
grid.arrange(p,q) 

library(cowplot)

ggdraw(switch_axis_position(p + axis = 'x'))

p <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue")
ggdraw(switch_axis_position(p, axis = 'y'))
ggdraw(switch_axis_position(p, axis = 'x'))
ggdraw(switch_axis_position(p + theme_bw(), axis = 'xy', keep = 'x'))


ggdraw(switch_axis_position(p , axis = 'xy', keep = 'xy'))

library(ggplot2)
library(cowplot)
p <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue")
ggdraw(switch_axis_position(p , axis = 'xy', keep = 'xy'))



# some stuff on managers --------------------------------------------------

sort(names(managers))

## get games managed

sort(names(playerGame))
library(lubridate)
stints <- managers %>% 
  filter(ManagerID=="MourinhoJ") %>% 
  mutate(Left=as_date(ifelse(is.na(Left),Sys.Date(),Left)))

# Lastname FirstName ManagerID TeamID     Joined       Left  TEAMNAME Caretaker ManagerTeam
# <chr>     <chr>     <chr>  <chr>     <date>     <date>     <chr>     <chr>       <int>
#   1 Mourinho      Jose MourinhoJ    CHL 2004-06-02 2007-09-20   Chelsea      <NA>         156
# 2 Mourinho      Jose MourinhoJ    CHL 2013-06-03 2015-12-17   Chelsea      <NA>         286
# 3 Mourinho      Jose MourinhoJ    MNU 2016-05-27       <NA> Man. Utd.      <NA>         345

i <- 1
for (i in 1:nrow(stints)) {

tempdf <-playerGame %>% 
  filter(gameDate>=stints$Joined[i]&gameDate<=stints$Left[i]&TEAMNAME==stints$TEAMNAME[i])

if (i!=1) {
  df <- bind_rows(df,tempdf)
} else {
  df <- tempdf
}
  
}

## df is all player games 3645

youngestByGame <-df %>% 
  filter(mins>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1)

warnings()
  
i <- 1
for (i in 1:nrow(stints)) {
tempdf <-playerGame %>% 
  filter(gameDate>=stints$Joined[i]&gameDate<=stints$Left[i]&TEAMNAME==stints$TEAMNAME[i])

if (i!=1) {
  df <- bind_rows(df,tempdf)
} else {
  df <- tempdf
}

}

## df is all player games 3645

ysbg <-df %>% 
  filter(mins>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1) %>% 
  ungroup()  %>% 
  mutate(cumminAge=cummin(age)) %>% 
  filter(age==cumminAge) %>% 
  select(name,date=gameDate,team=TEAMNAME,age=cumminAge)

## expand to by team, scorer, starter

df %>% 
  filter(mins>0&Gls>0&START>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1) %>% 
  ungroup()  %>% 
  mutate(cumminAge=cummin(age)) %>% 
  filter(age==cumminAge) %>% 
  select(name,date=gameDate,team=TEAMNAME,age=cumminAge)



cummin(ysbg$age)


df %>% 
  filter(mins>0&Gls>0&START>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1) %>% 
  ungroup()  %>% 
  mutate(cumminAge=cummin(age)) %>% 
 # filter(age==cumminAge) %>% 
  select(name,date=gameDate,team=TEAMNAME,age=cumminAge)


## youngest age scoring
 young<-df %>% 
  filter(mins>0&Gls>0) %>% #2992
  select(name,PLAYERID,age,gameDate,TEAMNAME,LASTNAME) %>% 
  arrange(gameDate,age) %>% 
  group_by(gameDate) %>% 
  slice(1) %>% 
  ungroup()  %>% 
  mutate(cumminAge=cummin(age)) %>% 
  group_by(name,PLAYERID) %>% 
  slice(1) %>% 
mutate(alpha=ifelse(age==cumminAge,0.8,0.2)) %>% ## not working in plot_ly
arrange(gameDate) 

cols1 <- c("cornflowerblue","rosybrown1")
cols2 <- c("blue","red")

## issue with not showing correctly
  plot_ly() %>% 
  add_markers(data=subset(young,age!=cumminAge),x=~gameDate, y=~age,color= ~TEAMNAME,colors=cols1,
              hoverinfo="text",
              text= ~paste0(name,"<br>",round(age,2),"yrs")) %>%
    add_markers(data=subset(young,age==cumminAge),x=~gameDate, y=~age,color= ~TEAMNAME,colors=cols2,
                hoverinfo="text",
                text= ~paste0(name,"<br>",round(age,2),"yrs")) %>% 
  layout(hovermode = "closest",
         xaxis=list(title=""),
         yaxis=list(title="Age"))
  
  
  
  plot_ly() %>% 
    add_text(data=subset(young,age!=cumminAge),x=~gameDate, y=~age,text= ~name,color= ~TEAMNAME,colors=cols1,
                hoverinfo="text",
                text= ~paste0(name,"<br>",round(age,2),"yrs")) %>%
    add_markers(data=subset(young,age==cumminAge),x=~gameDate, y=~age,color= ~TEAMNAME,colors=cols2,
                hoverinfo="text",
                text= ~paste0(name,"<br>",round(age,2),"yrs")) %>% 
    layout(hovermode = "closest",
           xaxis=list(title=""),
           yaxis=list(title="Age"))

## problems
plot_ly %>% 
  add_markers(x = 1:10, y = 1:10, color = I("red"), alpha = 0.1)

plot_ly(mtcars, x = ~wt, y = ~mpg, text = ~rownames(mtcars), mode = "text")

# used in annual
plot_ly(young, x = ~gameDate, y = ~age, text = ~LASTNAME, mode = "text",color= ~TEAMNAME,colors=cols2,
        hoverinfo="info",
        info= ~paste0(name,"<br>",round(age,2),"yrs")) %>% 
  layout(hovermode = "closest",
         title="Youngest age at which player scored in Premier League under Mourinho",
         xaxis=list(title=""),
         yaxis=list(title="Age")) %>% 
  config(displayModeBar = F,showLink = F)

## get all managers into a shiny app for main


## alternative ggplot with ggrepel ((? did ))

#http://stackoverflow.com/questions/27050797/efficient-method-of-obtaining-successive-high-values-of-data-frame-column
df[df$value > cummax(c(-Inf, head(df$value, -1))),]

ysbg[ysbg$age < cummin(c(-Inf, head(ysbg$age, -1))),]

ysbg <- ysbg %>% 
  ungroup() %>% 
   mutate(cumminAge=cummin(age))


cummin(c(3:1, 2:0, 4:2))
#  order value


# look at recordr package -------------------------------------------------
library(recordr)
rc <- new("Recordr")
executionId <- record(rc, file="code/playerSeqs.R", tag="first run of recordr")
# Error:: object 'output' not found had run it so not sure what problem is


# Venn diagram appearances ------------------------------------------------

library(d3vennR) # looks good


http://www.buildingwidgets.com/blog/2015/6/5/week-22-d3vennr

# say leading players for team

# actually appeared in same game

most <- playerGame %>% 
  filter(TEAMNAME=="Man. Utd."&(START+subOn)>0) %>% 
  group_by(name,PLAYERID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(3)

id <- most$PLAYERID
i <- 2

games <- vector("character",3)

for (i in 1:nrow(most)) {
  print(i)
  temp <- playerGame %>% 
    filter(TEAMNAME=="Man. Utd."&(START+subOn)>0&PLAYERID==most$PLAYERID[i]) %>% 
    select(name,PLAYERID,MATCHID)
  if(i!=1) {
    df <- rbind(df,temp)
  } else {
    df <- temp
  }
}


m1 <- df$MATCHID[df$PLAYERID==most$PLAYERID[1]]
m2 <- df$MATCHID[df$PLAYERID==most$PLAYERID[2]]
m3 <- df$MATCHID[df$PLAYERID==most$PLAYERID[3]]

m12 <- intersect(m1,m2)
m13 <- intersect(m1,m3)
m23 <- intersect(m2,m3)
m123 <- intersect(m12,m3)

n12 <-c(most$name[1],most$name[2])
names12 <-cat(paste(shQuote(n12, type="cmd"), collapse=", "))
names12 <- paste0(sprintf("'%s'", n12), collapse = ", ") # "'Ryan Giggs', 'Paul Scholes'"
#names12 <- paste0(sprintf('%s', n12), collapse = ", ") [# "Ryan Giggs, Paul Scholes"
x <-  str_replace(names12,'["]','')

d3vennR(
  data = list(
    list( sets = list(most$name[1]), size = most$n[1]),
    list( sets = list(most$name[1]), size = most$n[2]),
    list( sets = list(most$name[1]), size = most$n[3]),
    list( sets = list( most$name[1], most$name[2]), size = length(m12)),
    list( sets = list( most$name[2], most$name[3] ), size = length(m23)),
    list( sets = list( most$name[1], most$name[2] ), size = length(m13)),
    list( sets = list( paste0(most$name[1],",", most$name[2],",",  most$name[3]) ), size = length(m123))
  )
)

# paste0(most$name[1],'","', most$name[2]),sep=""),collapse=" ")
# 
# n12 <-c(most$name[1],most$name[2])
# names12 <-cat(paste(shQuote(one, type="cmd"), collapse=", "))
# 
# c(most$name[1],most$name[2])
# 
# paste("'",as.character(one),"'",collapse=", ",sep="")
# paste("'",as.character(one),"'",collapse=", ",sep="")

d3vennR(
  data = list(
    list( sets = list(most$name[1]), size = most$n[1]),
    list( sets = list(most$name[2]), size = most$n[2]),
    
    list( sets = list( names12), size = length(m12))
   
  )
)

d3vennR(
  data = list(
    list( sets = list("First"), size = 65),
    list( sets = list("Second"), size = 75),
    list( sets = list("Third"), size = 85),
    list( sets = list( "First", "Second"), size = 35),
    list( sets = list( "Second", "Third" ), size = 15),
    list( sets = list( "First", "Third" ), size = 25),
    list( sets = list( "First", "Second", "Third" ), size = 5)
  )
)

a <-"Giggs"
b <- "Paul Scholes"
c <- "Gary Neville"

## this seems to work
a <-(most$name[1])
b <-(most$name[2])
c <-(most$name[3])

d3vennR(
data = list(
  list( sets = list(a), size = most$n[1]),
  list( sets = list(b), size = most$n[2]),
  list( sets = list(c), size = most$n[3]),
  list( sets = list( a, b), size = length(m12)),
  list( sets = list( b, c ), size =length(m23)),
  list( sets = list( a, c ), size = length(m13)),
  list( sets = list( a, b, c ), size = length(m123))
)
)

venn_tooltip <- function( venn ){
  venn$x$tasks[length(venn$x$tasks)+1] <- list(
    htmlwidgets::JS('
                    function(){
                    var div = d3.select(this);
                    
                    // add a tooltip
                    var tooltip = d3.select("body").append("div")
                    .attr("class", "venntooltip")
                    .style("position", "absolute")
                    .style("text-align", "center")
                    .style("width", 128)
                    .style("height", 16)
                    .style("background", "#333")
                    .style("color","#ddd")
                    .style("padding","2px")
                    .style("border","0px")
                    .style("border-radius","8px")
                    .style("opacity",0);
                    
                    div.selectAll("path")
                    .style("stroke-opacity", 0)
                    .style("stroke", "#fff")
                    .style("stroke-width", 0)
                    
                    // add listeners to all the groups to display tooltip on mousover
                    div.selectAll("g")
                    .on("mouseover", function(d, i) {

                    // sort all the areas relative to the current item
  venn.sortAreas(div, d);
                    
                    // Display a tooltip with the current size
                    tooltip.transition().duration(400).style("opacity", .9);
                    tooltip.text(d.size);
                    
                    // highlight the current path
                    var selection = d3.select(this).transition("tooltip").duration(400);
                    selection.select("path")
                    .style("stroke-width", 3)
                    .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
                    .style("stroke-opacity", 1);
})
                    
                    .on("mousemove", function() {
                    tooltip.style("left", (d3.event.pageX) + "px")
                    .style("top", (d3.event.pageY - 28) + "px");
                    })
                    
.on("mouseout", function(d, i) {
  tooltip.transition().duration(400).style("opacity", 0);
                    var selection = d3.select(this).transition("tooltip").duration(400);
                    selection.select("path")
                    .style("stroke-width", 0)
                    .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                    .style("stroke-opacity", 0);
});
                    }
                    ')
  )
  venn
  }

venn_tooltip(d3vennR(
  data = list(
    list( sets = list(a), size = most$n[1]),
    list( sets = list(b), size = most$n[2]),
    list( sets = list(c), size = most$n[3]),
    list( sets = list( a, b), size = length(m12)),
    list( sets = list( b, c ), size =length(m23)),
    list( sets = list( a, c ), size = length(m13)),
    list( sets = list( a, b, c ), size = length(m123))
  )
)
)
  
  
  
  
  
  
  


d3vennR(
  data = list(
    list( sets = list("First"), size = 65),
    list( sets = list("Second"), size = 75),
    
    list( sets = list( "First", "Second"), size = 35)
   
  )
)

d3vennR(
  data = list(
    list( sets = list(a), size = 65),
    list( sets = list(b), size = 75),
    
    list( sets = list( a, b), size = 35)
    
  )
)

paste0(most$name[1],",", most$name[2],",",  most$name[3])


# test notifications ------------------------------------------------------

shinyApp(
  ui = fluidPage(
    textInput("txt", "Content", "Text of message"),
    radioButtons("duration", "Seconds before fading out",
                 choices = c("2", "5", "10", "Never"),
                 inline = TRUE
    ),
    radioButtons("type", "Type",
                 choices = c("default", "message", "warning", "error"),
                 inline = TRUE
    ),
    checkboxInput("close", "Close button?", TRUE),
    actionButton("show", "Show"),
    actionButton("remove", "Remove most recent")
  ),
  server = function(input, output) {
    id <- NULL
    
    observeEvent(input$show, {
      if (input$duration == "Never")
        duration <- NA
      else 
        duration <- as.numeric(input$duration)
      
      type <- input$type
      if (is.null(type)) type <- NULL
      
      id <<- showNotification(
        ui="Check out this my twitter account"
        duration = NULL, 
        closeButton = input$close,
        type = type
      )
    })
    
    observeEvent(input$remove, {
      removeNotification(id)
    })
  }
)
