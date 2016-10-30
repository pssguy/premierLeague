# # # # Bit like snippets
# # # 
# # # trnasfers
# # # 
# # # # does overhauling squad help
# # # 
# # # cf minutes played between seasons
# # # also need to looka at price of bringing in
# # # forced departures rather than wished for (
# # # }
# # # easier ta have long winning rather than losing run
# # # 
# # # ## chelsea share of minutes by players - some sort of cumulative
# # # 
# # # names(playerGame)
# # # 
# # # starters <-playerGame %>% 
# # #   filter(TEAMNAME=="Chelsea"&season=="2015/16"&START>0) %>% 
# # #   group_by(name) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number())
# # # 
# # # starters %>% 
# # #   ggvis(~count, ~cumStarts) %>% 
# # #   layer_lines()
# # # 
# # # ## by year  
# # # starters <-playerGame %>% 
# # #   filter(season=="2014/15"&START>0) %>% 
# # #   group_by(name,TEAMNAME) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   group_by(TEAMNAME) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number()) 
# # #   
# # # 
# # # starters %>% 
# # #   group_by(TEAMNAME) %>% 
# # #   ggvis(~count, ~cumStarts) %>% 
# # #   layer_lines(stroke= ~TEAMNAME) ## would need to zoom in or restrict count range
# # # ## have legend
# # # 
# # # ## actual top 11
# # # 
# # # starters %>% 
# # #   arrange(cumStarts) %>% 
# # #   slice(11) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(cumStarts)) %>% 
# # #   mutate(pc=round(100*cumStarts/418,1)) %>% 
# # #   tail(10) %>% 
# # #   select(Team=TEAMNAME,PC=pc,Player_11=name,Starts=n) %>% 
# # #   datatable(width=600,options=list(paging=F, searching=F, info=F))
# # # 
# # # 
# # # %>% 
# # #   select(Player=name,Starts=n) %>% 
# # #   datatable(width=300,options=list(paging=F, searching=F, info=F))
# # # 
# # # ## by club  
# # # starters <-playerGame %>% 
# # #   filter(TEAMNAME=="Chelsea"&START>0) %>% 
# # #   group_by(name,season) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   group_by(season) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number()) 
# # #   
# # #   
# # #   starters %>% 
# # #   group_by(season) %>% 
# # #   ggvis(~count, ~cumStarts) %>% 
# # #   layer_lines(stroke= ~season) ## would need to zoom in or restrict count range
# # # ## have legend
# # # 
# # # ## actual top 11
# # # 
# # # eleven <-starters %>% 
# # #   arrange(cumStarts) %>% 
# # #   slice(11) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(cumStarts)) %>% 
# # #   mutate(pc=round(100*cumStarts/418,1)) %>% 
# # #   select(Season=season,PC=pc,Player_11=name,Starts=n)
# # # 
# # # eleven  %>% 
# # #   ggvis(~Season,~PC) %>% 
# # #   layer_lines(stroke:= "blue") %>% 
# # #   add_axis("x", properties = axis_props(labels = list(
# # #     angle = 45, align = "left", fontSize = 9
# # #   )),title = "") %>% 
# # #   add_axis("y", title = "% Starts by Top 11 Players - Chelsea") # could adjust to 
# # # 
# # # 
# # # ## by year   and team %>% 
# # # starters <-playerGame %>% 
# # #   filter(START>0) %>% 
# # #   group_by(name,TEAMNAME,season) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   group_by(TEAMNAME,season) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number()) 
# # # 
# # # eleven <-starters %>% 
# # #   arrange(cumStarts) %>% 
# # #   slice(11) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(cumStarts)) %>% 
# # #   mutate(pc=round(100*cumStarts/418,1)) %>% 
# # #   select(Season=season,Team=TEAMNAME,PC=pc,Player_11=name,Starts=n)
# # # 
# # # ## man u 92/3
# # # starters <-playerGame %>% 
# # #   filter(TEAMNAME=="Man. Utd."&season=="1992/93"&START>0) %>% 
# # #   group_by(name) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number())
# # # 
# # # starters <-playerGame %>% 
# # #   filter(TEAMNAME=="Man. Utd."&season=="1992/93"&START>0) %>% 
# # #   group_by(name) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number()) %>% 
# # #   head(11) %>% 
# # #   select(Player=name,Starts=n) %>% 
# # #   datatable(width=300,options=list(paging=F, searching=F, info=F))
# # # 
# # # 
# # # starters <-playerGame %>% 
# # #   filter(TEAMNAME=="Man. Utd."&START>0) %>% 
# # #   group_by(name,season) %>% 
# # #   tally() %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(n)) %>% 
# # #   group_by(season) %>% 
# # #   mutate(cumStarts=cumsum(n), count=row_number()) 
# # # 
# # # 
# # # starters %>% 
# # #   group_by(season) %>% 
# # #   ggvis(~count, ~cumStarts) %>% 
# # #   layer_lines(stroke= ~season)
# # # 
# # # 
# # # 
# # # eleven <-starters %>% 
# # #   arrange(cumStarts) %>% 
# # #   slice(11) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(cumStarts)) %>% 
# # #   mutate(pc=round(100*cumStarts/418,1)) %>% 
# # #   select(Season=season,PC=pc,Player_11=name,Starts=n)
# # # 
# # # eleven  %>% 
# # #   ggvis(~Season,~PC) %>% 
# # #   layer_lines(stroke:= "red") %>% 
# # #   add_axis("x", properties = axis_props(labels = list(
# # #     angle = 45, align = "left", fontSize = 9
# # #   )),title = "") %>% 
# # #   add_axis("y", title = "% Starts by Top 11 Players- Man. Utd.")
# # # 
# # # ### 
# # # 
# # # names(standings)
# # # 
# # # standings %>% 
# # #   filter(tmYrGameOrder==2&cumGF==2&cumPts==6)
# # # 
# # # standings %>% 
# # #   filter(tmYrGameOrder==2&cumPts==6) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(final_Pos))
# # # 
# # # standings %>% 
# # #   filter(tmYrGameOrder==2&cumPts==6) %>% 
# # #  # ggplot(aes(x=final_Pos)) +
# # #   ggvis(~final_Pos) %>% 
# # #   add_axis("y", title="Counts") %>% 
# # #   add_axis("x",title="Final Standing", breaks=pretty_breaks())
# # #   
# # #   
# # #   ggplot(~final_Pos) %>% 
# # #     
# # #     
# # #     standings %>% 
# # #     filter(GF==1,GA==0) %>% 
# # #     group_by(season,team) %>% 
# # #     tally() %>% 
# # #     ungroup() %>% 
# # #     arrange(desc(n))
# # #   
# # #   standings %>% 
# # #     filter(GF==1,GA==0) %>% 
# # #     group_by(team) %>% 
# # #     tally() %>% 
# # #     ungroup() %>% 
# # #     arrange(desc(n))
# # #   
# # #   ## youngest following on west ham youngest (poss also show apps)
# # #   names(summary)
# # #   apps <- summary %>% 
# # #     #filter((St+On)>0) %>% 
# # #     group_by(PLAYERID) %>% 
# # #       summarize(Apps=(sum(St)+sum(On)))
# # #   
# # #   names(playerGame)
# # # df <-  playerGame %>% 
# # #     filter((START+subOn)>0) %>% 
# # #     group_by(PLAYERID,name,TEAMNAME) %>% 
# # #     select(age,gameDate) %>% 
# # #     summarize(minAge=min(age), theDate=min(gameDate)) %>% 
# # #  inner_join(apps)
# # # 
# # # str(df)
# # # df$minAge <- as.numeric(df$minAge)
# # # 
# # # temp <-df %>% 
# # #   ungroup() %>% 
# # #   filter(TEAMNAME=="Arsenal") %>% 
# # #   arrange(theDate,minAge) %>% 
# # #   group_by(theDate) %>% 
# # # 
# # #   ## go back and 
# # #   df <-  playerGame %>% 
# # #   filter((START+subOn)>0&TEAMNAME=="Arsenal") %>% 
# # #   group_by(gameDate) %>% 
# # #     mutate(youngest=min(age)) %>% 
# # #     filter(age==youngest) %>% 
# # #     ungroup() %>% 
# # #   arrange(gameDate) %>% 
# # #   select(name,age,gameDate)
# # # ## from stackoverflow nees to rework for youger
# # # #df[df$age > cummax(c(-Inf, head(df$age, -1))),]
# # # df$age <- as.numeric(df$age)
# # # df[df$age < cummin(c(Inf, head(df$age, 1))),]
# # # 
# # # cummin(head(df$age)) # [1] 22.52692 22.52692 19.46327 19.46327 19.46327 19.46327
# # # df[df$age < cummin(head(df$age)),]
# # # 
# # # df$youngest <- cummin(df$age)
# # # 
# # # df <-  playerGame %>% 
# # #   filter((START+subOn)>0&TEAMNAME=="Crystal P") %>% 
# # #   group_by(gameDate) %>% 
# # #   mutate(youngest=min(age)) %>% 
# # #   filter(age==youngest) %>% 
# # #   ungroup() %>% 
# # #   arrange(gameDate) %>% 
# # #   select(name,age,gameDate,PLAYERID)
# # # df$age <- as.numeric(df$age)
# # # test <-df %>% 
# # #   mutate(youngest=cummin(age)) %>% 
# # #   filter(age==youngest) %>% 
# # #   mutate(year=as.integer(str_sub(age,1,2)))
# # # 
# # # 
# # # %>% 
# # # #str(test)
# # # #test%>% 
# # #   mutate(days=floor(365.25*(age-year))) %>% 
# # #   mutate(showAge=paste0(year," ",days," days")) %>% 
# # #   ggvis(~gameDate,~youngest) %>% 
# # #   layer_points(fill =~name)
# # # 
# # # %>% 
# # #   group_by(theDate) %>% 
# # #   filter(theDate=="1992-08-18") %>% 
# # #   arrange(minAge) %>% 
# # #   slice(1)
# # # 
# # # str(temp)
# # # 
# # # sort(unique(playerGame$TEAMNAME))
# # # 
# # #   filter(TEAMNAME=="Arsenal") %>% 
# # #   group_by(theDate) %>% 
# # #   filter(min(minAge)==minAge)
# # # 
# # # temp[temp$minAge > cummax(c(-Inf, head(temp$minAge, -1))),]
# # # 
# # # unique(playerGame$TEAMNAME)
# # 
# # ## look at fabregas decline since first half of last year - moving av, group by 10s
# # 
# # 
# # # names(playerGame)
# # # cesc <- playerGame %>% 
# # #   filter(PLAYERID=="SOLERF") #253 obs
# # # 
# # # need to cut by plGameOrder
# # # cesc$group <-cut(cesc$plGameOrder,25)
# # # 
# # # cesc %>% 
# # #   group_by(group) %>% 
# # #   summarise(games=n(),sumGoals=sum(Gls),sumAssists=sum(Assists),
# # #             sumMins=sum(mins),sumPoints=sumAssists+sumGoals,pp90=90*sumPoints/sumMins) %>% 
# # #   ggvis(~group,~pp90) %>% 
# # #   layer_bars()
# # # 
# # # 
# # # ## 10 games with chelsea
# # # 
# # # cescCHL <- playerGame %>% 
# # #   filter(PLAYERID=="SOLERF"&TEAMNAME=="Chelsea") #253 obs
# # # 
# # # need to cut by plGameOrder
# # # cescCHL$group <-cut(cescCHL$plGameOrder,5)
# # # 
# # # cescCHL %>% 
# # #   group_by(group) %>% 
# # #   summarise(games=n(),sumGoals=sum(Gls),sumAssists=sum(Assists),
# # #             sumMins=sum(mins),sumPoints=sumAssists+sumGoals,pp90=90*sumPoints/sumMins) %>% 
# # #   ggvis(~group,~pp90) %>% 
# # #   layer_bars()
# # # 
# # # 
# # # ## or just do a chart with moving average (but by game not minute)
# # # 
# # # cescCHL <- cescCHL %>% 
# # #   mutate(points=Gls+Assists)
# # # cescCHL %>% 
# # #   ggvis(~plTmGameOrder,~points) %>% 
# # #   layer_points()
# # # 
# # # library(dygraphs)
# # # str(discoveries) # Time-Series [1:100] from 1860 to 1959: 5 3 0 2 0 3 2 3 6 1 ...
# # # 
# # # disc <- data(dygraphs::discoveries)
# # # mdeaths
# # # 
# # # str(mdeaths)
# # # library(xts)
# # # points_ts <- as.xts(cescCHL)
# # # 
# # # cescCHL$date <- as.Date(cescCHL$plTmGameOrder)  #"1970-01-02" "1970-01-03"
# # # 
# # # test <- cescCHL %>% 
# # #   select(date,points,gameDate)
# # # rownames(test) = test[[1]]
# # # 
# # # dygraph(as.xts(test)) %>% 
# # #   dyRoller(rollPeriod = 11)
# # # 
# # # dygraph(discoveries, main = "Important Discoveries") %>% 
# # #   dyRoller(rollPeriod = 5)
# # # 
# # #   
# # # 
# # # cesc %>% 
# # #   group_by(group) %>% 
# # #   summarise(sumGoals=sum(Gls))
# # # 
# # # library(Hmisc)
# # # 
# # # set.seed(1)
# # # x <- runif(1000, 0, 100)
# # # z <- cut2(x, c(10,20,30))
# # # table(z)
# # # table(cut2(x, g=10))      # quantile groups
# # # table(cut2(x, m=50)
# # 
# # 
# # ## calculate how far back  a team has been and still won title
# # ## could go further back in time and also look at how far from relegation and still 
# # ## gone down eg cpfc
# # 
# # #' names(standings)
# # #' 
# # #' ## calc leading points at any date
# # #' 
# # #' standings %>% 
# # #'   group_by(season,tmYrGameOrder) %>% 
# # #'   mutate(leadPoints=max(cumPts),defecit=leadPoints-cumPts) %>% 
# # #'   select(season,team,tmYrGameOrder,cumPts,leadPoints,defecit,final_Pos) %>% 
# # #'   filter(final_Pos==1) %>% 
# # #'   ggvis(~tmYrGameOrder,~defecit) %>% 
# # #'   group_by(season) %>% 
# # #'   layer_lines(stroke = ~season)
# # #' 
# # #' standings %>% 
# # #'   group_by(season,tmYrGameOrder) %>% 
# # #'   mutate(leadPoints=max(cumPts),defecit=leadPoints-cumPts) %>% 
# # #'   select(season,team,tmYrGameOrder,cumPts,leadPoints,defecit,final_Pos) %>% 
# # #'   filter(final_Pos==1&defecit>=8) %>% 
# # #'   ggvis(~tmYrGameOrder,~defecit) %>% 
# # #'   group_by(season) %>% 
# # #'   layer_lines(stroke = ~season)
# # #' 
# # #' 
# # #' standings %>% 
# # #'   group_by(season,tmYrGameOrder) %>% 
# # #'   mutate(leadPoints=max(cumPts),defecit=leadPoints-cumPts) %>% 
# # #'   select(season,team,tmYrGameOrder,cumPts,leadPoints,defecit,final_Pos) %>% 
# # #'   filter(final_Pos==1&defecit>=8&season>"1997/98") %>% 
# # #'   ggvis(~tmYrGameOrder,~defecit) %>% 
# # #'   group_by(season) %>% 
# # #'   layer_lines(stroke = ~season)
# # #' 
# # #' # season      team tmYrGameOrder cumPts leadPoints defecit final_Pos
# # #' # (chr)     (chr)         (int)  (dbl)      (dbl)   (dbl)     (int)
# # #' # 1 2002/03 Man. Utd.            11     19         27       8         1
# # #' # 2 2002/03 Man. Utd.            12     22         30       8         1
# # #' # 3 2002/03 Man. Utd.            13     22         30       8         1
# # #' # 4 2002/03 Man. Utd.            14     23         32       9         1
# # #' # 5 2011/12 Man. City            32     71         79       8         1
# # #' 
# # #' 
# # #' 
# # #' standings %>% 
# # #'   group_by(season,tmYrGameOrder) %>% 
# # #'   mutate(leadPoints=max(cumPts),defecit=leadPoints-cumPts) %>% 
# # #'   select(season,team,tmYrGameOrder,cumPts,leadPoints,defecit,final_Pos) %>% 
# # #'   filter(defecit>=8&season>"1997/98"&season<"2015/16") %>% 
# # #'   ungroup() %>% 
# # #'   select(season,team) %>% 
# # #'   unique()
# # #' 
# # #' #317
# # #' 
# # #' standings %>% 
# # #'   group_by(season,tmYrGameOrder) %>% 
# # #'   mutate(leadPoints=max(cumPts),defecit=leadPoints-cumPts) %>% 
# # #'   select(season,team,tmYrGameOrder,cumPts,leadPoints,defecit,final_Pos) %>% 
# # #'   filter(defecit>=8&season>"1997/98"&final_Pos!=1&season<"2015/16") %>% 
# # #'   ungroup() %>% 
# # #'   select(season,team) %>% 
# # #'   unique()
# # #' 
# # #' #315
# # #' 
# # #' 
# # #' In last 17 #BPL years 317 teams have had deficit of 8+ pts  from top spot during season. Only #MUFC 02/3 #MCFC 11/12 won title
# # #' @acjimbo
# # #' #CFC
# # #' 
# # #' ### palace 8 wins out of 10 away - how often done
# # #' 
# # #' 
# # #' sort(names(standings))
# # #' 
# # #' cp <- standings %>% 
# # #'   ungroup() %>% 
# # #'   filter(team=="Crystal P"&venue=="A") %>% 
# # #'   arrange(gameDate) %>% 
# # #'   mutate(rn=row_number()) %>% 
# # #'   select(rn,res)
# # #' 
# # #' tot <- nrow(cp) #120
# # #' 
# # #' lead(1:10, 1)
# # #' lead(1:10, 2)
# # #' 
# # #' cp %>% 
# # #'   filter(rn>=1&rn<=10)
# # #' 
# # #' cp %>% 
# # #'  filter(between(rn,1,10)) %>% 
# # #'   group_by(res) %>% 
# # #'   group_size() #4,4,2 but does not give names
# # #' 
# # #' cp %>% 
# # #'   filter(between(rn,1,10)) %>% 
# # #'   group_by(res) %>% 
# # #'   table()
# # #' 
# # #' for(i in 1:(nrow(cp)-9)) {
# # #'   print(i)
# # #'  x[i] <-(cp %>% 
# # #'   filter(between(rn,i,i+9)&res=="Draw") %>% 
# # #'   tally())$n 
# # #' }
# # #' tempdf <- data.frame(team="cp",max=max(x),min=min(x))
# # #' x
# # #' 
# # #' [1] 2 3 3 3 4 3 3 4 4 4 3 2 2 2 1 2 2 2 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 3 4 3 4 4 3 4 5 5 5 4 3 3 2 2 2 2
# # #' [51] 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 2 2 2 2 2 2 2 2 3 4 3 3 3 4 4 4 4 3 2 1 1
# # #' [101] 1 2 2 3 3 4 5 5 6 7 8tea
# # #' 
# # #' ## all options
# # #' 
# # #' allteams <- standings %>% 
# # #'   ungroup() %>% 
# # #'   group_by(team) %>% 
# # #'   filter(venue=="A") %>% 
# # #'   arrange(gameDate) %>% 
# # #'   mutate(rn=row_number()) %>% 
# # #'   select(team,rn,res) #9026
# # #' 
# # #' teamChoice <- unique(allteams$team)
# # #' 
# # #' #j<- 1
# # #' for (j in 1:length(teamChoice)) {
# # #'   cp <- allteams %>% 
# # #'     filter(team==teamChoice[j])
# # #'   for(i in 1:(nrow(cp)-9)) {
# # #'     print(i)
# # #'     x[i] <-cp %>% 
# # #'               filter(between(rn,i,i+9)&res=="Win") %>% 
# # #'              nrow()
# # #'   }
# # #'   tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
# # #'   x <- NULL
# # #'   if (j!=1) {
# # #'     df <- rbind(df,tempdf)
# # #'   } else {
# # #'     df <- tempdf
# # #'   }
# # #' }
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #'    ## from worldsoccer - prob not necessary
# # #' results <- vector("list",length(myurls))
# # #' 
# # #' for(i in 1:length(myurls)){
# # #'   x <- readHTMLTable(myurls[i])
# # #'   results[[i]] <- getresults2(x[[2]])
# # #' }
# # #' 
# # #' df.1 <- do.call("rbind", results)
# # #' 
# # #' 
# # #' require("nycflights13")
# # #' by_day <- flights %>% group_by(year, month, day)
# # #' n_groups(by_day) #365 
# # #' group_size(by_day) # number of flights each day
# # #' 
# # #' 
# # #' x <- rnorm(1e2)
# # #' x[between(x, -1, 1)]
# # #'  
# # #' 
# # #' follow up to jalapic chart on shots per minute
# # #' 
# # #' names(goals)
# # #' names(teamGames)
# # #' 
# # #' test <-goals %>% 
# # #'   left_join(teamGames,by="TEAMMATCHID") %>% 
# # #'   select(TIME,venue) %>% 
# # #'   group_by(TIME,venue) %>% 
# # #'   tally() 
# # #' 
# # #' #adjustment for added time
# # #' 
# # #' test$n <- ifelse(test$TIME==45,test$n/2,test$n)
# # #' test$n <- ifelse(test$TIME==90,test$n/3,test$n)
# # #' 
# # #' 
# # #' test %>% 
# # #'   filter(!is.na(venue)) %>% 
# # #'   ggvis(~TIME,~n) %>% 
# # #'   layer_points(fill = ~venue) %>% 
# # #'   add_axis("y", title="PL goals 1992-2015. 45mins/2,90mins/3") %>% 
# # #'   add_axis("x", title="Minutes Played")
# # #' 
# # #' 
# # #' ### how often do 1st play second mnc and cp
# # #' 
# # #' names(standings)
# # #' 
# # #' ## prob can just use tmYrGame Order for both teams
# # #' ars <- standings %>% 
# # #'   filter(season=="2014/15"&team=="Arsenal") %>% 
# # #'   select(team,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# # #'   mutate(prevPos=lag(position))
# # #' 
# # #' tm1 <- standings %>% 
# # #'   filter(season=="2014/15") %>% 
# # #'   group_by(team) %>% 
# # #'   select(team,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# # #'   mutate(prevPos=lag(position))
# # #'  
# # #' # looks reasonable 
# # #' test <-tm1 %>% 
# # #'   inner_join(tm1,by=c("OppTeam"="team","gameDate")) %>% 
# # #'   select(team,gameDate,OppTeam,venue.x,res.x,prevPos.x,prevPos.y) %>% 
# # #'   filter(team=="Chelsea")
# # #' 
# # #' 
# # #' test <-tm1 %>% 
# # #'   inner_join(tm1,by=c("OppTeam"="team","gameDate")) %>% 
# # #'   select(team,gameDate,OppTeam,venue.x,res.x,prevPos.x,prevPos.y,tmYrGameOrder.x,tmYrGameOrder.y) %>% 
# # #'   filter(prevPos.x==1&prevPos.y==2)
# # #' 
# # #' # team   gameDate   OppTeam venue.x res.x prevPos.x prevPos.y tmYrGameOrder.x tmYrGameOrder.y
# # #' # (chr)     (date)     (chr)   (chr) (chr)     (int)     (int)           (int)           (int)
# # #' # 1 Chelsea 2014-09-13   Swansea       H   Win         1         2               4               4
# # #' # 2 Chelsea 2015-01-31 Man. City       H  Draw         1         2              23              23
# # #' # 3 Chelsea 2015-04-26   Arsenal       A  Draw         1         2              33              33
# # #' 
# # #' 
# # #' ## all seasons
# # #' 
# # #' tm1 <- standings %>% 
# # #'   select(team,season,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# # #'   group_by(team,season) %>% 
# # #'   mutate(prevPos=lag(position))
# # #' 
# # #' 
# # #' test <-tm1 %>% 
# # #'   inner_join(tm1,by=c("OppTeam"="team","gameDate")) %>% 
# # #'   ungroup() %>% 
# # #'  select(team,season.x,gameDate,OppTeam,venue.x,res.x,prevPos.x,prevPos.y,tmYrGameOrder.x,tmYrGameOrder.y) %>% 
# # #'   filter(prevPos.x==1&prevPos.y==2)
# # #' 
# # #' away <-test %>% 
# # #'   filter(venue.x=="A")
# # #' 
# # #' table(away$res.x)
# # #' 
# # #' ## could look at position/points gap to help estimate result/recent run/history against club etc
# # #' 
# # #' 
# # #' ## minutes since scoring - allowing goal player and team
# # #' 
# # #' see dirty sheets in pssblog
# # #' 
# # #' # inc games against self but no issue presumably #36104
# # #' oppGames <- teamGames %>%
# # #'   select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
# # #'   inner_join(teamGames)
# # #' 
# # #' tempAg <- oppGames %>% #18052
# # #'   arrange(tmGameOrder) %>%   
# # #'   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
# # #'   filter(OppTeam!=TEAMNAME) %>%
# # #'   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
# # #'   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
# # #'   left_join(teamGames) %>%
# # #'   select(TEAMNAME,season,otherID,tmGameOrder) %>%
# # #'   rename(TEAMMATCHID=otherID)
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' all <-tempAg %>%
# # #'   left_join(goals) %>%
# # #'   select(tmGameOrder,TIME) %>%
# # #'   arrange(tmGameOrder,TIME)
# # #' 
# # #' 
# # #' allYear <- all %>%
# # #'   mutate(firstGame = min(tmGameOrder)) %>%
# # #'   filter(!is.na(TIME)) %>% 
# # #'   mutate(
# # #'     game_start = (tmGameOrder - firstGame) * 90,
# # #'     goal_time = game_start + TIME,
# # #'     diff = goal_time - lag(goal_time, default = 0)
# # #'   )
# # #' ## check - looks good
# # #' #allYear %>%
# # #' #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # #' 
# # #' seqGlsAgYear <-allYear %>%
# # #'   filter(season=="2015/16") %>%
# # #'   
# # #'   group_by(TEAMNAME) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   group_by(TEAMNAME) %>%
# # #'   slice(1) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   select(Team=TEAMNAME,mins=diff)
# # #' 
# # #' current <-tempAg %>%
# # #'   left_join(goals) %>%
# # #'   select(tmGameOrder,TIME) %>%
# # #'   arrange(desc(tmGameOrder),desc(TIME))
# # #' 
# # #' current <-current %>%
# # #'   mutate(lastGame = max(tmGameOrder)) %>%
# # #'   filter(!is.na(TIME)) %>% 
# # #'   mutate(
# # #'     game_start = (lastGame -tmGameOrder)* 90,
# # #'     currentTime = game_start + 90 - TIME
# # #'   )
# # #' 
# # #' ## check - looks good
# # #' #current %>%
# # #' # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # #' 
# # #' current <- current %>%
# # #'   filter(season=="2015/16") %>%
# # #'   group_by(TEAMNAME) %>% 
# # #'   select(Team=TEAMNAME,currentTime,season) %>%
# # #'   slice(1) 
# # #' 
# # #' seqGlsAgYear <-seqGlsAgYear %>%
# # #'   inner_join(current) %>%
# # #'   select(Team,Max=mins,Current=currentTime)
# # #' 
# # #' seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Max <- seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Current
# # #' 
# # #' ## Goals for
# # #' tempFor <- oppGames %>% #18052
# # #' arrange(tmGameOrder) %>%   
# # #'   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
# # #'   filter(OppTeam==TEAMNAME) %>%
# # #'   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
# # #'   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
# # #'   left_join(teamGames) %>%
# # #'   select(TEAMNAME,season,otherID,tmGameOrder) %>%
# # #'   rename(TEAMMATCHID=otherID)
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' all <-tempFor %>%
# # #'   left_join(goals) %>%
# # #'   select(tmGameOrder,TIME) %>%
# # #'   arrange(tmGameOrder,TIME)
# # #' 
# # #' 
# # #' allYear <- all %>%
# # #'   mutate(firstGame = min(tmGameOrder)) %>%
# # #'   filter(!is.na(TIME)) %>% 
# # #'   mutate(
# # #'     game_start = (tmGameOrder - firstGame) * 90,
# # #'     goal_time = game_start + TIME,
# # #'     diff = goal_time - lag(goal_time, default = 0)
# # #'   )
# # #' ## check - looks good
# # #' #allYear %>%
# # #' #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # #' 
# # #' seqGlsForYear <-allYear %>%
# # #'   filter(season=="2015/16") %>%
# # #'   
# # #'   group_by(TEAMNAME) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   group_by(TEAMNAME) %>%
# # #'   slice(1) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   select(Team=TEAMNAME,mins=diff)
# # #' 
# # #' current <-tempFor %>%
# # #'   left_join(goals) %>%
# # #'   select(tmGameOrder,TIME) %>%
# # #'   arrange(desc(tmGameOrder),desc(TIME))
# # #' 
# # #' current <-current %>%
# # #'   mutate(lastGame = max(tmGameOrder)) %>%
# # #'   filter(!is.na(TIME)) %>% 
# # #'   mutate(
# # #'     game_start = (lastGame -tmGameOrder)* 90,
# # #'     currentTime = game_start + 90 - TIME
# # #'   )
# # #' 
# # #' ## check - looks good
# # #' #current %>%
# # #' # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # #' 
# # #' current <- current %>%
# # #'   filter(season=="2015/16") %>%
# # #'   group_by(TEAMNAME) %>% 
# # #'   select(Team=TEAMNAME,currentTime,season) %>%
# # #'   slice(1) 
# # #' 
# # #' seqGlsForYear <-seqGlsForYear %>%
# # #'   inner_join(current) %>%
# # #'   select(Team,Max=mins,Current=currentTime)
# # #' 
# # #' seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Max <- seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Current
# # #' 
# # #' ## need to sort man city no goals ag
# # #' 
# # #' names(seqGlsForYear)
# # #' 
# # #' seqGlsForYear$category<-"sinceGF"
# # #' seqGlsAgYear$category<-"sinceGA"
# # #' 
# # #' # seqGlsAgYear$Max <- -seqGlsAgYear$Max
# # #' # seqGlsAgYear$Current <- -seqGlsAgYear$Current
# # #' #or
# # #' seqGlsForYear$Max <- -seqGlsForYear$Max
# # #' seqGlsForYear$Current <- -seqGlsForYear$Current
# # #' 
# # #' df <- rbind(seqGlsAgYear,seqGlsForYear)
# # #' manCity <- data.frame(Team="Man. City",Max=-450,Current=-450,category="sinceGA")
# # #' df <-rbind(df,manCity)
# # #' 
# # #' ## this does not work
# # #' tmOrder <- df %>% 
# # #'   arrange((Current)) %>% 
# # #'   .$Team
# # #' 
# # #' ggplot(df, aes(x = Team, y = Current, fill = category)) +
# # #'   geom_bar(subset = .(category == "sinceGF"), stat = "identity", alpha=0.4) +
# # #'   geom_bar(subset = .(category == "sinceGA"), stat = "identity") +
# # #'  #  scale_x_discrete(limits=tmOrder) + ## works but scrunched up
# # #'    scale_y_continuous(breaks = seq(-max(abs(df$Current)), max(abs(df$Current)),50)) +
# # #' #   ylab("Number of Games") +
# # #' #   xlab("Run Difference") +
# # #'   coord_flip() +
# # #'   labs(title="Minutes Since Goal Allowed and Goal Scored") +
# # #'   labs(x="") +
# # #'   labs(y="Min since Conceding                    Mins Since Scoring") +
# # #'   guides(fill=FALSE) +
# # #'  # ggtitle(title) +
# # #'   theme_bw()
# # #' 
# # #' ## try and get y into geom bar so can add max
# # #' 
# # #' ggplot(df) +
# # #'   geom_bar(subset = .(category == "sinceGF"),aes(x = Team, y = Current, fill = category), stat = "identity") +
# # #'   geom_bar(subset = .(category == "sinceGA"), aes(x = Team, y = Current, fill = category),stat = "identity") +
# # #'   geom_bar(subset = .(category == "sinceGF"),aes(x = Team, y = Max, fill = category), stat = "identity", alpha=0.4) +
# # #'   geom_bar(subset = .(category == "sinceGA"), aes(x = Team, y = Max, fill = category),stat = "identity", alpha=0.4) +
# # #'   #  scale_x_discrete(limits=tmOrder) + ## works but scrunched up
# # #'   scale_y_continuous(breaks = seq(-max(abs(df$Max)), max(abs(df$Max)),50)) +
# # #'   #   ylab("Number of Games") +
# # #'   #   xlab("Run Difference") +
# # #'   coord_flip() +
# # #'   labs(title="Current and Season Max Runs since Conceding or Scoring Goal in BPL") +
# # #'   labs(x="") +
# # #'   labs(y="Min since Conceding                    Mins Since Scoring") +
# # #'   guides(fill=FALSE) +
# # #'   # ggtitle(title) +
# # #'   theme_bw()
# # #' 
# # #' ## can do for one team each season
# # #' 
# # #' 
# # #' # inc games against self but no issue presumably #36104
# # #' oppGames <- teamGames %>%
# # #'   select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
# # #'   inner_join(teamGames)
# # #' 
# # #' tempAg <- oppGames %>% #18052
# # #'   arrange(tmGameOrder) %>%   
# # #'   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
# # #'   filter(OppTeam!=TEAMNAME) %>%
# # #'   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
# # #'   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
# # #'   left_join(teamGames) %>%
# # #'   select(TEAMNAME,season,otherID,tmGameOrder) %>%
# # #'   rename(TEAMMATCHID=otherID)
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' 
# # #' all <-tempAg %>%
# # #'   left_join(goals) %>%
# # #'   select(tmGameOrder,TIME) %>%
# # #'   arrange(tmGameOrder,TIME)
# # #' 
# # #' 
# # #' allYear <- all %>%
# # #'   mutate(firstGame = min(tmGameOrder)) %>%
# # #'   filter(!is.na(TIME)) %>% 
# # #'   mutate(
# # #'     game_start = (tmGameOrder - firstGame) * 90,
# # #'     goal_time = game_start + TIME,
# # #'     diff = goal_time - lag(goal_time, default = 0)
# # #'   )
# # #' ## check - looks good
# # #' #allYear %>%
# # #' #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # #' 
# # #' ## look at club rather than year
# # #' names(allYear)
# # #' seqGlsAgYear <-allYear %>%
# # #'   filter(TEAMNAME=="Chelsea") %>%
# # #'   
# # #'   group_by(season) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   group_by(season) %>%
# # #'   slice(1) %>%
# # #'   ungroup() %>%
# # #'   arrange(desc(diff)) %>%
# # #'   select(Season=season,mins=diff)
# # #' 
# # 
# # 
# # 
# # # 
# # # 
# # # current <-tempAg %>%
# # #   left_join(goals) %>%
# # #   select(tmGameOrder,TIME) %>%
# # #   arrange(desc(tmGameOrder),desc(TIME))
# # # 
# # # current <-current %>%
# # #   mutate(lastGame = max(tmGameOrder)) %>%
# # #   filter(!is.na(TIME)) %>% 
# # #   mutate(
# # #     game_start = (lastGame -tmGameOrder)* 90,
# # #     currentTime = game_start + 90 - TIME
# # #   )
# # # 
# # # ## check - looks good
# # # #current %>%
# # # # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # 
# # # current <- current %>%
# # #   filter(season=="2015/16") %>%
# # #   group_by(TEAMNAME) %>% 
# # #   select(Team=TEAMNAME,currentTime,season) %>%
# # #   slice(1) 
# # # 
# # # seqGlsAgYear <-seqGlsAgYear %>%
# # #   inner_join(current) %>%
# # #   select(Team,Max=mins,Current=currentTime)
# # # 
# # # seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Max <- seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Current
# # 
# # ## Goals for
# # # tempFor <- oppGames %>% #18052
# # #   arrange(tmGameOrder) %>%   
# # #   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
# # #   filter(OppTeam==TEAMNAME) %>%
# # #   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
# # #   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
# # #   left_join(teamGames) %>%
# # #   select(TEAMNAME,season,otherID,tmGameOrder) %>%
# # #   rename(TEAMMATCHID=otherID)
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # all <-tempFor %>%
# # #   left_join(goals) %>%
# # #   select(tmGameOrder,TIME) %>%
# # #   arrange(tmGameOrder,TIME)
# # # 
# # # 
# # # allYear <- all %>%
# # #   mutate(firstGame = min(tmGameOrder)) %>%
# # #   filter(!is.na(TIME)) %>% 
# # #   mutate(
# # #     game_start = (tmGameOrder - firstGame) * 90,
# # #     goal_time = game_start + TIME,
# # #     diff = goal_time - lag(goal_time, default = 0)
# # #   )
# # # ## check - looks good
# # # #allYear %>%
# # # #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # 
# # # seqGlsForYear <-allYear %>%
# # #   filter(TEAMNAME=="Chelsea") %>%
# # #   
# # #   group_by(season) %>%
# # #   ungroup() %>%
# # #   arrange(desc(diff)) %>%
# # #   group_by(season) %>%
# # #   slice(1) %>%
# # #   ungroup() %>%
# # #   arrange(desc(diff)) %>%
# # #   select(Season=season,mins=diff)
# # # 
# # # current <-tempFor %>%
# # #   left_join(goals) %>%
# # #   select(tmGameOrder,TIME) %>%
# # #   arrange(desc(tmGameOrder),desc(TIME))
# # # 
# # # current <-current %>%
# # #   mutate(lastGame = max(tmGameOrder)) %>%
# # #   filter(!is.na(TIME)) %>% 
# # #   mutate(
# # #     game_start = (lastGame -tmGameOrder)* 90,
# # #     currentTime = game_start + 90 - TIME
# # #   )
# # # 
# # # ## check - looks good
# # # #current %>%
# # # # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # 
# # # current <- current %>%
# # #   filter(season=="2015/16") %>%
# # #   group_by(TEAMNAME) %>% 
# # #   select(Team=TEAMNAME,currentTime,season) %>%
# # #   slice(1) 
# # # 
# # # seqGlsForYear <-seqGlsForYear %>%
# # #   inner_join(current) %>%
# # #   select(Team,Max=mins,Current=currentTime)
# # # 
# # # seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Max <- seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Current
# # # 
# # # ## need to sort man city no goals ag
# # # 
# # # names(seqGlsForYear)
# # # 
# # # seqGlsForYear$category<-"sinceGF"
# # # seqGlsAgYear$category<-"sinceGA"
# # # 
# # # # seqGlsAgYear$Max <- -seqGlsAgYear$Max
# # # # seqGlsAgYear$Current <- -seqGlsAgYear$Current
# # # #or
# # # seqGlsForYear$Max <- -seqGlsForYear$Max
# # # seqGlsForYear$Current <- -seqGlsForYear$Current
# # # 
# # # df <- rbind(seqGlsAgYear,seqGlsForYear)
# # # manCity <- data.frame(Team="Man. City",Max=-450,Current=-450,category="sinceGA")
# # # df <-rbind(df,manCity)
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # 
# # # ## are man c longest
# # # 
# # # standings %>% 
# # #   ungroup() %>% 
# # #   filter(tmYrGameOrder==5) %>% 
# # #   arrange(cumGA) %>% 
# # #   select(season,team,cumGA)
# # #   
# # # 
# # # ggplot(gameGoal, aes(x=slength,y=n)) +
# # #   geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# # #   geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# # #   
# # # 
# # # 
# # # 
# # # 
# # # 
# # # ## minutes ahead/level/behind in season
# # #   
# # #   ## look at individual game first and then extend from there
# # #   
# # #   ## 
# # #   
# # #   
# # #   ### 1-0 down at half time how scoring time impacts result
# # # 
# # # sort(names(teamGames)) #18092
# # # 
# # # sort(names(standings))
# # # 
# # # sort(names(goals))
# # # 
# # # ## start with games where 1-0 up at half time
# # # 
# # # oneGoalTeams <- goals %>% 
# # #   filter(TIME<=45) %>% 
# # #   group_by(TEAMMATCHID) %>% 
# # #   tally()  %>% #7880
# # #   filter(n==1) %>% #5742 
# # #   left_join(teamGames) %>% 
# # #   select(TEAMNAME,TEAMMATCHID,MATCHID,gameDate,venue,season)
# # # 
# # # allMatchIDs <- teamGames %>% 
# # #   ungroup() %>% 
# # #   select(TEAMMATCHID,MATCHID) #18092
# # #   
# # # 
# # # anyGoalTeams <- goals %>% 
# # #   filter(TIME<=45) %>% 
# # #   group_by(TEAMMATCHID) %>%  #10553
# # #   select(TEAMMATCHID) %>%   ## NB teammatchid 33816 an issue
# # #   unique()  %>% #7880
# # #   ungroup()
# # #   
# # #   noGoalTeams <- allMatchIDs %>% 
# # #   anti_join(anyGoalTeams) #10213 +7880 = 18093 (just that one funny one)
# # #   ungroup() %>% 
# # #     select(TEAMMATCHID,MATCHID)
# # #   
# # #   
# # #   names(oneGoalTeams)
# # #   names(noGoalTeams)
# # #   
# # #   oneUpTeams <-
# # #     oneGoalTeams %>% 
# # #     left_join(noGoalTeams,by="MATCHID") %>% 
# # #   ## need to knock ou na for
# # #     filter(!is.na(TEAMMATCHID.y)) %>% 
# # #     rename(TEAMMATCHID=TEAMMATCHID.x)#3262 
# # #   
# # #   # now need to get time scored of each of these and the result
# # #   
# # #   res <- oneUpTeams %>% 
# # #      inner_join(standings) %>% 
# # #     select(team,OppTeam,gameDate,season,GF,GA,res,TEAMMATCHID)
# # #   
# # #   ## table(res$res) Draw Loss  Win 
# # #                  #    727  276 2259   2259/3262 #69%
# # #                      
# # #                      
# # #   goals %>% 
# # #     filter(TIME<=45&TEAMMATCHID %in% oneUpTeams$TEAMMATCHID) %>%  #3262 (good)
# # #     ggvis(~TIME) %>% 
# # #     layer_histograms(width=5)
# # #   
# # #   timeScored <- goals %>% 
# # #     filter(TIME<=45&TEAMMATCHID %in% oneUpTeams$TEAMMATCHID) %>% 
# # #     select(TIME,TEAMMATCHID) %>% 
# # #     inner_join(res)
# # #   
# # #   timeScored$period <- cut(timeScored$TIME,b=c(0,5,10,15,20,25,30,35,40,45))
# # #   
# # #   
# # #  z<- timeScored %>% 
# # #     group_by(res,period) %>% 
# # #     summarize(games=n()) %>% 
# # #     ungroup() %>% 
# # #     group_by(period) %>% 
# # #     mutate(pc=games/sum(games))
# # #  
# # #  z %>% 
# # #    filter(res=="Win") %>% 
# # #    ggvis(~period,~pc)
# # #   
  #mangers at variuous clubs and clubs by manager
 managers <- readRDS("managers.rds")
 
 managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01')
  
 managerGame <-managers %>% 
   mutate(name=paste(FirstName,Lastname)) %>% 
   group_by(ManagerID,ManagerTeam) %>% 
   inner_join(standings,by=c("TEAMNAME"="team")) %>% 
   select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>% 
   filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>% 
   mutate(points=ifelse(res=="Win",3,ifelse(res=="Draw",1,0)))
 
 
 #all stints at team
 ppgManagerTeam <- managerGame %>% 
   group_by(TEAMNAME,ManagerID,name) %>% 
   summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2)) 
 
 #separate stints at team eg Ball at Sunderland
 ppgManagerTeamStint <- managerGame %>% 
   group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>% 
   summarize(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2)) 
 
 ppgManagerTeam  %>% 
   filter(TEAMNAME=="Tottenham H") %>% 
   arrange(desc(ppg))
 
 #now combine to get start end date for chart
 
 allManagerStints <- 
   managerGame %>% 
   select(name,ManagerTeam,Joined,Left) %>% 
   unique()
 
 #allManagerStints$Joined <- as.Date("1992-08-15")
 
 allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"
 
 ppgManagerTeamStint  %>% 
   
   select(TEAMNAME,name,ManagerTeam,games,ppg) %>% 
   inner_join(allManagerStints) %>% 
 
   filter(TEAMNAME=="Arsenal") %>% 
   ggvis(~Joined,~ppg) %>% 
   layer_points()

 
 
 
# # #  
# # # # Mins by round by country ------------------------------------------------
# # # 
# # #  names(summary)
# # #   allTemp <-summary %>% 
# # #    ungroup() %>% 
# # #    select(PLAYERID,mins,season) %>% 
# # #    left_join(allPlayers) %>% 
# # #    select(season,mins,COUNTRY) %>% 
# # #    group_by(season,COUNTRY) %>% 
# # #    summarize(totMins=sum(mins)) %>% 
# # #    group_by(season) %>% 
# # #    mutate(allMins=sum(totMins),pc=round(100*totMins/allMins))
# # #   
# # #   sort(names(playerGame))
# # #   
# # #   ## this is not correct as totMins varies a lot by round
# # #   playerGame %>% 
# # #     group_by(season,plYrTmGameOrder,COUNTRY) %>% 
# # #     summarize(totMins=sum(mins)) %>% 
# # #     group_by(season,plYrTmGameOrder) %>% 
# # #     mutate(pc=totMins/sum(totMins)) %>% 
# # #     filter(COUNTRY=="England"&season=="2015/16") %>% 
# # #     ungroup() %>% 
# # #     arrange(pc)
# # #     
# # #     
# # #     names(teamGames)
# # #     
# # #     a <- teamGames %>% 
# # #       ungroup() %>% 
# # #     select(TEAMMATCHID,season,tmYrGameOrder)
# # #     
# # #     
# # #     b <- playerGame %>% 
# # #       select(TEAMMATCHID,mins,COUNTRY) 
# # #     
# # #     names(a)
# # #     names(b)
# # #     
# # #  c <-   a %>% 
# # #       left_join(b) %>% 
# # #       group_by(season,tmYrGameOrder,COUNTRY) %>% 
# # #       summarize(totMins=sum(mins)) %>% 
# # #       ungroup()
# # #  
# # # d <- c %>% 
# # #    group_by(season,tmYrGameOrder) %>% 
# # #   mutate(pc=totMins/sum(totMins)) %>% 
# # #    filter(season=="2015/16"&tmYrGameOrder==9) %>% 
# # #   summarize(all=sum(pc))
# # # 
# # # 
# # # c %>% 
# # #   group_by(season,tmYrGameOrder) %>% 
# # #   mutate(pc=totMins/sum(totMins)) %>% 
# # #   filter(season=="2015/16"&COUNTRY=="England") %>% 
# # #   ggvis(~tmYrGameOrder,~pc)
# # # 
# # # c %>% 
# # #   group_by(season,tmYrGameOrder) %>% 
# # #   mutate(pc=totMins/sum(totMins)) %>% 
# # #   filter(pc<0.3&COUNTRY=="England") 
# # # 
# # # 
# # # c %>% 
# # #   group_by(season,tmYrGameOrder) %>% 
# # #   mutate(pc=totMins/sum(totMins)) %>% 
# # #   filter(pc>0.09999&COUNTRY!="England") %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(pc))
# # # 
# # # 
# # # 
# # # # Most goals by age range -------------------------------------------------
# # # 
# # # sort(names(playerGame))
# # # 
# # # playerGame %>% 
# # #   filter(PLAYERID!="OWNGOAL") %>% 
# # #   group_by(name,PLAYERID) %>% 
# # #   summarize(Goals=sum(Gls)) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(Goals)) %>% 
# # #   select(name,Goals) %>% 
# # #   head(.,10) %>% 
# # #   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
# # #                 width=200,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# # # 
# # # 
# # # 
# # # playerGame %>% 
# # #   filter(PLAYERID!="OWNGOAL"&age<20) %>% 
# # #   group_by(name,PLAYERID) %>% 
# # #   summarize(Goals=sum(Gls)) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(Goals)) %>% 
# # #   select(name,Goals) %>% 
# # #   head(.,10) %>% 
# # #   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
# # #                 width=200,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# # # 
# # # playerGame %>% 
# # #   filter(PLAYERID!="OWNGOAL"&age>=20&age<30) %>% 
# # #   group_by(name,PLAYERID) %>% 
# # #   summarize(Goals=sum(Gls)) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(Goals)) %>% 
# # #   select(name,Goals) %>% 
# # #   head(.,10) %>% 
# # #   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
# # #                 width=200,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# # # 
# # # 
# # # temp <-playerGame %>% 
# # #   filter(PLAYERID!="OWNGOAL"&age>=30) %>% 
# # #   group_by(name,PLAYERID) %>% 
# # #   summarize(Goals=sum(Gls)) %>% 
# # #   ungroup() %>% 
# # #   arrange(desc(Goals)) %>% 
# # #   select(name,Goals) %>% 
# # #   head(.,10) %>% 
# # #   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
# # #                 width=200,options= list(paging = FALSE, searching = FALSE,info=FALSE)) # interesting no current players
# # 
# # 
# # # # cumalative point comparisons --------------------------------------------
# # # 
# # # ## eg scholes, giggs, beckham
# # # 
# # # sort(names(playerGame))
# # # 
# # # players <- c("BECKHAD","SCHOLEP","GIGGSR")
# # # 
# # # a <-playerGame %>% 
# # #   filter((START+subOn>0)&PLAYERID %in% players) %>% 
# # #   select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
# # #   arrange(gameDate) %>% 
# # #   group_by(name,PLAYERID) %>% 
# # #   mutate(gameOrder=row_number(),points=Assists+Gls,
# # #          cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) %>% 
# # #   ggvis(~gameOrder,~cumPoints) %>% 
# # #   
# # #   layer_lines(stroke= ~name) %>% 
# # #   add_axis("x", title="PL Appearances") %>% 
# # #   add_axis("y", title="cumulative Goals+Assists (inc. secondary") %>% 
# # #   add_legend("stroke", title="")
# #   
# #   
# # 
# # # goalscoring sequences vardy -----------------------
# # 
# # appeared <-playerGame %>%
# #   filter((START+subOn)>0) %>%  # 380 as sh
# #   arrange(gameDate) %>%
# #   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
# # 
# # 
# # 
# # appeared$Scored <- 0
# # appeared$Scored[appeared$Gls>0] <- 1
# # 
# # ## prob with van nist
# # # van <- appeared %>% 
# # #   filter(PLAYERID=="VANNISR")
# # # 
# # # 
# # # ## look at vardy
# # # 
# # # appeared %>% 
# # #   filter(PLAYERID=="VARDYJ") %>% 
# # #   select(Scored) %>% 
# # #   subSeq()
# # # 
# # # runApp <- subSeq(appeared$Scored)
# # # 
# # # 
# # # by_cyl <- group_by(mtcars, cyl)
# # # #glimpse(by_cyl) str(by_cyl) Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	32 obs. of  11 variables:
# # # do(by_cyl, head(., 2))
# # # 
# # # by_player <- group_by(appeared,PLAYERID)
# # # str(by_player)
# # 
# # by_player <- appeared %>% 
# #   ungroup() %>% 
# #   arrange(PLAYERID,gameDate) %>% 
# # group_by(PLAYERID)
# # 
# # # by_player %>% 
# # #   filter(PLAYERID=="VANNISR")
# # # 
# # # test <-goalSeqs %>% 
# # #   filter(PLAYERID=="VANNISR")
# # 
# # goalSeqs <- do(by_player,subSeq(.$Scored)) %>% 
# #   filter(value==1) #166688
# # 
# # ## all those 5 or over - and then do best for each player
# # 
# # bestRun <- goalSeqs %>% 
# #   #filter(slength>4) %>% 
# #   select(PLAYERID,slength) %>% 
# #   arrange(PLAYERID,desc(slength)) %>% 
# #   group_by(PLAYERID) %>% 
# #   slice(1)
# # 
# # topScorers <- playerGame %>% 
# #   filter(PLAYERID %in% bestRun$PLAYERID) %>% 
# #   group_by(name,PLAYERID) %>% 
# #   
# #   summarize(totGoals=sum(Gls)) %>% 
# #   inner_join(bestRun)
# # 
# # 
# # topScorers  <- cbind(topScorers, id = seq_len(nrow(topScorers)))  
# # 
# # all_values <- function(x) {
# #   if(is.null(x)) return(NULL)
# #   row <- topScorers[topScorers$id == x$id,c("name","totGoals")]
# #   #paste0(names(row),": ", format(row), collapse = "<br />")
# #   paste0( format(row), collapse = "<br />")
# # }
# # 
# # topScorers %>% 
# #   ggvis(~totGoals,~slength,key := ~id) %>% 
# #   add_axis("y",title="Best Scoring Run in PL games", format='d') %>% 
# #   add_axis("x", title="Career Premier League goals") %>% 
# #  add_tooltip(all_values,"click") 
# # 
# # 
# # appeared <-playerGame %>%
# #     filter((START+subOn)>0) %>%  # 380 as sh
# #     arrange(gameDate) %>%
# #     select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
# #   
# #   
# #   
# #   appeared$Scored <- 0
# #   appeared$Scored[appeared$Gls>0] <- 1
# #   
# #   
# #   
# # by_playerClub <- appeared %>% 
# #   ungroup() %>% 
# #   arrange(PLAYERID,gameDate) %>% 
# #   group_by(PLAYERID,TEAMNAME) # this needs to be here to carry PLAYERID across
# # 
# # 
# # 
# # goalSeqsClub <- do(by_playerClub,subSeq(.$Scored)) %>% 
# #   filter(value==1)
# # 
# # --results on whether higher team wins any particular encounter ------------
  
 sort(names(standings))

# doubles in season -------------------------------------------------------


a <- standings %>% 
  select(team,OppTeam,season,MATCHID,tmYrGameOrder,res,position)

b <- a %>% 
  inner_join(a,by=c("MATCHID","team"="OppTeam")) %>% 
  ungroup() %>% 
  select(team,OppTeam,season=season.x,tmYrGameOrder=tmYrGameOrder.x,res=res.x,teamPos=position.x,oppPos=position.y)
 

str(b)

c <- b %>% 
  group_by(season,team) %>% 
  mutate(teamPosLag=lag(teamPos),oppPosLag=lag(oppPos)) %>% 
  filter(!is.na(teamPosLag)) %>% 
  mutate(higher=ifelse(teamPosLag>oppPosLag,0,1)) %>% 
 # filter(team=="Man. Utd."&season=="1992/93") just to confirm there are 41
  ungroup()
str(c)

wins <-c %>% 
  group_by(season) %>% 
  filter(higher==1&res=="Win") %>% 
  tally() %>% 
  ggvis(~season,~n) %>% 
  layer_lines()

## look at specific rounds
test <- c %>% 
  group_by(season,tmYrGameOrder) %>% 
  filter(higher==1&res=="Win") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(n)  #mean(test$n) 4.27 (inc earlier years)
#2015/16
# round=13
# n=3

## app 160 per year - seems too many??


draws <-c %>% 
  group_by(season) %>% 
  filter(higher==1&res=="Draw") %>% 
  tally() %>% # about 90
  ggvis(~season,~n) %>% 
  layer_lines()


c %>% 
  group_by(season) %>% 
  filter(higher==1&res=="Loss") %>% 
  tally() %>% 
  ggvis(~season,~n) %>% 
  layer_lines() # about 120


c %>% 
  filter(tmYrGameOrder<39) %>% 
  group_by(tmYrGameOrder) %>% 
  filter(higher==1&res=="Loss") %>% 
  tally() %>% 
  ggvis(~tmYrGameOrder,~n) %>% 
  layer_lines()
  
# 
# # plotly dumbbell plot for home/away ppg ----------------------------------
# 
# sort(names(standings))
# 
# glimpse(standings)
# 
# ppg_home <- standings %>% 
#   filter(venue=="H") %>% 
#   group_by(team) %>% 
#   summarize(home=mean(points))
# 
# ppg<- standings %>% 
#   filter(venue=="A") %>% 
#   group_by(team) %>% 
#   summarize(away=mean(points)) %>% 
#   inner_join(ppg_home) %>% 
#   mutate(gap=home-away,average=(home+away)/2)
# 
# 
# ppg <- ppg[order(ppg$averas), ]
# 
# ## sent as twitter - could easily adapt to team by season, presumably
#  ppg %>% 
#   arrange(average) %>% 
# gather(venue, value, away, home, average) %>%
#   plot_ly(x = value, y = team, mode = "markers",
#           color = venue, colors = c("red","green","blue")) %>%
#   add_trace(x = value, y = team, mode = "lines", 
#             group = team, showlegend = F, line = list(color = "gray", width=1)) %>%
#   layout(hovermode = "closest", autosize= F, width=800, height= 900, 
#     title = "PL Home and Away point per game disparity",
#     xaxis = list( title = "Average points in Premier League"),
#     yaxis = list(title = ""),
#     margin = list(l = 100)
#   )
# 
# 
# 
# # plotly goals by game by each club ---------------------------------------
# 
# sort(names(standings))
#  
 standings %>%
   ungroup() %>%
   filter(season=="2015/16") %>%
   select(team,GF,GA) %>%
   group_by(team) %>%
   summarize(For=sum(GF),Against=sum(GA),Total=For+Against) %>%
   ungroup() %>%
   arrange(Total) %>%
   gather(goals,value,For,Against,Total) %>%
   plot_ly(x = value, y = team, mode = "markers",
           color = goals, colors = c("red","green","blue")) %>%
   add_trace(x = value, y = team, mode = "lines",
             group = team, showlegend = F, line = list(color = "gray", width=1)) %>%
   layout(hovermode = "closest", autosize= F, width=600, height= 700,
          title = "PL Goals scored For and Against 2015/6",
          xaxis = list( title = "Goals"),
          yaxis = list(title = ""),
          margin = list(l = 100)
   )
#  
#  
#  
# # played for 2 clubs added to specials on site------------------------------------------------------
# 
# sort(names(playerGame))
#  
#  unique(playerGame$TEAMNAME)
#  
#  chelsea <- playerGame %>% 
#    filter(TEAMNAME=="Chelsea"&mins>0) %>% 
#    select(name) %>% 
#    unique() %>% 
#    .$name
#  
#  spurs <- playerGame %>% 
#    filter(TEAMNAME=="Tottenham H"&mins>0) %>% 
#    select(name) %>% 
#    unique() %>% 
#    .$name
#  
#  intersect(chelsea,spurs)
#  
#  sort(names(playerGame))
#  
#  teams <- c("Chelsea","Tottenham H")
#  
#  team1 <- playerGame %>% 
#    filter(TEAMNAME==teams[1]&(START+subOn)>0) %>% 
#    group_by(PLAYERID,name) %>% 
#    tally() %>%
#    ungroup() %>% 
#    select(-PLAYERID)
#    
#  
#  names(team1)[2] <- teams[1]
#  
#  
#  team2 <- playerGame %>% 
#    filter(TEAMNAME==teams[2]&(START+subOn)>0) %>% 
#    group_by(PLAYERID,name) %>% 
#    tally() %>%
#    ungroup() %>% 
#    select(-PLAYERID)
#  
#  
#  ## on books
#  team1 <- playerGame %>% 
#    filter(TEAMNAME==teams[1]&PLAYERID!="OWNGOAL") %>% 
#    group_by(PLAYERID,name) %>% 
#    tally() %>%
#    ungroup() %>% 
#    select(-PLAYERID)
#  
#  
#  names(team1)[2] <- teams[1]
#  
#  
#  team2 <- playerGame %>% 
#    filter(TEAMNAME==teams[2]&PLAYERID!="OWNGOAL") %>% 
#    group_by(PLAYERID,name) %>% 
#    tally() %>%
#    ungroup() %>% 
#    select(-PLAYERID)
#  
#  names(team2)[2] <- teams[2]
#  
#   team1 %>% 
#     inner_join(team2) %>% 
#     rename(Player=name) %>% 
#     DT::datatable(class='compact stripe hover row-border order-column',options= list(paging = FALSE, searching = FALSE,info=FALSE))
#  
#  # should extend and 


# standings %>% 
#   ungroup() %>% 
#   filter(team=="Crystal P") %>% 
#   arrange(desc(GF))
# 
# 
# all <- data.frame(team=(unique(standings$team)))
# 
# latestTime <-standings %>% 
#   ungroup() %>% 
#   filter(GF>4) %>% 
#   arrange(desc(tmGameOrder)) %>% 
#   group_by(team) %>% 
#   slice(1)  %>% 
#   ungroup() %>% 
#   select(team,latest=tmGameOrder)
# 
# firstTime <-standings %>% 
#   ungroup() %>% 
#   filter(GF>4) %>% 
#   arrange(tmGameOrder) %>% 
#   group_by(team) %>% 
#   slice(1)  %>% 
#   ungroup() %>% 
#   select(team,first=tmGameOrder)
# 
# allGames <- standings %>% 
#   ungroup() %>% 
#   arrange(desc(tmGameOrder)) %>% 
#   group_by(team) %>% 
#   slice(1)  %>% 
#   ungroup() %>% 
#   select(team,total=tmGameOrder)
# 
# standings %>% 
#   ungroup() %>% 
#   filter(GF>4) %>% 
#  
#   group_by(team) %>% 
#   tally() %>% 
#   rename(count=n)  %>% 
#   inner_join(firstTime) %>% 
#   inner_join(latestTime) %>% 
#   right_join(allGames) %>% 
#   mutate(count=ifelse(is.na(count),0,count)) %>% 
#   mutate(since=ifelse(count!=0,total-latest,NA)) %>% 
#   select(team,count,total,first,since) %>% 
#   DT::datatable(width=350,rownames=FALSE,class='compact stripe hover row-border',options= list(paging = FALSE, searching = FALSE,info=FALSE))
# 
# #, columnDefs = list(list(className = 'dt-right', targets = 3)) done in case since should be
# firstTime %>% 
#   ungroup() %>% 
#   select(team,match=tmGameOrder) %>% 
#     arrange(match) %>% 
#    DT::datatable(width=250,class='compact stripe hover row-border',options= list(paging = FALSE, searching = FALSE,info=FALSE))
# 
# fivers <- test$team
# all <- unique(standings$team)
# 
# nonfivers <-setdiff(all,fivers)
# 
# teamSeason %>% 
#   filter(team %in% nonfivers) %>% 
#   arrange(team)
# 
# standings %>% 
#   ungroup() %>% 
#   filter(team=="Blackburn") %>% 
#   arrange(desc(GF)) #152
# 
# ## also look at games since


# different teams top in a season -----------------------------------------


# # decline of chelsea players ----------------------------------------------
# 
# sort(names(playerGame))
# 
players <- c("HAZARDE","OSCAR","COSTAD","SOLERF","WILLIA")

playerGame %>%
  filter(PLAYERID=="OSCAR") %>%
  arrange(desc(gameDate)) %>%
  select(gameDate,plYrGameOrder,plYrTmGameOrder,plYrTmGameOrderApp)

## doesnt help - just do all thsi year and to game 15 last year

sort(names(teamGames))

finalDate <- teamGames %>%
  filter(TEAMNAME=="Chelsea"&season=="2014/15"&tmYrGameOrder==15) %>%
  .$gameDate %>%
  unique()

ly <- playerGame %>%
  ungroup() %>% 
  filter(PLAYERID %in% players &gameDate<=finalDate&season=="2014/15") %>%
  group_by(name) %>%
  select(name,Gls,Assists) %>% 
  summarize(goals=sum(Gls),Assts=sum(Assists),points=goals+Assts) ## ends up with total value
ly$season <- "2014/15"


ty <- playerGame %>%
  filter(PLAYERID %in% players &season=="2015/16") %>%
  group_by(name) %>%
  summarize(goals=sum(Gls),Assts=sum(Assists),points=goals+Assts)
ty$season <- "2015/16"

df <- rbind(ly,ty)

df %>%
  group_by(name) %>%
  ggvis(~season,~points,group=name) %>%
  geom_line(stroke=~name)

#library(ggplot2)

df %>%
  ggplot(aes(x=season,y=points,group=name,color=name)) +
  geom_line() +
  geom_point()


#https://rpubs.com/walkerke/slopegraph


ggplot(df) +
  geom_line(aes(x = season, y=points,group=name,color=name), size = 2) +
  geom_point(aes(x = season, y = points, color = name), size = 5) +
  theme_minimal(base_size = 18) +
  scale_color_brewer(palette = "Dark2") +
  xlab("") +
  geom_text(data = subset(df, season == "2014/15"),
            aes(x = season, y = points, color = name, label = paste0(name," ",points," pts")),
            size = 6, hjust = 1.15) +
  geom_text(data = subset(df, season == "2015/16"),
            aes(x = season, y = points, color = name, label = paste0(points," pts")),
            size = 6, hjust = -1.15) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 18)) +
  ggtitle("Yr on Yr Change in Goals+Assists after 15 PL Games \n (inc. Secondary Assists)")

# # teams beating those below them ------------------------------------------
# 
# 
# # nil nils ----------------------------------------------------------------
# 
# sort(names(standings))
# 
# boreDraws <-standings %>% 
#   group_by(team,season) %>% 
#   filter(GF==0&GA==0) %>% 
#   tally()
# 
# 
# boreDraws <-standings %>% 
#   group_by(team) %>% 
#   filter(GF==0&GA==0) %>% 
#   tally()
# 
# 
# # since score draw --------------------------------------------------------
# 
# output$teamSeqCurrent <- DT::renderDataTable({
#   
#   ##
#   a <- standings %>% 
#     ungroup() %>% 
#     filter(season=="2015/16") 
#   
#   teams <- sort(unique(a$team))
#   
#   scores <- standings %>% 
#     ungroup() %>% 
#     filter(team %in% teams) %>% 
#     group_by(team,GF,GA) %>% 
#     arrange(tmGameOrder) %>% 
#     select(res,tmGameOrder) %>% 
#     mutate(res=ifelse((res=="Draw"&GF==0),"bore",res))
#   
#   ## split into score/no score draw
#   
#   
# 
#   ## this is scoredraws now I think? but looks like issue
#   Dx <- scores %>% 
#     mutate(cat=ifelse(res=="Draw",1,0)) %>% 
#   
# 
#     do(subSeq(.$cat)) %>% 
#     group_by(team) %>% 
#     mutate(maxFirst=max(first)) %>% 
#     filter(first==maxFirst)
#   
#   Draw <- 
#     Dx  %>% filter(value==1) %>% 
#     ungroup() %>% 
#     arrange(desc(slength)) %>% 
#     mutate(rank=min_rank(-slength),Category="Draws") %>% 
#     filter(rank==1) %>% 
#     mutate(teams = paste(team,collapse=", ")) %>% 
#     select(Category,teams,count=slength) %>% 
#     head(1)
#   
#   noDraw <- Dx  %>%
#     filter(value==0) %>% 
#     ungroup() %>% 
#     arrange(desc(slength)) %>% 
#     mutate(rank=min_rank(-slength), Category="No Draws") %>% 
#     filter(rank==1) %>% 
#     mutate(teams = paste(team,collapse=", ")) %>% 
#     select(Category,teams,count=slength) %>% 
#     head(1)
#   
#   bind_rows(Win,noWin,Draw,noDraw,Loss,noLoss) %>% 
#     select(Category,Games=count,Teams=teams) %>% 
#     DT::datatable(rownames=FALSE,class='compact stripe hover row-border',options= list(paging = FALSE, searching = FALSE,info=FALSE))
#   
# })
# 
# 
# # bench value -------------------------------------------------------------
# 
# sort(names(playerGame))
# 
# sort(names(teamGames))
# 
# bench <-teamGames %>% 
#   ungroup() %>% 
#   rename(team=TEAMNAME) %>% 
#   right_join(playerGame,by="TEAMMATCHID") %>% 
#   filter(START==0&FEE!=99) %>% 
#   select(team,TEAMMATCHID,FEE,gameDate.x) %>% 
#   group_by(TEAMMATCHID,team,gameDate.x) %>% 
#   summarize(benchFee=sum(FEE,na.rm=T)) %>% 
#   ungroup() %>% 
#   arrange(desc(benchFee))
# 
# #EAMMATCHID      team gameDate.x benchFee
# (int)     (chr)     (date)    (int)
# #1        39034 Man. City 2012-03-03   152000
# 
# playerGame %>% 
#   filter(TEAMMATCHID==39034&START==0&LASTNAME!="Own Goal") %>% 
#   select(name,FEE,gameDate)
# 
# # name   FEE   gameDate
# # (chr) (int)     (date)
# # 1      Sergio Aguero 38000 2012-03-03
# # 2         Edin Dzeko 27000 2012-03-03
# # 3 Aleksander Kolarov 19000 2012-03-03
# # 4       James Milner 26000 2012-03-03
# # 5  Costel Pantilimon    NA 2012-03-03
# # 6        David Silva 26000 2012-03-03
# # 7         Kolo Toure 16000 2012-03-03
# 
# 
# starters <-teamGames %>% 
#   ungroup() %>% 
#   rename(team=TEAMNAME) %>% 
#   right_join(playerGame,by="TEAMMATCHID") %>% 
#   filter(START>0&FEE!=99) %>% 
#   select(team,TEAMMATCHID,FEE,gameDate.x) %>% 
#   group_by(TEAMMATCHID,team,gameDate.x) %>% 
#   summarize(starterFee=sum(FEE,na.rm=T)) %>% 
#   ungroup() %>% 
#   arrange(desc(starterFee))
# 
# test <- starters %>% 
#   group_by(team) %>% 
#   arrange(desc(starterFee)) %>% 
#   slice(1)
# 
# 
# playerGame %>% 
#   filter(TEAMMATCHID==41725) %>% 
#   select(name,FEE,gameDate)


# game within goal after 80 mins say --------------------------------------

## first 90 mins



# sort(names(standings))
# 
# standings %>% 
#   ungroup() %>% 
#   filter(GF==1&GA==0) %>% 
#   group_by(team,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# standings %>% 
#   ungroup() %>% 
#   filter(GF==1&GA==0&season=="2008/09"&team=="Man. Utd.") 


#  standings %>% 
#    ungroup() %>% 
#    filter(season=="2015/16") %>% 
#    select(team,GF,GA) %>% 
#    group_by(team) %>% 
#    summarize(For=sum(GF),Against=sum(GA),Total=For+Against) %>% 
#    ungroup() %>% 
#    arrange(Total) %>% 
#    gather(goals,value,For,Against,Total) %>% 
#    plot_ly(x = value, y = team, mode = "markers",
#            color = goals, colors = c("red","green","blue")) %>%
#    add_trace(x = value, y = team, mode = "lines", 
#              group = team, showlegend = F, line = list(color = "gray", width=1)) %>%
#    layout(hovermode = "closest", autosize= F, width=600, height= 700, 
#           title = "PL Goals scored For and Against 2015/6",
#           xaxis = list( title = "Goals"),
#           yaxis = list(title = ""),
#           margin = list(l = 100)
#    )


# boxplot of by year points spread ----------------------------------------

# sort(names(standings))
# 
# df <- standings %>% 
#   ungroup() %>% 
#   filter(tmYrGameOrder==16) %>% 
#   select(team,season,cumPts,tmYrGameOrder)
# 
# df2015 <- df %>% 
#   filter(season=="2015/16"&tmYrGameOrder==16)
#   
# 
# plot_ly(y = rnorm(50), type = "box") %>%
#   add_trace(y = rnorm(50, 1))
# 
# plot_ly(y = df, type = "box") %>%
#   add_trace(y = rnorm(50, 1))
# 
# plot_ly(df2015,y=cumPts, type = "box", boxpoints = "all", jitter = 0.3, color=team,
#         pointpos = -1.8, hoverinfo="text", text = paste(team,"<br> Points:",cumPts)) %>%
#   layout(hovermode = "closest", autosize= F, width=600, height= 700, 
#          title = "Points after 16 Games 2015/16",
#          xaxis = list( title = ""),
#          yaxis = list(title = "",tickfont=list(size=0)),
#          legend=list(y=0.95,font=list(size=15))
#   )
# 
# 
# plot_ly(df,y=cumPts,x=season, type = "box", key=season)
#   
 

# ozil and secondary assists ----------------------------------------------


# 12 AWAY WINS IN CALENdar year -------------------------------------------

# sort(names(standings))
# 
# 
# dozen <-standings %>% 
#   ungroup() %>% 
#   mutate(year =str_sub(gameDate,1,4)) %>% 
#   select(year,team,res,venue) %>% 
#   filter(res=="Win"&venue=="A") %>% 
#   group_by(year,team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) %>% 
#   filter(team=="Crystal P")


#  correlation between position half way and final cf goal diff -----------

# also for next year second half v whole



# revisit time since scoring etc ------------------------------------------



## minutes since scoring - allowing goal player and team

#see dirty sheets in pssblog

# inc games against self but no issue presumably #36104
# oppGames <- teamGames %>%
#   select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
#   inner_join(teamGames)
# 
# tempAg <- oppGames %>% #18052
#   arrange(tmGameOrder) %>%   
#   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
#   filter(OppTeam!=TEAMNAME) %>%
#   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
#   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
#   left_join(teamGames) %>%
#   select(TEAMNAME,season,otherID,tmGameOrder) %>%
#   rename(TEAMMATCHID=otherID)
# 
# 
# 
# 
# 
# 
# #
# all <-tempAg %>%
#   left_join(goals) %>%
#   select(tmGameOrder,TIME) %>%
#   arrange(tmGameOrder,TIME)
# 
# 
# allYear <- all %>%
#   mutate(firstGame = min(tmGameOrder)) %>%
#   filter(!is.na(TIME)) %>% 
#   mutate(
#     game_start = (tmGameOrder - firstGame) * 90,
#     goal_time = game_start + TIME,
#     diff = goal_time - lag(goal_time, default = 0)
#   )
# ## check - looks good
# #allYear %>%
# #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# 
# seqGlsAgYear <-allYear %>%
#   filter(season=="2015/16") %>%
#   
#   group_by(TEAMNAME) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   group_by(TEAMNAME) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   select(Team=TEAMNAME,mins=diff)
# 
# current <-tempAg %>%
#   left_join(goals) %>%
#   select(tmGameOrder,TIME) %>%
#   arrange(desc(tmGameOrder),desc(TIME))
# 
# current <-current %>%
#   mutate(lastGame = max(tmGameOrder)) %>%
#   filter(!is.na(TIME)) %>% 
#   mutate(
#     game_start = (lastGame -tmGameOrder)* 90,
#     currentTime = game_start + 90 - TIME
#   )
# 
# ## check - looks good
# current %>%
#  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# 
# current <- current %>%
#   filter(season=="2015/16") %>%
#   group_by(TEAMNAME) %>% 
#   select(Team=TEAMNAME,currentTime,season) %>%
#   slice(1) 
# 
# seqGlsAgYear <-seqGlsAgYear %>%
#   inner_join(current) %>%
#   select(Team,Max=mins,Current=currentTime)
# 
# seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Max <- seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Current
# 
# ## Goals for
# tempFor <- oppGames %>% #18052
# arrange(tmGameOrder) %>%   
#   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
#   filter(OppTeam==TEAMNAME) %>%
#   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
#   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
#   left_join(teamGames) %>%
#   select(TEAMNAME,season,otherID,tmGameOrder) %>%
#   rename(TEAMMATCHID=otherID)
# 
# 
# 
# 
# 
# 
# 
# all <-tempFor %>%
#   left_join(goals) %>%
#   select(tmGameOrder,TIME) %>%
#   arrange(tmGameOrder,TIME)
# 
# 
# allYear <- all %>%
#   mutate(firstGame = min(tmGameOrder)) %>%
#   filter(!is.na(TIME)) %>% 
#   mutate(
#     game_start = (tmGameOrder - firstGame) * 90,
#     goal_time = game_start + TIME,
#     diff = goal_time - lag(goal_time, default = 0)
#   )
# ## check - looks good
# #allYear %>%
# #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# 
# seqGlsForYear <-allYear %>%
#   filter(season=="2015/16") %>%
#   
#   group_by(TEAMNAME) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   group_by(TEAMNAME) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   select(Team=TEAMNAME,mins=diff)
# 
# current <-tempFor %>%
#   left_join(goals) %>%
#   select(tmGameOrder,TIME) %>%
#   arrange(desc(tmGameOrder),desc(TIME))
# 
# current <-current %>%
#   mutate(lastGame = max(tmGameOrder)) %>%
#   filter(!is.na(TIME)) %>% 
#   mutate(
#     game_start = (lastGame -tmGameOrder)* 90,
#     currentTime = game_start + 90 - TIME
#   )
# 
# ## check - looks good
# #current %>%
# # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# 
# current <- current %>%
#   filter(season=="2015/16") %>%
#   group_by(TEAMNAME) %>% 
#   select(Team=TEAMNAME,currentTime,season) %>%
#   slice(1) 
# 
# seqGlsForYear <-seqGlsForYear %>%
#   inner_join(current) %>%
#   select(Team,Max=mins,Current=currentTime)
# 
# seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Max <- seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Current
# 
# 
# ## for a tweet
# seqGlsForYear %>% 
#   arrange(desc(Current)) %>% 
#   select(Team,Current,YearMax=Max) %>% 
#   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# 
# ## need to sort man city no goals ag
# 
# names(seqGlsForYear)
# 
# seqGlsForYear$category<-"sinceGF"
# seqGlsAgYear$category<-"sinceGA"
# 
# # seqGlsAgYear$Max <- -seqGlsAgYear$Max
# # seqGlsAgYear$Current <- -seqGlsAgYear$Current
# #or
# seqGlsForYear$Max <- -seqGlsForYear$Max
# seqGlsForYear$Current <- -seqGlsForYear$Current
# 
# df <- rbind(seqGlsAgYear,seqGlsForYear)
# manCity <- data.frame(Team="Man. City",Max=-450,Current=-450,category="sinceGA")
# df <-rbind(df,manCity)
# 
# ## this does not work
# tmOrder <- df %>% 
#   arrange((Current)) %>% 
#   .$Team
# 
# ggplot(df, aes(x = Team, y = Current, fill = category)) +
#   geom_bar(subset = .(category == "sinceGF"), stat = "identity", alpha=0.4) +
#   geom_bar(subset = .(category == "sinceGA"), stat = "identity") +
#  #  scale_x_discrete(limits=tmOrder) + ## works but scrunched up
#    scale_y_continuous(breaks = seq(-max(abs(df$Current)), max(abs(df$Current)),50)) +
# #   ylab("Number of Games") +
# #   xlab("Run Difference") +
#   coord_flip() +
#   labs(title="Minutes Since Goal Allowed and Goal Scored") +
#   labs(x="") +
#   labs(y="Min since Conceding                    Mins Since Scoring") +
#   guides(fill=FALSE) +
#  # ggtitle(title) +
#   theme_bw()
# 
# # Error in layer(data = data, mapping = mapping, stat = stat, geom = GeomBar,  : 
# #                  could not find function "."
# 
# ## try and get y into geom bar so can add max
# 
# ggplot(df) +
#   geom_bar(subset = .(category == "sinceGF"),aes(x = Team, y = Current, fill = category), stat = "identity") +
#   geom_bar(subset = .(category == "sinceGA"), aes(x = Team, y = Current, fill = category),stat = "identity") +
#   geom_bar(subset = .(category == "sinceGF"),aes(x = Team, y = Max, fill = category), stat = "identity", alpha=0.4) +
#   geom_bar(subset = .(category == "sinceGA"), aes(x = Team, y = Max, fill = category),stat = "identity", alpha=0.4) +
#   #  scale_x_discrete(limits=tmOrder) + ## works but scrunched up
#   scale_y_continuous(breaks = seq(-max(abs(df$Max)), max(abs(df$Max)),50)) +
#   #   ylab("Number of Games") +
#   #   xlab("Run Difference") +
#   coord_flip() +
#   labs(title="Current and Season Max Runs since Conceding or Scoring Goal in BPL") +
#   labs(x="") +
#   labs(y="Min since Conceding                    Mins Since Scoring") +
#   guides(fill=FALSE) +
#   # ggtitle(title) +
#   theme_bw()
# 
# ## can do for one team each season
# 
# 
# # inc games against self but no issue presumably #36104
# oppGames <- teamGames %>%
#   select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
#   inner_join(teamGames)
# 
# tempAg <- oppGames %>% #18052
#   arrange(tmGameOrder) %>%   
#   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
#   filter(OppTeam!=TEAMNAME) %>%
#   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
#   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
#   left_join(teamGames) %>%
#   select(TEAMNAME,season,otherID,tmGameOrder) %>%
#   rename(TEAMMATCHID=otherID)
# 
# 
# 
# 
# 
# 
# 
# all <-tempAg %>%
#   left_join(goals) %>%
#   select(tmGameOrder,TIME) %>%
#   arrange(tmGameOrder,TIME)
# 
# 
# allYear <- all %>%
#   mutate(firstGame = min(tmGameOrder)) %>%
#   filter(!is.na(TIME)) %>% 
#   mutate(
#     game_start = (tmGameOrder - firstGame) * 90,
#     goal_time = game_start + TIME,
#     diff = goal_time - lag(goal_time, default = 0)
#   )
# ## check - looks good NB check need to finish off the current game
# allYear %>%
#   filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# 
# ## look at club rather than year
# names(allYear)
# seqGlsAgYear <-allYear %>%
#   filter(TEAMNAME=="Chelsea") %>%
#   
#   group_by(season) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   group_by(season) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(desc(diff)) %>%
#   select(Season=season,mins=diff)
# 
# # # 
# # # 
# # # 
# # # # 
# # # # 
# # # # current <-tempAg %>%
# # # #   left_join(goals) %>%
# # # #   select(tmGameOrder,TIME) %>%
# # # #   arrange(desc(tmGameOrder),desc(TIME))
# # # # 
# # # # current <-current %>%
# # # #   mutate(lastGame = max(tmGameOrder)) %>%
# # # #   filter(!is.na(TIME)) %>% 
# # # #   mutate(
# # # #     game_start = (lastGame -tmGameOrder)* 90,
# # # #     currentTime = game_start + 90 - TIME
# # # #   )
# # # # 
# # # # ## check - looks good
# # # # #current %>%
# # # # # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # # 
# # # # current <- current %>%
# # # #   filter(season=="2015/16") %>%
# # # #   group_by(TEAMNAME) %>% 
# # # #   select(Team=TEAMNAME,currentTime,season) %>%
# # # #   slice(1) 
# # # # 
# # # # seqGlsAgYear <-seqGlsAgYear %>%
# # # #   inner_join(current) %>%
# # # #   select(Team,Max=mins,Current=currentTime)
# # # # 
# # # # seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Max <- seqGlsAgYear[seqGlsAgYear$Current>seqGlsAgYear$Max,]$Current
# # # 
# # # ## Goals for
# # # # tempFor <- oppGames %>% #18052
# # # #   arrange(tmGameOrder) %>%   
# # # #   select(TEAMMATCHID,MATCHID,TEAMNAME) %>%
# # # #   filter(OppTeam==TEAMNAME) %>%
# # # #   select(OppTeam,season,TEAMMATCHID,MATCHID) %>% # try to add the oppTeam tmGameNumber
# # # #   rename(TEAMNAME=OppTeam,otherID=TEAMMATCHID) %>%
# # # #   left_join(teamGames) %>%
# # # #   select(TEAMNAME,season,otherID,tmGameOrder) %>%
# # # #   rename(TEAMMATCHID=otherID)
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # all <-tempFor %>%
# # # #   left_join(goals) %>%
# # # #   select(tmGameOrder,TIME) %>%
# # # #   arrange(tmGameOrder,TIME)
# # # # 
# # # # 
# # # # allYear <- all %>%
# # # #   mutate(firstGame = min(tmGameOrder)) %>%
# # # #   filter(!is.na(TIME)) %>% 
# # # #   mutate(
# # # #     game_start = (tmGameOrder - firstGame) * 90,
# # # #     goal_time = game_start + TIME,
# # # #     diff = goal_time - lag(goal_time, default = 0)
# # # #   )
# # # # ## check - looks good
# # # # #allYear %>%
# # # # #  filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # # 
# # # # seqGlsForYear <-allYear %>%
# # # #   filter(TEAMNAME=="Chelsea") %>%
# # # #   
# # # #   group_by(season) %>%
# # # #   ungroup() %>%
# # # #   arrange(desc(diff)) %>%
# # # #   group_by(season) %>%
# # # #   slice(1) %>%
# # # #   ungroup() %>%
# # # #   arrange(desc(diff)) %>%
# # # #   select(Season=season,mins=diff)
# # # # 
# # # # current <-tempFor %>%
# # # #   left_join(goals) %>%
# # # #   select(tmGameOrder,TIME) %>%
# # # #   arrange(desc(tmGameOrder),desc(TIME))
# # # # 
# # # # current <-current %>%
# # # #   mutate(lastGame = max(tmGameOrder)) %>%
# # # #   filter(!is.na(TIME)) %>% 
# # # #   mutate(
# # # #     game_start = (lastGame -tmGameOrder)* 90,
# # # #     currentTime = game_start + 90 - TIME
# # # #   )
# # # # 
# # # # ## check - looks good
# # # # #current %>%
# # # # # filter(TEAMNAME=="Tottenham H"&season=="2015/16")
# # # # 
# # # # current <- current %>%
# # # #   filter(season=="2015/16") %>%
# # # #   group_by(TEAMNAME) %>% 
# # # #   select(Team=TEAMNAME,currentTime,season) %>%
# # # #   slice(1) 
# # # # 
# # # # seqGlsForYear <-seqGlsForYear %>%
# # # #   inner_join(current) %>%
# # # #   select(Team,Max=mins,Current=currentTime)
# # # # 
# # # # seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Max <- seqGlsForYear[seqGlsForYear$Current>seqGlsForYear$Max,]$Current
# # # # 
# # # # ## need to sort man city no goals ag
# # # # 
# # # # names(seqGlsForYear)
# # # # 
# # # # seqGlsForYear$category<-"sinceGF"
# # # # seqGlsAgYear$category<-"sinceGA"
# # # # 
# # # # # seqGlsAgYear$Max <- -seqGlsAgYear$Max
# # # # # seqGlsAgYear$Current <- -seqGlsAgYear$Current
# # # # #or
# # # # seqGlsForYear$Max <- -seqGlsForYear$Max
# # # # seqGlsForYear$Current <- -seqGlsForYear$Current
# # # # 
# # # # df <- rbind(seqGlsAgYear,seqGlsForYear)
# # # # manCity <- data.frame(Team="Man. City",Max=-450,Current=-450,category="sinceGA")
# # # # df <-rbind(df,manCity)
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # ## are man c longest
# # # # 
# # # # standings %>% 
# # # #   ungroup() %>% 
# # # #   filter(tmYrGameOrder==5) %>% 
# # # #   arrange(cumGA) %>% 
# # # #   select(season,team,cumGA)
# # # #   
# # # # 
# # # # ggplot(gameGoal, aes(x=slength,y=n)) +
# # # #   geom_bar(data=subset(gameGoal,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
# # # #   geom_bar(data=subset(gameGoal,cond==TRUE),stat="identity", width=0.7, fill="blue") +
# # # #   
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# 
# # #  assist progress fabregas 150 -------------------------------------------
# # 
# # sort(names(summary))
# # 
# # # goog only six
# # topGuys <- summary %>% 
# #   ungroup() %>% 
# #   select(PLAYERID,Assists) %>% 
# #   group_by(PLAYERID) %>% 
# #   summarize(Ass=sum(Assists, na.rm=T)) %>% 
# #     filter(Ass>149)
# #   
# # 
# # df <-playerGame %>% 
# #   filter((START+subOn>0)&PLAYERID %in% topGuys$PLAYERID) %>% 
# #   select(name,PLAYERID,Gls,Assists,gameDate) %>% 
# #   arrange(gameDate) %>% 
# #   group_by(name,PLAYERID) %>% 
# #   mutate(gameOrder=row_number(),points=Assists+Gls,
# #          cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
# # 
# # 
# # yTitle <-"Cumulative Assists (inc. secondary)"
# # xTitle <-"PL Apps"
# # plot <- df %>% 
# #   ggvis(~gameOrder,~cumAssists) 
# # 
# # plot %>% 
# #   layer_lines(stroke= ~name) %>% 
# #   add_axis("x", title=xTitle) %>% 
# #   add_axis("y", title=yTitle) %>% 
# #   add_legend("stroke", title="") %>% 
# #   bind_shiny("sp_comparisons")
# # 
# # df <-playerGame %>% 
# #   filter((START+subOn>0)) %>% 
# #   select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
# #   arrange(gameDate) %>% 
# #   group_by(name,PLAYERID) %>% 
# #   mutate(gameOrder=row_number(),points=Assists+Gls,
# #          cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
# # 
# # df <-playerGame %>% 
# #   filter((START+subOn>0)&PLAYERID %in% input$playerComps) %>% 
# #   select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
# #   arrange(gameDate) %>% 
# #   group_by(name,PLAYERID) %>% 
# #   mutate(gameOrder=row_number(),points=Assists+Gls,
# #          cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) 
# # 
# # 
# # 
# # # exploding boxplot -------------------------------------------------------
# # 
# # df <- standings %>% 
# #   ungroup() %>% 
# #   filter(tmYrGameOrder==20) %>% 
# #   select(team,season,cumPts,tmYrGameOrder)
# # 
# # df2015 <- df %>% 
# #   filter(season=="2015/16")
# #   
# # 
# # plot_ly(y = rnorm(50), type = "box") %>%
# #   add_trace(y = rnorm(50, 1))
# # 
# # plot_ly(y = df, type = "box") %>%
# #   add_trace(y = rnorm(50, 1))
# # 
# # plot_ly(df2015,y=cumPts, type = "box", boxpoints = "all", jitter = 0.3, color=team,
# #         pointpos = -1.8, hoverinfo="text", text = paste(team,"<br> Points:",cumPts)) %>%
# #   layout(hovermode = "closest", autosize= F, width=600, height= 700, 
# #          title = "Points after 16 Games 2015/16",
# #          xaxis = list( title = ""),
# #          yaxis = list(title = "",tickfont=list(size=0)),
# #          legend=list(y=0.95,font=list(size=15))
# #   )
# # 
# # names(df)
# # 
# # glimpse(df)
# # 
# # df <-df %>% 
# #   mutate(year=str_sub(season,1,4)) %>% 
# #   rename(points=cumPts)
# # 
# # plot_ly(df,y=cumPts,x=season, type = "box", key=season)
# # 
# # exploding_boxplot(
# #   df,
# #   y = "points",
# #   group = "year",
# #   #color = "spray", # looks like has to be a range of colors if you group- which you need to to sep data
# #   label = "team", # could be another column name but not combo
# #   iqr = 2,
# #   width = 600,
# #   margin = list(bottom = 30, left = 50, top = 20, right = 20),
# #  # xlab = "Spray Type",
# #  # ylab = "Count Provided"
# # )
# #   
# # exploding_boxplot(
# #   data.frame(
# #     rowname = rownames(InsectSprays),
# #     InsectSprays,
# #     stringsAsFactors = FALSE
# #   ),
# #   y = "count",
# #   group = "spray",
# #   #color = "spray", # looks like has to be a range of colors if you group- which you need to to sep data
# #   label = "rowname", # could be another column name but not combo
# #   iqr = 2,
# #   #margin = list(bottom = 50, left = 30, top = 20, right = 20),
# #   xlab = "Spray Type",
# #   ylab = "Count Provided"
# # )
# # 
# # 
# # # another look at resuts against team lower/higher - specific pos ---------
# # 
# #  ## prob can just use tmYrGame Order for both teams
# #  ars <- standings %>% 
# #    filter(season=="2014/15"&team=="Arsenal") %>% 
# #    select(team,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# #    mutate(prevPos=lag(position))
# #  
# #  tm1 <- standings %>% 
# #    filter(season=="2014/15") %>% 
# #    group_by(team) %>% 
# #    select(team,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# #    mutate(prevPos=lag(position))
# #   
# 
# 
# # exploding crowds --------------------------------------------------------
# 
# sort(names(teamGames))
# 
# glimpse(teamGames)
# 
# avCrowds <-teamGames %>% 
#   ungroup() %>% 
#   filter(venue=="H") %>% 
#   mutate(year=str_sub(season,1,4)) %>% 
#   group_by(year,TEAMNAME) %>% 
#   summarize(avCrowd=round(mean(CROWD),0)) %>% 
#   ungroup()
# 
# 
# exploding_boxplot(
#  avCrowds,
#   y = "avCrowd",
#   group = "year",
#   #color = "spray", # looks like has to be a range of colors if you group- which you need to to sep data
#   label = "TEAMNAME", # could be another column name but not combo
#   iqr = 2,
#   margin = list(bottom = 50, left = 70, top = 20, right = 20),
#   xlab = "",
#   ylab = "Av Attendance"
# )
# 
# 
# df <- standings %>% 
#   ungroup() %>% 
#   filter(tmYrGameOrder==input$st_boxGames) %>% 
#   select(team,season,cumPts,tmYrGameOrder)  %>% 
#   mutate(year=str_sub(season,1,4)) %>% 
#   rename(points=cumPts)
# 
# print("expl ")
# print(glimpse(df))
# 
# exploding_boxplot(
#   df,
#   y = "points",
#   group = "year",
#   width= 600,
#   label = "team", 
#   iqr = 2,
#   margin = list(bottom = 30, left = 50, top = 20, right = 20)
#   
# )
# 
# 
# test <- avCrowds %>% 
#   group_by(year) %>% 
#   summarize(tot=mean(avCrowd))
#   
# #  # looks reasonable 
# 
# 
# #  test <-tm1 %>% 
# #    inner_join(tm1,by=c("OppTeam"="team","gameDate")) %>% 
# #    select(team,gameDate,OppTeam,venue.x,res.x,prevPos.x,prevPos.y) %>% 
# #    filter(team=="Chelsea")
# #  
# #  ## extend to all seasons
# #  
# #  sort(names(standings))
# #  
# #  tm1 <- standings %>% 
# #    
# #    group_by(team,season) %>% 
# #    select(team,season,gameDate,OppTeam,tmYrGameOrder,venue,res,position) %>% 
# #    mutate(prevPos=lag(position))
# #  
# #  # looks reasonable 
# #  chelsea <-tm1 %>% 
# #    inner_join(tm1,by=c("OppTeam"="team","gameDate","season")) %>% 
# #    select(team,season,gameDate,OppTeam,venue=venue.x,res=res.x,teamPos=prevPos.x,oppPos=prevPos.y) %>% 
# #    
# #    filter(team=="Chelsea"&!is.na(teamPos))
# #  
# #  ## issue with color - not 
# #  plot_ly(test, x = jitter(teamPos), y = oppPos, mode = "markers", hoverinfo = "text",color=res,
# #          text = paste(season,"<br>",OppTeam,"<br>",gameDate)) %>%
# #                        
# #    layout(hovermode = "closest",
# #           title="Results by Opposition Standing",
# #           xaxis=list(title="Team Standing",tickfont=list(size=10),tickcolor="#000",tickangle=45),
# #           yaxis=list(title="Opposition Standing"
# #           )
# #    )
# #  
# #  # not sure how to do variable symbol - maybe like opacity
# #  plot_ly(test, x = jitter(teamPos), y = oppPos, type = "scatter",mode = "markers", hoverinfo = "text",color=res, marker=list(symbol="square-open-dot", size=10),
# #          text = paste(season,"<br>",OppTeam,"<br>",gameDate)) %>%
# #    
# #    layout(hovermode = "closest",
# #           title="Results by Opposition Standing",
# #           xaxis=list(title="Team Standing",tickfont=list(size=10),tickcolor="#000",tickangle=45),
# #           yaxis=list(title="Opposition Standing"
# #           )
# #    )
# #  
# # ## posss diff look boxplots chart - pyramid chart
# #  library(tidyr)
# #  # looks good prob best to divide by 
# # chelsea %>% 
# #   group_by(teamPos,oppPos,res) %>% 
# #   tally() %>% 
# #   spread(res,n, fill =0) %>% 
# #   mutate(Pl=(Draw+Loss+Win),ppg=round((3*Win+Draw)/Pl,2))
# 
# 
# # share of teams goals in terms of goals and assists ----------------------
# 
# # sort(names(playerGame))
# # 
# # pl <- playerGame %>% 
# #   ungroup() %>% 
# #   select(name,PLAYERID,Gls,Assists,season,TEAMNAME) %>% 
# #   group_by(season,TEAMNAME,PLAYERID,name) %>% 
# #   summarize(totGls=sum(Gls),totAss=sum(Assists),points=totGls+totAss)
# # 
# # tm <- pl %>% 
# #   group_by(TEAMNAME,season) %>% 
# #   summarize(tmGls=sum(totGls))
# # 
# # star <- pl %>% 
# #   left_join(tm) %>% 
# #   mutate(gls_pc=100*totGls/tmGls,pts_pc=round(100*(points/tmGls),1)) %>% 
# #   ungroup() %>% 
# #   arrange(desc(pts_pc)) %>% 
# #   select(name,team=TEAMNAME,season,goals=totGls,assists=totAss,pc=pts_pc) %>% 
# #   filter(season!="2015/16") %>% 
# #   head(10) %>% 
# #   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# # 
# # 
# # # player results playing not playing --------------------------------------
# # 
# # 
# # 
# # 
# # # vardy scoring -----------------------------------------------------------
# # 
# # sort(names(playerGame))
# # 
# # 
# # ppgPlayer <- playerGame %>%
# #   filter(PLAYERID=="FELLANM"&(START+subOn)>0)
# # 
# # 
# # 
# # plot_ly(ppgPlayer , x=plGameOrder, y=Gls, name="Goals", type="bar",
# #         hoverinfo="text",
# #         text=paste(Opponents,"<br>",gameDate)) %>%
# #   add_trace(x=plGameOrder,y=Assists, name="Assists (inc secondary)")  %>%
# #   layout(hovermode = "closest", barmode="stack",
# #          xaxis=list(title="Appearance Order"),
# #          yaxis=list(title="Points"),
# #          title=" Hover bar for details", titlefont=list(size=20)
# #   )
# #   
# # 
# # library(plotly)
# # plot_ly(z = volcano, type = "heatmap")
# # 
# # 
# # 
# # library(plotly)
# # p <- plot_ly(
# #   x = c("giraffes", "orangutans", "monkeys"),
# #   y = c(20, 14, 23),
# #   name = "SF Zoo",
# #   type = "bar",
# #   filename="r-docs/simple-bar"
# # )
# # p
# # 
# # 
# # p2 <- add_trace(
# #   p,
# #   x = c("giraffes", "orangutans", "monkeys"),
# #   y = c(12, 18, 29),
# #   name = "LA Zoo",
# #   filename="r-docs/simple-bars"
# # ) %>% 
# #   layout(barmode = "stack")
# # p2
# # 
# # 
# # # by age ------------------------------------------------------------------
# # 
# # sort(names(playerGame))
# # 
# # playerGame %>% 
# #   filter(season=="2015/16"&START>0) %>% 
# #   ungroup() %>% 
# #   group_by(TEAMNAME) %>% 
# #   summarize(avAge=mean(age)) %>% 
# #   ungroup() %>% 
# #   arrange(avAge)
# 
# 
# 
# # goal and/or assist sequence ---------------------------------------------
# 
# # appeared <-playerGame %>%
# #   filter((START+subOn)>0) %>%  # 380 as sh
# #   arrange(gameDate) %>%
# #   select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
# # 
# # 
# # 
# # appeared$Scored <- 0
# # appeared$Scored[appeared$Gls>0] <- 1
# # 
# # 
# # 
# # ## look at vardy this gives the table
# # 
# # appeared %>%
# #   filter(PLAYERID=="VARDYJ") %>%
# #   select(Scored) %>%
# #   do(subSeq(.$Scored))
# # 
# # # as does this
# # runApps <- subSeq(appeared$Scored)
# # 
# # ## want to do for every player using group_id
# # 
# # # appeared %>%
# # #   filter(PLAYERID=="VARDYJ") %>%
# # #   select(Scored) %>%
# # #   do(subSeq(.$Scored)) %>% 
# # #   cbind(id=appeared$PLAYERID)
# # # 
# # # 
# # # appeared %>%
# # #   group_by(PLAYERID) %>% 
# # #   select(Scored) %>%
# # #   do(subSeq(.$Scored)) %>% 
# # #   filter(PLAYERID=="VARDYJ")
# # 
# # 
# # 
# # appeared %>%
# #   group_by(PLAYERID) %>% 
# #   select(Scored) %>%
# #   do(subSeq(.$Scored)) %>% 
# #   filter(value==1)%>% 
# #   group_by(PLAYERID) %>% 
# #   mutate(best=max(slength)) %>% 
# #   filter(slength==best) %>% 
# #   ungroup() %>% 
# #   arrange(desc(slength))
# #   filter(PLAYERID=="VARDYJ")
# #   
# #   
#   ### assists only
#   sort(names(playerGame))
# 
#   appeared <-playerGame %>%
#     filter((START+subOn)>0) %>%  # 380 as sh
#     arrange(gameDate) %>%
#     select(PLAYERID,Gls,Assists,plGameOrder,TEAMNAME,gameDate,Opponents) %>%
#     mutate(points=Gls+Assists)
# 
# 
# 
#   appeared$Assisted <- 0
#   appeared$Assisted[appeared$Assists>0] <- 1
# 
# 
#   appeared %>%
#     group_by(PLAYERID) %>%
#     select(Assisted) %>%
#     do(subSeq(.$Assisted)) %>%
#     filter(value==1)%>%
#     group_by(PLAYERID) %>%
#     mutate(best=max(slength)) %>%
#     filter(slength==best) %>%
#     ungroup() %>%
#     arrange(desc(slength))
# 
#   # PLAYERID first  last slength midpoint value  best
#   # (chr) (dbl) (dbl)   (dbl)    (dbl) (dbl) (dbl)
#   # 1   SILVAD2   159   167       9      163     1     9
#   # 2   BECKHAD   169   176       8      173     1     8
#   # 3    SOLERF   209   216       8      213     1     8
# 
#   appeared$Pointed <- 0
#   appeared$Pointed[appeared$points>0] <- 1
# 
# 
#   appeared %>%
#     group_by(PLAYERID) %>%
#     select(Pointed) %>%
#     do(subSeq(.$Pointed)) %>%
#     filter(value==1)%>%
#     group_by(PLAYERID) %>%
#     mutate(best=max(slength)) %>%
#     filter(slength==best) %>%
#     ungroup() %>%
#     arrange(desc(slength)) %>% 
#     left_join(summary) %>% 
#     select(name,run=slength) %>% 
#     unique() %>% 
#     filter(run>11) %>% 
#     DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
#     
#     
#     
#     
#     
#   # PLAYERID first  last slength midpoint value  best
#   # (chr) (dbl) (dbl)   (dbl)    (dbl) (dbl) (dbl)
#   # 1    VARDYJ    34    51      18       43     1    18
#   # 2   SUTTONC    58    73      16       66     1    16
#   # 3   BECKHAD   168   182      15      175     1    15
# 
# 
#   # NB http://www.whoscored.com/Players/13756 example where assists are listed
#   # EG Vardy did not get an assist on ozadiki? goal v asv but even they have him at 15 assists during his run
#   # the site does not go bac k to sutton stats for instance maybe just 10 years?
# 
# 
# # adebayor ----------------------------------------------------------------
# 
#   ppgPlayer <- playerGame %>%
#     filter(PLAYERID=="ADEBAYE"&(START+subOn)>0) %>% 
#     mutate(gameOrder=row_number())
#   
#   joined man city 105 tot 139
#   games <- nrow(ppgPlayer)
#   
#   xTitle <- paste0("Appearance Order - Total ",games)
#   
#   plot_ly(ppgPlayer , x=gameOrder, y=Gls, name="Goals", type="bar",
#           hoverinfo="text",
#           text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
#     add_trace(x=gameOrder,y=Assists, name="Assists (inc secondary)", type="bar",
#               hoverinfo="text",
#               text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder))  %>%
#     add_trace(x = c(105, 105), y= c(5, 5), mode = "lines", line = list(color = "green",width=1, dash = "10px"), showlegend = FALSE) %>%
#     layout(hovermode = "closest", barmode="stack",
#            
#            xaxis=list(title=xTitle),
#            yaxis=list(title="Points"),
#            title=" Hover bar for details", titlefont=list(size=16)
#     )
#            ### look at first 15 games
#            
#            
#            names(playerGame)
#        test <-    playerGame %>%
#              filter(PLAYERID=="ADEBAYE"&(START+subOn)>0&plTmGameOrder<16)  %>% 
#            group_by(TEAMNAME) %>% 
#            summarize(G=sum(Gls), A=sum(Assists))
#     
#   