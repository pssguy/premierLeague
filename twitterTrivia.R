# 
# 
# ## NBC sports commentary no side has lost except Man U 07/8
# names(standings)
# standings %>% 
#   filter(final_Pos==1&tmYrGameOrder<3&res!="Win") %>% 
#   select(season,team,res)
# 
# test <- standings %>% 
#   filter(team=="Chelsea"&cumGA>4) %>% 
#  group_by(season) %>% 
#   arrange(tmYrGameOrder) %>% 
#   slice(1) %>% 
#   mutate(year=str_sub(season,1,4)) %>% 
#   select(season,tmYrGameOrder)
# 
# ty <- data.frame(season="2015/16",tmYrGameOrder=2)
# test <- rbind(test,ty)
# 
# mean(test$tmYrGameOrder) 6.5
# 
# str(test)
# 
# test <- data.frame(test)
# 
# test %>%  ggvis(~season,~tmYrGameOrder) %>% 
#   layer_bars(width=.03) %>% 
#   layer_points(fill:="blue") %>% 
#   add_axis("x", properties = axis_props(
#     labels = list(angle = 45, align = "left", fontSize = 9)
#   ),title="") %>%
#   add_axis("y",values=c(0:15),title="Game Allowed 5th Goal of Season",title_offset=50)
#   
# 
# # ## last time chelsea 5 poinrts behind so many teams
# 
# ## pardew team win half or losee half home games
# 
# names(standings)
# standings$res
# temp <- HW <-standings %>% 
#   group_by(team,season) %>% 
#   filter(venue=="H"&res=="Loss"&season<"2015/16") %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange
# 
# temp <- HW <-standings %>% 
#   group_by(team,season) %>% 
#   filter(venue=="H"&res=="Win"&season<"2015/16") %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
#   
# ## home wins in 1 round
# 
# names(standings)
# 
# standings %>% 
#   filter(season=="2015/16"&tmYrGameOrder==3&venue=="H"&res=="Win")
# 
# ## total 889
# test <- standings %>%
#   group_by(season,tmYrGameOrder) %>% 
#   #filter(venue=="A"&res=="Draw") %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange((n))
# 
# 
#   ## look at 20 goal scorers
# 
# sort(names(playerGame))
# 
# test <-playerGame %>% 
#   group_by(TEAMNAME,season,PLAYERID,name) %>% 
#   summarize(goals=sum(Gls),pens=sum(PENS)) %>% 
#   mutate(pointSize=0.5) %>% 
#   filter(goals>19) %>% 
#   rename(team=TEAMNAME) %>% 
#   ungroup() %>% 
#   arrange(season,team)
# 
# str(test)
#   
#   
#   tauchart(test) %>% tau_point("season", "goals",color="team", size="pointSize") %>% 
#   tau_legend()  %>% 
#   tau_tooltip(c("name","team","goals","pens")) %>% 
#   #tau_guide_x(tick_period='day', tick_format="year") %>% 
#   tau_guide_y(min= 19, max = 40)
# 
# ## follow up to DT tweet on how few home wins
# 
# names(standings)
# 
# standings %>% 
#   group_by(season) %>% 
#   tally() #760
# 
# 
# standings %>% 
#   group_by(season) %>% 
#   filter(res=="Win") %>% 
#   summarize(pc=count(res)/780)
# 
# library(Lahman)
# batting_tbl <- tbl_df(Batting)
# names(batting_tbl)
# batting_tbl %>% count(playerID) # number of seasons
# batting_tbl %>% count(playerID, wt = G) # number of games presumably
# batting_tbl %>% count(playerID, wt = G, sort = TRUE) # rosepe01 3562
# 
# standings %>% 
#   group_by(season) %>% 
#   count(res)
# 
# ## not sure can use
# names(standings)
# standings$venue
# hw <- standings %>% 
#   group_by(season) %>% 
#   filter(res=="Win"&venue=="H"&tmYrGameOrder<=3) %>% 
#   tally() %>% 
#   rename(w=n)
# 
# standings %>% 
#   group_by(season) %>% 
#   filter(tmYrGameOrder<=3) %>% 
#   tally() %>% 
#   inner_join(hw) %>% 
#   mutate(pc=200*w/n) %>% 
#   ggvis(~season,~pc) %>% 
#   layer_points() %>% 
#   layer_lines() %>% 
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 11
#   )),title = "") %>% 
#   add_axis("y", title= "% Home Wins first 3 rounds")
# 
# 
# 
#  library(dplyrExtras)
# 
# 
# set.seed(123456)
# n = 10
# df = data.frame(a= sample(1:3,n,replace=TRUE),
#                 b= sample(1:100,n,replace=TRUE),
#                 x=rnorm(n))
# dt = as.data.table(df)
# mutate_if(df,a==3,x=100)
# mutate_if(tbl_df(df),a==1,x=200)
# mutate_if(as.tbl(df),a==1,x=300,b=400)
# mutate_if(dt,a==1 | a==2,x=400)
# mutate_if(group_by(dt,a),a==1 | a==2,x=mean(b)) 
# mutate_if(group_by(df,a),a==1 | a==2,x=mean(b))
# 
# 
# ## gf after so many games
# names(standings)
# 
# test <-standings %>% 
#   filter(tmYrGameOrder==4&team=="Liverpool") %>% 
#   select(season,cumGF,final_Pos)
# test %>% 
#   ggvis(~cumGF) %>% 
#   layer_histograms()
# 
# once before but had scored 19 goals in other comps and finisheed 3rd
# 
# ## lst time man u lost after being ahead after 60 mins
# key is to see if team is ahead/leel/down at any time
# 
# 
# names(standings)
# names(teamGames)
# 
# names(goals)
# 
# tmResults <- teamGames %>% 
#   ungroup() %>% 
#   filter(TEAMNAME=="Man. Utd.") %>% 
#   select(TEAMMATCHID,gameDate,team=TEAMNAME,MATCHID) %>%   #889
#   left_join(standings)  %>%  # still 889
#   select(TEAMMATCHID,gameDate,OppTeam,GF,GA,res,MATCHID) 
# 
# ## need to get oppo games in here
# 
# glsFor <-goals %>% 
#   filter(TEAMMATCHID %in% tmResults$TEAMMATCHID) %>%  #1755 looks good
#   left_join(tmResults) %>% 
#   select(TIME,TEAMMATCHID,gameDate,OppTeam,GF,GA,res,MATCHID)
# 
# glsAg <- goals %>% 
#   filter(TEAMMATCHID %in% tmResults$TEAMMATCHID) %>% 
#   left_join(tmResults) %>% 
#   select(TIME,TEAMMATCHID,MATCHID)
# 
# ## look at glsFor at 60 mins
# 
#  a <-glsFor %>% 
#   filter(res=="Loss"&TIME<61) %>% 
#   group_by(TEAMMATCHID,MATCHID) %>% 
#    
#   tally() %>% 
#    rename(gFor=n)
#  
#  
# theMatches <-teamGames %>% 
#   filter(TEAMMATCHID %in% a$TEAMMATCHID) %>% 
#          select(MATCHID,TEAMMATCHID)
# 
# oppID <- teamGames %>% 
#      filter(MATCHID %in% theMatches$MATCHID&!TEAMMATCHID %in% a$TEAMMATCHID) %>% 
#   select(TEAMMATCHID,MATCHID)
#          
# 
# glsAg <- goals %>% 
#   filter(TEAMMATCHID %in% oppID$TEAMMATCHID) %>% 
#   
#   left_join(teamGames,by="TEAMMATCHID") %>% 
#   select(TIME,TEAMMATCHID,MATCHID)
# 
# 
# b <-glsAg %>% 
#   filter(TIME<61) %>% 
#   group_by(TEAMMATCHID,MATCHID) %>% 
#   
#   tally() %>% 
#   rename(gAg=n) 
#   
# c <-  a %>% 
#     ungroup() %>% 
#   full_join(b,by="MATCHID") %>% 
#   select(MATCHID,gFor,gAg)#36 which is same as b kinda expected some NA
# 
# str(c)
# 
# d <- c %>% 
#   filter(gFor>gAg) %>% 
#   inner_join(glsFor) 
# 
# 
#   
# 
# b
#  
#  
# 
# standings %>% 
#   
# 
# names(summary)
# 
# played <-summary %>% 
#   filter(season=="2015/16"&mins>0) %>% 
#   group_by(TEAMNAME) %>% 
#   tally() %>% 
#   rename(All=n) 
# 
# AllMins <-summary %>% 
#   filter(season=="2015/16"&mins==360) %>% 
#   group_by(TEAMNAME) %>% 
#   tally() %>% 
#   rename(EverPresent=n) %>% 
#   inner_join(played) %>% 
#   rename(Team=TEAMNAME) %>% 
#   arrange(EverPresent) %>% 
#   DT::datatable(class='compact stripe hover row-border',width=300,options= list(
#     paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE))
# 
# ## last year
# played <-summary %>% 
#   filter(season=="2014/15"&mins>0) %>% 
#   group_by(TEAMNAME) %>% 
#   tally() %>% 
#   rename(All=n) 
# 
# AllMins <- summary %>% 
#   filter(season=="2014/15"&mins==3420) %>% 
#   group_by(TEAMNAME) %>% 
#   tally() %>% 
#   rename(EverPresent=n) %>% 
#   right_join(played) %>% 
#   rename(Team=TEAMNAME) %>% 
#   arrange(EverPresent)
# 
# AllMins[is.na(AllMins$EverPresent),]$EverPresent <- 0
# 
# AllMins %>% 
#   DT::datatable(class='compact stripe hover row-border',width=300,options= list(
#     paging = FALSE, searching = FALSE, info=FALSE,sorting = FALSE))
# 
# 
# ### curley heatmaps
# Goals_team_for %>% 
#   filter(season=="2014/15") %>% 
#   select(team,LongRange,SixYd)
# 
# 
# ## teams that have won at both arsenal and Liverpool during season
# 
# ## 
# sort(names(standings)) #18052
# sort(unique(standings$team))
# test <-standings %>% 
#   filter(team!="Arsenal"&team!="Liverpool")
# sort(unique(test$team))
# 
# test %>% #16272
#   
#   filter((OppTeam=="Liverpool"|OppTeam=="Arsenal")&venue=="A"&res=="Win") %>% #109
#   #mutate(totGF=sum(GF),totGA=sum(GA)) %>% 
#   group_by(team,season) %>% 
#   summarise(games=n(),totGF=sum(GF),totGA=sum(GA)) %>% 
#   ungroup() %>% 
#   arrange(desc(season)) %>% 
#   filter(games==2)
# 
# 
# ## NAISMITH perfect HT as sub
# 
# names(goals)
# names(teamGames)
# names(playerGame)
# 
# ht <- goals %>% 
#   left_join(playerGame) %>% 
#   group_by(PLAYER_MATCH) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   filter(n>2) %>% 
#   .$PLAYER_MATCH #284
# 
# htsub <-playerGame %>% 
#   filter(Gls>2&subOn>0) %>% 
#   select(name,TEAMNAME,Opponents,gameDate,PLAYER_MATCH) %>% #6th
#   arrange(desc(gameDate))
# (chr)     (chr)        (chr)     (date)        (int)
# # 1 Emmanuel Adebayor   Arsenal    Derby Co. 2008-04-28       580992
# # 2   Robert Earnshaw West Brom     Charlton 2005-03-19       541861
# # 3 Jimmy Hasselbaink   Chelsea       Wolves 2004-03-27       529608
# # 4     Romelu Lukaku West Brom    Man. Utd. 2013-05-19       650421
# # 5   Steven Naismith   Everton      Chelsea 2015-09-12       679730
# # 6     Ole Solskjaer Man. Utd. Nottm Forest 1999-02-06       466499
# 
# htsub <-playerGame %>% 
#   filter(Gls>2&TEAMNAME=="West Brom") # 2 of 4
# 
# dets <-htsub %>% 
#   left_join(goals) %>% 
#   arrange(PLAYER_MATCH)
# 
# #LUKAKU also perfect
# 
# htsub <-playerGame %>% 
#   filter(Gls>2&subOn>0) %>% 
#   select(name,team=TEAMNAME,opponents=Opponents,date=gameDate) %>% #6th
#   arrange(desc(date)) %>% 
#   datatable(width='50%')
# 
# 
#   
# 
# ## FINISHING TOP 4 IF AS LOW AS ---
# 
# ## 
# top4s <- standings %>% 
#   ungroup() %>% 
#   filter(final_Pos<5&season<"2015/16") %>% 
#   select(team,final=final_Pos,season) %>% 
#   unique()
# 
# standings %>% 
#   ungroup() %>% 
#   right_join(top4s) %>% 
#   select(team,season,final_Pos,position,round=tmYrGameOrder) %>% 
#   group_by(round) %>% 
#   summarize(worst=max(position))
# 
# temp <-standings %>% 
#   ungroup() %>% 
#    
#   right_join(top4s) %>% 
#   filter(season>"1994/95") %>%
#   select(team,season,position,round=tmYrGameOrder) %>% 
#   group_by(round) %>% 
#   mutate(worst=max(position))
# 
# ## shoul b 4*20*38 #3040
# 
# temp %>% 
#   ggvis(~round,~position) %>% 
#   layer_points(fill=~team)
# 
# ## not that great but newcastle 2002/3 were 19th with 4pts finished 3rd
# #No team lower than 12th after round 8 have made top 4 
# ## need to improve grap
# 
# temp %>% 
#   ggvis(~round,~worst) %>% 
#   layer_points(fill=~team)
# 
# 
# 
# standings %>% 
#   ungroup() %>% 
#   right_join(top4s) %>% 
# filter(season=="2014/15"&tmYrGameOrder==1) %>% 
#   select(season,team,tmYrGameOrder,position)
# 
# 
# 
# 
# newcastle scoring 
# 
# standings %>% 
#   ungroup() %>%
#   filter(team=="Newcastle U"&gameDate>"2014-12-31") %>% 
#   arrange(desc(gameDate)) %>% 
#   .$GF
# 
# leaders <- readRDS("leaders.rds")
# standings <- readRDS("standings.rds")
# 
# ##
# ## crowds with bournemouth
# 
# names(teamGames)
# 
# temp <- teamGames %>% 
#   ungroup() %>% 
#   filter(venue=="H") %>% 
#   group_by(season,TEAMNAME) %>% 
#   summarize(attendance=mean(CROWD,na.rm=T))
# 
# temp %>% 
#   ggvis(~season,~attendance) %>% 
#   layer_boxplots()
# 
# temp %>% 
#   arrange(attendance)
# 
# teamGames %>% 
# 
# teamGames %>% 
#   ungroup() %>% 
#   filter(season=="2015/16") %>% 
#   arrange(CROWD)
# 
# 
# x <- teamGames %>% 
#   ungroup() %>% 
#   filter(venue=="H"&CROWD==9999) %>% 
#   arrange(TEAMNAME,desc(gameDate))
# 
# 
# ### martial - players scored on 1st 2 games etc
# 
# sort(names(playerGame))
# 
# temp <-playerGame %>% 
#   select(PLAYERID,name,plGameOrderApp,Gls) %>% 
#   mutate(scored=ifelse(Gls>0,1,0))
# 
# runGoals <- temp %>% 
#   group_by(PLAYERID) %>% 
#   arrange(plGameOrderApp) %>% 
#   do(subSeq(.$scored))
# 
# # did not score on debut
# 
# didNot <- runGoals %>% 
#   filter(first==1&value==0) %>% 
#   ungroup() %>% 
#   tally() #3967
# 
#  runGoals %>%  #191 191/3967 #4.8%
#   filter(first==1&value==1&PLAYERID!="OWNGOAL") %>% 
#   select(PLAYERID,slength) %>% 
#   group_by(slength) %>% 
#   tally() %>% 
#    ggvis(~slength, ~n) %>% 
#    layer_bars(fill:="red", width=.7) %>% 
#    add_axis("x",title="Run of games scored in at start of PL career", format='d') %>% 
#    add_axis("y", title="Number of Players")
# 
#  y <-runGoals %>% 
#   filter(first==1&value==1) %>% 
#   ungroup() %>% 
#   arrange(desc(slength))
# 
# playerGame %>% 
#   filter(PLAYERID=="QUINNM") %>% 
#   select(gameDate,Gls,TEAMNAME)
# 
# ##   (date) (int)
# 1  1992-11-21     2
# 2  1992-11-28     1
# 3  1992-12-05     1
# 4  1992-12-12     2
# 5  1992-12-19     2
# 6  1992-12-26     2
# 
# 
# playerGame %>% 
#   filter(PLAYERID=="MACHEDF") %>% 
#   select(Gls,TEAMNAME)
# 
# 
# # defence of realm --------------------------------------------------------
# 
# holders <-standings %>% 
#   filter(final_Pos==1&season!="2015/16") %>% 
#   select(season,team) %>% 
#   group_by(season,team) %>% 
#   unique()
# 
# temp <- data.frame(season="1991/92",team="Leeds U")
# 
# holders<- rbind(temp,holders)
# 
# write_csv(holders,"holders.csv")
# 
# ## apply a lag
# 
# holders$defense <- lead(holders$season)
# glimpse(holders)
# 
# holders$season <- as.character(holders$season)
# holders$defense <- as.character(holders$defense)
# holders$defense[24] <- "2015/16"
# write_csv(holders,"holders.csv")
# ## join
# 
# sort(names(standings))
# df <-holders %>% 
#   select(season=defense,team) %>% 
#   left_join(standings) %>% 
#   select(season,team,position,tmYrGameOrder)
# 
# glimpse(temp)
# 
# 
# df <- cbind(df, id = seq_len(nrow(df)))
# all_values <- function(x) {
#   if(is.null(x)) return(NULL)
#   row <- df[df$id == x$id,c("team","season") ]
#   paste0( names(row),": ",format(row), collapse = "<br />") 
# }
# 
# str(df)
# 
# df %>% 
#   filter(tmYrGameOrder<=10) %>% 
#   group_by(season) %>% 
#   ggvis(~tmYrGameOrder,~jitter(position), key := ~id) %>% 
#   #add_tooltip(all_values,"click") %>% 
#   layer_points(fill=~team, size:=30) %>% 
#   
#   layer_lines(stroke:='grey') %>% 
#   
#   scale_numeric("y", reverse=T,label="Position") %>% 
#   add_axis("x",title="Games Played")
#   
# 
# # mnu 
# 
# av goals by game by club ------------------------------------------------
# 
# sort(names(standings))
# 
# fi
# 
# test <-standings %>% 
#   group_by(team,season) %>% 
#   mutate(goals=GF+GA) %>% 
#   summarize(totGoals=sum(goals)) %>% 
#   ungroup() %>% 
#   arrange(desc(totGoals)) %>% 
#   group_by(season) %>% 
#   mutate(goalRank=row_number()) %>% 
#   filter(team=="Man. Utd.")
# 
# test <-standings %>% 
#   group_by(team,season) %>% 
#   mutate(goals=GF+GA) %>% 
#   summarize(totGoals=sum(goals)) %>% 
#   ungroup() %>% 
#   arrange(desc(totGoals)) %>% 
#   group_by(season) %>% 
#   mutate(goalRank=min_rank(-totGoals)) %>% 
#   filter(season=="2015/16")
# 
# 
# 
# test <-standings %>% 
#   group_by(team,season) %>% 
#   mutate(goals=GF+GA) %>% 
#   summarize(totGoals=sum(goals),totGF=sum(GF),totGA=sum(GA)) %>% 
#   ungroup() %>% 
#   arrange(desc(totGoals)) %>% 
#   group_by(season) %>% 
#   mutate(goalRank=row_number()) %>% 
#   left_join(standings) %>% 
#   select(team,season,totGoals,goalRank,final_Pos,totGF,totGA)  %>% 
#   unique() %>% 
#   filter(team=="Man. Utd.")
# 
# seasons <- sort(unique(standings$season))
# 
# test$year <- factor(test$season)
# 
# test %>% 
#   mutate(pos=as.character(final_Pos)) %>% 
#   ggvis(~year,~goalRank) %>% 
#   layer_points(fill =~pos) %>% 
#   layer_text(text:=~final_Pos,dy:=-5,dx:=5) %>% 
#   hide_legend("fill") %>% 
#   scale_numeric("y", reverse=T,label="Rank of Goals per Game For and Against") %>% 
#   add_axis("x", properties = axis_props(labels = list(
#           angle = 45, align = "left", fontSize = 11
#         )),title = "")
# 
# 
# # Goals and assists per min played leaders --------------------------------
# 
# sort(names(playerGame))
# 
# temp <- playerGame %>% 
#   group_by(name,PLAYERID,season) %>% 
#   summarize(A=sum(Assists),G=sum(Gls),P=A+G,M=sum(mins),minsPerP=round(M/P,0)) %>% 
#   ungroup() %>% 
#   arrange(minsPerP) %>% 
# #  group_by(season) %>% 
#  # mutate(pointsRank=min_rank(minsPerP)) %>% 
#   filter(season=="2015/16"&PLAYERID!="OWNGOAL"&P>=7) %>% 
#   select(name,Gls=G,Assts=A,Pts=P,minsPerPt=minsPerP) %>% 
#   head(10) %>% 
#   DT::datatable(class='compact stripe hover row-border order-column',
#                 rownames= FALSE,escape=FALSE,width=400,options=list(
#                   searching=FALSE,info=FALSE,paging=FALSE))
# 
# # minsperP not so good as he has generally played a fair bit (prob should do min )
# playerGame %>% 
#   filter(PLAYERID!="OWNGOAL") %>% 
#   group_by(name,PLAYERID,season) %>% 
#   summarize(A=sum(Assists),G=sum(Gls),P=A+G,M=sum(mins),minsPerP=round(M/P,0)) %>% 
#   ungroup() %>% 
#   arrange(desc(P)) %>% 
#   group_by(season) %>% 
#   mutate(pointsRank=min_rank(-P)) %>% 
#   filter(PLAYERID=="ROONEYX"&season>"2001/02") %>% 
#   arrange(season) %>% 
#   select(Gls=G,Assts=A,Pts=P,minsPerPt=minsPerP,pointsRank) %>% 
#   DT::datatable(class='compact stripe hover row-border order-column',
#                 rownames= FALSE,escape=FALSE,width=400,options=list(
#                   searching=FALSE,info=FALSE,paging=FALSE))
# 
# 
# # no 3 goal by team in round ----------------------------------------------
# 
# sort(names(standings))
# temp <- standings %>% 
#  group_by(season,tmYrGameOrder) %>% 
#   summarize(maxGoals=max(GF),sumGoals=sum(GF)) %>% 
#  filter(sumGoals<17)
#   
# 
# 
# # guardian article top 7 divide -------------------------------------------
# 
# sort(names(standings))
# temp<- standings %>% 
#   filter(tmYrGameOrder==12&(position==1|position==7)) %>% 
#  
#   select(season,cumPts,team)
#   
#   temp %>% spread(cumPts)
#   
#   stocks <- data.frame(
#     time = as.Date('2009-01-01') + 0:9,
#     X = rnorm(10, 0, 1),
#     Y = rnorm(10, 0, 2),
#     Z = rnorm(10, 0, 4)
#   )
#   stocksm <- stocks %>% gather(stock, price, -time)
#   stocksm %>% spread(stock, price)
#   stocksm %>% spread(time, price)
# 
#   
#   
#   
#   
# # teams ever top - LCFC ---------------------------------------------------
# 
# sort(names(standings))
#   
# ever <-  standings %>% 
#     ungroup() %>% 
#     filter(position==1) %>% 
#     select(team) %>% 
#         unique() 
# 
# ## had already inc Leicester
# 
# 1	Leicester C	8	16	5	7	2	13
# 2	Man. Utd.	8	15	12	20	8	1
# 
# ## should change that to add linkk to results
# 
# 
# standings %>% 
#   ungroup() %>% 
#   filter(position==1&tmYrGameOrder>12) %>% 
#   select(team) %>% 
#   unique() ## 11th 
# 
# 
# 
# test <-standings %>% 
#   ungroup() %>% 
#   filter(position==1) %>% 
#  select(team,season) %>% 
#   unique() %>% 
# 
#   ungroup() %>% 
#   select(team) %>% 
#   group_by(team) %>% 
#   tally() %>% 
#   arrange(desc(n))
# 
# 
# # worst resukts of champiosn ----------------------------------------------
# 
# sort(names(standings))
# 
# worst <-standings %>% 
#   ungroup() %>% 
#   mutate(GD=GF-GA) %>% 
#   filter(final_Pos==1) %>% 
#   
#   arrange(GD,desc(GF)) %>% 
#   group_by(season) %>% 
#   slice(1) # prev mnc only one goal diff
# 
# 
# 
# threeGoals <-standings %>% 
#   ungroup() %>% 
#   mutate(GD=GF-GA) %>% 
#   filter(final_Pos==1&GD<=-3) %>% 
#     group_by(season) %>% 
#   tally() # man u 1995/6 5 times
# 
# 
# ## 
#   
# 
# # lukaku 50 goals by age 23 -----------------------------------------------
# 
# sort(names(playerGame))
# 
# playerGame %>% 
#   filter(age<23&PLAYERID!="OWNGOAL") %>% 
#   group_by(name) %>% 
#   summarize(totGls=sum(Gls)) %>% 
#               filter(totGls>49)
# 
# test <-playerGame %>% 
#   filter(PLAYERID=="LUKAKUR") %>% 
#   summarize(totGls=sum(Gls))

# more than 17 per season -------------------------------------------------
names(summary)
test <-summary %>% 
  group_by(PLAYERID,season) %>% 
mutate(Goals=subGls+StGls) %>% 
  filter(Goals>17) %>% 
  group_by(season) %>% 
  tally()



# any teams scored 4 away and been relegated ------------------------------
## actually quirte a few
# sort(names(standings))
# 
# standings %>% 
#   ungroup() %>% 
#   filter(GF>3&venue=="A") %>% 
#   arrange(desc(final_Pos))

# player % 90 mins --------------------------------------------------------

# sort(names(playerGame))
# 
# table(playerGame$POSITION)
# 
# test <-playerGame %>% 
#   filter(mins>0) %>% 
#   mutate(fullGame=(ifelse(mins==90,1,0))) %>% 
#            group_by(PLAYERID,name,POSITION) %>% 
#            summarize(count=n(),fullPC=round(100*mean(fullGame),1)) %>% 
#              ungroup() %>% 
#              filter(count>=49) 
# 
# 
# 
# plot_ly(test, x = count, y = fullPC, mode = "markers", hoverinfo = "text",color=POSITION,
#         text = paste(name,"<br> % Full games:",fullPC)) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="% Complete Games"
#          )
#   )
#  
# ##   crosstalk linkmto the plotly like 
# 
# 
# 
# # ozils assists -----------------------------------------------------------
# 
# summary %>% 
#   ungroup() %>% 
#   filter(Assists>15) %>% 
#   
#   
#   
#   arrange(desc(Assists))
# 
# # most apps nver off ------------------------------------------------------
# 
# sort(names(summary))
# 
# glimpse(summary)
# 
# test <- summary %>% 
#   ungroup() %>% 
#   group_by(PLAYERID,name,POSITION,TEAMNAME) %>% 
#   summarize(sumOff=sum(Off,na.rm=T),sumOn=sum(On,na.rm=T),sumSt=sum(St,na.rm=T)) %>% 
#   filter((sumOff+sumOn)==0&POSITION!="Goalkeeper") %>% 
#   ungroup() %>% 
#   arrange(desc(sumSt)) 
#   
# ward <-  summary %>% 
#     filter(PLAYERID=="WARDJ") # 2 subs for por
#   
#   
# 
# # assist to goals by season vardy mahrez ----------------------------------
# 
# 
# # times to goals hazard ---------------------------------------------------
# 
# 
# # number of changes at top ------------------------------------------------
# 
# sort(names(standings))
# 
# test <- standings %>% 
#   ungroup() %>% 
#   filter(position==1) %>% 
#   group_by(team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# #leicester now same as spurs
# 
# bySeason<- standings %>% 
#   ungroup() %>% 
#   filter(position==1) %>% 
#   group_by(team,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# 
# standings %>% 
#   ungroup() %>% 
#   filter(position==1&team=="Tottenham H") # latest is 4
# 
# 
# standings %>% 
#   ungroup() %>% 
#   filter(position==1&team=="Everton")   #1 2007/08         5 Everton Tottenham H     3     1 2007-08-14         584             2
# 
# 
# standings %>% 
#   ungroup() %>% 
#   filter(position==1&team=="Leicester C")
# 
# # number od season a team has figured man u top 20
# bySeason %>% 
#   ungroup() %>% 
#  select(team) %>% 
#   group_by(team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# 
# diffTeams <-bySeason %>% 
#   ungroup() %>% 
#   select(season) %>% 
#   group_by(season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) # 7 in 1998/9
# 
# sort(names(standings))
# 
# standings %>% 
#   filter(season=="2015/16"&position==1) %>% 
#   select(team,tmYrGameOrder) %>% 
#   arrange(tmYrGameOrder) %>% 
#   mutate(lastteam=lag(team)) %>% 
#   filter(tmYrGameOrder!=1) %>% 
#   mutate(change=ifelse(team==lastteam,0,1)) %>% 
#   summarize(changes=sum(change))
# 
# # season changes
# # (chr)   (dbl)
# # 1 2015/16    
# 
# 5
# ## for map
# 
# bySeason<- standings %>% 
#   ungroup() %>% 
#   filter(position==1) %>% 
#   group_by(team,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# # diff teams per season
# diffTeams <-standings %>% 
#   ungroup() %>% 
#   filter(position==1) %>% 
#   group_by(team,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   select(season) %>% 
#   group_by(season) %>% 
#   tally() %>% 
#   arrange(desc(n))  %>% 
#   rename(teams=n)
# 
# topChanges <- standings %>% 
#   filter(position==1) %>% 
#   select(season,team,tmYrGameOrder) %>% 
#   arrange(tmYrGameOrder) %>% 
#   group_by(season) %>% 
#   mutate(lastteam=lag(team)) %>% 
#   filter(tmYrGameOrder!=1) %>% 
#   mutate(change=ifelse(team==lastteam,0,1)) %>% 
#   group_by(season) %>% 
#   summarize(changes=sum(change)) %>% 
#   inner_join(diffTeams)
# 
# names(topChanges)
# 
# plot_ly(topChanges, x = season, y = changes, type = "bar", marker=list(color=teams), showlegend=TRUE, key=season) %>%
#           layout(hovermode = "closest",
#                  xaxis=list(title=""),
#                  yaxis=list(title="Changes in Team Leading Table"),
#                             title="Number of leadership changes by Year (Click bar for details)", titlefont=list(size=12)
#                            )
#       
#         
# ## connect to graph of teams position that season
# 
# numeroUnos <- standings %>% 
#   ungroup() %>% 
#   filter(season=="1992/93"&position==1) %>% 
#   select(team) %>% 
#   unique() 
# 
# numeroUnos <- numeroUnos$team
# 
# df <-standings %>% 
#   ungroup() %>% 
#   filter(season=="1992/93"&team %in% numeroUnos)
# 
# glimpse(df)
# 
# yr <- "1992/93"
# theTitle<-paste0("Teams that topped Premier League ",yr, " (click team to show/no show)")
# 
# plot_ly(df,x=tmYrGameOrder, y=position, mode="markers+lines", color=team,
#         hoverinfo = "text",
#         text = paste(team,
#                      #"<br>",Date,
#                      "<br> v ",OppTeam," "," ",GF,"-",GA,
#                      "<br> Position:",position,
#                      "<br> Played:",tmYrGameOrder,
#                      "<br> Points:",cumPts))  %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="League Position", autorange="reversed"),
#                    
#          title=theTitle, titlefont=list(size=12)
#          )
#  
# 
#   
# 
# 
# View(standings %>% 
#   filter(season=="2015/16"&position==1) %>% 
#   select(team,tmYrGameOrder) %>% 
#   arrange(tmYrGameOrder))
# 
# 
# # krul assists 2015 -------------------------------------------------------
# Rooney, Routledge, Sakho, Mirallas, Fellaini, Gomis, Cleverley and Schweinsteiger put together
# testPlayers <- c("KRULT","ROONEYX","ROUTLEw","SAKHOD","GOMISB","CLEVERT","SCHWEIB")
# 
# playerGame %>% 
#     filter(gameDate>"2014-12-31"&PLAYERID %in% testPlayers) %>% 
#   group_by(name,PLAYERID) %>% 
#   summarize(Assists=sum(Assists,na.rm=T)) %>% 
#   ungroup() %>% 
#   arrange(desc(Assists)) %>% 
#   select(name, Assists) %>% 
#   datatable()
#   

# goalscorers on team -----------------------------------------------------

# sort(names(playerGame))
# 
# playerGame %>% 
#   ungroup() %>% 
#   group_by(name,PLAYERID,season,TEAMNAME) %>% 
#   filter
# 
# sort(names(summary))
# 
# # name PLAYERID    TEAMNAME  season     n
# # (chr)    (chr)       (chr)   (chr) (int)
# # 1  Curtis Davies DAVIESC2 Aston Villa 2008/09     2
# # 2   Lars Bohinen  BOHINEL   Blackburn 1997/98     2
# 
# summary %>% 
#   ungroup() %>% 
#   
#   filter(PLAYERID!="OWNGOAL"&(StGls+subGls)>0) %>% 
#   group_by(TEAMNAME,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) %>% 
#   filter(TEAMNAME=="Crystal P")
# 
# 
# # hernandez goalscoring ---------------------------------------------------
# 
# summary %>% 
#   ungroup() %>% 
#   filter((StGls+subGls)>9&St<=10)



#  top 2 players on team most goals ---------------------------------------


# sort(names(summary))
# 
# team <- summary %>% 
#   ungroup() %>% 
# #  group_by(LASTNAME,PLAYERID) 
#   mutate(goals=(StGls+subGls)) %>% 
#   arrange(desc(goals)) %>% 
#   group_by(TEAMNAME,season) %>% 
#   summarize(tot=sum(goals))
# 
# 
# a <- summary %>% 
#   ungroup() %>% 
#   #  group_by(LASTNAME,PLAYERID) 
#   mutate(goals=(StGls+subGls)) %>% 
#   arrange(desc(goals)) %>% 
#   group_by(TEAMNAME,season) 
# 
# topTwo <- slice(a, 1:2) %>% 
#   group_by(season,TEAMNAME) %>% 
#   summarize(totTwo=sum(goals)) %>% 
#   inner_join(team) %>% 
#   mutate(pc=round(100*totTwo/tot,1)) %>% 
#   ungroup() %>% 
#   arrange(desc(pc))
# 
# library(tidyr)
#   
# b <- slice(a, 1:2) %>% 
#   group_by(season,TEAMNAME) %>% 
#   select(TEAMNAME,season,name,goals) %>% 
#   mutate(display=paste0(name," (",goals,")")) %>% 
#   select(TEAMNAME,season,display) %>% 
#   ungroup()
# 
# c <- b %>% 
#   spread(TEAMNAME,-season) %>% 
#   ungroup()
# 
# 
# stocks <- data.frame(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# stocksm <- stocks %>% gather(stock, price, -time)
# str(stocksm)
# 
# str(b)
# 
# stocksm %>% spread(stock, price)
# 
# b %>% spread(c(season,TEAMNAME),display)
# 
# c <-b %>% 
#   group_by(TEAMNAME,season) %>% 
#   summarize(all=paste0(display,collapse=" ")) %>% 
#   inner_join(topTwo) %>% 
#   ungroup() %>% 
#   arrange(desc(pc)) %>% 
#   head(10) %>% 
#   select(pc,team=TEAMNAME,season,top2=all) %>% 
#   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging =    FALSE, searching = FALSE,info=FALSE))
# 
# 
# ddply(data, .(TEAMNAME, goals), summarize, Players = paste(player, collapse = " "))
# 
# 
# # % goals outside area over 50 tot - toure --------------------------------
# 
# 
# sort(names(Place))
# 
# ## 
# fiftyGoals <- Place %>% 
#   mutate(goals=(SixYd+PenArea+LongRange)) %>% 
#   group_by(PLAYERID) %>% 
#  summarize(tot=sum(goals)) %>% 
#   filter(tot>49) %>% 
#   .$PLAYERID
# 
# df <- Place %>% 
#   filter(PLAYERID %in% fiftyGoals&PLAYERID!="OWNGOAL") %>% 
#   mutate(goals=(SixYd+PenArea+LongRange)) %>% 
#   group_by(PLAYERID,name) %>% 
#   summarize(tot=sum(goals),lr=sum(LongRange),pc=round(100*lr/tot)) %>% 
#   ungroup() %>% 
#   arrange(desc(pc))  
# 
# 
# plot_ly(df, x = tot, y = pc, mode = "markers", hoverinfo = "text",key=PLAYERID,
#         text = paste(name,
#                      "<br>Category: ",lr,
#                      "<br>Toatal: ",tot,
#                      "<br>PerCent: ",pc,"%")) %>%
#   layout(hovermode = "closest",
#          title="% of Premier League Goals by Category",
#          xaxis=list(title="Total Goals"),
#          yaxis=list(title="% By category"
#          )
#   )
# 
# ## poss add current players
#   
#   
#   plot_ly(df, x = count, y = fullPC, mode = "markers", hoverinfo = "text",color=POSITION,key=PLAYERID,
#           text = paste(name,
#                        "<br>Appearances:",count,
#                        "<br>Full games:",fullPC,"%")) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="% Complete Games"
#            )
#     )
#   
# 
# 
# # worst defeat by champs --------------------------------------------------
# 
#   sort(names(standings))
#   
# standings %>% 
#     filter(final_Pos==1&season!="2015/16") %>% 
#     mutate(GD=GF-GA) %>% 
#     arrange(GD,GA) %>% 
#     group_by(season) %>% 
#  
#     slice(1) %>% 
#   ungroup() %>% 
#   mutate(score=paste(GF,GA,sep="-")) %>% 
#   select(season,champs=team,score,Opponent=OppTeam) %>% 
#   
#   DT::datatable(rownames=FALSE,class='compact stripe hover row-border',options= list(paging = FALSE, searching = FALSE,info=FALSE))
#   



# 0-0 games ---------------------------------------------------------------

# sort(names(standings))
# 
# test <-standings %>% 
#   ungroup() %>% 
#   group_by(season,team) %>% 
#   filter(team=="Man. Utd."&GA==0&GF==0) %>% 
#   tally() %>% 
#   left_join(standings) 
# 
# test %>% 
#   ggvis(~season, ~n) %>% 
#   layer_points()
# 
# do they lead system
# 
# 
# # difference in cost starting lineups -------------------------------------
# 
# # 17 lei
# # 309 mnc on tv
# # 
# # guardian has 18 292.1
# 
# 
# 
# # HOW HAVE TEASM DONE THAT FINISHED WELL SEASON BEFORE --------------------
# 
# sort(names(standings))
# 
# 
# 
# 
# # BIGGEST deficit ever made up tp avoid relegation--------------------------------------------
# 
# 
# sort(names(standings))
# 
# ## since went to 38
# 
# modern <-
#   standings %>% 
#   filter(season>"1994/95")
# 
# modern %>% 
#   select(team,cumPts,final_Pos,season,tmYrGameOrder,position)
# 
# ## look at 17th place which would need to exceed so that number +1
# safeSpot <- modern %>% 
#   filter(position==17) %>% 
#   select(season,tmYrGameOrder,pts=cumPts)
# 
# 
# stayUp <- modern %>% 
#   left_join(safeSpot) %>% 
#   select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
#   mutate(diff=cumPts-pts-1) %>% 
#   ## only teams that have survived relegation and been below 18th at some point
#   filter(final_Pos<18&position>17)  %>% 
#   ungroup()
# 
# 
# a <- stayUp %>% 
#   arrange(diff,desc(tmYrGameOrder)) # most is -11 whu after 29 games in 2006
# 
# 
# ### look at best by season and round
# 
# df <- a %>% 
#   group_by(season) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   mutate(deficit=-diff)
# 
# plot_ly(df, x = season, y = deficit, mode = "markers", hoverinfo = "text",color=team,
#         text = paste(team,
#                      "<br>Deficit:",deficit,
#                      "<br>After:",tmYrGameOrder," games",
#                      "<br>Finished:",final_Pos)) %>%
#   layout(hovermode = "closest",
#          title="Biggest points deficit overcome in 38 game PL season",
#          xaxis=list(title=""),
#          yaxis=list(title=""
#          )
#   )
# 
# plot_ly(df, x = count, y = fullPC, mode = "markers", hoverinfo = "text",color=POSITION,key=PLAYERID,
#         text = paste(name,
#                      "<br>Appearances:",count,
#                      "<br>Full games:",fullPC,"%")) %>%
#   layout(hovermode = "closest",
#          xaxis=list(title="Games Played"),
#          yaxis=list(title="% Complete Games"
#          )
#   )
#   
# 
# #wigan 2x, everton west brom and bradford have escaped on final day
# 
# ## go down
# 
# goDown <- modern %>% 
#   left_join(safeSpot) %>% 
#   select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
#   mutate(diff=cumPts-pts) %>% 
#   ## only teams that have survived relegation and been below 18th at some point
#   filter(final_Pos>17&position<18)  %>% 
#   ungroup()
# 
# 
# b <- goDown %>% 
#   arrange(diff)
# 
# # max blackpool 9 clear after 17 games 2010/11 but had GD -3
# 
# ## look at leads lost at top
# 
# madeUp <- modern %>% 
#   filter(position==1) %>% 
#   select(season,tmYrGameOrder,pts=cumPts)
# 
# 
# stayUp <- modern %>% 
#   left_join(madeUp) %>% 
#   select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
#   mutate(diff=cumPts-pts) %>% 
#   ## only teams that have survived relegation and been below 18th at some point
#   filter(final_Pos<2&position>1)  %>% 
#   ungroup()
#   
# c <- stayUp %>% 
#   arrange(diff) #manu made up 12 points on New after 25 games - though they were second
# 
# 
# 
# topFour <- modern %>% 
#   filter(position==4) %>% 
#   select(season,tmYrGameOrder,pts=cumPts)
# 
# 
# madeEuro <- modern %>% 
#   left_join(topFour) %>% 
#   select(final_Pos,team,tmYrGameOrder,cumPts,position,pts) %>% 
#   mutate(diff=cumPts-pts) %>% 
#   ## only teams that have survived relegation and been below 18th at some point
#   filter(final_Pos<5&position>4)  %>% 
#   ungroup()
# 
# d <- madeEuro %>% 
#   arrange(diff)  # leeds 10 back after 22 in 2000/1 they were in 11th place
# # aston villa winless sequence ----------------------------------------------------
# 
# ## cf worst in EPL
# 
# W <-standings %>% 
#   ungroup() %>% 
#   filter(team=="Sunderland") %>% 
#   arrange(tmGameOrder) %>% 
#   select(res,tmGameOrder) %>% 
#   mutate(cat=ifelse(res=="Win",1,0)) %>% 
#   do(subSeq(.$cat))
# 
# 81
# 171
# 196
# 26
# 184
# 0
# 
# a <-standings %>% 
#   filter(team=="Sunderland"&tmGameOrder>=318&tmGameOrder<=331) %>% 
#   select(gameDate,res)
# 
# 83
# 198
# 211
# 14
# 205
# 0
# 
# 85
# 213
# 226
# 14
# 220
# 0
# 
# 133
# 318
# 331
# 14
# 325
# 0
# 
# confirms there are 3 at 14 but not acc to graph? ( could be 3 prob best to witch to plotly)
# 
# ## for all teams
# 
# sort(names(standings))
# 
# WTeam <-standings %>% 
#   ungroup() %>% 
#  
#   arrange(tmGameOrder) %>% 
#   group_by(team) %>% 
#   select(res,tmGameOrder) %>% 
#   mutate(cat=ifelse(res=="Win",1,0)) %>% 
#   do(subSeq(.$cat))
# 
# # D <-standings %>% 
# #   ungroup() %>% 
# #   filter(team==input$teamA) %>% 
# #   arrange(tmGameOrder) %>% 
# #   select(res,tmGameOrder) %>% 
# #   mutate(cat=ifelse(res=="Draw",1,0)) %>% 
# #   do(subSeq(.$cat))
# # 
# # L <-standings %>% 
# #   ungroup() %>% 
# #   filter(team==input$teamA) %>% 
# #   arrange(tmGameOrder) %>% 
# #   select(res,tmGameOrder) %>% 
# #   mutate(cat=ifelse(res=="Loss",1,0)) %>% 
# #   do(subSeq(.$cat))
# 
# 
# ## no wins by team
# 
# ## worst runs
# NoWinRecords <- WTeam %>% 
#   filter(value==0) %>% 
#   group_by(slength,team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(slength))
# 
# 
# NoWinTeam <- WTeam %>% 
#   filter(value==0) %>% 
#   group_by(slength,team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(slength)) %>% 
#   group_by(team) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   arrange(desc(slength))
# 
# # sunderland have had 3 at 14 !! as well as 26 -- not sure they do looks more like 2 from
# 
# test <-standings %>% 
#   ungroup() %>% 
#   filter(team=="Sunderland") %>% 
#   arrange(tmGameOrder) %>% 
#   select(res,tmGameOrder)  %>% 
#   mutate(cat=ifelse(res=="Win",1,0)) 
# 
# 
# Win <- W %>% 
#   filter(value==0) %>% 
#   group_by(slength) %>% 
#   tally()
# if (tail(W,1)$value==0) {
#   cond <- Win$slength == tail(W,1)$slength
# } else {
#   cond <- FALSE
# }
# ggplot(Win, aes(x=slength,y=n)) +
#   geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
#   geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
#   theme_bw() +
#   xlab("Sequence") +
#   ylab("Count") +
#   ggtitle("Winless")
# 
# ## manu have garnered 1 point in past 4 games against teams average 
# 
# # 18 chelsea 6 v 15 #CFC
# # 17 stoke   5 v 11 #SCFC
# # 16 norwich 4 v 18 #NCFC
# # 15 bournemouth 4 v 17 #AFCB
# 
# 
# 
# # evertons -3-4 results ---------------------------------------------------
# 
# threeFour <- standings %>% 
#   ungroup() %>% 
#   filter(GF==3&GA==4) %>% 
#   group_by(team) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# season <-standings %>% 
#   ungroup() %>% 
#   select(season,team) %>% 
#   unique() %>% 
#   group_by(team) %>% 
#   tally() %>% 
#   rename(seasons=n)
# 
# a <- season %>% 
#   left_join(threeFour) %>% 
#   replace(is.na(.), 0)
# 
# plot_ly(a, x = seasons, y = jitter(n), mode = "markers",hoverinfo = "text",
#         text = paste(team,
#                      "<br>Seasons:",seasons,
#                      "<br>Count:",n
#                      )
# ) %>%
#   layout(hovermode = "closest",
#          title="3-4 Premier league defeats (points jittered to identify when hovering)",
#          xaxis=list(title="Seasons in Premier League"),
#          yaxis=list(title="Count"
#          )
#   )
# 
# plot_ly(a, x = seasons, y = n, mode = "markers", hoverinfo = "text",color=team,
#         text = paste(season,
#                      "<br>",team,
#                      "<br>Deficit:",deficit,
#                      "<br>After:",tmYrGameOrder," games",
#                      "<br>Finished:",final_Pos)) %>%
#   layout(hovermode = "closest",
#          title="Biggest points deficit overcome in 38 game PL season",
#          xaxis=list(title="",tickfont=list(size=10),tickcolor="#000",tickangle=45),
#          yaxis=list(title="Points in arrears of safety"
#          )
#   )


#  Liv goals for ----------------------------------------------------------

#best after 20 games little tricky think done before - shoould have highlighted
# other work done in world soccer



# 0-0 draws ---------------------------------------------------------------

# test <-standings %>% 
#   ungroup() %>% 
#   filter(season>"1994/95"&GF==0&GA==0&tmYrGameOrder<22) %>% 
#   group_by(season) %>% 
#   tally()
# 
# # totGoals
# tgTest <-standings %>% 
#   ungroup() %>% 
#   filter(season>"1994/95"&tmYrGameOrder<22) %>% 
#   group_by(season) %>% 
#   summarize(goals=sum(GF)) %>% 
#   inner_join(test) %>% 
#   ggvis(~n,~goals)
# 
# 
# ## close games nothing special this season
# 
# test <-standings %>% 
#   ungroup() %>% 
#   filter(season>"1994/95"&abs(GF-GA)<2&tmYrGameOrder<22) %>% 
#   group_by(season) %>% 
#   tally() %>% 
#   mutate(pc=round(100*n/420))
# 
# allGames <-standings %>% 
#   ungroup() %>% 
#   filter(season>"1994/95"&tmYrGameOrder<22) %>% 
#   group_by(season) %>% 
#   tally()  %>% 
#   inner_join(test) # this year 66 similar to 
# 
# 
# 
# # manu no-one yet at 9 goals +assists -------------------------------------
# 
# 
# # teams with 1000+ minute players -----------------------------------------
# 
# sort(names(summary))
# 
# test <- summary %>% 
#   ungroup() %>% 
#   filter(season=="2015/16"&mins>999) %>% 
#   group_by(TEAMNAME) %>% 
#   tally() # range 8 to 12 manu one with 9 or less
# 
# # this does not work as summary is all season
# test <- summary %>% 
#   ungroup() %>% 
#   filter(mins>999) %>% 
#   group_by(TEAMNAME,season) %>% 
#   tally()
# 
# ## is away wins still lot
# 
# sort(names(standings))
# 
# test <-standings %>% 
#   ungroup() %>% 
#   filter(season>"1994/95"&tmYrGameOrder<22&venue=="A"&res=="Win") %>% 
#   group_by(season) %>% 
#   tally() %>% 
#   ggvis(~season,~n) %>% 
#   layer_points() %>% 
#   scale_numeric("y", zero=T) %>% 
#   ggvis::add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 11
#   )),title = "") %>% 
#   ggvis::add_axis("y",title="Total Away Wins after 21 rounds", format='d')
# 
# 
# standings %>% 
#   ungroup() %>% 
#   filter(season=="2015/16"&venue=="A"&res=="Win") %>% 
#   group_by(tmYrGameOrder) %>% 
#   tally() %>% 
#   ggvis(~tmYrGameOrder,~n) %>% 
#   layer_points() %>% 
#   layer_smooths(stroke="red") %>% 
#   ggvis::add_axis("x", title="Week") %>% 
#   ggvis::add_axis("y",title="Away Wins 2015/16", format='d')
# 
# # how do young teams perform inc next year --------------------------------
# 
# 
# ## GAMES IN WHCH manu have scored 0 or 1
# 
# sort(names(standings))
# 
# test <- standings %>% 
#   ungroup() %>% 
#   group_by(season) %>% 
#   filter(team=="Man. Utd."&GF<2&tmYrGameOrder<23) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# 
# # LFC scoring first goal from outside area - how long has it taken --------
# 
# 
# 
# # # Palace lose 5 in row - how many teams done this and stayed up ---------
# 
# 
# 
# # players at least on bench -----------------------------------------------
# 
# sort(names(playerGame))
# 
# a <-playerGame %>% 
#   filter(PLAYERID!="OWNGOAL"&tmY) %>% 
#   group_by(PLAYERID,name,TEAMNAME,season) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# a %>% 
#   filter(season=="2015/16"&TEAMNAME=="Newcastle U")
#   
# 
# # leaguePos
# 
# sort(names(standings))
# 
# leagueTable <- standings %>% 
#   group_by(team,season) %>% 
#   mutate(maxGames=max(tmYrGameOrder)) %>% 
#   filter(tmYrGameOrder==maxGames) %>% 
#   select(team,position)
# 
# b <-
#   ) a %>% 
#   select(-n) %>% 
#   group_by(TEAMNAME,season) %>% 
#   tally() %>% 
# # filter(season=="2015/16") %>% 
#   ungroup() %>% 
#   arrange(desc(n)) %>% 
#   rename(team=TEAMNAME) %>% 
#   inner_join(leagueTable) %>% 
#   filter(season=="2014/15") %>% 
#   plot_ly(x=n,y=position, mode = "markers",  hoverinfo = "text",
# text = paste(
#              team,
#              "<br>Squad Total: ",n,
#              "<br>Position: ",position)) %>%
#   layout(hovermode = "closest",
#         # title="paste0(pcPlayerGoalsData()$category," as % of Premier League Goals""),
#          xaxis=list(title="Matchday Squad Players"),
#          yaxis=list(title="League Position", autorange="reversed", zeroline=FALSE, range=list(1,20) # would be nice to set range
#          )
#   )
#   
# 
# 


# asv home defeat ---------------------------------------------------------
# 
# sort(names(standings))
# 
# standings %>% 
#   ungroup() %>% 
#   filter(venue=="H") #9236
# 
# standings %>% 
#   ungroup() %>% 
#   filter(venue=="H"&(GA-GF)>5)
# 
# ##
# 
# sort(names(playerGame))
# 
# test <-playerGame %>% 
#   filter(PLAYERID=="FOWLERR"&Gls>0)
# 
# 
# 
# # 2 players outcome together  ---------------------------------------------
# 
# sort(names(playerGame))
# 
# a <-playerGame %>% 
#   filter(PLAYERID=="FERNAN2"&START>0) %>% 
#   select(LASTNAME,TEAMMATCHID,season,MATCHID)
# 
# b <-playerGame %>% 
#   filter(PLAYERID=="FERNAN"&START>0) %>% 
#   select(LASTNAME,TEAMMATCHID,season) %>% 
#   inner_join(a, by=c("TEAMMATCHID")) %>% 
#   inner_join(standings) %>% 
#   select(team,res) %>% 
#   filter(team=="Man. City")
# 
# sort(names(standings))
# 
# 
# 
# # goals concede by time Norwich by 10 min spells --------------------------
# 
# sort(names(goalsFor))
# 
# 
# ## look at Norwich against
# 
# sort(unique(goalsFor$team))
# 
# 
# 
# glsAg <-goalsFor %>% 
#   filter(opponent=="Norwich C"&season=="2015/16") %>% 
#   select(TIME) %>% 
#   mutate(End="Ag")
# 
# glsFor <- goalsFor %>% 
#   filter(team=="Norwich C"&season=="2015/16") %>% 
#   select(TIME) %>% 
#   mutate(End="For")
# 
# gls <- rbind(glsAg,glsFor)
# 
# 
# ## need to do cut first
# 
# Z <- stats::rnorm(10000)
# table(cut(Z, breaks = -6:6))
# 
# 
# gls$band <-cut(gls$TIME,breaks=9,labels=c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90"))
# 
# tots <-gls %>% 
#   group_by(band,End) %>% 
#   tally() %>% 
#   mutate(count=ifelse(End=="Ag",-n,n))
# 
# tot
# 
# n1 <- ggplot(tots, aes(x=band,y=count,fill=End))+ 
#   geom_bar(data=subset(tots,End == "Ag"), stat = "identity") + 
#   geom_bar(data=subset(tots,End == "For"), stat = "identity") +
#   coord_flip() + 
#  
# 
# geom_bar(data = subset(year_data, SEX == "Female"), stat = "identity")
# 
# ## actually did a dimple 1
# 
#  plot_ly(x=test$TIME,type="histogram")  # need the x= cannot just put first
#   
#   
#   plot_ly(x=test$TIME,type="histogram",xbins=list(start=1,end=90,size=10))
#   
# ## ggplot
#   
#   # n1 <- ggplot(nigeria, aes(x = Age, y = Population, fill = Gender)) + 
#   #   geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
#   #   geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
#   #   scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
#   #                      labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
#   #   coord_flip() + 
#   #   scale_fill_brewer(palette = "Set1") + 
#   #   theme_bw()
#   # 
#   # n1
# 
#   
# 
# # english players per min and tot points ----------------------------------
# 
#   df <-playerGame %>% 
#     filter(PLAYERID==input$playerA) %>% 
#     group_by(season,PLAYERID,name) %>% 
#     select(Gls,Assists,mins) %>% 
#     summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
#     filter(Points!=0) %>% 
#     mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
#     ungroup()
#   
#   sort(names(playerGame))
#   
#  test <- playerGame %>% 
#     filter(COUNTRY=="England"&season=="2015/16")  %>% 
#     group_by(PLAYERID,name) %>% 
#     select(Gls,Assists,mins) %>% 
#     summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
#     filter(Points!=0) %>% 
#     mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
#     ungroup()
#  
#  
#  plot_ly(test, x = Points, y = Ppm, mode = "markers", hoverinfo = "text",
#          marker=list(size=Mins/10, sizemode="area"),
#          text = paste(name,
#            "<br>Goals: ",Goals,
#            "<br>Assists: ",Assists,
#            "<br>Points: ",Points,
#            "<br>Minutes: ",Mins
#          )) %>%
#    layout(hovermode = "closest",
#           title="Points (inc. secondary assists) per 90 mins by Season <br>
#           Zoom and Hover points for Details",
#           xaxis=list(title="Points in Season"),
#           yaxis=list(title="Points per 90 mins",rangemode="tozero"
#           )
#    )
#   
#  
#  
#  
#  
# # scotland decline --------------------------------------------------------
# 
#  test <- playerGame %>% 
#    filter(COUNTRY=="Scotland")  %>% 
#    group_by(season) %>% 
#    select(Gls,Assists,mins) %>% 
#    summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
#    filter(Points!=0) %>% 
#    mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
#    ungroup()
#  
#  
#  plot_ly(test, x = season, y = Points, type="bar", hoverinfo = "text",
#          marker=list(size=Mins/10, sizemode="area"),
#          text = paste(
#                       "<br>Goals: ",Goals,
#                       "<br>Assists: ",Assists,
#                       "<br>Points: ",Points,
#                       "<br>Minutes: ",Mins
#          )) %>%
#    layout(hovermode = "closest",
#           title="Points (inc. secondary assists) per 90 mins by Season <br>
#           Zoom and Hover points for Details",
#           xaxis=list(title="Points in Season"),
#           yaxis=list(title="Points per 90 mins",rangemode="tozero"
#           )
#    )
#  
#  # 2008/9 only 4 goals
#  
#  ## number of players
#  
#  ## could do article on it - 
#  
#  
#  
#  
#  
# # spurs simila points by year ---------------------------------------------
# 
# 
#  sort(names(standings))
#  
#  standings %>% 
#    filter(team=="Tottenham H"&tmYrGameOrder==28&season>="2011/12") %>% 
#    ungroup() %>% 
#    arrange(season) %>% 
#    plot_ly(x=season,y=cumPts) %>% 
#       layout(xaxis=list(title=" "),
#           yaxis=list(title="Points ",rangemode="tozero"),
#           title="Tottenham Hotspur - Points after 29 PL games")
#  
#  
#  
# final <- standings %>% 
#    filter(team=="Tottenham H"&tmYrGameOrder==38&season>="2011/12") %>% 
#    ungroup() %>% 
#    arrange(season) %>% 
#   select(season,finalPts=cumPts)
#  
#  
#  
# 
# ## looks good
#  standings %>% 
#   filter(team=="Tottenham H"&tmYrGameOrder==28&season>="2011/12") %>% 
#   ungroup() %>% 
#   arrange(season) %>% 
#   select(season,cumPts) %>% 
#   left_join(final)%>% 
#    plot_ly(x=season,y=cumPts,hoverinfo="text", showlegend=FALSE,
#            text=paste("29 games <br>",
#                       cumPts,' points')) %>% 
#    add_trace(x=season,y=finalPts, mode="markers",hoverinfo="text",showlegend=FALSE,
#              text=paste("38 games <br>",
#                         finalPts,' points')) %>% 
#    layout(xaxis=list(title=" "),
#           yaxis=list(title="Points ",rangemode="tozero"),
#           title="Tottenham Hotspur - Points after 29 PL games <br> Hover for details")
#  
#  
#  
# # when title was clinched -------------------------------------------------
# 
# 
# # goals per min - benteke -------------------------------------------------
# 
# sort(names(playerGame))
#  
#  
# temp <-playerGame %>% 
#   filter(season=="2015/16") %>% 
#   group_by(PLAYERID,name) %>% 
#   summarize(totGls=sum(Gls,na.rm=T),totMins=sum(mins,na.rm=T),mpg=round(totMins/totGls,1)) %>% 
#               ungroup() %>% 
#            arrange(mpg)   %>% 
#   filter(PLAYERID!="OWNGOAL"&totGls>6)
# 
# p <- temp  %>% 
#   head(10) %>% 
#   select(name,Goals=totGls,mins_per_goal=mpg) %>%
#                          DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# 
# ## testing bob rudis addin
# 
# subt <- 'This looks better'
# subt <- paste0(strwrap(subt, 100), sep="", collapse="\n")
# ggplot_with_subtitle(p, label = subt, fontfamily = "Helvetica", 
#     fontsize = "14", hjust = "0", vjust = "0", 
#     bottom_margin = "13", lineheight = "1")
#  <-ggplot(temp, aes(x=totGls, y=mpg)) +
#   geom_point()
# 
# 
# library(hrbrmisc)
# subst <- 'Another boring subtitle'
# subst <- paste0(strwrap(subt, 100), sep="", collapse="\n")
# ggplot_with_subtitle(p, label = subst, fontfamily = "Helvetica", 
#     fontsize = "80", hjust = "0", vjust = "0", color="red",
#     bottom_margin = "5.5", lineheight = "1")
# 
#  
# 
# 
# 
# # GD v final_pos ----------------------------------------------------------
# 
# sort(names(standings))
# 
# ## need to check everton standin(after 28 games) and extrapolate for GD to fu
# df <-standings %>%  
#   group_by(season,team) %>% 
#   filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
#   select(team,season,final_Pos,tmYrGameOrder,cumPts,cumGD,cumGF) %>% 
#   mutate(avPts=round(cumPts/tmYrGameOrder,2))
# 
# pastYears<- df %>% 
#   filter(season!="2015/16")
# 
# thisYear <- df %>% 
#   filter(season=="2015/16") %>% 
#   ungroup() %>% 
#   mutate(totGD=round(cumGD*38/tmYrGameOrder),totGF=round(cumGF*38/tmYrGameOrder)) %>% 
#   arrange(desc(avPts),desc(totGD),desc(totGF),team) %>% 
#   mutate(pos=row_number())
# 
# ## shpuld do order on ppg and GD as diff number of games played
# 
# 
# EVE <- list(
#   x = 16,
#   y = 1.36,
#   text = "Everton 2015/16",
#   xref = "x",
#   yref = "y",
#   showarrow = TRUE,
#   arrowhead = 4,
#   ax = 20,
#   ay = 20
# )
# 
# MNC <- list(
#   x = 1,
#   y = 1.08,
#   text = "Man City 2003/04",
#   xref = "x",
#   yref = "y",
#   showarrow = TRUE,
#   arrowhead = 4,
#   ax = 20,
#   ay = 20
# )
# 
# pastYears %>% 
#   plot_ly(x=cumGD,y=avPts,mode="markers", hoverinfo = "text", name="Past Yrs",
# text = paste(team,"<br>", season,"<br> Pos:",final_Pos,"<br> PPG:",avPts,"<br> GD:",cumGD)) %>%
#   add_trace(data=thisYear,x=totGD,y=avPts,mode="markers", name="2015/16",hoverinfo = "text", 
#             text = paste(team,"<br>", season,"<br> Pos:",pos,"<br> PPG:",avPts,"<br> GD:",totGD)) %>% 
# 
#   layout(hovermode = "closest",
#          xaxis=list(title="Goal Difference. 2015/16 extrapolated to 38 game season"),
#          yaxis=list(title="Average points per game "),
#          title="Comparison of Average Points per Game and Goal Diff in Premier League",
#          annotations = list(EVE,MNC)
#          )   %>% 
#  
#   config(displayModeBar = F,showLink = F)
#   
# ### looks odd - this is what is done in updating  title=" Hover bar for details", titlefont=list(size=16)
# 
# completeRounds <-standings %>%
#   filter(season=="2015/16") %>% 
#   group_by(season,team) %>%
#   mutate(finalGame=max(tmYrGameOrder)) %>%
#   ungroup() %>% 
#   arrange(finalGame) %>% 
#   slice(1) %>% 
#   select(finalGame)  ## 28
# 
# newStandings <-standings %>%
#   filter(season=="2015/16") %>% 
#   group_by(season) %>%
#   # mutate(finalGame==completeRounds$finalGame) %>%
#   filter(tmYrGameOrder==completeRounds$finalGame) %>%   
#   select(final_Pos = position,team) %>%
# #   left_join(standings)  #Joining by: c("season", "final_Pos", "team") so has table as at game 28 currently
# # although 29 have been played and west brom overtook watford in that round
#   
#   
#   
#   
#   
# # obsession with win ratio ------------------------------------------------
# 
# sort(names(standings))
# 
# W <- standings %>% 
#   group_by(season,team) %>% 
#   filter(res=="Win") %>% 
#   select(res) %>% 
#   tally() %>% 
#   rename(W=n)
# 
# D <- standings %>% 
#   group_by(season,team) %>% 
#   filter(res=="Draw") %>% 
#   select(res) %>% 
#   tally() %>% 
#   rename(D=n)
# 
# 
# L <- standings %>% 
#   group_by(season,team) %>% 
#   filter(res=="Loss") %>% 
#   select(res) %>% 
#   tally() %>% 
#   rename(L=n)
# 
# Res <- W %>% 
#   inner_join(D) %>% 
#   inner_join(L) %>% 
#   mutate(ppg=(3*W+D)/(W+D+L),winPC=round(100*W/(W+D+L),1)) %>% 
#   ungroup() %>% 
#   arrange(desc(ppg)) %>% 
#   group_by(season) %>% 
#   mutate(Pos=row_number()) %>% 
#   ungroup() %>% 
#   arrange(desc(winPC)) %>% 
#   group_by(season) %>% 
#   mutate(WpcRank=row_number()) %>% 
#   filter(WpcRank!=Pos) 
#   
# 
# ## 1-0 victories by season
# 
# sort(names(standings))
# 
# oneNils <-standings %>% 
#   group_by(season,team) %>% 
#   filter(GF==1&GA==0) %>% 
#   tally() %>% 
#   inner_join(standings) %>% 
#   select(season,team,n,final_Pos) %>% 
#   unique(.) %>% 
#   plot_ly(x=jitter(n),y=final_Pos, mode="markers", hoverinfo = "text",
#                     text = paste("Count:",n,"<br> Team:",team,"<br> Pos:",final_Pos,"<br> Season:",season))
#           )
# 
# 
# # agbonlahor scoring rate -------------------------------------------------
# 
# 
# topScorers <-  playerGame %>% 
#   group_by(PLAYERID,name) %>% 
#   summarize(Goals=sum(Gls)) %>% 
#   filter(Goals>=74) %>% 
#   select(PLAYERID) %>% 
#   unlist(use.names = FALSE)
# 
# sort(names(playerGame))
# 
# 
# 
# p <-playerGame %>% 
#   filter(PLAYERID %in% topScorers&PLAYERID!="OWNGOAL") %>% 
#   group_by(PLAYERID,name) %>% 
#   summarize(Goals=sum(Gls),Mins=sum(mins),mpg=round(Mins/Goals,1)) %>% 
#   ungroup() 
# 
# 
# n <- p[p$name=="Gabriel Agbonlahor", ]
# b <- list(
#   x = n$Goals,
#   y = n$mpg,
#   text = n$name,
#   xref = "x",
#   yref = "y",
#   showarrow = TRUE,
#   arrowhead = 7,
#   ax = 20,
#   ay = -40
# )
# 
#  p %>% 
#   plot_ly(y=mpg,x=Goals,mode="markers"#,
#           #hoverinfo = "text",
#           #text=paste0(name,"<br>Goals:",Goals,"<br>MPG:",mpg)
#           ) %>% 
#   layout(hovermode = "closest",
#                                   title="Minutes per Goal",
#                                   xaxis=list(title="Total Goals"),
#                                   yaxis=list(title="Minutes per Goal",autorange="reversed",
#                                              annotations=b
#                                   )
#           )
#  
#  
#  m <- mtcars[which.max(mtcars$mpg), ]
#  
#  b <- list(
#    x = m$wt,
#    y = m$mpg,
#    text = rownames(m),
#    xref = "x",
#    yref = "y",
#    showarrow = TRUE,
#    arrowhead = 7,
#    ax = 20,
#    ay = -40
#  )
#  
#  plot_ly(mtcars, x = wt, y = mpg, mode = "markers") %>%
#    layout(annotations = a)
# ### excl pens
# 
# 
# playerGame %>% 
#   filter(PLAYERID %in% topScorers&PLAYERID!="OWNGOAL") %>% 
#   group_by(PLAYERID,name) %>% 
#   summarize(Goals=sum(Gls)-sum(PENS),Mins=sum(mins),mpg=round(Mins/Goals,1)) %>% 
#   ungroup() %>% 
#   plot_ly(y=mpg,x=Goals,mode="markers",
#           hoverinfo = "text",
#           text=paste0(name,"<br>Goals:",Goals,"<br>MPG:",mpg)) %>% 
#   layout(hovermode = "closest",
#          title="Minutes per Goal (exc pens)",
#          xaxis=list(title="Total Goals (exc pens)"),
#          yaxis=list(title="Minutes per Goal",autorange="reversed"
#          )
#   )
# 
# 
# ## goalkeeper conceding
# 
# names(playerGame)
# names(matchTeam)
# 
# unique(playerGame$POSITION)
# 
# bothTeams <- playerGame %>% 
#   filter(POSITION=="Goalkeeper"&START>0) %>% 
#   select(name,PLAYERID,gameDate,MATCHID) %>% 
#   left_join(matchTeam)
# 
# ownteam <-playerGame %>% 
#   filter(POSITION=="Goalkeeper"&START>0) %>% 
#   select(name,PLAYERID,gameDate,MATCHID,TEAMMATCHID) %>% 
#   left_join(matchTeam)
# 
# oppTeam <- bothTeams %>% 
#   anti_join(ownteam)
# 
# oppTeam %>% 
#   filter(PLAYERID=="GIVENS") %>% 
#   arrange(desc(gameDate)) %>% 
#   plot_ly(x=gameDate,y=GOALS,mode="markers")
#   
# 
# 
# # compare man u young guns ------------------------------------------------
# 
# Januzai / memphis comparison
# 
# 
# 
# 
# # youngest team - what happened next season -------------------------------
# 
# 
# sort(names(playerGame))
# 
# avAge <-playerGame %>% 
#   filter(START>0) %>% 
#   group_by(TEAMNAME,season) %>% 
#   summarize(avAge=mean(age,na.rm=T)) %>% 
#   rename(team=TEAMNAME) %>% 
#   ungroup()
# 
# avAge$avAge <- round(as.numeric(avAge$avAge),2)
# 
# ## check
# check <- avAge %>% 
#   filter(season=="2015/16") %>% 
#   
#   arrange(avAge)
# 
# sort(names(standings))
# 
# leaguePos <- standings %>% 
#   select(season,team,final_Pos) %>% 
#   unique()
# 
# df <- avAge %>% 
#   inner_join(leaguePos)
# 
# dfPast <- df %>% 
#   filter(season<"2015/16")
# 
# dfCurrent <- df %>% 
#   filter(season=="2015/16")
# 
# dfPast %>% 
#   plot_ly(x=avAge,y=final_Pos,mode="markers",
#           hoverInfo="text",
#         
#          text=paste0(team,"<br>", season,
#                      "<br>Av Age: ", avAge,
#                      "<br>Position", final_Pos)) %>% 
#   add_trace(data=dfCurrent,x=avAge,y=final_Pos,mode="markers",
#             color="red",
#             
#             hoverInfo="text",
#          
#             text=paste0(team,"<br>", season,
#                         "<br>Av Age: ", avAge,
#                         "<br>Position: ", final_Pos)) %>% 
#   layout(hovermode = "closest",showlegend = FALSE,
#          xaxis=list(title="Average Age Starters"),
#          yaxis=list(title="Final League position",autorange="reversed",rangemode="nonnegative",
#         title="League position by Starters Average Age - Hover points for details"            
#          )
#   )
# 
# ```{r YT, echo=TRUE}
# 
# # create an average age of staters and link to
# avAge <-playerGame %>% 
#   filter(START>0) %>% 
#   group_by(TEAMNAME,season) %>% 
#   summarize(avAge=mean(age,na.rm=T)) %>% 
#   rename(team=TEAMNAME) %>% 
#   ungroup()
# 
# # avAge is initially a datediff field
# avAge$avAge <- round(as.numeric(avAge$avAge),2)
# 
# leaguePos <- standings %>% 
#   select(season,team,final_Pos) %>% 
#   unique()
# 
# df <- avAge %>% 
#   inner_join(leaguePos)
# 
# 
# ## separate current season for highlighting
# dfPast <- df %>% 
#   filter(season<"2015/16")
# 
# dfCurrent <- df %>% 
#   filter(season=="2015/16")
# 
# dfPast %>% 
#   plot_ly(x=avAge,y=final_Pos,mode="markers",
#           hoverInfo="text",
#           
#           text=paste0(team,"<br>", season,
#                       "<br>Av Age: ", avAge,
#                       "<br>Position: ", final_Pos)) %>% 
#   add_trace(data=dfCurrent,x=avAge,y=final_Pos,mode="markers",
#             color="red",
#             
#             hoverInfo="text",
#             
#             text=paste0(team,"<br>", season,
#                         "<br>Av Age: ", avAge,
#                         "<br>Position: ", final_Pos)) %>% 
#   layout(hovermode = "closest",showlegend = FALSE,
#          xaxis=list(title="Average Age Starters"),
#          yaxis=list(title="Final League position",autorange="reversed",rangemode="nonnegative"),
#                     title="League position by Starters Average Age - Hover points for details"            
#          )
#  
# 
# ```
# 
# # ## # Use order_by if data not already ordered
# # df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
# # scrambled <- df[sample(nrow(df)), ]
# # 
# # wrong <- mutate(scrambled, prev = lag(value))
# # arrange(wrong, year)
# # 
# # right <- mutate(scrambled, prev = lag(value, order_by = year))
# # arrange(right, year)
# # 
# # ## just look at Arsenal
# # 
# # arse <- df %>% 
# #   filter(team=="Arsenal")
# # 
# # right <- mutate(arse, following = lead(final_Pos))
# 
# nextYear <- df %>% 
#   group_by(team) %>% 
#   mutate(following = lead(final_Pos)) %>% 
#   filter(season!="2015/16") %>% 
#   ungroup()
# 
# glimpse(right)
# 
# right$avAge <- as.numeric(right$avAge)
# 
# youngestTeam <-df %>% 
#   ungroup() %>% 
#   arrange(avAge) %>% 
#   group_by(season) %>% 
#   slice(1) %>%
#   ungroup()
# 
# #youngestTeam$avAge <- as.numeric(youngestTeam$avAge)
# 
# 
# change <-youngestTeam %>% 
#   left_join(nextYear) %>% 
#   mutate(change=final_Pos-following,avAge=round(avAge,1))  %>%
#                          DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
# 
# df %>% 
#   filter(season=="1995/96") %>% 
#   ungroup() %>% 
#   arrange(avAge)
# 
# mean(change$change,na.rm=T) # -0.2272727
# 
# 
#   plot_ly(x=final_Pos,y=following, mode="markers")
#   
#   
#   
#   
# # lei v spurs during season -----------------------------------------------
# 
#   sort(names(standings))
# standings %>% 
#     filter((team=="Leicester C"|team=="Tottenham H")&season=="2015/16") %>% 
#     plot_ly(x=tmYrGameOrder,y=cumPts,mode="markers+lines",color=team,
#             hoverinfo="text",
#             text=paste0("Games: ",tmYrGameOrder,"<br>Points: ",cumPts,"<br> v ",OppTeam," ",GF,"-",GA)) %>% 
#                         
#   layout(
#                     xaxis=list(title="Games Played"),
#                     yaxis=list(title="Cumulative Points"),
#          title="Leicester and Spurs cumulative points by game"
#                     )
# 
# 
# 
# # tot cards ---------------------------------------------------------------
# 
# sort(names(playerGame))
# 
# sort(unique(playerGame$TEAMNAME))
# 
# theTeam <- "Crystal P"
# 
# cardGames <-playerGame %>% 
#   filter(TEAMNAME==theTeam&season=="2015/16"&CARD>"a") %>% 
#   group_by(TEAMMATCHID) %>% 
#   tally()  
# 
# 
# sort(names(standings))
# gamesPlayed <- standings %>% 
#      filter(team==theTeam&season=="2015/16") %>% 
#     .$tmYrGameOrder %>% 
#   max()
#   
# 
# str(gamesPlayed) #int 36
# 
# gamesPlayed-28
# 
# zero <- data.frame(count=0,n=gamesPlayed-nrow(cardGames))
# 
# 
# df <- cardGames %>% 
#   rename(count=n) %>% 
#   group_by(count) %>% 
#   tally()
# 
# rbind(zero,df) %>% 
#   plot_ly(x=count,y=n,type="bar", color="orange",
#           hoverinfo = "text",
#           text = paste("Cards:",count,"<br> Games:",n)) %>% 
#   layout(hovermode = "closest",
#      xaxis=list(title="Card Count"),
#      yaxis=list(title="Number of Games"),
#      title="Team's Card count by Game by Season"            
#                                                          )
#  
# theTeam <- "Tottenham H"
# theSeason <- "2008/09"
# 
# unique(standings$season)
# 
# gamesPlayed <- standings %>% 
#   filter(team==theTeam&season==theSeason) %>% 
#   .$tmYrGameOrder %>% 
#   max()
#   
#   
# 
# # Most half time subs -----------------------------------------------------
# 
# sort(names(playerGame))
# 
# glimpse(playerGame)
# 
# playerGame %>% 
#   group_by(TEAMNAME,season) %>% 
#   filter(subOn==45|subOn==46) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) %>% 
#   filter(TEAMNAME=="Newcastle U")  
# 
# ## could link to manager
# 

# gayle v berahino --------------------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(season=="2015/16"&PLAYERID!="OWNGOAL") %>% 
 group_by(PLAYERID,name) %>% 
  summarize(totAss=sum(Assists,na.rm=T),totGls=sum(Gls,na.rm=T))
  mutate(pp90mins=(Assists+Gls)*90/mins) %>% 
  arrange(desc(pp90mins)) %>% 
  select(name,Gls,Assists,pp90mins)

# manu u goal diff --------------------------------------------------------


sort(names(standings))


standings %>% 
  filter(final_Pos<6) %>% 
  arrange(desc(tmYrGameOrder)) %>% 
  group_by(team,season) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  plot_ly(x=season,y=cumGD,mode="markers+lines",
          color=as.factor(final_Pos),
          hoverinfo = "text",
                  text = paste(team,"<br> GD:",cumGD,"<br>",season)) %>% 
    layout(hovermode = "closest",
       xaxis=list(title=" "),
       yaxis=list(title="Goal Difference"),
       title="Top 5 PL - season Goals Difference",
       margin=list(b=100)) %>% 
         config(displayModeBar = F,showLink = F)
                                                        

# arsenal better than spurs for points gd ---------------------------------

sort(names(standings))

test <- standings %>% 
  ungroup() %>% 
  filter(team=="Arsenal"|team=="Tottenham H") %>% 
  arrange(desc(tmYrGameOrder)) %>% 
  group_by(season,team) %>% 
    slice(1)


#gather(s, Sex, value, Women, Men) %>%
m <- test[test$season=="1994/95"&test$team=="Tottenham H", ]


a <- list(
  x = m$cumGD,
  y = m$season,
  text = "#COYS",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 1,
  ax = 80,
  ay = 0,
  font=list(color="1d00ff", size=20)
)

## adjustment as done up to 37 games
test[test$season=="2015/16"&test$team=="Arsenal",]$final_Pos <- 2
test[test$season=="2015/16"&test$team=="Tottenham H",]$final_Pos <- 3

test %>% 
  plot_ly(x = jitter(cumGD), y = season, mode = "markers", 
          color = team, colors = c("red", "black"),
          hoverinfo="text",
          text=paste0(team,
                      "<br>GD:", cumGD,
                      "<br>Pos:", final_Pos)) %>%
  add_trace(x = cumGD, y = season, mode = "lines",
            group = season, showlegend = F, line = list(color = "gray"),
  hoverinfo="text",
  text=paste0(team,
              "<br>GD:", cumGD,
              "<br>Pos:", final_Pos)) %>%
  layout(annotations = a,
    hovermode = "closest",
    title = "End of season Goals Difference<br>Hover for details",
    xaxis = list(title = " "),
    yaxis = list(title = " "),
    margin = list(l = 65, title=" ")
  )


## need to sort out final pos

## think just needs restatr session each time to make sure dplyr is last loaded


library(tidyr)
library(plotly)
s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
s <- s[order(s$Men), ]
gather(s, Sex, value, Women, Men) %>%
  plot_ly(x = value, y = School, mode = "markers",
          color = Sex, colors = c("pink", "blue")) %>%
  add_trace(x = value, y = School, mode = "lines",
            group = School, showlegend = F, line = list(color = "gray")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 65)
  )



# most players used in PL season ------------------------------------------

sort(names(playerGame))

test <- playerGame %>% 
  ungroup() %>% 
  filter(mins>0) %>% 
  select(PLAYERID,TEAMNAME,season) %>% 
  unique() %>% 
  group_by(season,TEAMNAME) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(n) %>% 
  filter(season=="2015/16")%>%
  select(Team=TEAMNAME,Players=n) %>% 
   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# where would team have finished in other seasons -------------------------

sort(names(standings))

finalStandings <-
  standings %>% 
  ungroup() %>% 
  arrange(desc(tmYrGameOrder)) %>% ## act doesnt matter as all games have final_pos in
  group_by(season,team) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(season,final_Pos,team,cumPts,cumGD,cumGF)

## 

theTeam <- "Tottenham H"
theSeason <- "2015/16"

choice <- finalStandings %>% 
  filter(team==theTeam&season==theSeason) 
choice$season <- "extra"

# ## works for one season
# finalStandings %>% 
#   filter(season=="2014/15") %>% 
#   rbind(choice) %>% 
#   arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
#   mutate(position=row_number()) %>% 
#   ungroup() %>% 
#   arrange(position)
# 
# 
# finalStandings %>% 
#   group_by(season) %>% 
#   rbind(choice) %>% 
#   arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
#   mutate(position=row_number()) %>% 
#   ungroup() %>% 
#   arrange(position)

# prob need to do loop

# just capture 38 game seasons
seasons <- unique(finalStandings$season)
seasons <-
  seasons[4:24] 

# i <- 1
# for (i in seq_along(seasons)) {
#   
#  tempdf <- finalStandings %>% 
#     filter(season==seasons[i]) %>% 
#     rbind(choice) %>% 
#     arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
#     mutate(position=row_number()) %>% 
#     ungroup() 
#  tempdf$trueSeason <- seasons[i]
#  if(i!=1) {
#    df <- rbind(df,tempdf)
#  } else {
#    df <- tempdf
#  }
# 
# }

## problem with above is that it leaves team in 2x
for (i in seq_along(seasons)) {
  
  tempdf <- finalStandings %>% 
    filter(season==seasons[i]&team!=theTeam) %>% 
    rbind(choice) %>% 
    arrange(desc(cumPts),desc(cumGD),desc(cumGF)) %>% 
    mutate(position=row_number()) %>% 
    ungroup() 
  tempdf$trueSeason <- seasons[i]
  if(i!=1) {
    df <- rbind(df,tempdf)
  } else {
    df <- tempdf
  }
  
}


# df %>% 
#   filter(season=="extra") %>% 
#   plot_ly(x=trueSeason,y=position,mode="markers") %>%
#   layout(
#  #   title = " ",
#  #  xaxis = list(title = " "),
#     yaxis = list(title="Position",
#                  range=list(20,1),dtick=1)
#   )
  

df %>% 
  filter(season=="extra") %>% 
  plot_ly(x=trueSeason,y=position,mode="markers") %>%
  layout(
       title = " ",
      xaxis = list(title = " "),
    yaxis = list(title="Position",
                 range=list(20,0.8),dtick=1) # the 0.8 ensures that all the point gets shown
  )


# opta - 1st of all nationalities to score prem hat-trick -----------------

sort(names(playerGame))

playerGame %>% 
  ungroup() %>% 
  arrange(gameDate) %>% 
  filter(Gls>=3&PLAYERID!="OWNGOAL") %>% 
  group_by(COUNTRY) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(player=LASTNAME,country=COUNTRY,date=gameDate,team=TEAMNAME,Opposition=Opponents) %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# another look at appeance by age -----------------------------------------

sort(names(playerGame))

english <-playerGame  %>% 
  filter(mins>0) %>% 
  select(name,PLAYERID,COUNTRY,born) %>% 
  filter(COUNTRY=="England") %>% 
  unique()

glimpse(english)
library(lubridate)

english %>% 
  mutate(month=month(born,label = TRUE)) %>% 
  group_by(month) %>% 
  tally() %>% 
  filter(!is.na(n)) %>% 
  plot_ly(x=month, y=n, type="bar") %>% 
  layout(title="Number of English Born Premier League Players by Birth Month",
         xaxis=list(title=""),
        yaxis=list(title="Count"))


# super sub ---------------------------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(PLAYERID=="VARDYJ"&subOn>0) %>% 
  summarize(pl=n(),mins=sum(mins),gls=sum(Gls))
  

# most games by top players -----------------------------------------------

#Schmeichel, Vardy, Mahrez, Kante, Wes Morgan and Robert Huth started 218 of a possible 228 Premier League games


sort(names(playerGame))

playerGame %>% 
  filter(season >"1994/95"&st>0) %>% 
  group_by(season,TEAMNAME,PLAYERID) %>% 
  summarise(starts=n()) %>% 
  arrange(desc(starts)) %>% 
  group_by(season,TEAMNAME) %>% 
  slice(1:6) %>% 
  summarize(totStarts=sum(starts)) %>% 
  arrange(desc(totStarts))



# berahino ----------------------------------------------------------------

sort(names(playerGame))
sort(names(summary))

summary %>% 
  ungroup() %>% 
  filter(season=="2015/16"&PLAYERID!="OWNGOAL"&mins>0) %>% 
 # group_by(PLAYERID,name) %>% 
  select(PLAYERID,subGls,StGls,Assists,mins,name) %>% 
  mutate(Goals=subGls+StGls,pp90mins=round((Goals+Assists)*90/mins,2)) %>% 
  arrange(desc(pp90mins)) %>% 
  select(name,Goals,Assists,mins,pp90mins) %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=TRUE,options= list(paging = TRUE, pageLength = 16, searching = FALSE,info=FALSE))

 

# all prem scorers by season ----------------------------------------------

sort(names(playerGame))
playerGame %>% 
  filter(season=="2015/16"&Gls>0) %>% 
  select(name,Gls,PLAYERID) %>% 
  group_by(PLAYERID,name) %>% 
  summarize(Goals=sum(Gls)) %>% 
  arrange(desc(Goals),PLAYERID)%>%
  ungroup() %>% 
  # select(name,Goals) %>% 
  #                        DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
  group_by(Goals) %>% 
    tally()


# CONCEDING 4 GOALS AT HOEM 1ST GAME --------------------------------------

sort(names(standings))

standings %>% 
  filter(tmYrGameOrder==1&GA>3) %>% 
  group_by(team) %>% 
  tally() %>% 
  arrange(desc(n))

standings %>% 
  filter(tmYrGameOrder==1&GA>3&team=="QPR") # 3
 
standings %>% 
  filter(tmYrGameOrder==1&GA>3&venue=="H") %>% 
  group_by(team) %>% 
  tally() %>% 
  arrange(desc(n)) 


#  leading scorer through any round ---------------------------------------

sort(names(playerGame))

## lets lookm last year issue combing date with team game order

yr <-"2014/15"

title <- paste0("Leading Goalscorers by club by PL round - ",yr,"<br>(jittered to show all values)")

temp <-playerGame %>% 
  ungroup() %>% 
  filter(season==yr) %>% 
  arrange(gameDate) %>% 
  select(PLAYERID,TEAMNAME,Gls,gameDate) %>% 
  ungroup() %>% 
  group_by(PLAYERID,TEAMNAME) %>% 
  data.frame() %>% 
  mutate(totGls=cumsum(Gls)) %>% 
  arrange(desc(totGls)) %>% 
  left_join(teamGames) %>% 
  select(PLAYERID,TEAMNAME,tmYrGameOrder,gameDate,totGls) ## cannot get rid of the cumulative issue

test <-temp %>% 
  group_by(tmYrGameOrder) %>% 
  mutate(leader=max(totGls)) %>% 
  filter(leader==totGls) %>% 
  arrange(desc(tmYrGameOrder),desc(totGls))#48 ## leader has lots of attributes

test %>% 
  plot_ly(x=jitter(tmYrGameOrder),y=jitter(totGls),mode="markers",color=name,colors="Set1",
          hoverinfo=text,marker=list(size=8),
          text=paste0(name,"<br> Goals:",totGls,"<br> Round:",tmYrGameOrder)) %>% 
  layout(hovermode = "closest",title=title,
        xaxis=list(title="Round (NB player may not have played in game)"),
        yaxis=list(title="Cumulative Goals"))

test %>% 
  filter(PLAYERID=="AGUEROS")

## problem weher aguero leads and is then missing need full
## look at jitter issue

library(plotly)
library(dplyr)
df <- tibble(x=c(2,3,4,4),y=c(1,2,4,4),z=c(1,2,1,2))

df %>% 
  plot_ly(x=x,y=y,mode="markers",color=z,clors=c("green","blue"))
  


  

sort(names(teamGames))



# points per minutr best by season ----------------------------------------

sort(names(playerGame))

test <-playerGame %>% 
  ungroup() %>% 
  filter(season=="2015/16") %>% 
  group_by(name,PLAYERID) %>% 
  summarize(totGls=sum(Gls),totAss=sum(Assists),points=(totGls+totAss),totMins=sum(mins),ppm=round(90*points/totMins,2)) %>% 
  filter(points>12&PLAYERID!="OWNGOAL") %>% 
  arrange(desc(ppm)) %>% 
  head(25) %>% 
    select(Player=name,Goals=totGls,Assists=totAss,per_90mins=ppm) %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=TRUE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  

# wins in 22 palace -------------------------------------------------------

allteams <- standings %>% 
  ungroup() %>% 
  group_by(team) %>% 
  
  arrange(gameDate) %>% 
  mutate(rn=row_number()) %>% 
  select(team,rn,res,season,gameDate,points,tmYrGameOrder)

tm <- allteams %>% 
  filter(team=="Crystal P")

# run <- integer()
# endDate=as.Date(character(())
# season <- character()
# points <- integer()
# 
# 
# # df <- data.frame(endDate=as.Date(character()),
# #                  season=character(), 
# #                  run=integer(), 
# #                  stringsAsFactors=FALSE) 
# # 
# # df <- tibble(endDate=as.Date(character()),
# #              season=character(), 
# #              run=integer())

endDate=as.Date(character())
season=character()
run=integer()
points = integer()
round= integer()

# tm %>% 
#   filter(between(rn,i,i+22)) %>% 
#   summarize(tot=sum(points)) %>% 
#   .$tot


#l <-  list(run=integer(),endDate=as.Date(character(()),season=character())
#i <- 1
  for(i in (1:nrow(tm)-23)) {
    run[i] <- tm %>% 
      filter(between(rn,i,i+23)&res=="Win") %>% 
      nrow()
    points[i] <- tm %>% 
      filter(between(rn,i,i+23)) %>% 
      summarize(tot=sum(points)) %>% 
      .$tot
    endDate[i] <- tm$gameDate[i+23]
    season[i] <- tm$season[i+23]
    round[i] <- tm$tmYrGameOrder[i+23]
  }

df <- tibble(run=run,endDate=endDate,season=season,points=points,round=round)

latest <- tail(df,1)

## these both work
  plot_ly(data=df,x=~run, type="histogram", # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>%   # reduce width - might need to do bars or change layout?
  add_histogram(data=latest,x=~run, type="histogram",marker=list(color="red"),name="Current") %>%
  layout(barmode="overlay",
         title="Wins by Crystal Palace in every 24 game PL sequence",
         yaxis=list(title="Count"),
         xaxis=list(title="Wins"))

  
  
  plot_ly(data=df,x=~points, type="histogram", # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>%   # reduce width - might need to do bars or change layout?
    add_histogram(data=latest,x=~points, type="histogram",marker=list(color="red"),name="Current") %>%
    layout(barmode="overlay")

  
  ## alternative - more like ggplot and adding histograms
  plot_ly() %>% 
    add_histogram(data=df,x=~points, , # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>%   # reduce width - might need to do bars or change layout?
    add_histogram(data=latest,x=~points, type="histogram",marker=list(color="red"),name="Current") %>%
    layout(barmode="overlay")
  
  ## works
  plot_ly() %>% 
        add_lines(data=df,y=~points,x=~round, color=~season)
  
  
  
  
  
# seasosn with theese three teams in club in top tier---------------------------------

  sort(names(standings))
  
  sort(unique(standings$team))
  
yrs <- sort(unique(standings$season))
yrs <- tibble(season=sort(unique(standings$season)))
tms <- c("Middlesbro","Newcastle U","Sunderland")

temp <- standings %>% 
  ungroup() %>% 
  select(season,team) %>% 
  unique() %>% 
  filter(team %in% tms) %>% 
  group_by(season) %>% 
  tally() %>% 
  right_join(yrs)

  temp %>% 
  plot_ly() %>% 
  add_bars(x =~season, y=~n)
  
  ## now have colors there
  
  test <- standings %>% 
    ungroup() %>% 
    select(season,team) %>% 
    unique() %>% 
    filter(team %in% tms) %>% 
    mutate(value=1)
  
  test %>% 
    filter(team=="Sunderland") %>% 
  plot_ly() %>% 
    add_bars(x =~season, y= ~value) # works showing all seasons - could do a gris earlier (and prob should)
  
  plot_ly() %>% 
    add_bars(data=subset(test,team=="Sunderland",x =~season, y= ~value)
  
             test %>% 
            #   filter(team=="Sunderland") %>% 
               plot_ly() %>% 
               add_bars(x =~season, y= ~value, color= ~team) %>% 
               layout(barmode="stack",
                    xaxis=list(title=""),
                    yaxis=list(title="",range = c(0, 3),tickmode="linear")
                    )
             
             ## now look at top 10 finishes
             
             test <- standings %>% 
               ungroup() %>% 
               select(season,team,final_Pos) %>% 
               unique() %>% 
               filter(team %in% tms&final_Pos<11) %>% 
               mutate(value=1)
             
             test %>% 
               right_join(yrs) %>% 
               mutate(team=ifelse(is.na(team),"None",team),value=ifelse(is.na(value),0.066,value)) %>% 
               filter(season<"2016/17") %>% 
               plot_ly() %>% 
               add_bars(x =~season, y= ~value, color= ~team) %>% 
               layout(barmode="stack",
                      title="NE clubs in Top Ten Premier League",
                      xaxis=list(title=""),
                      yaxis=list(title="",tickmode="linear")
               )
             
             ## look using eng soccer data
             
             library(engsoccerdata)
             
             df <- england
             sort(names(df))
             
             sort(names())
             sort(unique(df$home))
             
             tms <- c("Middlesbrough","Newcastle United","Sunderland")
             
             yrs <- sort(unique(df$Season))
             yrs <- tibble(season=sort(unique(df$Season)))
             
             glimpse(df)
             
             test <- df %>% 
               select(season=Season,team=home,tier) %>% 
               unique() %>% 
               filter(team %in% tms&tier==1) %>% 
               mutate(value=1,team=as.character(team))
             
             glimpse(test)
             
             test %>% 
               right_join(yrs) %>% 
               mutate(team=ifelse(is.na(team),"None",team),value=ifelse(is.na(value),0.1,value)) %>% 
               plot_ly() %>% 
               add_bars(x =~season, y= ~value, color= ~team) %>% 
               layout(barmode="stack",
                      xaxis=list(title=""),
                      yaxis=list(title="",tickmode="linear")
               )
             
             
             
             
# sunburstr example ? player names ----------------------------------------

library(sunburstR)
             library(stringr)
             
    sort(names(allPlayers))  
    
    allPlayers$LASTNAME
    
    ## look at with 4 letters at least
    
  df <-  allPlayers %>%  #4441
      filter(nchar(LASTNAME)>3) %>% #4356
  mutate(start=str_sub(LASTNAME,1,4)) 
  
  i <- 1
     for (i in 1:nrow(df)) {
    seqs[i]=paste(str_sub(df$start[i],1,1),str_sub(df$start[i],2,2),str_sub(df$start[i],3,3),str_sub(df$start[i],4,4),sep="-")
     }
  
  df$seqs <- seqs 
  
  
 summary <- df %>% 
    group_by(seqs) %>% 
    tally()
  
 
 
  sunburst(summary,count=TRUE)
    
    
  
  
# players appearing all minutes of season ---------------------------------

sort(names(playerGame)) 
  
  playerGame %>% 
    filter(season=="2016/17") %>% 
    group_by(PLAYERID,name,TEAMNAME) %>% 
    summarize(totMins=sum(mins)) %>% 
    filter(totMins==270) %>% 
    group_by(TEAMNAME) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    rename(players=n,team=TEAMNAME)%>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE),width=200)
    
  
  
  
# different nationalities -------------------------------------------------

## OptaJoe  Watford have used 21 different players in 3 PL matches so far this season, with 17 different nationalities
  #  represented
  
  sort(names(playerGame))
  
  playerGame %>% 
    filter(season=="2016/17") %>% 
    group_by(TEAMNAME) %>% 
    select(COUNTRY) %>% 
    unique(.) %>% 
    tally() %>% 
    arrange(desc(n))
  
  # TEAMNAME     n
  # <chr> <int>
  #   1      Watford    16
  # 2      Stoke C    14
  # 3   Sunderland    13
  # 4   West Ham U    13
  # 5      Everton    12
  # 6  Leicester C    12
  # 7  Southampton    12
  # 8      Arsenal    11
  # 9    Crystal P    11
  # 10   Liverpool    11
  # 11     Swansea    11
  # 12      Hull C    10
  # 13   Man. City    10
  # 14   Man. Utd.    10
  # 15   West Brom    10
  # 16     Chelsea     9
  # 17  Middlesbro     9
  # 18 Tottenham H     9
  # 19 Bournemouth     8
  # 20     Burnley     7
 ## so one different
  
  playerGame %>% 
    filter(season=="2016/17"&mins>0) %>% 
    group_by(TEAMNAME) %>% 
    select(COUNTRY) %>% 
    unique(.) %>% 
    tally() %>% 
    arrange(desc(n))  #Watford down to 15
  
  
  ## look by every season
  
  library(plotly)
  
  playerGame %>% 
    filter(mins>0) %>% 
    group_by(TEAMNAME,season) %>% 
    select(COUNTRY) %>% 
    unique(.) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    plot_ly() %>% 
    add_boxplot(x=~season,y=~n)
  
  
 test <- playerGame %>% 
    filter(season=="1992/93"&mins>0) %>% 
    group_by(TEAMNAME) %>% 
    select(COUNTRY) %>% 
    unique(.) %>% 
    tally() %>% 
    arrange(desc(n))
 
 test <- playerGame %>% 
   filter(season=="1992/93"&mins>0&TEAMNAME=="Sheff. Utd.") %>% 
   group_by(TEAMNAME) %>% 
   select(name,COUNTRY) %>% 
   unique(.) 
 
 
 
 
# wes brown seasons for 1 club --------------------------------------------

seasons <-playerGame %>% 
   filter(mins>0) %>% 
   select(name,PLAYERID,TEAMNAME,season) %>% 
   unique() %>% 
   group_by(TEAMNAME,name,PLAYERID) %>% 
   tally() %>% 
   filter(TEAMNAME=="Man. Utd.") %>% 
   arrange(desc(n))
   
 mins <-playerGame %>% 
   filter(mins>0) %>% 
   select(name,PLAYERID,TEAMNAME,mins) %>% 
   group_by(TEAMNAME,name,PLAYERID) %>% 
   summarize(totmins=sum(mins)) %>% 
   filter(TEAMNAME=="Man. Utd.") %>% 
   arrange(desc(totmins)) %>% 
  
   left_join(seasons) %>% 
   ungroup() %>% 
   select(player=name,seasons=n,mins=totmins) %>% 
   head(15) %>% 
                         DT::datatable(width=250,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
 
 
 
 
 
# clean shetss down - goals up --------------------------------------------

sort(names(standings))
 
 cs <-standings %>% 
   filter(tmYrGameOrder<7&GA==0) %>% 
   group_by(season) %>% 
   tally() 
 
 %>% 
   plot_ly() %>% 
   add_lines(x=~season,y=~n)
 
gls <- standings %>% 
  ungroup() %>% 
   filter(tmYrGameOrder<7) %>% 
   group_by(season) %>% 
   mutate(goals=sum(GF)) %>% 
  select(season,goals) %>% 
  arrange(season) %>% 
  unique()
 
glimpse(gls)

dput(gls)

gls %>% 
 plot_ly() %>% 
   add_markers(x = ~season,y = ~goals) 

cs %>% 
  plot_ly() %>% 
  add_bars(x = ~season,y = ~n)

gls %>% 
  plot_ly() %>% 
  add_markers(x = ~season,y = ~goals) %>% 
  add_bars(cs,x = ~season,y = ~n)


# sherwood at tot ---------------------------------------------------------

managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01')

managerGame <-managers %>% 
  mutate(name=paste(FirstName,Lastname)) %>% 
  group_by(ManagerID,ManagerTeam) %>% 
  inner_join(standings,by=c("TEAMNAME"="team")) %>% 
  select(Lastname,FirstName,name,ManagerID,ManagerTeam,Joined,Left,TEAMNAME,gameDate,res,GF,GA,position) %>% 
  filter(gameDate>=as.Date(Joined)&gameDate<=as.Date(Left)) %>% 
  mutate(win=ifelse(res=="Win",1,0)) %>% 
  ungroup()



ppgManagerTeamStint <- managerGame %>% 
  group_by(TEAMNAME,ManagerID,ManagerTeam,name) %>% 
  dplyr::summarize(sumWins=sum(win),games=n(),pc=round(sumWins/games,2)) %>% 
  ungroup()




allManagerStints <- 
  managerGame %>% 
  select(name,ManagerTeam,Joined,Left) %>% 
  unique()



## artificial start date for those hired before PL existed
allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"


winpc <- ppgManagerTeamStint  %>% 
  
  select(TEAMNAME,name,ManagerTeam,games,pc) %>% 
  inner_join(allManagerStints) %>% 
  filter(TEAMNAME=="Tottenham H") 

## if win %
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


# most assists - helping out ben ------------------------------------------

playerGame %>% 
  group_by(PLAYERID) %>% 
  summarize(totAss=sum(Assists)) %>% 
  arrange(desc(totAss))

# chelsea goals per min ---------------------------------------------------

playerGame %>% 
  filter(TEAMNAME=="Chelsea"&mins>0) %>% 
  group_by(PLAYERID) %>% 
  summarize(tMins=sum(mins,na.rm=T), tGoals=sum(Gls,na.rm=T)) %>% 
  filter(tGoals>0) %>% 
  mutate(rate=tMins/tGoals) %>% 
  arrange(rate)
