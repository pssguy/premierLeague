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
    add_bars(data=subset(test,team=="Sunderland",x =~season, y= ~value))
  
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



# rashford goals as sub ---------------------------------------------------

sort(names(playerGame))

r1 <-playerGame %>% 
  filter(PLAYERID=="RASHFOM"&START>0) %>% 
  summarize(sumMins=sum(mins),sumGoals=sum(Gls),sumAssists=sum(Assists))


r2 <-playerGame %>% 
  filter(PLAYERID=="RASHFOM"&subOn>0) %>% 
  summarize(sumMins=sum(mins),sumGoals=sum(Gls),sumAssists=sum(Assists))

playerGame %>% 
  filter(PLAYERID=="RASHFOM") %>% 
  arrange(desc(gameDate)) %>% 
  select(START,mins,Gls,Assists) 


# opta q more assits than goals -------------------------------------------

temp <-playerGame %>% 
  group_by(PLAYERID,name) %>% 
summarise(Goals=sum(Gls),Assts=sum(Assists)) %>% 
  filter(Goals>49&Assts>Goals) %>% 
  arrange(desc(Assts)) %>% 
  mutate(ratio=Assts/Goals) %>% 
  ungroup() %>% 
  arrange(desc(ratio)) %>% 
  select(name, Goals,Assts) %>%
  DT::datatable(width=250,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# oldest assist -----------------------------------------------------------

playerGame %>% 
  filter(Assists>0) %>% 
  arrange(desc(age))



# DEENEYÉIGHALO -----------------------------------------------------------


# PALACE LOSING RUN NOT RELEGATED -----------------------------------------




# FIRST PLAYER TO 10 GOALS ------------------------------------------------

sort(names(playerGame))

toTen <-playerGame %>% 
  arrange(gameDate) %>% 
  filter(PLAYERID!="OWNGOAL") %>% 
  group_by(PLAYERID,season) %>% 
  mutate(cumGls=cumsum(Gls)) %>% 
  select(name,PLAYERID,season,cumGls,gameDate) %>% 
  filter(cumGls>9) %>% 
  group_by(season) %>% 
  slice(1)

playerGame %>% 
  arrange(gameDate) %>% 
  filter(PLAYERID!="OWNGOAL"&season!="2016/17") %>% 
  group_by(PLAYERID,season) %>% 
  mutate(cumGls=cumsum(Gls)) %>% 
  select(name,PLAYERID,season,cumGls,gameDate) %>% 
 # group_by(PLAYERID,season) %>% 
  arrange(desc(gameDate)) %>% 
  slice(1) %>% 
  inner_join(toTen,by=c("PLAYERID","season")) %>% 
  ungroup() %>% 
  select(name=name.x,tenth=gameDate.y,total=cumGls.x) %>% 
  arrange(desc(tenth))%>%
    DT::datatable(width= 300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
# defoe 150 ---------------------------------------------------------------


# ford scored inside min - check earliest other teams ---------------------


# number of changes between games - swansea -------------------------------


# number of times unchaged chelsea ----------------------------------------





# Oscar minutes leaving ---------------------------------------------------

sort(names(playerGame))

test <- playerGame %>% 
  filter(season=="2016/17") %>% 
  group_by(PLAYERID,name) %>% 
  summarise(Mins=sum(mins)) %>% 
  arrange(desc(Mins)) %>% 
  ungroup() %>% 
  select(name,Mins) %>%
                         DT::datatable(width = 300,class='compact stripe hover row-border order-column',rownames=TRUE,options= list(paging = TRUE, searching = TRUE,info=FALSE))


## looks good - just need to wait for latest update


## eins at etihad



standings %>% 
  filter(season>"2002/03" & team =="Man. City" & OppTeam =="Arsenal"& venue=="H")


# players with 3 assists in game ------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(Assists>2) %>% 
  arrange(desc(gameDate)) %>% 
  select(name,gameDate) #396

temp <- playerGame %>% 
  filter(Assists>2)  %>% #396 occurrences
  group_by(PLAYERID) %>% 
  summarise(tot=n()) %>% #245
  filter(tot==1) %>% 
  arrange(PLAYERID)


# man u previous times 41 points 22 games ---------------------------------
sort(names(standings))

thisYear <- standings %>% 
  filter(team=="Man. Utd."&season=="2016/17")

fortyOne <- standings %>% 
  filter(team=="Man. Utd."&cumPts==41&tmYrGameOrder==22) %>% 
  .$season

fortyOneYears <- standings %>% 
  filter(team=="Man. Utd."&season %in% fortyOne)


gameThirtyEight <- standings %>% 
  filter(team=="Man. Utd."&tmYrGameOrder==38&season %in% fortyOne)

 p <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(gameDate) %>% 
 # group_by(season) %>% 
  plot_ly(x=~tmYrGameOrder,y=~cumPts) %>% 
  add_lines(color = ~season, colors = "black", alpha = 0.1)
 
 ## highlights lines but they are still black
q <-  p %>% 
  add_lines(data=fortyOneYears,
            x=~tmYrGameOrder,y=~cumPts,
            color = ~season, alpha = 0.3) %>%
    add_lines(data=thisYear, color = ~season,
              x=~tmYrGameOrder,y=~cumPts
              )
 q %>% 
   add_markers(data=gameThirtyEight,x=~tmYrGameOrder,y=~cumPts,
               hoverinfo=text,
               text=~paste0(season,"<br>points:",
                            cumPts,"<br>Final:",
                            final_Pos)) %>% 
   layout(title="Cumulative Points by Season - Man. Utd.",
          xaxis=list(title="Games Played"),
          yaxis=list(title="Cumulative Points")) %>% 
   config(displayModeBar = F,showLink = F)
   
 

  

# goals between rooneys outside area --------------------------------------

sort(names(goals))
 sort(names(playerGame))
 
 
 goals %>% 
   filter(PLACE=="Long_Range") %>% 
   left_join(playerGame) %>% 
   select(name,gameDate,TIME) %>% 
   arrange(desc(gameDate),desc(TIME)) %>% 
   mutate(order=row_number()) %>% 
   filter(name=="Wayne Rooney")
 
 
 
 
# rodwell starter never wins ----------------------------------------------

sort(names(playerGame))
 
 sort(names(standings))
 
 test <- playerGame %>% 
   filter(PLAYERID=="RODWELJ") %>% 
   group_by(TEAMNAME) %>% 
   left_join(standings) %>% 
   select(res,gameDate,START) %>% 
   mutate(win=ifelse(res=="Win",1,0)) %>% 
   #group_by(TEAMNAME) %>%
   do(subSeq(.$win))
 
 
 playerGame %>% 
   filter(START>0) %>% 
   group_by(PLAYERID,TEAMNAME,name) %>% 
   left_join(standings) %>% 
   select(res) %>% 
   mutate(win=ifelse(res=="Win",1,0)) %>% 
   #group_by(TEAMNAME) %>%
   do(subSeq(.$win)) %>% 
   filter(first==1,value==0) %>% 
   arrange(desc(slength))
 
 ## rodwell 5 wins in row
 
 test <-playerGame %>% 
   filter(START>0) %>% 
   group_by(PLAYERID,TEAMNAME,name) %>% 
   left_join(standings) %>% 
   select(res) %>% 
   mutate(win=ifelse(res=="Win",1,0)) %>% 
   #group_by(TEAMNAME) %>%
   do(subSeq(.$win)) %>% 
   filter(first==1,value==1&slength>4) %>% 
   arrange(desc(slength))
 
 ## try map instead of do
temp <- playerGame %>% 
   filter(START>0) %>% 
   group_by(PLAYERID,TEAMNAME,name) %>% 
   left_join(standings) %>% 
   select(res) %>% 
   mutate(win=ifelse(res=="Win",1,0)) %>% 
   nest() %>% 
   purrr::map(.win, subSeq()) # not right see JB
 
 
 by_am <- mtcars %>%
   group_by(am) %>%
   nest() %>%
   mutate(model = purrr::map(data, ~ lm(mpg ~ wt, data = .)))
 
 by_am %>%
   unnest(model %>% purrr::map(tidy))
 
 
   
 
# rooney goals sinc e jan 2016 --------------------------------------------
sort(names(playerGame))
 
test <-playerGame %>% 
   filter(gameDate>=as.Date("2016-02-17")) %>% 
   group_by(PLAYERID) %>% 
   summarise(tot=sum(Gls)) %>% 
  arrange(desc(tot))


# eriksen assists ---------------------------------------------------------

temp <- playerGame %>% 
  filter(Assists>3) %>% 
  select(name,gameDate,TEAMNAME,Opponents)



# player playing for 2 premier winning teams ------------------------------

sort(names(playerGame))
sort(names(standings))

champs <- standings %>% 
  filter(final_Pos==1&season<"2016/17") %>% 
  select(team,season) %>% 
  unique() 

champPlayers <- playerGame %>% 
  rename(team=TEAMNAME) %>% 
  right_join(champs) %>% 
  filter((START+subOn)>0) %>% 
  group_by(name,PLAYERID,season,team) %>% 
  count()

multiples <- champPlayers %>% 
  select(name,PLAYERID,team) %>% 
  ungroup() %>% 
  select(-season) %>% 
  unique() %>% 
  group_by(name,PLAYERID) %>% 
  count() %>% 
  filter(n>1)

# name PLAYERID     n
# <chr>    <chr> <int>
#   1      Ashley Cole   COLEA2     2
# 2     Carlos Tevez   TEVEZC     2
# 3    Danny Simpson  SIMPSOD     2
# 4      Gael Clichy  CLICHYG     2
# 5     Henning Berg    BERGH     2
# 6       Kolo Toure   TOUREK     2
# 7  Nicholas Anelka  ANELKAN     2
# 8  Owen Hargreaves  HARGREO     2
# 9  Ritchie De Laet  DELAETR     2
# 10     Robert Huth    HUTHR     2
> 
  

temp <-multiples %>% 
  select(-n) %>% 
  left_join(champPlayers)


most (inc single club)

champPlayers %>% 
  group_by(PLAYERID) %>% 
  count(sort=TRUE) %>% 
  arran

# Kane goalscoring --------------------------------------------------------

#consec 20 goal games - as many goals by his age/gmaes played 

twenty <- playerGame %>% 
  group_by(PLAYERID,season) %>% 
  summarise(totGls=sum(Gls)) %>% 
  filter(totGls>19&PLAYERID!="OWNGOAL") #65

twenty %>% 
  group_by(PLAYERID) %>% 
  tally() %>% #38 players
  filter(n>2)

# Only 5 players Aguero, Ferdinand, Henrym Shearer and van Nistelrooy have done it on least three occasions with just 

## age first goal

maxAge <- playerGame %>% 
  arrange(gameDate) %>% 
  filter(PLAYERID=="KANEH"&Gls>0) %>% 
  slice(1) %>% 
  .$age

playerGame %>% 
  filter(Gls>0&age<maxAge) %>% #1068
  select(PLAYERID,name) %>% 
  unique() #298 currently
#Kyle Walker 1 day older??


kane <-playerGame %>% 
  arrange(gameDate) %>% 
  filter(Gls>0&PLAYERID=="KANEH") %>% 
  slice(1)

## look what age first

playerGame %>% 
  arrange(gameDate) %>% 
  filter(Gls>0&PLAYERID!="OWNGOAL") %>% 
  group_by(PLAYERID) %>% 
  slice(1) %>% 
  plot_ly(x=~gameDate,y=~age) %>% 
  add_markers(name="All Scorers", opacity=0.5,
              hoverinfo="text",
              text=~paste0(name," ",round(age,2),'yrs')) %>% 
  add_markers(data=kane, color=I('red'), name="Harry Kane",
              hoverinfo="text",
              text=~paste0(name," ",round(age,2),'yrs')) %>% 
  layout(
    title="Age scoring first EPL goal. Hover For Player info",
    xaxis=list(title=""),
    yaxis=list(title="Age")
  )  %>% 
  config(displayModeBar = F,showLink = F) # for some reason this is causing an issue

kane$age #class(kane$age) #difftime causes isssues

youngScorers <-playerGame %>% 
  select(name,PLAYERID,Gls,age=as.numeric(age),mins,Assists) %>% 
  group_by(PLAYERID) %>% 
  mutate(cumAssists=cumsum(Assists),cumGls=cumsum(Gls),cumMins=cumsum(mins),mpg=round(cumMins/cumGls,1))

topScorersAge <- temp %>% 
  filter(age<=23.584&cumGls>=66) %>% 
  select(PLAYERID,name) %>% 
  unique() %>% 
  .$PLAYERID

playerGame %>% 
  select(name,PLAYERID,Gls,age,mins,Assists) %>% 
  group_by(PLAYERID) %>% 
  mutate(cumAssists=cumsum(Assists),cumGls=cumsum(Gls),cumMins=cumsum(mins),mpg=round(cumMins/cumGls,1)) %>% 
  filter(age<=23.584&cumGls>0) %>% 
 # filter(cumGls>0) %>% 
  select(PLAYERID) %>% 
  unique() %>% 
  .$PLAYERID
    

# ### need to check Gary Holt  confirm it should be Grant now corrected
# 
# prob <- playerGame %>% 
#   arrange(gameDate) %>% 
#   filter(Gls>0&PLAYERID=="HOLTG")
    



###

sort(names(playerGame))

temp <-playerGame %>% 
select(name,PLAYERID,Gls,age,mins,Assists) %>% 
group_by(PLAYERID) %>% 
mutate(cumAssists=cumsum(Assists),cumGls=cumsum(Gls),cumMins=cumsum(mins),mpg=round(cumMins/cumGls,1)) #%>% 
#filter(PLAYERID=="KANEH")

# 20
# Harry Kane
# KANEH
# 3
# 23.583960757472
# 85
# 66
# 8376

topScorersAge <- temp %>% 
  filter(age<=23.584&cumGls>=66) %>% 
  select(PLAYERID) %>% 
  unique() %>% 
  .$PLAYERID

temp %>% 
  filter(PLAYERID %in% topScorersAge) %>% 
  plot_ly(x=~age,y=~cumGls) %>% 
 # add_markers(marker=list(size=2)) %>% 
  add_lines(color=~name)





topScorersMins <- temp %>% 
  filter(cumMins<=8376&cumGls>=66&PLAYERID!="OWNGOAL") %>% 
  select(PLAYERID) %>% 
  unique() %>% 
  .$PLAYERID

temp %>% 
  filter(PLAYERID %in% topScorersMins) %>% 
  plot_ly(x=~cumMins,y=~cumGls) %>% 
  # add_markers(marker=list(size=2)) %>% 
  add_lines(color=~name)


temp %>% 
  filter(PLAYERID %in% topScorersMins) %>% 
  plot_ly(x=~cumMins,y=~mpg) %>% 
  # add_markers(marker=list(size=2)) %>% 
  add_lines(color=~name)

# above dows not look right
temp %>% 
  filter(PLAYERID %in% topScorersMins) %>% 
  arrange(desc(age)) %>% 
  group_by(PLAYERID) %>% 
  slice(1)

shearer <- temp %>% 
  filter(PLAYERID =="SHEAREA")


temp %>% 
  filter(PLAYERID %in% topScorersMins) %>% 
  plot_ly(x=~cumGls,y=~cumAssists)


temp %>% 
  filter(PLAYERID %in% topScorersMins) %>% 
  arrange(desc(age)) %>% 
  group_by(PLAYERID) %>% 
  slice(1) %>% 
  plot_ly(x=~cumGls,y=~cumAssists)

## torres bes t assistor - who to?


temp %>% 
  filter((cumAssists+cumGls)>=91) %>% 
  arrange(desc(age)) %>% 
  group_by(PLAYERID) %>% 
  slice(1) %>% 
  plot_ly(x=~cumGls,y=~cumAssists,
          hoverinfo="text",
          text=~name)

## think about penalties 12 
## englishman who hadnt scored in prem league by his first age (cole??)

sort(names(playerGame))

playerGame %>% 
  filter(PLAYERID=="KANEH"&Gls>0) %>% 
  arrange(mins) %>% 
  select(mins,START)

# mins START
# <dbl> <int>
#   1     34     0
# 2     72     6

asSub <- playerGame %>% 
  filter(PLAYERID=="KANEH"&mins>0&START!=6) %>% 
  arrange(mins) %>% 
  select(mins,START,Gls,Assists)
# 1 gl no assists in around 200 mins

## look as results as starter

sort(names(standings))

outcome <-playerGame %>% 
  filter(PLAYERID=="KANEH"&START==6) %>% 
  left_join(standings) %>% 
  select(Gls,res)

df <-table(outcome) #class(df)

df <- as.data.frame(table(outcome)) #class(df)

Gls  res Freq
1    0 Draw   17
2    1 Draw    7
3    2 Draw    0
4    3 Draw    0
5    0 Loss   17
6    1 Loss    2
7    2 Loss    0
8    3 Loss    0
9    0  Win   16
10   1  Win   18
11   2  Win   13
12   3  Win    4

df <-df %>% 
  spread(key=res,value=Freq) %>% 
  mutate(ppg=(3*Win+Draw)/(Draw+Loss+Win), Games=(Draw+Loss+Win))



Gls Draw Loss Win      ppg Games
1   0   17   17  16 1.300000    50
2   1    7    2  18 2.259259    27
3   2    0    0  13 3.000000    13
4   3    0    0   4 3.000000     4


## only lost 2 games he has scored in (as starter)

#extend to all other players

#eg Aguero


outcome <-playerGame %>% 
  filter(PLAYERID=="AGUEROS"&START==6) %>% 
  left_join(standings) %>% 
  select(Gls,res)



df <- as.data.frame(table(outcome)) %>% 
  spread(key=res,value=Freq) %>% 
  mutate(ppg=(3*Win+Draw)/(Draw+Loss+Win), Games=(Draw+Loss+Win))

#    Gls Draw Loss Win      ppg Games
# 1   0   16   24  33 1.575342    73
# 2   1    6    3  40 2.571429    49
# 3   2    3    1  15 2.526316    19
# 4   3    0    0   3 3.000000     3
# 5   4    0    0   1 3.000000     1
# 6   5    0    0   1 3.000000     1

# aguero 4

outcome <-playerGame %>% 
  filter(PLAYERID=="SHEAREA"&START==6) %>% 
  left_join(standings) %>% 
  select(Gls,res)



df <- as.data.frame(table(outcome)) %>% 
  spread(key=res,value=Freq) %>% 
  mutate(ppg=(3*Win+Draw)/(Draw+Loss+Win), Games=(Draw+Loss+Win))

# shearer 13 admittedly 3x as many but ppg if didnt only 1.02 - relegation form


outcome <-playerGame %>% 
  filter(PLAYERID=="KANEH"&(START+subOn)>0) %>% 
  left_join(standings) %>% 
  select(Gls,res)



df <- as.data.frame(table(outcome)) %>% 
  spread(key=res,value=Freq) %>% 
  mutate(ppg=(3*Win+Draw)/(Draw+Loss+Win), Games=(Draw+Loss+Win))


## what about the most games in which player has scored and team never lost?


outcome <-playerGame %>% 
  filter((START+subOn)>0&Gls>0) %>% 
  left_join(standings) %>% 
  select(Gls,res,PLAYERID,name) %>% 
  group_by(PLAYERID,name) %>% 
  mutate(L=ifelse(res=="Loss",1,0)) %>% 
  summarize(losses=sum(L),games=n()) %>% 
  filter(losses<3) %>% 
  arrange(desc(games))



# Gls Draw Loss Win      ppg Games
# 1   0   16   21  21 1.362069    58
# 2   1   11    4  27 2.190476    42
# 3   2    0    0   6 3.000000     6
# 4   3    0    0   1 3.000000     1

## who has kane assisted

sort(names(goals))
sort(names(assists))
sort(names(playerGame))

# goalscorer

scorer <-goals %>% 
  select(PLAYER_MATCH,PLAYER_MATCH_GOAL) %>% 
  left_join(playerGame) %>% #25421
  rename(scorer=name)

assister <-assists %>% 
    left_join(playerGame) %>%  #32097
    select(assister=name,PLAYER_MATCH_GOAL) %>% 
    left_join(scorer)

assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(assister=="Harry Kane") %>% 
  arrange(desc(n))

assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(assister=="Sergio Aguero") %>% 
  arrange(desc(n))

## best combo all time

assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(scorer=="Harry Kane") %>% 
  arrange(desc(n))

# assister           scorer     n
# <chr>            <chr> <int>
#   1  Steve McManaman    Robbie Fowler    33
# 2    Thierry Henry     Robert Pires    33
# 3    Frank Lampard  Didier Drogba      32
# 4     Robert Pires    Thierry Henry    30
# 5  Darren Anderton Teddy Sheringham    29
# 6  Dennis Bergkamp    Thierry Henry    26
# 7      David Silva    Sergio Aguero    25
# 8       Ryan Giggs     Paul Scholes    25
# 9       Ryan Giggs        Andy Cole    23
# 10   David Beckham    Ole Solskjaer    22
  

assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(scorer=="Harry Kane") %>% 
  arrange(desc(n))

# Deli
assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(assister=="Dele Alli") %>% 
  arrange(desc(n))


assister %>% 
  group_by(assister,scorer) %>% 
  count() %>% 
  filter(assister=="Paul Pogba") %>% 
  arrange(desc(n))


## leading goals coe since firts harry kane goal 7th aprilm 2014 or 2nd nov

playerGame %>% 
  filter(gameDate>=as.Date("2014-04-07")) %>% 
  group_by(PLAYERID) %>% 
  summarise(totGls=sum(Gls)) %>% 
  arrange(desc(totGls)) 

playerGame %>% 
  filter(gameDate>=as.Date("2014-04-07")&TEAMNAME=="Tottenham H") %>% 
  group_by(PLAYERID) %>% 
  summarise(totGls=sum(Gls)) %>% 
  arrange(desc(totGls)) 

## Henry
topScorersMins <- cum_data %>% 
  filter(PLAYERID=="HENRYT")


# time to get to 6 assits -------------------------------------------------

temp <-playerGame %>% 
  filter(TEAMNAME=="Man. Utd.") %>% 
  arrange(gameDate) %>% 
  group_by(PLAYERID,season) %>% 
  mutate(totAss=cumsum(Assists)) %>% 
  select(season,gameDate,name,Assists,totAss) %>% 
  filter(totAss>5) %>% 
  group_by(season) %>% 
  slice(1)

#paul ince 1993/4

playerGame %>% 
  filter(PLAYERID=="INCEP"&season=="1993/94") %>% 
  select(gameDate,Assists)



# time taken to exceed previous years pointcount - chelsea ----------------




# CPFC back to back unbeaten ----------------------------------------------



    

# time of mane's goals and assists ----------------------------------------

sort(names(goals))
scorer <-goals %>% 
  select(PLAYER_MATCH,PLAYER_MATCH_GOAL,TIME) %>% 
  left_join(playerGame) %>% #25421
  rename(scorer=name)

topPlayers <- c("MANES")

sort(names(scorer))
scorer %>% 
  filter(PLAYERID %in% topPlayers&season=="2016/17") %>% 
  plot_ly(x=~jitter(TIME),y=~PLAYERID) %>% 
  add_markers(color=I("red"),opacity=0.5,name="Goals") %>% 
  add_markers(data=assister,color=I("blue"),opacity=0.5,name="Assists")

plot_ly(data=assister[assister$PLAYERID %in% topPlayers&assister$season=="2016/17",],x=~jitter(TIME),y=~PLAYERID,color=I("blue"),opacity=0.5,name="Assists", type="scatter")


sort(names(assister))
# assister <-assists %>% 
#   left_join(playerGame) %>%  #32097
#   select(assister=name,PLAYER_MATCH_GOAL) %>% 
#   left_join(scorer)

# sort(names(assister))
# 
# assister %>% 
#   filter(PLAYERID=="MANES"&season=="2016/17") %>% 
#   plot_ly(x=~jitter(TIME),y=~PLAYERID) %>% 
#   add_markers()

topPlayers <- c("MANES","SANCHEA","HAZARDE","ALLID","LUKAKUR","DEBRUYK","KANEH")

scorer %>% 
  filter(PLAYERID %in% topPlayers&season=="2016/17") %>% 
  plot_ly(x=~jitter(TIME),y=~PLAYERID) %>% 
  add_markers(color=I("red"),opacity=0.5,name="Goals") %>% 
  add_markers(data=assister[assister$PLAYERID %in% topPlayers&assister$season=="2016/17",],x=~jitter(TIME),y=~PLAYERID,color=I("blue"),opacity=0.5,name="Assists")

## dont look like enough assists - need to do a bit more work
temp <- assister[assister$PLAYERID %in% topPlayers&assister$season=="2016/17",]
## here assister$Playerid is that of goalscorer


sort(names(assists)) "PLAYER_MATCH"      "PLAYER_MATCH_GOAL"
sort(names(goals))
assister <- assists %>% 
  left_join(playerGame) %>% 
  select(name,PLAYERID,season,Opponents,PLAYER_MATCH_GOAL) %>% 
  inner_join(goals,by=c("PLAYER_MATCH_GOAL"))
  
scorer <-goals %>% 
  select(PLAYER_MATCH,PLAYER_MATCH_GOAL,TIME) %>% 
  left_join(playerGame) %>% #25421
  rename(scorer=name)


scorer %>% 
  filter(PLAYERID %in% topPlayers&season=="2016/17") %>% 
    plot_ly(x=~jitter(TIME),y=~scorer) %>% 
  add_markers(color=I("red"),opacity=0.5,name="Goals",
              hoverInfo="text",
              text=~paste0("Time: ",TIME," min",
                           "<br>Opps: ",Opponents)) %>% 
  add_markers(data=assister[assister$PLAYERID %in% topPlayers&assister$season=="2016/17",],x=~jitter(TIME),y=~name,color=I("blue"),opacity=0.5,name="Assists",
              hoverInfo="text",
              text=~paste0("Time: ",TIME," min",
                           "<br>Opps: ",Opponents)) %>% 
 # add_lines(x=c(45,45),y=c(topPlayers[1],topPlayers[7])) %>% prob could improve with name in
  layout(title="2016/7 EPL Leading Players - Timing of Goals and Assists",
         xaxis=list(title="Minutes"),
         yaxis=list(title=""),
         margin = list(l = 100)
         ) %>%  config(displayModeBar = F,showLink = F)

# 10 assist and 10 goalscorer ---------------------------------------------

## maybe have a list column by year

playerGame %>% 
  group_by(PLAYERID,TEAMNAME,season,name) %>% 
  summarize(goals=sum(Gls),assists=sum(Assists)) %>% 
  filter(goals>9&assists>9) %>% 
  ungroup() %>% 
  select(season,TEAMNAME) %>% 
  unique() %>% 
  filter(TEAMNAME=="Man. Utd.") %>% 
  arrange(season)



# Longest unbeaten run in season - final position -------------------------

L <-standings %>% 
  ungroup() %>% 
  filter(team==input$teamA) %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmGameOrder) %>% 
  mutate(cat=ifelse(res=="Loss",1,0)) %>% 
  do(subSeq(.$cat))

L <-standings %>% 
  ungroup() %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmGameOrder) %>% 
  mutate(cat=ifelse(res=="Loss",1,0)) %>% 
  do(subSeq(.$cat))
# current 0 =16 looks right - longest is 2 of 29  - now look at group by season (should also look at map)

L <-standings %>% 
  ungroup() %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmYrGameOrder,season) %>% 
  group_by(season) %>% 
  mutate(cat=ifelse(res=="Loss",1,0)) %>% 
  do(subSeq(.$cat))  # also looks good

L <-standings %>% 
  ungroup() %>% 
  #filter(team=="Man. Utd.") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmYrGameOrder,season,team) %>% 
  group_by(season,team) %>% 
  mutate(cat=ifelse(res=="Loss",1,0)) %>% 
  do(subSeq(.$cat))  # also looks good

## do best by season (now inc all teams)

bestBySeason <- L %>% 
  filter(value==0) %>% 
  arrange(desc(slength)) %>% 
  group_by(season,team) %>% 
  slice(1)

pos <- standings %>% 
 # filter(team=="Man. Utd.") %>% 
  select(final_Pos,season,team) %>% 
  unique() %>% 
  inner_join(bestBySeason) %>% 
  select(team,season,best=slength,pos=final_Pos) %>% 
  arrange(season) 


standings %>% 
  filter(team=="Man. Utd.") %>% 
  select(final_Pos,season) %>% 
  unique() %>% 
  inner_join(bestBySeason) %>% 
  select(season,best=slength,pos=final_Pos) %>% 
  arrange(season) %>% 
  plot_ly(x=)

pos[pos$season=="2016/17",]$best <- 17

pos %>%
  arrange(desc(best)) %>% 
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

## most draws by season

sort(names(standings))

standings %>% 
  filter(team=="Man. Utd."&res=="Draw") %>% 
  group_by(season) %>% 
  tally() %>% 
  arrange(desc(n))

## those of 20+ in premier league

finish <-standings %>% 
  # filter(team=="Man. Utd.") %>% 
  select(final_Pos,season,team) %>% 
  unique()

top20 <- L %>% 
  filter(slength>19) %>% 
  left_join(finish) %>% 
  ungroup() %>% 
  arrange(desc(slength)) %>% 
  select(team,season,run=slength,Position=final_Pos) %>%
                         DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  


levels <- top20 %>% 
  arrange(desc(slength)) %>% 
  select(team) %>% 
  .$team

top20 %>% 
  arrange(desc(slength)) %>% 
  mutate(ts=paste0(team,season)) %>% 
  plot_ly(x=~slength,y=~ts)
  
  ppg %>%
  arrange(average) %>%
  gather(venue, value, away, home, average) %>%
  mutate(teamName=factor(team, levels = levels)) %>% 
  group_by(team) %>% 
  plot_ly(x = ~value, y = ~teamName,width=800, height= 900) %>% 
  add_markers


# age 150th app -----------------------------------------------------------

playerGame %>% 
  filter((START+subOn)>0) %>% 
group_by(PLAYERID) %>% 
  mutate(order=row_number()) %>% 
  filter(order==150) %>% 
  arrange(age)



# time since scoring ------------------------------------------------------
#wk11 annualcode
a <- teamGames %>% 
  
  filter(TEAMNAME=="Crystal P"&season=="2017/18") %>% 
 # filter(TEAMNAME=="Crystal P") %>% not sure this is right
  select(TEAMMATCHID,gameDate) %>% 
  mutate(totMins=90*(row_number()-1)) %>% # want to start with 0 to make subsequent addition easier
  left_join(goals) %>% 
  arrange(gameDate,TIME) %>% 
  select(totMins,TIME,TEAMMATCHID) 

## duplicate last row ? should do better

a <- rbind(a,tail(a,1))
a[nrow(a),]$TIME <- 90

a <-a %>% 
  filter(!is.na(TIME)) %>% # this gets rid of games with no goals against NB not good for latest result
  mutate(cumTime=totMins+TIME,lagtime=(lag(cumTime)),leadtime=(lead(cumTime))) %>% 
  mutate(lagtime=ifelse(is.na(lagtime),0,lagtime),leadtime=ifelse(is.na(leadtime),totMins+90,leadtime)) %>% 
  mutate(cumdiff=cumTime-lagtime,goalOrder=row_number())
a %>% 
  plot_ly(x=~goalOrder, y=~ cumdiff) %>% 
  add_bars(color= ~ as.factor(TEAMMATCHID)) %>% 
  layout(showlegend=FALSE,
         height = 350,
         autosize = F,
         title="Time between Goals",
         xaxis=list(title="Goal order"),
         yaxis=list(title="Minutes")) %>% 
  config(displayModeBar = F,showLink = F)

-- might wan to loook diff - can do for players not scoring droughts


# win% manu  --------------------------------------------------------------

sort(names(standings))

temp <-standings %>% 
  mutate(category=ifelse(res=="Win",1,0)) %>% 
  group_by(team,season) %>% 
  summarize(wins=sum(category),games=n(),win_pc=round(100*wins/games)) %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(win_pc)


# Best finish by player ---------------------------------------------------

sort(names(playerGame)) 
sort(names(standings))

temp <-playerGame %>% 
  filter((START+subOn)>0&season<"2016/17") %>% 
  left_join(standings,by=c("MATCHID","TEAMNAME"="team")) %>% 
  select(name,PLAYERID,season=season.x,final_Pos,TEAMNAME) %>%   ## could be 2 teams
    unique() #13731

#individ player
temp %>% 
 filter(PLAYERID=="WHELANP") %>% #16 19 22 12 14
  plot_ly(x=~final_Pos) %>% 
  add_histogram(xbins=list(size=1))

# looks good
temp %>% 
#  filter(PLAYERID=="WHELANP") %>% 
  group_by(name,final_Pos,PLAYERID) %>% 
  tally() %>% 
  filter(PLAYERID=="WARDJ") %>% 
  plot_ly(x=~final_Pos,y=~n) %>% 
  add_bars()

# all best finishes
allTimeBest <-temp %>% 
  #  filter(PLAYERID=="WHELANP") %>% 
  group_by(name,PLAYERID) %>% 
 # tally() %>% 
  arrange(final_Pos) %>% 
  slice(1)

allTime %>% #3827
  group_by(final_Pos) %>% 
  tally() %>% 
  plot_ly(x=~final_Pos,y=~n) %>% 
  add_bars()
  
allTimeBest %>% 
  filter(final_Pos>17&season>"1994/95"|final_Pos>19&season<"1994/95")



# conte record cf last mourinho -------------------------------------------

#chelsea 27 66 points

sort(names(standings))

temp <- standings %>% 
  filter(gameDate <"2015-12-18"&team=="Chelsea") %>% 
  arrange(desc(gameDate)) %>% 
  select(gameDate,OppTeam,points) %>% 
  mutate(cumPoints=cumsum(points))

#last 40 66 points 1.65

#currently 49 from 26 1.88


# Rashford performance ----------------------------------------------------


# rle functom -------------------------------------------------------------



# played only one game ----------------------------------------------------

sort(names(playerGame))

temp <-playerGame %>% 
  filter((START+subOn)>0) %>% 
           group_by(PLAYERID,name,COUNTRY) %>% 
           tally() %>% 
  filter(n==1) %>% #280
  group_by(COUNTRY) %>% 
  tally()
           

# spanish players for liverpool -------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(TEAMNAME=="Liverpool"&mins>0&COUNTRY=="Spain") %>% 
  group_by(PLAYERID,name) %>% 
  summarize(totmins=sum(mins)) %>% 
  ungroup() %>% 
  select(-PLAYERID,name,mins=totmins) %>%
  arrange(desc(mins))
 DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
 
 

# Man u gf by season by game played ---------------------------------------

sort(names(standings))
 
 
allYears <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  group_by(season)  

p <-allYears %>% 
  plot_ly(x=~tmYrGameOrder,y=~cumGF) %>% 
  add_lines(color=I('lightgrey'), name="Premier Seasons")
p

allYears <-allYears %>% 
  filter(season=="2015/16") %>% 
  add_lines(color=I('blue'), name="2015/16")

allYears %>% 
  filter(season=="2016/17") %>% 
  add_lines(color=I('red'), name="2016/17")



layer_season <- function(plot, name) {
  plot %>% filter(season == name) %>% add_lines(name = name,
                                                hoverinfo="text",
                                                text=~paste0(season,
                                                             "<br>Pl: ",tmYrGameOrder,
                                                             "<br>Gls: ",cumGF))
}

allYears <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  group_by(season)  

p <-allYears %>% 
  plot_ly(x=~tmYrGameOrder,y=~cumGF) %>% 
  add_lines(color=I('lightgrey'), name="EPL Seasons",
            hoverinfo="text",
            text=~paste0(season,
                         "<br>Pl: ",tmYrGameOrder,
                         "<br>Gls: ",cumGF)) %>% 
  add_fun(layer_season,"2015/16") %>% 
  add_fun(layer_season,"2016/17") %>% 
  layout(hovermode="closest",
    title="Man. Utd. Cumulative Goals For by Game by PL Season",
         xaxis=list(title="Games Played"),
         yaxis=list(title="Goals Scored")) %>% 
   config(displayModeBar = F,showLink = F)

p
rangeslider(p)

## check how many titles they have won

standings %>% 
  filter(final_Pos==1) %>% 
  select(team,season) %>% 
  unique() %>% 
  group_by(team) %>% 
  tally()
  select
  
  

# 1st 2nd pos after 30 games ----------------------------------------------
  
  sort(names(standings))

standings %>% 
    filter(tmYrGameOrder==30&position %in% c(1,2)) %>% 
    group_by(position) %>% 
    plot_ly(x=~season,y=~cumPts) %>% 
  add_markers(color=~position,  showlegend = FALSE) %>% 
  layout(showlegend=FALSE)


# alli start --------------------------------------------------------------


sort(names(playerGame))

alliAge <-playerGame %>% 
  filter(PLAYERID=="ALLID") %>% 
  .$age %>% 
  max %>% 
  parse_number()
  

playerGame %>% 
  filter(age<=alliAge) %>% 
  group_by(name,PLAYERID) %>% 
  summarise(mins=sum(mins),goals=sum(Gls),assists=sum(Assists),points=goals+assists,ppg=points*90/mins) %>% 
  arrange(desc(points))


# fabregas yellow ---------------------------------------------------------

sort(names(playerGame))

table(playerGame$CARD)


playerGame %>% 
  filter(CARD>="1") %>% 
  group_by(PLAYERID,name) %>% 
 tally() %>% 
  arrange(desc(n))

temp <-playerGame %>% 
  filter(CARD>="1") %>% 
  group_by(PLAYERID,name) %>% 
  summarise(tot=n(),mins=sum(mins),mpc=mins/tot) %>% 
  arrange(mpc) %>% 
  filter(tot>49)

temp <-playerGame %>% 
  filter(CARD>="1") %>% 
  group_by(PLAYERID,name,season) %>% 
  summarise(tot=n(),mins=sum(mins),mpc=mins/tot) %>% 
  arrange(mpc) %>% 
  filter(tot>6)


# Spurs run 7 wins --------------------------------------------------------

# in season
W <-standings %>% 
  ungroup() %>% 
  #filter(team=="Man. Utd.") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmYrGameOrder,season,team) %>% 
  group_by(season,team) %>% 
  mutate(cat=ifelse(res=="Win",1,0)) %>% 
  do(subSeq(.$cat)) %>% 
  filter(team=="Tottenham H"&value==1) %>% 
  arrange(desc(slength),desc(season)) %>% 
  ungroup() %>% 
  select(season,run=slength) %>% 
  filter(run>4)%>%
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
W

# overlap

W <-standings %>% 
  ungroup() %>% 
  #filter(team=="Man. Utd.") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmYrGameOrder,season,team) %>% 
  group_by(team) %>% 
  mutate(cat=ifelse(res=="Win",1,0)) %>% 
  do(subSeq(.$cat)) %>% 
  filter(team=="Tottenham H"&value==1) %>% 
  arrange(desc(slength))
W


# players scoring since allardyce joined ----------------------------------

playerGame %>% 
  filter(gameDate>=as.Date("2016-12-23")&Gls>0) %>% 
  group_by(PLAYERID,name,TEAMNAME) %>% 
  summarise(Goals=sum(Gls)) %>% 
  filter(TEAMNAME=="Crystal P")


# fabregas assists --------------------------------------------------------

# look at top 5

playerGame %>% 
  group_by(PLAYERID,name) %>% 
  summarize(sumAssists=sum(Assists,na.rm=T)) %>% 
  select(name,sumAssists) %>% 
  arrange(desc(sumAssists)) 
  

# teams relegated without away win ----------------------------------------

sort(names(standings))

temp <- standings %>% 
  filter(venue=="A") %>% 
  group_by(team,season,final_Pos) %>% 
  select(res) %>% 
  mutate(count=ifelse(res=="Win",1,0)) %>% 
  summarize(tot=sum(count),Pl=n()) %>% 
  filter(tot==0) %>% 
  arrange(final_Pos,desc(Pl)) 

temp[2,3] <- 16
  
temp %>% 
  select(team,season,Games=Pl,Position=final_Pos)%>%
                         DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# sequence of away game runs without losing by nore than one --------------

# Burnley

sort(names(standings))

temp <- standings %>% 
  filter(season=="2016/17"&venue=="A") %>% 
  arrange(gameDate) %>% 
    mutate(cat=ifelse(GA-GF<=1,1,0)) 

temp %>% 
group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  arrange(desc(slength)) %>% 
  slice(1) %>% 
  arrange(desc(slength)) %>% 
  select(team, run=slength)%>%
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# compaing top arsenal players --------------------------------------------

## like playerComparisons.R which also ought to change to plotly

plIds <- c("BERGKAD","OZILM","SANCHEA","PIRESR")

playerGame %>% 
  filter((START+subOn>0)&PLAYERID %in% plIds) %>% 
  select(name,PLAYERID,Gls,Assists,age,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID) %>% 
  mutate(gameOrder=row_number(),points=Assists+Gls,
         cumGoals=cumsum(Gls),cumAssists=cumsum(Assists),cumPoints=cumsum(points)) %>% 
     plot_ly(x=~gameOrder,y=~cumPoints,color=~name) %>% 
  add_lines() %>% 
  layout(title="Arsenal Players - Cumulative Goals and Assists Premier League",
    xaxis=list(title="Games Played"),
    yaxis=list(title="Goals+Assists(up to 2 allowed per goal)")) %>% 
    config(displayModeBar = F,showLink = F)
 

# Giroud as sub -----------------------------------------------------------

# all players by season

head(playerGame)

playerGame %>% 
  filter(subOn>0&Gls>0) %>% 
  group_by(name,PLAYERID,season) %>% 
  summarize(totGls=sum(Gls)) %>% 
  arrange(desc(totGls))


# 50 goal scoers by club --------------------------------------------------

sanchez joins lots at arsenal

# Number of weeks at specific position ------------------------------------

manu at 6



# Man U lineup experience -------------------------------------------------



# Number of weeks at one position -----------------------------------------

sort(names(standings))

temp <- standings %>% 
  group_by(season,position,team) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(position) %>% 
  arrange(desc(n)) %>% 
  slice(1)
## just add in final_pos as well


# points gap between teams at end of year -----------------------------------------



# Wilshere playing or not -------------------------------------------------


# most goals by day/round

standings %>% 
  group_by(gameDate) %>% 
  summarize(gls=sum(GF)) %>% 
  arrange(desc(gls)) %>%
  rename(date=gameDate,Goals=gls) %>% 
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# peak was 1993-05-08

## 

temp <- standings %>%
  filter(gameDate=="2008-01-01")

standings[standings$gameDate=="2008-01-01",]

## consistently giving 0

glimpse(standings)

standings$gameDate

# number of teenager per game

sort(names(playerGame))

playerGame %>% 
  filter(age<21) %>% 
  group_by(gameDate,TEAMNAME) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(TEAMNAME=='Man. Utd.')


playerGame %>% 
  filter(TEAMNAME=='Man. Utd.') %>% 
  select(name,age,gameDate) %>% 
  arrange(desc(gameDate,age)) %>% 
  head(18)




# Players whose last game in PL was final game of a season ----------------



# top goal scorer - wordt gf by team --------------------------------------

pl <- playerGame %>% 
  group_by(season,name,PLAYERID) %>% 
  summarize(goals=sum(Gls)) %>% 
  filter(PLAYERID!="OWNGOAL") %>% 
  ungroup() %>% 
  arrange(desc(goals)) %>% 
  group_by(season) %>% 
  slice(1)


tm <- standings %>% 
  group_by(season) %>% 
  filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
  select(team,season,final_Pos,cumGF) %>% 
#  ungroup() %>% 
  arrange(cumGF) %>% 
  group_by(season) %>% 
  slice(1) %>% 
  inner_join(pl) %>% 
  select(season,team,tm_goals=cumGF,name,pl_goals=goals) %>%

                         DT::datatable(width=500,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# wilshere with without ---------------------------------------------------

#adapt this some sort of 
outcome <-playerGame %>% 
  filter(PLAYERID=="KANEH"&(START+subOn)>0) %>% 
left_join(standings) %>% 
  select(Gls,res)



as.data.frame(table(outcome)) %>% 
  spread(key=res,value=Freq) %>% 
  mutate(ppg=round((3*Win+Draw)/(Draw+Loss+Win),2), Games=(Draw+Loss+Win)) %>%
  select(Goals_Scored=Gls,Win,Draw,Loss,ppg,Games) %>% 
  DT::datatable(width=400,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

sort(names(playerGame))
outcome <-playerGame %>% 
  filter(PLAYERID=="KANEH"&(START+subOn)>0) %>% 
  left_join(standings) %>% 
  select(Gls,res)

sort(names(playerGame))


outcome <-playerGame %>% 
  filter(PLAYERID=="WORTHIM"&(START+subOn)>0&season=="2016/17"&TEAMNAME=="Bournemouth") %>% 
  left_join(standings) %>% 
  select(gameDate,res,name) #27 could also look as starter

  player<- unique(outcome$name)

as.data.frame(table(outcome)) %>% #81 = 3 poss results x27
  spread(key=res,value=Freq) %>% ## if say only draws then
  mutate(ppg=round((3*Win+Draw)/(Draw+Loss+Win),2), Games=(Draw+Loss+Win)) %>%
  select(Win,Draw,Loss,ppg,Games) %>% 
  DT::datatable(width=400,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


temp <-as.data.frame(table(outcome)) %>% #81 = 3 poss results x27
  spread(key=res,value=Freq) %>% 
  mutate(ppg=round((3*Win+Draw)/(Draw+Loss+Win),2)) %>% 
  summarize(games=n(),av_ppg=mean(ppg))   #games   av_ppg 1    27 1.185185
  df <- bind_cols(temp,player=player)
  
  unique(pla)
  
    pl <- playerGame %>% 
    filter((START+subOn)>0&season=="2016/17"&TEAMNAME=="Bournemouth") %>% 
    .$PLAYERID %>% 
      unique() #27 looks good

    ppg <- function(x) {
      print(x)
    outcome <-playerGame %>% 
      filter(PLAYERID==x) %>% 
      left_join(standings) %>% 
      select(gameDate,res,name) #27 could also look as starter
    
    player<- unique(outcome$name)
    print(player)
    temp <-as.data.frame(table(outcome)) %>% #81 = 3 poss results x27
      spread(key=res,value=Freq) %>% 
      mutate(ppg=round((3*Win+Draw)/(Draw+Loss+Win),2)) %>% 
      summarize(games=n(),av_ppg=mean(ppg))   #games   av_ppg 1    27 1.185185
    print(temp)
    bind_cols(temp,player=player)
    
    }

dataOneYear <-map_df(pl, ppg)


as.data.frame(table(outcome)) %>% 
  mutate(ppg=ifelse(res=="Win",3,ifelse(res=="Draw",1,0))) %>% 
  summarize(games=n(),av_ppg=mean(ppg))

#####
pl <- playerGame %>% 
  filter((START+subOn)>0&season=="2016/17"&TEAMNAME=="Crystal P") %>% 
  .$PLAYERID %>% 
  unique()

dataOneYear <- NULL
ppg <- function(x) {
  
  outcome <-playerGame %>% 
    filter(PLAYERID==x&(START+subOn)>0&season=="2016/17"&TEAMNAME=="Crystal P") %>% 
    left_join(standings) %>% 
    select(gameDate,res,name) #27 could also look as starter
  
 
  
  player<- unique(outcome$name)
 
  #temp <-as.data.frame(table(outcome)) %>% 
outcome %>% 
    mutate(ppg=ifelse(res=="Win",3,ifelse(res=="Draw",1,0))) %>% 
    summarize(games=n(),av_ppg=round(mean(ppg),2)) %>% cbind(player)
  #print(temp)
 # bind_cols(temp,player=player)
  
}

dataOneYear <-map_df(pl, ppg)
# this does not give results of games not appearing in

library(forcats)

levels <- dataOneYear %>% 
  arrange(games) %>% 
  #select(team,average) %>% 
  .$player

dataOneYear %>% 
  mutate(name=factor(player, levels = levels)) %>% 
  plot_ly(x=~av_ppg,y=~name,width=800, height= 700) %>% 
  add_markers(color = I("blue"),
              hoverinfo="text",
              text=~paste0(name,"<br>Apps:",games,
                           "<br>ppg: ",av_ppg)) %>% 
 # add_lines(x = ~av_ppg, y = ~name, showlegend = F, line = list(color = "gray", width=1)) %>% 
  layout(hovermode = "closest",  
         title = "Average points per PL game played in <br> Ordered by Appearances - Hover for Info",
         xaxis = list( title = "Points per Game"),
         yaxis = list(title = ""),
         margin = list(l = 130)
  ) %>% 
  config(displayModeBar = F,showLink = F)



##### starts only
pl <- playerGame %>% 
  filter(START>0&season=="2016/17"&TEAMNAME=="Bournemouth") %>% 
  .$PLAYERID %>% 
  unique()

dataOneYear <- NULL
ppg <- function(x) {
  
  outcome <-playerGame %>% 
    filter(PLAYERID==x&START>0&season=="2016/17"&TEAMNAME=="Bournemouth") %>% 
    left_join(standings) %>% 
    select(gameDate,res,name) #27 could also look as starter
  
  
  
  player<- unique(outcome$name)
  
  #temp <-as.data.frame(table(outcome)) %>% 
  outcome %>% 
    mutate(ppg=ifelse(res=="Win",3,ifelse(res=="Draw",1,0))) %>% 
    summarize(games=n(),av_ppg=round(mean(ppg),2)) %>% cbind(player)
  #print(temp)
  # bind_cols(temp,player=player)
  
}

dataOneYear <-map_df(pl, ppg)
# this does not give results of games not appearing in

# average points
mean(dataOneYear$av_ppg) #1.161304
 l <- round(weighted.mean(dataOneYear$av_ppg, dataOneYear$games),2) #1.210048
 
 
 

library(forcats)

levels <- dataOneYear %>% 
  arrange(games) %>% 
  #select(team,average) %>% 
  .$player

# set levels for vertical line
a <-levels[1] #"Ryan Allsopp"
b <- levels[length(levels)] #"Artur Boruc"

dataOneYear %>% 
  mutate(name=factor(player, levels = levels)) %>% 
  plot_ly(x=~av_ppg,y=~name,width=800, height= 700) %>% 
  add_markers(color = I("blue"),
              hoverinfo="text",
              text=~paste0(name,"<br>Starts:",games,
                           "<br>ppg: ",av_ppg)) %>% 
  # add_lines(x = ~av_ppg, y = ~name, showlegend = F, line = list(color = "gray", width=1)) %>% 
 # add_lines(x = c(l, l), y= c(a, b), mode = "lines") %>% # player order gets screwed
  add_lines(x = c(l, l), y= c(~min(name), ~max(name)), mode = "lines") %>% 
  layout(hovermode = "closest",  
         title = "Average points per PL game started in <br> Ordered by Starts - Hover for Info",
         xaxis = list( title = "Points per Game"),
         yaxis = list(title = ""),
         margin = list(l = 130)
  ) %>% 
  config(displayModeBar = F,showLink = F)


## getting problem with line

df <- data.frame(id=c("a","b","c"),s=c(1,3,2),t=c(3,2,1))

levels <- df %>% 
  arrange(s) %>% 
  .$id #[1] a c b

class(levels[1]) #factor
levels[length(levels)]

df %>% 
  mutate(id=factor(id, levels = levels)) %>% 
plot_ly(x=~t,y=~id) %>% 
 
  add_markers() 


  theMin <- levels[1] # factor
theMax <-levels[length(levels)]

line <- weighted.mean(df$t,df$s)
  
df %>% 
  plot_ly(x=~t,y=~id) %>% 
  mutate(id=factor(id, levels = levels)) %>% 
  add_markers() %>% 
  add_lines(x=c(line,line), y=c(theMin,theMax))



p <- df %>% 
  mutate(id=factor(id, levels = levels)) %>% 
  plot_ly(x=~t,y=~id) %>% 
 
  add_markers()
p

x_values <- c(mean(df$t) - 0.5 * line, mean(df$t) + 0.5 * line)


x_values <- weighted.mean(df$t,df$s)
y_values <- c('a', 'c') # [1] "a" "c"class(y_values) character

y_values <- c(as.character(levels[1]), as.character(levels[length(levels)])) # 1 2

p <- add_lines(p, 
               x = x_values, 
               y = y_values)
p




# leading scorer by nationality -------------------------------------------

sort(names(playerGame))

df <- playerGame %>% 
  filter(PLAYERID!="OWNGOAL") %>% 
  group_by(name,PLAYERID,COUNTRY) %>% 
  summarize(sumGoals=sum(Gls)) %>% 
  arrange(desc(sumGoals)) %>% 
  group_by(COUNTRY) %>% 
  slice(1) %>% 
  select(name,Country=COUNTRY,Goals=sumGoals) %>%
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


second <- playerGame %>% 
  filter(PLAYERID!="OWNGOAL") %>% 
  group_by(name,PLAYERID,COUNTRY) %>% 
  summarize(sumGoals=sum(Gls)) %>% 
  arrange(desc(sumGoals)) %>% 
  filter(sumGoals>0) %>% 
  group_by(COUNTRY) %>% 
  slice(2) %>% 
  select(name,Country=COUNTRY,Goals=sumGoals) %>%
   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# How did players fare boight last summer ---------------------------------

sort(names(playerClub))

df <-playerClub %>% 
  filter(JOINED>"2016-05-15"&JOINED<="2016-09-01"&FEE!=99) %>% 
  left_join(summary) %>% 
  filter(season=="2016/17") %>% 
  plot_ly(x=~FEE,y=~mins,color=~TEAMID,
          hoverinfo="text",
          text=~PLAYERID) 

## need to link to names and teamnames e.g boasie

resuce to 20mill - give names


# Rooney PL imortance to United -------------------------------------------

sort(names(playerGame))

goaly <- c("G","Goalkeeper")

df <-playerGame %>% 
  filter(TEAMNAME=="Man. Utd."& !POSITION %in% goaly) %>% 
  group_by(season,name,PLAYERID) %>% 
  summarize(totMins=sum(mins)) %>% 
  arrange(desc(totMins)) %>% 
  group_by(season) %>% 
  mutate(minOrder=row_number())

df %>% 
  filter(PLAYERID=="ROONEYX") %>% 
  plot_ly(x=~season,y=~minOrder) %>% 
  add_markers(color=I("red")) %>% 
  #add_lines(color=I("lightgrey")) %>% 
  layout(showlegend=FALSE,margin=list(b=80),
         title="Rooney ranking in PL minutes played <br>for Man Utd by season",
         yaxis=list(autorange="reversed",title="Rank"),
         xaxis=list(title="")
  )



#  manu u starting with no big assisters ----------------------------------

sort(names(summary))


 summary %>% 
   filter(season=="2016/17"&TEAMNAME=="Man. Utd.") %>% 
   mutate(Gls=StGls+subGls,Pts=Gls+Assists,per90Mins=round(Pts*90/mins,2)) %>% 
   select(name,Gls,Assts=Assists,Pts,mins,per90Mins) %>% 
   arrange(desc(per90Mins)) %>% 
   head(6) %>% 
   datatable()
 
 
 summary %>% 
   filter(season=="2016/17"&TEAMNAME=="Everton") %>% 
   mutate(Gls=StGls+subGls,Pts=Gls+Assists,per90Mins=round(Pts*90/mins,2)) %>% 
   select(name,Gls,Assts=Assists,Pts,mins,per90Mins) %>% 
   arrange(desc(per90Mins)) %>% 
   head(6) %>% 
   datatable()
 
 summary %>% 
   filter(season=="2016/17"&TEAMNAME=="Crystal P") %>% 
   mutate(Gls=StGls+subGls,Pts=Gls+Assists,per90Mins=round(Pts*90/mins,2)) %>% 
   select(name,Gls,Assts=Assists,Pts,mins,per90Mins) %>% 
   arrange(desc(per90Mins)) %>% 
   head(6) %>% 
   datatable()
 
 
 test <-summary %>% 
   filter(season=="2016/17"&POSITION=="Midfielder") %>% 
   mutate(Gls=StGls+subGls,Pts=Gls+Assists,per90Mins=round(Pts*90/mins,2)) %>% 
   select(name,Gls,Assts=Assists,Pts,mins,per90Mins,age=floor(as.numeric(difftime(Sys.Date(),born,units="days"))/365.25)) %>% 
   arrange(desc(per90Mins)) %>% 
   head(6) %>% 
   datatable()
glimpse(summary)

class(Sys.Date()-summary$born[1])
floor(as.numeric(difftime(Sys.Date(),summary$born[1],units="days"))/365.25)

 
 there have been 506 starting lineups for 1st game of #premierleague seasons
 How many times have these played the most for their club that season
 

# 2 SUBS SCORING ----------------------------------------------------------

 sort(names(playerGame))
 
  playerGame %>% 
   filter(subOn>0&Gls>0) %>% 
   group_by(TEAMMATCHID,gameDate,TEAMNAME) %>% 
   tally()%>% 
     filter(n>1) %>% 
    arrange(desc(n),TEAMNAME) %>% 
    ungroup() %>% 
    group_by(TEAMNAME) %>% 
    tally() %>% 
    DT::datatable()
 
def image possibility

sort(names(teamGames))
head(teamGames)


sevens <- teamGames %>% 
  ungroup() %>% 
  filter(tmYrGameOrder==1) %>% 
  group_by(MATCHID) %>% 
  summarize(totGls=sum(GOALS)) %>% 
  arrange(desc(totGls)) %>% 
  filter(totGls==7) %>% 
  pull(MATCHID)

teamGames %>% 
  ungroup() %>% 
  filter(MATCHID %in% sevens) %>% 
  arrange(MATCHID,desc(venue)) %>% 
  select(MATCHID,season,venue,TEAMNAME,GOALS) 
@ would be nice to tidy up


# 2017-8 possibilities ----------------------------------------------------

crowd changes
foreignerd - uddersfield - german born # could do both week 1 and season average
time taken for 3 goal away win prem



sort(names(teamGames))
sort(names(playerGame))
sort(names(standings))

library(plotly)

## crowd mean of first 2 rounds  - might be better after next week
teamGames %>% 
  ungroup() %>% 
  filter(tmYrGameOrder<3) %>% 
  group_by(season) %>% 
  select(season,CROWD) %>% 
  summarise(games=n(),avCrowd=sum(CROWD)/(2*games)) %>% 
  plot_ly(x=~season,y=~avCrowd) %>% 
  add_lines()

## dropped last year but smaller clubs in and NEW out - start graph from zero
# mebbe look at boxplot as well

## foreign

games <- teamGames %>% 
  ungroup() %>% 
  filter(tmYrGameOrder==1) %>% 
  pull(MATCHID)

playerGame %>% 
  filter(MATCHID %in% games&START>0) %>% 
  group_by(season,COUNTRY) %>% 
  summarise(tot=n()) %>% 
  group_by(season) %>% 
  mutate(pc=round(100*tot/sum(tot),1)) %>% 
  filter(COUNTRY=="England") %>% 
  ungroup() %>% 
  plot_ly(x=~season,y=~pc) %>% 
  add_lines()
# maybe look at just G1

## away games no big win - check how many teams alltogether unique(standings$team)

allTeams <-standings %>% 
  arrange(gameDate) %>% 
  filter(venue=="A") %>% 
  group_by(team) %>% 
  mutate(awayGameOrder=row_number()) %>% 
    filter((GF-GA)>=3) %>% 
  group_by(team) 
  


#allTeamsAway <- allTeams

## want somehow to add in number of games by teams that never did it - come back to
didIt <-allTeams %>% 
    slice(1) #39

didIt %>% 
    select(team,awayGameOrder) %>% 
    plot_ly(x=~awayGameOrder,y=~team)

setdiff(unique(standings$team),unique(didIt$team))

never <-setdiff(unique(standings$team),unique(didIt$team))
[1] "Sheff. Utd."  "Oldham"       "Swindon T"    "Birmingham C" "Watford"      "Wolves"       "Bradford C"  
[8] "Cardiff C"    "Barnsley"     "Huddersfield" "Brighton" 
sort(names(standings))
neverTeams <- standings %>% 
  filter(team %in% never&venue=="A") %>% 
  group_by(team) %>% 
  arrange(tmGameOrder) %>% 
  mutate(awayGameOrder=row_number()) %>% 
  arrange(desc(awayGameOrder)) %>% 
  slice(1) %>% 
  select(team,awayGameOrder) %>% 
  mutate(ever="Never")

didItTeams <- didIt %>% 
  select(team,awayGameOrder) %>% 
  mutate(ever="First")

df <- rbind(neverTeams,didItTeams)

## for chart
df$team <-  factor(df$team, levels = df$team[order(df$awayGameOrder)])

str(df)
  
df %>% 
  plot_ly(x=~awayGameOrder,y=~team,height=1000) %>% 
  add_bars(color=~ever) %>% 
  layout(title=" EPL Away Games before 3 goal victory",
         margin=list(l=120),
         xaxis=list(title="Games"),
         yaxis=list(title="")
         
         ) %>%  config(displayModeBar = F,showLink = F)



# just need stidying up
standings %>% 
  
  filter(venue=="A" & team %in% never) %>% 
  count(team)

Birmingham 1337 seasons
## followin manu 
test <-standings %>% 
  arrange(gameDate) %>% 
  group_by(team) %>% 
    filter((GF-GA)>=4) %>% 
  group_by(team) %>% 
  tally() %>% 
  arrange(desc(n))
  
mnu <- standings %>% 
  arrange(gameDate) %>% 
  group_by(team) %>% 
  filter((GF-GA)>=4&team=="Man. Utd.") %>% 
  select(gameDate,tmGameOrder) %>% 
  mutate(last=lag(tmGameOrder)) %>% 
  mutate(last=(ifelse(is.na(last),0,last)),diff=tmGameOrder-last,order=row_names()-1) %>% 
  plot_ly(x=~order,y=~diff) %>% 
  add_bars(color=I("red")) %>% 
  layout(title="Man Utd sequences between 4 goal victories",
         xaxis=list(title=""),
         axis=list(title="")) %>%  config(displayModeBar = F,showLink = F)


# most goals as sub -------------------------------------------------------

asSub <- playerGame %>% 
  filter(START==0&PLAYERID=="DEFOEJ") %>% 
  group_by(name,PLAYERID,TEAMNAME) %>% 
  summarise(sub=sum(Gls)) 

asStarter<- playerGame %>% 
  filter(START!=0&PLAYERID=="DEFOEJ") %>% 
  group_by(name,PLAYERID,TEAMNAME) %>% 
  summarise(starter=sum(Gls)) %>% 
  right_join(asSub) %>% 
  ungroup() %>% 
  select(team=TEAMNAME,starter,sub)   %>%

                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# winning without Zaha ----------------------------------------------------



sort(names(playerGame))
sort(names(standings))

zaha <- playerGame %>% 
  filter(PLAYERID=="ZAHAW"&mins>0&TEAMNAME=="Crystal P")

test <-standings %>% 
  filter(team=="Crystal P"&gameDate>="2014-08-30") %>% 
  left_join(zaha) %>% 
  select(name,team,res,gameDate,tmGameOrder)
 arrange(desc(gameDate))

 # actually not too bad without him
 

# eriksen lst -------------------------------------------------------------

 sort(names(goals))
sort(names(playerGame))

goals %>% 
  left_join(playerGame) %>% 
  filter(PLAYERID=="ERIKSEC"&PLAY=="Direct_FK") %>% 
  select(gameDate)



# headed goals by season --------------------------------------------------

sort(names(teamGames))

temp <- goals %>% 
  left_join(playerGame) %>% 
  left_join(teamGames) %>% 
  filter(tmYrGameOrder<8) %>% 
  group_by(season) %>% 
  tally()


temp <- standings %>% 
  filter(tmYrGameOrder<=7) %>% 
  group_by(season) %>% 
  summarise(gls=sum(GF))
## 173 in total to date

x <- goals %>% 
  left_join(playerGame) %>% 
  left_join(teamGames,by="TEAMMATCHID") %>% 
  select(season.x,METHOD,tmYrGameOrder) %>% 
  filter(tmYrGameOrder<=7) %>%
  select(season.x,METHOD) %>% 
  group_by(season.x,METHOD) %>% 
  tally()  #173


y <- goals %>% 
  left_join(playerGame) %>% 
  filter(season=="2016/17"&METHOD=="Head") %>% 
  count(PLAYERID, sort=TRUE)


# ROONEY OUTSIDE AREA -----------------------------------------------------



test <-goals %>% 
  left_join(playerGame) %>% 
  filter(PLAYERID=="ROONEYX") %>% 
  arrange(desc(gameDate))

## not that long ago

