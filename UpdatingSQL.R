  #updates SQL and creates rds tables that can go directly to global
  
  #NB change milestone date each week and should be first date of round as we are using it for less than
  milestoneDate <- "2016-11-05"
  
  library(RODBC)
  
  library(stringr)
  library(lubridate)
  library(ggvis)
  library(ggplot2)
  library(shiny)
  library(RSQLite)
  library(RSQLite.extfuns)
  library(readr)
  library(daff)
  
  library(dplyr)
  
  #setwd("C:/Users/pssguy/Documents/R/premierLeague")
  
  channel <- odbcConnect("eplR")
  
  managers <- sqlQuery(channel,paste(
    "   SELECT        dbo.tblManagerBasic.Lastname, dbo.tblManagerBasic.FirstName, dbo.tblManagers.ManagerID, dbo.tblManagers.TeamID, dbo.tblManagers.Joined, dbo.tblManagers.[Left],
                           dbo.tblTeam_Names.TEAMNAME,dbo.tblManagers.Caretaker,dbo.tblManagers.ManagerTeam
  FROM            dbo.tblTeam_Names INNER JOIN
                           dbo.tblManagers ON dbo.tblTeam_Names.TEAMID = dbo.tblManagers.TeamID INNER JOIN
                           dbo.tblManagerBasic ON dbo.tblManagers.ManagerID = dbo.tblManagerBasic.ManagerID"
  
  ),stringsAsFactors=F)
  
  
  match <- sqlQuery(channel,paste(
    "  SELECT dbo.tblMatch.DATE,dbo.tblMatch.CROWD,dbo.tblMatch.REFEREE,dbo.tblMatch.MATCHID
  FROM dbo.tblMatch
  
     "
  ),stringsAsFactors=F)
  
  
  ## looks like dbo. is not necessary though does not cause errors
  matchTeam <- sqlQuery(channel,paste(
    "  SELECT tblMatchTeam.TEAMMATCHID,tblMatchTeam.MATCHID,tblMatchTeam.TEAMID,tblMatchTeam.[HOME/AWAY] as venue,
       tblMatchTeam.GOALS
  FROM  tblMatchTeam
  
     "
  ),stringsAsFactors=F)
  
  teams <- sqlQuery(channel,paste(
    "  SELECT tblTeam_Names.TEAMID,tblTeam_Names.TEAMNAME
  FROM tblTeam_Names
  
     "
  ),stringsAsFactors=F)
  
  # need to write lat lon to table - may be overwriting prob
  players <- sqlQuery(channel,paste(
    "  SELECT tblPlayers.PLAYERID,tblPlayers.FIRSTNAME,tblPlayers.LASTNAME,tblPlayers.BIRTHDATE,tblPlayers.PLACE,
  tblPlayers.COUNTRY,tblPlayers.POSITION,tblPlayers.lat,tblPlayers.lon
  FROM tblPlayers
  
     "
  ),stringsAsFactors=F)
  
  
  playerClub <- sqlQuery(channel,paste(
    "  SELECT tblPlayerClub.PLAYERID,tblPlayerClub.TEAMID,tblPlayerClub.JOINED,tblPlayerClub.[LEFT],
  tblPlayerClub.PERMANENT,tblPlayerClub.FEE,tblPlayerClub.FEEOUT,tblPlayerClub.PLAYER_TEAM
  FROM tblPlayerClub
  
     "
  ),stringsAsFactors=F)
  
  
  playerMatch <- sqlQuery(channel,paste(
    "  SELECT tblPlayer_Match.PLAYER_MATCH,tblPlayer_Match.TEAMMATCHID,tblPlayer_Match.PLAYER_TEAM,tblPlayer_Match.START,
  tblPlayer_Match.[OFF],tblPlayer_Match.[ON] as subOn,tblPlayer_Match.GOALS,tblPlayer_Match.PENS,
  tblPlayer_Match.CARD,tblPlayer_Match.OwnGoal,tblPlayer_Match.MissedPenalty
  FROM tblPlayer_Match
  
     "
  ),stringsAsFactors=F)
  #str(playerMatch)
  
  goals <- sqlQuery(channel,paste(
    "  SELECT tblGoals.PLAYER_MATCH,tblGoals.TIME,tblGoals.METHOD,tblGoals.PLACE,
  tblGoals.PLAY,tblGoals.PLAYER_MATCH_GOAL
  FROM tblGoals
  
     "
  ),stringsAsFactors=F)
  
  write_csv(goals,"goals1.csv")
  
  assists<- sqlQuery(channel,paste(
    "  SELECT tblAssists.PLAYER_MATCH,tblAssists.PLAYER_MATCH_GOAL
  FROM tblAssists
  
     "
  ),stringsAsFactors=F)
  
  odbcClose(channel)
  
  conn <- dbConnect(SQLite(), dbname = "pss.sqlite3")
  dbGetInfo(conn)
  dbListTables(conn) #[1] "assists"     "goals"       "match"       "matchTeam"   "playerClub"  "playerMatch" "players"     "teams" 
  
  if(dbExistsTable(conn, "managers")){
    dbRemoveTable(conn, "managers")
    dbWriteTable(conn, "managers", managers)
  } else {
    dbWriteTable(conn, "managers", managers )
  } 
  
  
  if(dbExistsTable(conn, "teams")){
    dbRemoveTable(conn, "teams")
    dbWriteTable(conn, "teams", teams)
  } else {
    dbWriteTable(conn, "teams", teams )
  } 
  
  if(dbExistsTable(conn, "matchTeam")){
    dbRemoveTable(conn, "matchTeam")
    dbWriteTable(conn, "matchTeam", matchTeam)
  } else {
    dbWriteTable(conn, "matchTeam", matchTeam )
  } 
  
  if(dbExistsTable(conn, "match")){
    dbRemoveTable(conn, "match")
    dbWriteTable(conn, "match", match)
  } else {
    dbWriteTable(conn, "match", match )
  } 
  
  if(dbExistsTable(conn, "players")){
    dbRemoveTable(conn, "players")
    dbWriteTable(conn, "players", players)
  } else {
    dbWriteTable(conn, "players", players )
  } 
  
  if(dbExistsTable(conn, "playerClub")){
    dbRemoveTable(conn, "playerClub")
    dbWriteTable(conn, "playerClub", playerClub)
  } else {
    dbWriteTable(conn, "playerClub", playerClub )
  } 
  
  if(dbExistsTable(conn, "playerMatch")){
    dbRemoveTable(conn, "playerMatch")
    dbWriteTable(conn, "playerMatch", playerMatch)
  } else {
    dbWriteTable(conn, "playerMatch", playerMatch )
  } 
  
  if(dbExistsTable(conn, "goals")){
    dbRemoveTable(conn, "goals")
    dbWriteTable(conn, "goals", goals)
  } else {
    dbWriteTable(conn, "goals", goals )
  } 
  
  if(dbExistsTable(conn, "assists")){
    dbRemoveTable(conn, "assists")
    dbWriteTable(conn, "assists", assists)
  } else {
    dbWriteTable(conn, "assists", assists )
  } 
  
  dbListTables(conn) # 2335kb  there may be max
  dbDisconnect(conn)
  
  
  #### create the rds tables
  
  library(shiny)
  library(readr)
  library(doBy) # uses MASS which has a eselect conflict with dplyr
  library(dplyr) # this masks select from MASS, filter from stats and intersect etc from base
  library(ggvis)
  library(RSQLite)
  library(lubridate)
  library(stringr)
  library(markdown)
  library(tidyr)
  library(shinyBS)
  library(ggplot2)
  library(leaflet)
  library(rCharts)
  
  #conn <- dbConnect("SQLite", dbname = "pss.sqlite3") #old version of RSQLIte
  conn <- dbConnect(SQLite(), dbname = "pss.sqlite3")
  # dbGetInfo(conn)
  # dbListTables(conn)
  
  ##### ?? is there playerMatch.subOn looks like ON also HOME/AWAY instead of venue
  playerGame <- tbl_df(dbGetQuery(conn, "SELECT  players.FIRSTNAME, players.LASTNAME, players.BIRTHDATE,players.PLACE as city,players.COUNTRY,players.POSITION,     match.DATE,matchTeam.TEAMMATCHID,match.MATCHID,matchTeam.venue,teams.TEAMNAME,
                                  playerMatch.START, playerMatch.[OFF], playerMatch.subOn, playerMatch.GOALS,  playerClub.PLAYERID, playerClub.FEE,playerClub.PERMANENT, playerClub.LEFT,playerClub.JOINED,
                                  playerMatch.PENS, playerMatch.CARD, playerMatch.OwnGoal, playerMatch.MissedPenalty,playerMatch.PLAYER_MATCH
                                  
                                  FROM players INNER JOIN
                                  playerClub ON players.PLAYERID=playerClub.PLAYERID INNER JOIN
                                  playerMatch ON playerClub.PLAYER_TEAM = playerMatch.PLAYER_TEAM INNER JOIN 
                                  matchTeam ON playerMatch.TEAMMATCHID= matchTeam.TEAMMATCHID INNER JOIN
                                  match ON matchTeam.MATCHID = match.MATCHID INNER JOIN
                                  teams ON matchTeam.TEAMID = teams.TEAMID  "
                                  
                                  
  ))
  
  
  allPlayers <- tbl_df(dbGetQuery(conn, "SELECT  *
                                  FROM players"
                                  
                                  
  ))
  
  ### confirms there are only 9 passed across instead of required 16 until sorted in access so that playerid is in upper
  # playerGameTest <- tbl_df(dbGetQuery(conn, "SELECT  players.FIRSTNAME, players.LASTNAME, players.BIRTHDATE,players.PLACE as city,players.COUNTRY,players.POSITION,     match.DATE,matchTeam.TEAMMATCHID,match.MATCHID,matchTeam.venue,teams.TEAMNAME,
  #                                 playerMatch.START, playerMatch.[OFF], playerMatch.subOn, playerMatch.GOALS,  playerClub.PLAYERID, playerClub.FEE,playerClub.PERMANENT, playerClub.LEFT,playerClub.JOINED,
  #                                 playerMatch.PENS, playerMatch.CARD, playerMatch.OwnGoal, playerMatch.MissedPenalty,playerMatch.PLAYER_MATCH
  #                                 
  #                                 FROM players INNER JOIN
  #                                 playerClub ON players.PLAYERID=playerClub.PLAYERID INNER JOIN
  #                                 playerMatch ON playerClub.PLAYER_TEAM = playerMatch.PLAYER_TEAM INNER JOIN 
  #                                 matchTeam ON playerMatch.TEAMMATCHID= matchTeam.TEAMMATCHID INNER JOIN
  #                                 match ON matchTeam.MATCHID = match.MATCHID INNER JOIN
  #                                 teams ON matchTeam.TEAMID = teams.TEAMID  
  #                                      WHERE playerMatch.TEAMMATCHID = 29304"
  #                                 
  #                                 
  # ))
  # 
  # 
  # playerGameTest2 <- tbl_df(dbGetQuery(conn, "SELECT   playerMatch.START,playerMatch.PLAYER_TEAM
  # 
  #                                 FROM players INNER JOIN
  #                                 playerClub ON players.PLAYERID=playerClub.PLAYERID INNER JOIN
  #                                 playerMatch  ON playerClub.PLAYER_TEAM = playerMatch.PLAYER_TEAM 
  #                                      WHERE playerMatch.TEAMMATCHID = 29304"
  #                                     
  #                                     
  # ))
  
  managers <- tbl_df(dbGetQuery(conn, "SELECT managers.Lastname, managers.FirstName, managers.ManagerID, managers.TeamID, managers.Joined, managers.[Left], 
                                managers.TEAMNAME,managers.Caretaker,managers.ManagerTeam
                                
                                FROM managers "
                                
                                
  ))
  
  managers$Left <- as.Date(managers$Left/(60*60*24), origin= '1970-01-01')
  managers$Joined <- as.Date(managers$Joined/(60*60*24), origin= '1970-01-01')
  
  
  assistsPlayerGame <- tbl_df(dbGetQuery(conn, "SELECT        playerMatch.TEAMMATCHID, playerClub.PLAYERID, COUNT(assists.PLAYER_MATCH_GOAL) AS Assists
                                         FROM            assists INNER JOIN
                                         playerMatch ON assists.PLAYER_MATCH = playerMatch.PLAYER_MATCH INNER JOIN
                                         playerClub ON playerMatch.PLAYER_TEAM = playerClub.PLAYER_TEAM
                                         GROUP BY playerMatch.TEAMMATCHID, playerClub.PLAYERID "
                                         
                                         
  ))
  
  # goals <- tbl_df(dbGetQuery(conn, "SELECT        PLAYER_MATCH, TIME, METHOD, PLACE, PLAY, PLAYER_MATCH_GOAL
  # FROM            goals  "
  #                            
  #                            
  # ))
  
  goals <- tbl_df(dbGetQuery(conn, "SELECT        playerMatch.PLAYER_MATCH, goals.TIME, goals.METHOD, goals.PLACE, goals.PLAY, goals,PLAYER_MATCH_GOAL,
                             playerMatch.TEAMMATCHID
                             FROM            goals INNER JOIN playerMatch 
                             ON goals.PLAYER_MATCH = playerMatch.PLAYER_MATCH"
                             
                             
  ))
  
  write_csv(goals,"goals2.csv")
  
  goalsFor <- tbl_df(dbGetQuery(conn, "SELECT        teams.TEAMNAME AS team, match.DATE, goals.TIME, goals.METHOD, goals.PLACE, goals.PLAY,
                                matchTeam.MATCHID,matchTeam.TEAMMATCHID
                                FROM            goals INNER JOIN
                                playerMatch ON goals.PLAYER_MATCH = playerMatch.PLAYER_MATCH INNER JOIN
                                matchTeam ON playerMatch.TEAMMATCHID = matchTeam.TEAMMATCHID INNER JOIN
                                match ON matchTeam.MATCHID = match.MATCHID INNER JOIN
                                teams ON matchTeam.TEAMID = teams.TEAMID
                                "
  ))                              
  
  match <- tbl_df(dbGetQuery(conn, "SELECT * from match"))
  
  matchTeam <- tbl_df(dbGetQuery(conn, "SELECT matchTeam.TEAMMATCHID,matchTeam.MATCHID,matchTeam.venue,matchTeam.GOALS,
                                 teams.TEAMNAME,matchTeam.TEAMID
                                 from matchTeam INNER JOIN
                                 teams on teams.TEAMID= matchTeam.TEAMID"))
  # matchTeam <- tbl_df(dbGetQuery(conn, "SELECT matchTeam.TEAMMATCHID, matchTeam.MATCHID, teams.TEAMNAME
  # FROM matchTeam INNER JOIN
  #       teams ON matchTeam.TEAMID=teams.TEAMID
  #                                "))
  dbDisconnect(conn)
  
  # rename
  goals[goals$METHOD=="L",]$METHOD <- "Left"
  goals[goals$METHOD=="R",]$METHOD <- "Right"
  goals[goals$METHOD=="H",]$METHOD <- "Head"
  goals[goals$PLAY=="O",]$PLAY <- "Open"
  goals[goals$PLAY=="C",]$PLAY <- "Corner"
  goals[goals$PLAY=="P",]$PLAY <- "Penalty"
  goals[goals$PLAY=="D",]$PLAY <- "Direct_FK"
  goals[goals$PLAY=="I",]$PLAY <- "Indirect_FK"
  goals[goals$PLAY=="T",]$PLAY <- "Throw"
  goals[goals$PLACE=="G",]$PLACE <- "6_Yd_Box"
  goals[goals$PLACE=="P",]$PLACE <- "Pen_Area"
  goals[goals$PLACE=="O",]$PLACE <- "Long_Range"
  
  playerGame$POSITION[which(playerGame$POSITION=="G")] <- "Goalkeeper"
  playerGame$POSITION[which(playerGame$POSITION=="D")] <- "Defender"
  playerGame$POSITION[which(playerGame$POSITION=="M")] <- "Midfielder"
  playerGame$POSITION[which(playerGame$POSITION=="F")] <- "Forward"
  
  
  
  match$gameDate <- as.Date(match$DATE/(60*60*24), origin= '1970-01-01')
  
  playerGame$gameDate <- as.Date(playerGame$DATE/(60*60*24), origin= '1970-01-01')
  playerGame$DATE <- NULL
  playerGame$born <- as.Date(playerGame$BIRTHDATE/(60*60*24), origin= '1970-01-01')
  playerGame$BIRTHDATE <- NULL
  playerGame$joined <- as.Date(playerGame$JOINED/(60*60*24), origin= '1970-01-01')
  playerGame$JOINED <- NULL
  playerGame$left <- as.Date(playerGame$LEFT/(60*60*24), origin= '1970-01-01')
  playerGame$LEFT <- NULL
  
  
  years <- c(1992:2017)
  playerGame$season <- as.character(cut(playerGame$gameDate,  breaks=as.Date(paste(years,"-08-01",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))
  match$season <- as.character(cut(match$gameDate,  breaks=as.Date(paste(years,"-08-01",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))
  
  
  playerGame <- playerGame %>%
    left_join(assistsPlayerGame)
  playerGame$Assists[which(is.na(playerGame$Assists))] <- 0
  
  
  playerGame[is.na(playerGame$FIRSTNAME),]$FIRSTNAME <-""
  playerGame$name <- paste0(playerGame$FIRSTNAME," ",playerGame$LASTNAME)
  # can drop firstname for sure
  playerGame$FIRSTNAME <- NULL
  
  playerGame <-playerGame %>%
    mutate(age=(gameDate-born)/365.25)
  
  
  
  teamGames <- matchTeam %>%  #17352
    inner_join(match)
  
  teamGames$gameDate <- as.Date(teamGames$DATE/(60*60*24), origin= '1970-01-01')
  teamGames$DATE <- NULL
  
  years <- c(1992:2017)
  teamGames$season <- as.character(cut(teamGames$gameDate,  breaks=as.Date(paste(years,"-08-01",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))
  
  teamGames <-teamGames %>%
    arrange(gameDate) %>%
    group_by(TEAMNAME) %>%
    mutate(tmGameOrder=row_number()) %>% # suddenlu got Error in rank(x, ties.method = "first", na.last = "keep") : 
    #argument "x" is missing, with no default. OK after restarting R
    group_by(TEAMNAME,season) %>%
    mutate(tmYrGameOrder=row_number()) 
  
  ## these inc bench games so may want to thinkaboutit
  playerGame <-playerGame %>%
    arrange(gameDate) %>%
    group_by(PLAYERID)  %>%
    mutate(plGameOrder=row_number()) %>%
    
    group_by(PLAYERID,TEAMNAME)   %>% 
    mutate(plTmGameOrder=row_number()) %>%
    group_by(PLAYERID,season) %>%
    mutate(plYrGameOrder=row_number()) %>%
    group_by(PLAYERID,season,TEAMNAME) %>%
    mutate(plYrTmGameOrder=row_number())
  
  
  ## add categories for actually playing need to look at goals as well
  
  playerGame <-playerGame %>%
   # filter((START+subOn)>0) %>%
    arrange(gameDate) %>%
    group_by(PLAYERID)  %>%
    mutate(plGameOrderApp=row_number()) %>%
    
    group_by(PLAYERID,TEAMNAME)   %>% 
    mutate(plTmGameOrderApp=row_number()) %>%
    group_by(PLAYERID,season) %>%
    mutate(plYrGameOrderApp=row_number()) %>%
    group_by(PLAYERID,season,TEAMNAME) %>%
    mutate(plYrTmGameOrderApp=row_number())
  
  playerGame$mins <- 90
  ## never appeared
  playerGame[playerGame$START==0&playerGame$subOn==0,]$mins <- 0
  
  # came on as sub
  playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 91-playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$subOn
  
  #mean(playerGame[playerGame$season>"1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins) #22 so assume applies at that value to all pre 1999
  playerGame[playerGame$season<="1998/99"&playerGame$START==0&playerGame$subOn>0,]$mins <- 22
  
  # starter removed
  playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0,]$OFF-1 # to take account of 90 min withdrawals
  playerGame[playerGame$season<="1998/99"&playerGame$OFF>0,]$mins <- 68
  
  ## sub removed
  playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$OFF-playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$subOn
  #mean(playerGame[playerGame$season>"1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins) #34
  playerGame[playerGame$season<="1998/99"&playerGame$OFF>0&playerGame$subOn>0,]$mins <- 34
  
  
  ## combine with match info
  #glimpse(playerGame) #names(playerGame)
  #glimpse(teamGames) #17352 NB GOALS are different values
  colnames(playerGame)[[12]] <- "Gls" ## need to check thios got into trouble deleting PLAYERID
  
  ## this was in original but want to add opponents
  #playerGame <-playerGame %>%
  #  left_join(teamGames)
  
  playerGame <- data.frame(playerGame)
  
  # need to construct a temp teamGames with just limited info
  # otherwise bogus columns get carried across
  #glimpse(teamGames)
  tg <- teamGames %>%
    select(MATCHID,Opponents=TEAMNAME) # carrying season É isthat rwmname
  
  
  playerGame <-playerGame %>%
    left_join(tg) %>%
    filter(TEAMNAME!=Opponents) 
  # playerGame <-playerGame %>%
  #   left_join(teamGames, by="MATCHID") %>%
  #   filter(TEAMNAME.x!=TEAMNAME.y) %>%
  #   rename(TEAMNAME=TEAMNAME.x,Opponents=TEAMNAME.y)
  
  
  
  # create a new df just for gamedispaly info. No prob just better to add some new cols
  #glimpse(playerGame)
  playerGame <- data.frame(playerGame)
  print(str(playerGame))
  print("str")
  
  
  # day(playerGame$gameDate)
  # month(playerGame$gameDate, label = TRUE, abbr = TRUE)
  # wday(playerGame$gameDate, label = TRUE, abbr = TRUE)
  
  playerGame$day <- paste(wday(playerGame$gameDate, label = TRUE, abbr = TRUE),day(playerGame$gameDate),
                          month(playerGame$gameDate, label = TRUE, abbr = TRUE),
                          sep=" ")
  playerGame$st <- "x"
  playerGame[playerGame$START==0,]$st <- ""
  # may want separte df but would be big
  playerGame$on <- playerGame$subOn
  playerGame[playerGame$subOn==0,]$on <- ""
  playerGame$off <- playerGame$OFF
  playerGame[playerGame$OFF==0,]$off <- ""
  # playerGame[playerGame$GOALS==0,]$GOALS <- ""
  
  #print(playerGame$subOn)
  #print(playerGame$OFF)
  glimpse(playerGame) # why has PLAYERID disappeared as has firstname
  str(playerGame) # 40 variables
  
  ## want to add various playerGame sequences
  
  #playerGame[playerGame$Assists==0,]$Assists <- ""
  
  #str(playerGame[playerGame$Assists==0,]$Assists)
  
  pgMini <- playerGame %>%
    select(PLAYERID,name)
  
  pgMini <- unique(pgMini)
  
  playerChoice <- pgMini$PLAYERID
  names(playerChoice) <- pgMini$name
  rm(pgMini)
  
  # sett up fdataframe of minutes
  allTimes <- data.frame(TIME=c(1:90))
  allTimes$dummy <- 1
  
  
  # 
  teamsChoice <- sort(unique(playerGame$TEAMNAME))
  teamsChoice_2 <- c("All Teams",teamsChoice)
  
  seasonChoice <- sort(unique(playerGame$season), decreasing = TRUE)
  seasonChoice_2 <- c("All Seasons",seasonChoice)
  
  
  #### calc of summary data to be used for ind player or team by year 
  starts <-playerGame %>%
    filter(START>0) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(St=n(),StGls=sum(Gls),startPens=sum(PENS))  
  
  off <-playerGame %>%
    filter(OFF>0) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(Off=n())
  
  on <-playerGame %>%
    filter(subOn>0) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(On=n(),subPens=sum(PENS),subGls=sum(Gls))
  
  bench <-playerGame %>%
    filter(subOn==0&START==0) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(Bench=n())
  
  Y <-playerGame %>%
    filter(CARD=="Y") %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(Y=n())
  
  R <-playerGame %>%
    filter((CARD=="P"|CARD=="R"|CARD=="X"|CARD=="Z")) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID) %>%
    summarise(R=n())
  print("r done")
  
  
  summary <- playerGame %>%
    #filter(PLAYERID==thePlayer) %>%
    group_by(TEAMNAME,season,name,LASTNAME,PLAYERID,born,left,POSITION) %>%
    summarise(OG=sum(OwnGoal),MP=sum(MissedPenalty),Assists=sum(Assists),mins=sum(mins)) %>%
    left_join(starts) %>%
    left_join(off) %>%
    left_join(on) %>%
    left_join(Y) %>%
    left_join(R) %>%
    left_join(bench)
  #str(summary)
  #summary <- data.frame(summary)
  summary[is.na(summary$Y),]$Y <- 0 # Error in as.Date.numeric(value) : 'origin' must be supplied
  # so may need to do all individ
  summary[is.na(summary$St),]$St <- 0
  summary[is.na(summary$StGls),]$StGls <- 0
  
  summary[is.na(summary$startPens),]$startPens <- 0
  summary[is.na(summary$Off),]$Off <- 0
  summary[is.na(summary$On),]$On <- 0
  summary[is.na(summary$subPens),]$subPens <- 0
  summary[is.na(summary$subGls),]$subGls <- 0
  summary[is.na(summary$R),]$R <- 0
  summary[is.na(summary$Bench),]$Bench <- 0
  summary[is.na(summary$OG),]$OG <- 0
  summary[is.na(summary$MP),]$MP <- 0# calculate max games in current season - still need to sort
  thisYearGames <- 7 #NB
  
  ## leader by club calc
  # goals
  
  temp <-summary %>%
    ungroup() %>% 
    filter(PLAYERID!="OWNGOAL") %>%
    mutate(goals=StGls+subGls) %>%
      select(PLAYERID,name,StGls,subGls,goals,TEAMNAME,season)
  
  glsTeamSeason <-temp %>% # cannot call goals already df which need to use later
    arrange(desc(goals)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    slice(1) %>%
    ungroup() %>% 
    #rename(n1=name,v1=goals) %>%
    mutate(n1=ifelse(goals==0,"",name),v1=goals) %>%
    mutate(goals=paste(n1,v1)) %>%
    select(TEAMNAME,season,goals)
  
 
  
  temp <-summary %>%
    ungroup() %>% 
      select(PLAYERID,name,Assists,TEAMNAME,season)
  #temp <- data.frame(temp) # has all attribyres in
  
  # assists <-temp %>%
  #   arrange(desc(Assists)) %>%
  #   group_by(TEAMNAME,season,name,PLAYERID) %>%
  #   ungroup() %>% 
  #   group_by(season,TEAMNAME) %>% 
  #   slice(1) %>%
  #   ungroup() %>% 
  #   rename(n1=name,v1=Assists) %>%
  #   mutate(assists=paste(n1,v1)) %>%
  #   select(TEAMNAME,season,assists)
  
  
  # take account of zero goals assisst
  assists <-temp %>%
    arrange(desc(Assists)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    #filter(season=="2016/17") %>% 
    slice(1) %>%
    ungroup() %>% 
    mutate(n1=ifelse(Assists==0,"",name),v1=Assists) %>%
    mutate(assists=paste(n1,v1)) %>%
    select(TEAMNAME,season,assists)
  
  #glimpse(summary)
  
  #points
  temp <-summary %>%
    ungroup() %>% 
    filter(PLAYERID!="OWNGOAL") %>%
    mutate(points=Assists+StGls+subGls) %>%
    select(PLAYERID,name,points,TEAMNAME,season)
  #temp <- data.frame(temp) # has all attribyres in
  points <-temp %>%
    arrange(desc(points)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    slice(1) %>%
    ungroup() %>% 
    rename(n1=name,v1=points) %>%
    mutate(points=paste(n1,v1)) %>%
    select(TEAMNAME,season,points)
  
  
  #sub
  # temp <-summary %>%
  #   select(On) 
  # temp <- data.frame(temp)
  # sub <-  temp  %>% 
  #   arrange(desc(On)) %>%
  #   group_by(TEAMNAME,season) %>%
  #   
  #   slice(1) %>%
  #   select(n1=name,v1=On) %>%
  #   mutate(sub=paste(n1,v1)) %>%
  #   select(TEAMNAME,season,sub)
  
  
  temp <-summary %>%
    ungroup() %>% 
    
    select(PLAYERID,name,On,TEAMNAME,season)
  
  sub <-temp %>%
    arrange(desc(On)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    slice(1) %>%
    ungroup() %>% 
    rename(n1=name,v1=On) %>%
    mutate(sub=paste(n1,v1)) %>%
    select(TEAMNAME,season,sub)
  
  #starts
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
  
  temp <-summary %>%
    ungroup() %>% 
    
    select(PLAYERID,name,St,TEAMNAME,season)
  
  starts <-temp %>%
    arrange(desc(St)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    slice(1) %>%
    ungroup() %>% 
    rename(n1=name,v1=St) %>%
    mutate(starts=paste(n1,v1)) %>%
    select(TEAMNAME,season,starts)
  
  
  #cards
  
  
  temp <-summary %>%
    ungroup() %>% 
    mutate(cards=Y+R) %>%
    select(PLAYERID,name,cards,TEAMNAME,season)
  # temp <-summary %>%
  #   
  #   mutate(cards=Y+R) %>%
  #   select(cards)
  # temp <- data.frame(temp) # has all attribyres in
  # cards <-temp %>%
  #   arrange(desc(cards)) %>%
  #   group_by(TEAMNAME,season) %>%
  #   
  #   slice(1) %>%
  #   select(n1=name,v1=cards) %>%
  #   mutate(cards=paste(n1,v1)) %>%
  #   select(TEAMNAME,season,cards)
  
  cards <-temp %>%
    arrange(desc(cards)) %>%
    group_by(TEAMNAME,season,name,PLAYERID) %>%
    ungroup() %>% 
    group_by(season,TEAMNAME) %>% 
    slice(1) %>%
    ungroup() %>% 
    rename(n1=name,v1=cards) %>%
    mutate(cards=paste(n1,v1)) %>%
    select(TEAMNAME,season,cards)
  
  
  leaders <- assists %>%
    inner_join(glsTeamSeason) %>%
    inner_join(points) %>%
    inner_join(cards) %>%
    inner_join(starts) %>%
    inner_join(sub) %>%
    select(TEAMNAME,season,starts,sub,goals,assists,points,cards) 
  
  ### goal by player summary ( may want to strip out to club and then recombine forplayer/team)
  playerSeason <- unique(playerGame %>%      ## NB was distinct which removed name
                             group_by(PLAYERID,season) %>%
                             select(name))
  
  SixYd <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLACE=="6_Yd_Box") %>%
    summarise(SixYd=n())
  
  PenArea<- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLACE=="Pen_Area") %>%
    summarise(PenArea=n())
  
  LongRange <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLACE=="Long_Range") %>%
    summarise(LongRange=n())
  
  Place <- playerSeason %>%
    left_join(SixYd) %>%
    left_join(PenArea) %>%
    left_join(LongRange)
  
  Place[is.na(Place$SixYd),]$SixYd <- 0
  Place[is.na(Place$PenArea),]$PenArea <- 0
  Place[is.na(Place$LongRange),]$LongRange <- 0
  
  Open <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Open") %>%
    summarise(Open=n())
  
  Corner <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Corner") %>%
    summarise(Corner=n())
  
  Throw <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Throw") %>%
    summarise(Throw=n())
  
  IFK <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Indirect_FK") %>%
    summarise(IFK=n())
  
  DFK <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Direct_FK") %>%
    summarise(DFK=n())
  
  Pen <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(PLAY=="Penalty") %>%
    summarise(Pen=n())
  
  
  Play <- playerSeason %>%
    left_join(Open) %>%
    left_join(Corner) %>%
    left_join(Throw) %>%
    left_join(IFK) %>%
    left_join(DFK) %>%
    left_join(Pen)
  
  Play[is.na(Play$Open),]$Open <- 0
  Play[is.na(Play$Corner),]$Corner <- 0
  Play[is.na(Play$Throw),]$Throw <- 0
  Play[is.na(Play$IFK),]$IFK <- 0
  Play[is.na(Play$DFK),]$DFK <- 0
  Play[is.na(Play$Pen),]$Pen <- 0
  
  saveRDS(Play,"Play.rds")
  
  Head <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(METHOD=="Head") %>%
    summarise(Head=n())
  
  Left <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,season) %>%
    filter(METHOD=="Left") %>%
    summarise(Left=n())
  
  Right <- goals %>%
    left_join(playerGame)  %>%  ##22955
    group_by(PLAYERID,name,season) %>%
    filter(METHOD=="Right") %>%
    summarise(Right=n())
  
  Method <- playerSeason %>%
    left_join(Right) %>%
    left_join(Left) %>%
    left_join(Head)
  
  Method[is.na(Method$Head),]$Head <- 0
  Method[is.na(Method$Right),]$Right <- 0
  Method[is.na(Method$Left),]$Left <- 0
  
  
  allGoalsPlayer <- Method %>%
    left_join(Place) %>%
    left_join(Play) %>%
    mutate(Tot=Right+Left+Head) %>%
    rename(Season=season) %>%
    
    select(16,4:15)
  
  
  
  ## standings for league tables
  
  oppGames <- teamGames %>%
    select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
    inner_join(teamGames)
  
  allMatches <-oppGames[oppGames$TEAMNAME!=oppGames$OppTeam,]
  
  allMatches <- allMatches %>%
    select(season,team=TEAMNAME,GF=GOALS,GA,gameDate,tmGameOrder,tmYrGameOrder,venue,MATCHID)
  allMatches$points <- 3
  allMatches[allMatches$GF==allMatches$GA,]$points <- 1
  allMatches[allMatches$GF<allMatches$GA,]$points <- 0
  
  ## come back and do head to head
  home <-allMatches %>%
    group_by(team,season) %>%
    filter(venue=="H")
  # #%>%
  #   arrange(tmYrGameOrder) %>%
  #   mutate(hcumGF=cumsum(GF),hcumGA=cumsum(GA),hcumPts=cumsum(points))
  
  away <-allMatches %>%
    group_by(team,season) %>%
    filter(venue=="A")
  
  # %>%
  #   arrange(tmYrGameOrder) %>%
  #   mutate(acumGF=cumsum(GF),acumGA=cumsum(GA),acumPts=cumsum(points))
  # does not have venue
  
  both <- rbind(home,away)
  
  both <-both %>%
    group_by(season,team) %>%
    arrange(tmYrGameOrder) %>%
    mutate(cumGF=cumsum(GF),cumGA=cumsum(GA),cumPts=cumsum(points),cumGD=cumGF-cumGA,allGames=max(tmYrGameOrder))
  
  standings <- both %>% #17352
    group_by(season,tmYrGameOrder) %>%
    arrange(desc(cumPts),desc(cumGD),desc(GF),team) %>%
    mutate(position=row_number())
  
  standings$res <- "Win"
  standings[standings$GF==standings$GA,]$res <- "Draw"
  standings[standings$GF<standings$GA,]$res <- "Loss"
  
  standings$tt <- sprintf("<table cellpadding='4' style='line-height:1'><tr>
                          <th>%1$s (%2$s)</th></tr>
                          <tr align='center'><td>%5$s</td></tr>
                          <tr align='center'><td>%3$s-%4$s</td></tr>
                          <tr align='center'><td>Pos: %6$s</td></tr>
                          </table>",
                          standings$OppTeam,
                          standings$venue,
                          standings$GF,
                          standings$GA,
                          standings$gameDate,
                          standings$position)
  
  ## addd final_pos
  ## NB needs to change for latest year if not all games played in round due to cup
  
  oldStandings <-standings %>%
    filter(season<"2016/17") %>% 
    group_by(season) %>%
    mutate(finalGame=max(tmYrGameOrder)) %>%
    filter(tmYrGameOrder==finalGame) %>%   
    select(final_Pos = position,team) %>%
    left_join(standings)
  
  
  
  completeRounds <-standings %>%
    filter(season=="2016/17") %>% 
    group_by(season,team) %>%
    mutate(finalGame=max(tmYrGameOrder)) %>%
    ungroup() %>% 
    arrange(finalGame) %>% 
    slice(1) %>% 
    select(finalGame)
  
  newStandings <-standings %>%
    filter(season=="2016/17") %>% 
    group_by(season) %>%
    # mutate(finalGame==completeRounds$finalGame) %>%
    filter(tmYrGameOrder==completeRounds$finalGame) %>%   
    select(final_Pos = position,team) %>%
    left_join(standings)
  
  # mow includes matchid
  standings <- rbind(oldStandings,newStandings)
  
  
  
  
  # oppGames <- teamGames %>%
  #   select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
  #   inner_join(teamGames)
  # 
  # allMatches <-oppGames[oppGames$TEAMNAME!=oppGames$OppTeam,]
  # 
  # allMatches <- allMatches %>%
  #   select(season,team=TEAMNAME,GF=GOALS,GA,gameDate,tmGameOrder,tmYrGameOrder,venue)
  # allMatches$points <- 3
  # allMatches[allMatches$GF==allMatches$GA,]$points <- 1
  # allMatches[allMatches$GF<allMatches$GA,]$points <- 0
  # 
  # ## come back and do head to head
  # home <-allMatches %>%
  #   group_by(team,season) %>%
  #   filter(venue=="H")
  # # #%>%
  # #   arrange(tmYrGameOrder) %>%
  # #   mutate(hcumGF=cumsum(GF),hcumGA=cumsum(GA),hcumPts=cumsum(points))
  # 
  # away <-allMatches %>%
  #   group_by(team,season) %>%
  #   filter(venue=="A")
  # 
  # # %>%
  # #   arrange(tmYrGameOrder) %>%
  # #   mutate(acumGF=cumsum(GF),acumGA=cumsum(GA),acumPts=cumsum(points))
  # # does not have venue
  # 
  # both <- rbind(home,away)
  # 
  # both <-both %>%
  #   group_by(season,team) %>%
  #   arrange(tmYrGameOrder) %>%
  #   mutate(cumGF=cumsum(GF),cumGA=cumsum(GA),cumPts=cumsum(points),cumGD=cumGF-cumGA,allGames=max(tmYrGameOrder))
  # 
  # standings <- both %>% #17352
  #   group_by(season,tmYrGameOrder) %>%
  #   arrange(desc(cumPts),desc(cumGD),desc(GF),team) %>%
  #   mutate(position=row_number())
  # 
  # standings$res <- "Win"
  # standings[standings$GF==standings$GA,]$res <- "Draw"
  # standings[standings$GF<standings$GA,]$res <- "Loss"
  # 
  # standings$tt <- sprintf("<table cellpadding='4' style='line-height:1'><tr>
  #                         <th>%1$s (%2$s)</th></tr>
  #                         <tr align='center'><td>%5$s</td></tr>
  #                         <tr align='center'><td>%3$s-%4$s</td></tr>
  #                         <tr align='center'><td>Pos: %6$s</td></tr>
  #                         </table>",
  #                         standings$OppTeam,
  #                         standings$venue,
  #                         standings$GF,
  #                         standings$GA,
  #                         standings$gameDate,
  #                         standings$position)
  # 
  # ## addd final_pos
  # ## NB needs to change for latest year if not all games played in round due to cup
  # 
  # oldStandings <-standings %>%
  #   filter(season<"2015/16") %>% 
  #   group_by(season) %>%
  #   mutate(finalGame=max(tmYrGameOrder)) %>%
  #   filter(tmYrGameOrder==finalGame) %>%   
  #   select(final_Pos = position,team) %>%
  #   left_join(standings)
  # 
  # 
  # 
  # completeRounds <-standings %>%
  #   filter(season=="2015/16") %>% 
  #   group_by(season,team) %>%
  #   mutate(finalGame=max(tmYrGameOrder)) %>%
  #   ungroup() %>% 
  #   arrange(finalGame) %>% 
  #   slice(1) %>% 
  #   select(finalGame)
  # 
  # newStandings <-standings %>%
  #   filter(season=="2015/16") %>% 
  #   group_by(season) %>%
  #  # mutate(finalGame==completeRounds$finalGame) %>%
  #   filter(tmYrGameOrder==completeRounds$finalGame) %>%   
  #   select(final_Pos = position,team) %>%
  #   left_join(standings)
  # 
  # standings <- rbind(oldStandings,newStandings)
  #   
  
  
  # goal categories
  cats <- c("Head","Right","Left","6_Yd_Box","Pen_Area","Long_Range","Open","Corner","Indirect_FK",
            "Direct_FK","Penalty","Throw")
  allCats <- data.frame(category=cats)
  
  
  # head to head df
  hth<-data.frame(standings %>%
                    select(team,OppTeam:gameDate,venue,points,res,MATCHID,season))
  hth$tmYrGameOrder <- NULL
  ## set up order for sequences (got to be prob after changing standings added team back in)
  hth <-data.frame(hth %>%
                     group_by(team,OppTeam) %>%
                     arrange(gameDate) %>%
                     mutate(gameOrder=row_number()))
  
  
  # latest week
  
  yrs <-sort(unique(standings$season), decreasing=T)
  
  currentYear <- yrs[1] #[1] "2014/15"
  lastYear <- yrs[2]
  currentRound <-max(standings[standings$season==currentYear,]$tmYrGameOrder)
  ## take account of few games
  if (currentRound<6) {
    currentValue <- currentRound
  } else {
    currentValue <- 6
  }
  
  
  tmYrs <-standings %>%
    select(season,team) %>%
    unique(.)
  
  
  ##### team Goals
  
  goalsFor[goalsFor$METHOD=="L",]$METHOD <- "Left"
  goalsFor[goalsFor$METHOD=="R",]$METHOD <- "Right"
  goalsFor[goalsFor$METHOD=="H",]$METHOD <- "Head"
  goalsFor[goalsFor$PLAY=="O",]$PLAY <- "Open"
  goalsFor[goalsFor$PLAY=="C",]$PLAY <- "Corner"
  goalsFor[goalsFor$PLAY=="P",]$PLAY <- "Penalty"
  goalsFor[goalsFor$PLAY=="D",]$PLAY <- "Direct_FK"
  goalsFor[goalsFor$PLAY=="I",]$PLAY <- "Indirect_FK"
  goalsFor[goalsFor$PLAY=="T",]$PLAY <- "Throw"
  goalsFor[goalsFor$PLACE=="G",]$PLACE <- "6_Yd_Box"
  goalsFor[goalsFor$PLACE=="P",]$PLACE <- "Pen_Area"
  goalsFor[goalsFor$PLACE=="O",]$PLACE <- "Long_Range"
  
  
  goalsFor$gameDate <- as.Date(goalsFor$DATE/(60*60*24), origin= '1970-01-01')
  
  
  years <- c(1992:2017)
  goalsFor$season <- as.character(cut(goalsFor$gameDate,  breaks=as.Date(paste(years,"-08-01",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))
  
  teamSeason <- distinct(playerGame %>%
                           group_by(TEAMNAME,season) %>%
                           select(team=TEAMNAME)) 
  
  
  head_team <-  goalsFor %>%
    group_by(season,team) %>%
    filter(METHOD=="Head") %>%
    summarise(Head=n()) 
  
  left_team <-  goalsFor %>%
    group_by(season,team) %>%
    filter(METHOD=="Left") %>%
    summarise(Left=n()) 
  
  right_team <-  goalsFor %>%
    group_by(season,team) %>%
    filter(METHOD=="Right") %>%
    summarise(Right=n())
  
  Method_team_for <- data.frame(teamSeason %>%
                                  left_join(right_team) %>%
                                  left_join(left_team) %>%
                                  left_join(head_team))
  #str(Method_team_for)
  
  if (nrow(Method_team_for[is.na(Method_team_for$Head),])>0) {
    Method_team_for[is.na(Method_team_for$Head),]$Head <- 0 
  }
  if (nrow(Method_team_for[is.na(Method_team_for$Left),])>0) {
    Method_team_for[is.na(Method_team_for$Left),]$Left <- 0 
  }
  if (nrow(Method_team_for[is.na(Method_team_for$Right),])>0) {
    Method_team_for[is.na(Method_team_for$Right),]$Right <- 0 
  }
  
  
  
  SixYd_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLACE=="6_Yd_Box") %>%
    summarise(SixYd=n())
  
  PenArea_team<- goalsFor %>%
    group_by(season,team) %>%
    filter(PLACE=="Pen_Area") %>%
    summarise(PenArea=n())
  
  LongRange_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLACE=="Long_Range") %>%
    summarise(LongRange=n())
  
  Place_team_for <- data.frame(teamSeason %>%
                                 left_join(SixYd_team) %>%
                                 left_join(PenArea_team) %>%
                                 left_join(LongRange_team))
  
  if (nrow(Place_team_for[is.na(Place_team_for$PenArea),])>0) {
    Place_team_for[is.na(Place_team_for$PenArea),]$PenArea <- 0 
  }
  if (nrow(Place_team_for[is.na(Place_team_for$SixYd),])>0) {
    Place_team_for[is.na(Place_team_for$SixYd),]$SixYd <- 0 
  }
  if (nrow(Place_team_for[is.na(Place_team_for$LongRange),])>0) {
    Place_team_for[is.na(Place_team_for$LongRange),]$LongRange <- 0 
  }
  
  Open_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Open") %>%
    summarise(Open=n())
  
  Corner_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Corner") %>%
    summarise(Corner=n())
  
  Throw_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Throw") %>%
    summarise(Throw=n())
  
  IFK_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Indirect_FK") %>%
    summarise(IFK=n())
  
  DFK_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Direct_FK") %>%
    summarise(DFK=n())
  
  Pen_team <- goalsFor %>%
    group_by(season,team) %>%
    filter(PLAY=="Penalty") %>%
    summarise(Pen=n())
  
  
  Play_team_for <- data.frame(teamSeason %>%
                                left_join(Open_team) %>%
                                left_join(Corner_team) %>%
                                left_join(Throw_team) %>%
                                left_join(IFK_team) %>%
                                left_join(DFK_team) %>%
                                left_join(Pen_team))
  
  if (nrow(Play_team_for[is.na(Play_team_for$Open),])>0) {
    Play_team_for[is.na(Play_team_for$Open),]$Open <- 0 
  }
  if (nrow(Play_team_for[is.na(Play_team_for$Throw),])>0) {
    Play_team_for[is.na(Play_team_for$Throw),]$Throw <- 0 
  }
  if (nrow(Play_team_for[is.na(Play_team_for$Corner),])>0) {
    Play_team_for[is.na(Play_team_for$Corner),]$Corner <- 0 
  }
  if (nrow(Play_team_for[is.na(Play_team_for$IFK),])>0) {
    Play_team_for[is.na(Play_team_for$IFK),]$IFK <- 0 
  }
  if (nrow(Play_team_for[is.na(Play_team_for$DFK),])>0) {
    Play_team_for[is.na(Play_team_for$DFK),]$DFK <- 0 
  }
  if (nrow(Play_team_for[is.na(Play_team_for$Pen),])>0) {
    Play_team_for[is.na(Play_team_for$Pen),]$Pen <- 0 
  }
  
  
  Goals_team_for <-Play_team_for %>%
    inner_join(Place_team_for) %>%
    inner_join(Method_team_for) %>%
    mutate(Tot=Right+Left+Head)
  
  ##### Goals Against
  
  
  
  opponents <- matchTeam %>%
    select(MATCHID,opponent=TEAMNAME)
  
  goalsFor <-goalsFor %>%
    left_join(opponents) %>%
    filter(team!=opponent)
  
  
  
  
  
  head_team_ag <-  goalsFor %>%
    group_by(season,opponent) %>%
    filter(METHOD=="Head") %>%
    summarise(Head=n()) 
  
  left_team_ag <-  goalsFor %>%
    group_by(season,opponent) %>%
    filter(METHOD=="Left") %>%
    summarise(Left=n()) 
  
  right_team_ag <-  goalsFor %>%
    group_by(season,opponent) %>%
    filter(METHOD=="Right") %>%
    summarise(Right=n())
  
  Method_team_ag <- data.frame(teamSeason %>% 
                                 select(season,opponent=team) %>%
                                 left_join(right_team_ag) %>%
                                 left_join(left_team_ag) %>%
                                 left_join(head_team_ag))
  #str(Method_team_for)
  
  if (nrow(Method_team_ag[is.na(Method_team_ag$Head),])>0) {
    Method_team_ag[is.na(Method_team_ag$Head),]$Head <- 0 
  }
  if (nrow(Method_team_ag[is.na(Method_team_ag$Left),])>0) {
    Method_team_ag[is.na(Method_team_ag$Left),]$Left <- 0 
  }
  if (nrow(Method_team_ag[is.na(Method_team_ag$Right),])>0) {
    Method_team_ag[is.na(Method_team_ag$Right),]$Right <- 0 
  }
  
  
  
  SixYd_team_ag <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLACE=="6_Yd_Box") %>%
    summarise(SixYd=n())
  
  PenArea_team_ag <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLACE=="Pen_Area") %>%
    summarise(PenArea=n())
  
  LongRange_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLACE=="Long_Range") %>%
    summarise(LongRange=n())
  
  Place_team_ag <- data.frame(teamSeason %>%
                                select(season,opponent=team) %>%
                                left_join(SixYd_team_ag) %>%
                                left_join(PenArea_team_ag) %>%
                                left_join(LongRange_team_ag))
  
  if (nrow(Place_team_ag[is.na(Place_team_ag$PenArea),])>0) {
    Place_team_ag[is.na(Place_team_ag$PenArea),]$PenArea <- 0 
  }
  if (nrow(Place_team_ag[is.na(Place_team_ag$SixYd),])>0) {
    Place_team_ag[is.na(Place_team_ag$SixYd),]$SixYd <- 0 
  }
  if (nrow(Place_team_ag[is.na(Place_team_ag$LongRange),])>0) {
    Place_team_ag[is.na(Place_team_ag$LongRange),]$LongRange <- 0 
  }
  
  Open_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Open") %>%
    summarise(Open=n())
  
  Corner_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Corner") %>%
    summarise(Corner=n())
  
  Throw_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Throw") %>%
    summarise(Throw=n())
  
  IFK_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Indirect_FK") %>%
    summarise(IFK=n())
  
  DFK_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Direct_FK") %>%
    summarise(DFK=n())
  
  Pen_team_ag  <- goalsFor %>%
    group_by(season,opponent) %>%
    filter(PLAY=="Penalty") %>%
    summarise(Pen=n())
  
  
  Play_team_ag <- data.frame(teamSeason %>%
                               select(season,opponent=team) %>%
                               left_join(Open_team_ag ) %>%
                               left_join(Corner_team_ag ) %>%
                               left_join(Throw_team_ag ) %>%
                               left_join(IFK_team_ag ) %>%
                               left_join(DFK_team_ag ) %>%
                               left_join(Pen_team_ag ))
  
  if (nrow(Play_team_ag[is.na(Play_team_ag$Open),])>0) {
    Play_team_ag[is.na(Play_team_ag$Open),]$Open <- 0 
  }
  if (nrow(Play_team_ag[is.na(Play_team_ag$Throw),])>0) {
    Play_team_ag[is.na(Play_team_ag$Throw),]$Throw <- 0 
  }
  if (nrow(Play_team_ag[is.na(Play_team_ag$Corner),])>0) {
    Play_team_ag[is.na(Play_team_ag$Corner),]$Corner <- 0 
  }
  if (nrow(Play_team_ag[is.na(Play_team_ag$IFK),])>0) {
    Play_team_ag[is.na(Play_team_ag$IFK),]$IFK <- 0 
  }
  if (nrow(Play_team_ag[is.na(Play_team_ag$DFK),])>0) {
    Play_team_ag[is.na(Play_team_ag$DFK),]$DFK <- 0 
  }
  if (nrow(Play_team_ag[is.na(Play_team_ag$Pen),])>0) {
    Play_team_ag[is.na(Play_team_ag$Pen),]$Pen <- 0 
  }
  
  
  Goals_team_ag <- Play_team_ag %>%
    inner_join(Place_team_ag) %>%
    inner_join(Method_team_ag) %>%
    mutate(Tot=Right+Left+Head)
  
  # names(Play_team_ag)
  # names(Place_team_ag)
  # names(Method_team_ag)
  # names(Goals_team_ag)
  # head(Goals_team_ag,1)
  
  #glimpse(Goals_team_ag)
  
  playerGame <- tbl_df(playerGame) # helps with snippets
  
  
  ## create the milestones (can put back into server if it has any interactivity
  #)
  
  #latest info
  tw <- 
    playerGame %>% 
    group_by(PLAYERID,name) %>% 
    summarise(Goals=sum(Gls)) 
  
  # previous data - may need to set date or from season maxround then gpo back one
  lw <- 
    playerGame %>% 
    filter(as.character(gameDate)<milestoneDate) %>% 
    group_by(PLAYERID,name) %>% 
    summarise(Goals=sum(Gls))
  
  dd <- diff_data(tw,lw) #s
  write_diff(dd, "diff.csv")
  res <- read_csv("diff.csv") #Inc new players 
  
  ## this caters for players who score on debut I hope
  names(res)[1] <- "col1"
  dfDebs <- res %>% 
      filter(col1=="->"&Goals>0)
  #names(dfDebs)[1] <- "@@"
  names(dfDebs)[4] <- "New"
  dfDebs$Old <- 0
  dfDebs$Old <- as.integer(dfDebs$Old)
  dfDebs$New <- as.integer(dfDebs$New)
  
  ## this caters for regular players
  df <- separate(data=res,col= Goals,into=c("New","Old"),sep="->", extra="drop")
  
  # remove all na rows
  df <- df[!is.na(df$Old),] # now down to 23 who scored
  
  df$Old <- as.integer(ifelse(is.na(df$Old),0,df$Old))
  df$New <- as.integer(df$New)
  df <-rbind(df,dfDebs)
  
  
  ## could also set it to ten presulably even if 11
  deb <- df %>% 
    filter(New>0&Old<=0) 
  
  ten <-df %>% 
    filter(New>9&Old<=9) %>% 
    mutate(New=10)
  
  twentyFive <-df %>% 
    filter(New>24&Old<=24)  %>% 
    mutate(New=25)
  fifty <-df %>% 
    filter(New>49&Old<=9)  %>% 
    mutate(New=50)
  hundred <-df %>% 
    filter(New>99&Old<=99)  %>% 
    mutate(New=100)
  hundredfifty <-df %>% 
    filter(New>149&Old<=149)  %>% 
    mutate(New=150)
  twohundred <-df %>% 
    filter(New>199&Old<=199)  %>% 
    mutate(New=200)
  threehundred <-df %>% 
    filter(New>299&Old<=299)  %>% 
    mutate(New=300)
  twohundredfifty <-df %>% 
    filter(New>249&Old<=249)  %>% 
    mutate(New=250)
  
  milestoneGoals <-bind_rows(deb,ten,twentyFive,fifty,hundred,hundredfifty,twohundred,twohundredfifty,threehundred) %>% 
    arrange(desc(New)) %>% 
    mutate(category="Goals") %>% 
    select(category,name,New)
  
  ## assists
  
  tw <- 
    playerGame %>% 
    group_by(PLAYERID,name) %>% 
    summarise(Assts=sum(Assists)) 
    
  
  # previous data - may need to set date or from season maxround then gpo back one
  lw <- 
    playerGame %>% 
    filter(as.character(gameDate)<milestoneDate) %>% 
    group_by(PLAYERID,name) %>% 
    summarise(Assts=sum(Assists))
  
  dd <- diff_data(tw,lw) 
  write_diff(dd, "diff.csv")
  
  res <- read_csv("diff.csv") #Balotell still just 21
  
  ## this caters for players who score on debut I hope
  names(res)[1] <- "col1"
  dfDebs <- res %>% 
    filter(col1=="->"&Assts>0)
  #names(dfDebs)[1] <- "@@"
  names(dfDebs)[4] <- "New"
  dfDebs$Old <- 0
  dfDebs$Old <- as.integer(dfDebs$Old)
  dfDebs$New <- as.integer(dfDebs$New)
  
  
  df <- separate(data=res,col= Assts,into=c("New","Old"),sep="->", extra="drop")
  
  # remove all na rows
  df <- df[!is.na(df$Old),] # now down to 23 who scored
  
  df$Old <- as.integer(ifelse(is.na(df$Old),0,df$Old))
  df$New <- as.integer(df$New)
  
  df<- rbind(df,dfDebs)
  
  
  ## could also set it to ten presulably even if 11
  deb <- df %>% 
    filter(New>0&Old<=0) 
  
  ten <-df %>% 
    filter(New>9&Old<=9) %>% 
    mutate(New=10)
  
  twentyFive <-df %>% 
    filter(New>24&Old<=24)  %>% 
    mutate(New=25)
  fifty <-df %>% 
    filter(New>49&Old<=9)  %>% 
    mutate(New=50)
  hundred <-df %>% 
    filter(New>99&Old<=99)  %>% 
    mutate(New=100)
  hundredfifty <-df %>% 
    filter(New>149&Old<=149)  %>% 
    mutate(New=150)
  twohundred <-df %>% 
    filter(New>199&Old<=199)  %>% 
    mutate(New=200)
  threehundred <-df %>% 
    filter(New>299&Old<=299)  %>% 
    mutate(New=300)
  twohundredfifty <-df %>% 
    filter(New>249&Old<=249)  %>% 
    mutate(New=250)
  
  milestoneAssists <-bind_rows(deb,ten,twentyFive,fifty,hundred,hundredfifty,twohundred,twohundredfifty,threehundred) %>% 
    arrange(desc(New)) %>% 
    mutate(category="Assists") %>% 
    select(category,name,New)
  
  ## apps
  
  
  # previous data - may need to set date or from season maxround then gpo back one
  lw <- 
    playerGame %>% 
    filter(as.character(gameDate)<milestoneDate&(START+subOn)>0) %>% 
    group_by(PLAYERID,name) %>% 
    tally()
  
  
  tw <- # 10 more
    playerGame %>% 
    filter((START+subOn)>0) %>% 
    group_by(PLAYERID,name) %>% 
    tally() 
  
  newIds <- setdiff(tw$PLAYERID,lw$PLAYERID) # could be zero
  newName <- setdiff(tw$name,lw$name)
  
  
  
  newPlayers <- data.frame(PLAYERID=newIds,name=newName,n=0)
  if (length(newIds) >0) lw <- rbind(lw,newPlayers)
  
  
  
  dd <- diff_data(tw,lw) #s
  write_diff(dd, "diff.csv")
  res <- read_csv("diff.csv") #Balotell still just 21
  
  
  df <- separate(data=res,col= n,into=c("New","Old"),sep="->", extra="drop") # warning too few values at 657 locations
  
  # remove all na rows
  df <- df[!is.na(df$Old),] # now down to 23 who scored
  
  df$Old <- as.integer(ifelse(is.na(df$Old),0,df$Old))
  df$New <- as.integer(df$New)
  
  
  ## could also set it to ten presulably even if 11
  deb <- df %>% 
    filter(New>0&Old<=0) 
  
  ten <-df %>% 
    filter(New>9&Old<=9) %>% 
    mutate(New=10)
  
  # twentyFive <-df %>% 
  #   filter(New>24&Old<=24)  %>% 
  #   mutate(New=25)
  fifty <-df %>% 
    filter(New>49&Old<=9)  %>% 
    mutate(New=50)
  hundred <-df %>% 
    filter(New>99&Old<=99)  %>% 
    mutate(New=100)
  twohundred <-df %>% 
    filter(New>199&Old<=199)  %>% 
    mutate(New=200)
  twohundred <-df %>% 
    filter(New>199&Old<=199)  %>% 
    mutate(New=200)
  threehundred <-df %>% 
    filter(New>299&Old<=299)  %>% 
    mutate(New=300)
  fourhundred <-df %>% 
    filter(New>399&Old<=399)  %>% 
    mutate(New=400)
  fivehundred <-df %>% 
    filter(New>499&Old<=499)  %>% 
    mutate(New=500)
  sixhundred <-df %>% 
    filter(New>599&Old<=599)  %>% 
    mutate(New=600)
  sevenhundred <-df %>% 
    filter(New>699&Old<=699)  %>% 
    mutate(New=700)
  
  milestoneApps <-bind_rows(deb,fifty,hundred,twohundred,threehundred,fourhundred,fivehundred,sixhundred,sevenhundred) %>% 
    arrange(desc(New)) %>% 
    mutate(category="Apps") %>% 
    select(category,name,New)
  
  milestones <- bind_rows(milestoneGoals,milestoneAssists,milestoneApps) %>% 
    arrange(desc(New)) %>% 
    rename(count=New) %>% 
    select(name,category,count)
  
  
  
  # all palyer goals seqs - takes too long for interactive ------------------
  
  appeared <-playerGame %>%
    filter((START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  
  
  
  appeared$Scored <- 0
  appeared$Scored[appeared$Gls>0] <- 1
  
  
  
  by_player <- appeared %>% 
    ungroup() %>% 
    arrange(PLAYERID,gameDate) %>% 
    group_by(PLAYERID) # this needs to be here to carry PLAYERID across
  
  
  
  goalSeqs <- do(by_player,subSeq(.$Scored)) %>% 
    filter(value==1)
  
  appeared <-playerGame %>%
  filter((START+subOn)>0) %>%  # 380 as sh
    arrange(gameDate) %>%
    select(PLAYERID,Gls,plGameOrder,TEAMNAME,gameDate,Opponents)
  
  
  
  appeared$Scored <- 0
  appeared$Scored[appeared$Gls>0] <- 1
  
  
  
  by_playerClub <- appeared %>% 
    ungroup() %>% 
    arrange(PLAYERID,gameDate) %>% 
    group_by(PLAYERID,TEAMNAME) # this needs to be here to carry PLAYERID across
  
  
  
  goalSeqsClub <- do(by_playerClub,subSeq(.$Scored)) %>% 
    filter(value==1)
  
  ## adding manager bought and sold info - need to check changing left and joined format is not an issue
  #i <- 2 # error when hits FloresE
  for (i in 1:nrow(managers)) {
   # print(i)
    if(!is.na(managers$Left[i])) {
      tempdf <- data.frame(theDate=seq(from = as.Date(managers$Joined[i]), 
                                       to = as.Date(managers$Left[i]),"days"),ManagerID=managers$ManagerID[i], TEAMID=managers$TeamID[i],stringsAsFactors = FALSE)
    } else {
      tempdf <- data.frame(theDate=seq(from = as.Date(managers$Joined[i]), 
                                       to = Sys.Date(),"days"),ManagerID=managers$ManagerID[i], TEAMID=managers$TeamID[i],stringsAsFactors = FALSE) 
    }
    if (i!=1) {
      managerTeam <- rbind(managerTeam,tempdf)
    } else {
      managerTeam <- tempdf
    }
  }
  
  playerClub$JOINED <- as.Date(playerClub$JOINED)
  playerClub$LEFT <- as.Date(playerClub$LEFT)  ##6615 (seems a little light but btterbthan 4166 of those who both joined and left)
  
  # not correct done for timevis elsewhere
  # playerClub <-playerClub %>% 
  #   rename(theDate=JOINED) %>% 
  #   merge(managerTeam) %>% 
  #   rename(JOINED=theDate,bought=ManagerID) %>% 
  #   rename(theDate=LEFT) %>% 
  #   merge(managerTeam) %>% 
  #   rename(LEFT=theDate,sold=ManagerID)
  
  ### end of adding manager
  
  ## add geos
  playerGeos <- readRDS("playerGeos.rds")
  
  newGuys <- allPlayers %>% 
    rename(playerID=PLAYERID) %>% 
    anti_join(playerGeos,by="playerID")
  
  if(nrow(newGuys>0)) {
    
    newGuys <- allPlayers %>% 
      rename(playerID=PLAYERID) %>% 
      anti_join(playerGeos,by="playerID")  #109 - not quite correct but hey!!
    
  } 
    
    library(ggmap)
   
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
    
    playerGeos <- rbind(playerGeos,df)
    playerGeos <- unique(playerGeos) # just in case
    
    saveRDS(playerGeos,"playerGeos.rds")
  
    ### sequencing of scoring/conceding without reply
    
    GF <- goals %>% 
      left_join(teamGames, by="TEAMMATCHID") %>% 
      select(TEAMMATCHID,gameDate,team=TEAMNAME,TIME,MATCHID) %>% 
      arrange(gameDate,MATCHID,TIME) %>% 
      left_join(standings) %>% 
      select(TEAMMATCHID,gameDate,team,OppTeam,TIME,MATCHID) 
    
    allTeams <- unique(standings$team)
    
    i <- 1
    for (i in seq_along(allTeams)) {
      print(i)
      temp_df <- GF %>% 
        filter(team==allTeams[i]|OppTeam==allTeams[i]) %>% 
        mutate(us=ifelse(team==allTeams[i],1,0)) %>% 
        do(subSeq(.$us))
      
      temp_df$team <- allTeams[i]
      
      if(i!=1)  {
        goalSeqTeam <- rbind(goalSeqTeam,temp_df)
      }   else {
        goalSeqTeam <- temp_df
      }                    
    }
    
    goalSeqTeam <- tbl_df(goalSeqTeam)
    
  
  ## NB prob should ungroup all but
    summary <- summary %>% 
      ungroup()
    
    standings <- standings %>% 
      ungroup()
    
    allGoalsPlayer <- allGoalsPlayer %>% 
      ungroup()
  
    goalSeqs <- goalSeqs %>% 
      ungroup()
  
    goalSeqsClub <- goalSeqsClub %>% 
      ungroup()
    
    Method <- Method %>% 
      ungroup()
   
    milestones <- milestones %>% 
      ungroup()
    
    Place <- Place %>% 
      ungroup()
    
    Play <- Play %>% 
      ungroup()
    
    playerGame <- playerGame %>% 
      ungroup()
  
  ## matbe create some basic files as rds
  saveRDS(managerTeam,"managerTeam.rds") # just George Graham by day - no idea what that is about offhand can form background to player timevis
  saveRDS(allPlayers,"allPlayers.rds")
  saveRDS(playerGame,"playerGame.rds")
  saveRDS(playerClub,"playerClub.rds")
  saveRDS(summary,"summary.rds")
  saveRDS(standings,"standings.rds")
  saveRDS(leaders,"leaders.rds")
  saveRDS(allGoalsPlayer,"allGoalsPlayer.rds")
  saveRDS(goals,"goals.rds")
  saveRDS(Goals_team_for,"Goals_team_for.rds")
  saveRDS(Goals_team_ag,"Goals_team_ag.rds")
  saveRDS(Play,"Play.rds")
  saveRDS(Method,"Method.rds")
  saveRDS(Place,"Place.rds")
  saveRDS(teamGames,"teamGames.rds")
  write_csv(milestones,"milestones.csv")
  print(glimpse(managers))
  saveRDS(managers,"managers.rds")
  saveRDS(goalSeqs,"goalSeqs.rds")
  saveRDS(goalSeqsClub,"goalSeqsClub.rds")
  saveRDS(hth,"hth.rds")
  saveRDS(goalSeqTeam,"goalSeqTeam.rds")
  
  #Sys.time() # takes 15 secs- seems quicker in browser
  
library(beepr)
  beep(3)