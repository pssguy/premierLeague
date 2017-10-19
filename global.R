options(warn=-1)
library(shiny)
library(shinydashboard)
library(httr)
library(rvest)
library(XML)
#library(doBy) # uses MASS which has a eselect conflict with dplyr - need for sequences
#library(dplyr) # this masks select from MASS, filter from stats and intersect etc from base
library(timelineR) # conflict with ggvis on add_axis so added to ggvis currently
library(ggvis)
library(ggthemes)
library(RSQLite)
library(lubridate)
library(stringr)
library(markdown)
#library(tidyr)
library(shinyBS)
library(scales)
#library(ggplot2)
library(ggrepel)
library(leaflet)
library(rCharts)
library(shinythemes)
library(DT)
#library(readr)
library(ggmap)
library(rgdal)
#library(choroplethr) removed as requirement tigris was causing deployment issue
#library(choroplethrMaps)
library(taucharts)
library(daff)
library(plotly)
#library(crosstalk) no longer needed as can now use event_data() in plotly
library(explodingboxplotR)
library(beeswarm) # just for test
#library(addins)
library(feather)
library(bsplus)

use_bs_tooltip()
use_bs_popover()
#library(purrr)
#library(dplyr)
library(tidyverse)

enableBookmarking(store = "url")

#enableBookmarking(store = "server")

positions <- read_csv("positions.csv") ##
playerGame <- readRDS("playerGame.rds") # reducing columns would help - though might be offset by calcs needed later
#playerGame <- read_feather("playerGame.feather")
playerClub <- readRDS("playerClub.rds")
summary <- readRDS("summary.rds")
leaders <- readRDS("leaders.rds")
standings <- readRDS("standings.rds")
allGoalsPlayer <- readRDS("allGoalsPlayer.rds")
goals <- readRDS("goals.rds")
Goals_team_for <- readRDS("Goals_team_for.rds")
Goals_team_ag <- readRDS("Goals_team_ag.rds")
Play <- readRDS("Play.rds")
Place <- readRDS("Place.rds")
Method<- readRDS("Method.rds")
teamGames <- readRDS("teamGames.rds")
managers <- readRDS("managers.rds")
milestones<- read_csv("milestones.csv")
#playerGeos <- read_csv("playerGeos.csv") backup
playerGeos <- readRDS("playerGeos.rds")
goalSeqs <- readRDS("goalSeqs.rds")
goalSeqsClub <- readRDS("goalSeqsClub.rds")
allPlayers <- readRDS("allPlayers.rds")
hth <- readRDS("hth.rds")
goalSeqTeam <- readRDS("goalSeqTeam.rds")
assists <- readRDS("assists.rds")
teamCodes <- readRDS("teamCodes.rds")

# switched to updating sql
# teamCodes <- teamGames %>% 
#   ungroup() %>% 
#   select(TEAMNAME,TEAMID) %>% 
#   unique()

## playerid name and lat/lon
# pgMini <- playerGame %>%  ## so wil only show those that have made an appearance - but that is prob ok
#   select(PLAYERID,name,city,COUNTRY) %>% 
#   unique() %>% 
#   left_join(playerGeos,by=c("PLAYERID"="playerID")) %>% 
#   mutate(place=paste0(city," ",COUNTRY))


pgMini <- playerGame %>%  ## so wil only show those that have made an appearance - but that is prob ok
  select(PLAYERID,name,city,COUNTRY) %>% 
  unique() %>% 
  
  left_join(playerGeos,by=c("PLAYERID"="playerID")) %>% 
  filter(PLAYERID!="OWNGOAL") %>% 
  mutate(place=ifelse(is.na(city),COUNTRY,paste0(city," ",COUNTRY)))

write_csv(pgMini,"pgMini.csv")

    
# playerChoice <- pgMini$PLAYERID
# names(playerChoice) <- pgMini$name  

pgMini <- pgMini %>% 
  arrange(name)

playerChoice <- pgMini$PLAYERID
names(playerChoice) <- pgMini$name  

teamsChoice <- sort(unique(playerGame$TEAMNAME))
teamsChoice_2 <- c("All Teams",teamsChoice)

managers <- managers %>% 
  mutate(name=paste0(FirstName," ",Lastname))

managerChoice <- sort(unique(managers$name))


seasonChoice <- sort(unique(playerGame$season), decreasing = TRUE)
seasonChoice_2 <- c("All Seasons",seasonChoice)


countryChoice <- sort(unique(playerGame$COUNTRY))
countryChoice_2 <- c("All Countries",countryChoice)

currentYear <-max(standings$season)
currentRound <-max(standings[standings$season==currentYear,]$tmYrGameOrder)


yrs <-sort(unique(standings$season), decreasing=T)

currentYear <- yrs[1] #[1] "2014/15"
lastYear <- yrs[2]
currentRound <-max(standings[standings$season==currentYear,]$tmYrGameOrder)
## take account of few games
if (currentRound<6) {
  currentValue <- 6
} else {
  currentValue <- currentRound
}
print("currentValue")
print(currentValue)

tmYrs <-standings %>%
  select(season,team) %>%
  unique(.)

# hth<-data.frame(standings %>%
#                   select(team,OppTeam:gameDate,venue,points,res,MATCHID,season))
# hth$tmYrGameOrder <- NULL
# print("a")
# ## set up order for sequences (got to be prob after changing standings added team back in)
# hth <-data.frame(hth %>%
#                    group_by(team,OppTeam) %>%
#                    arrange(gameDate) %>%
#                    dplyr::mutate(gameOrder=row_number(gameDate)))
# 
# 
# print("a2")
## Table Formats


GF_format = htmltools::withTags(table(
  thead(
    tr(
      th(colspan = 1, ''),
      th(colspan = 6, 'Method'),
      th(colspan = 3, 'Place'),
      th(colspan = 3, 'Play'),
      th(colspan = 1, '')
    ),
    tr(
      th('Season'),
      th('Open'),
      th('Corner'),
      th('Throw'),
      th('IFK'),
      th('DFK'),
      th('Pen'), 
      th('SixYd'),
      th('PenArea'),
      th('LongRange'), 
      th('Right'),
      th('Left'),
      th('Head'),
      th('Tot')
    )
  )
))


PL_format = htmltools::withTags(table(
  thead(
    tr(
      th(colspan = 2, ''),
      th(colspan = 3,align='center', 'Method'), # does not work
      th(colspan = 3, 'Place'),
      th(colspan = 6, 'Play')
      ),
      
    tr(
      th('Season'),
      th('Total'),
      th('Right'),
      th('Left'),
      th('Head'),
      th('Six Yd'),
      th('Pen Area'),
      th('Long Range'), 
      th('Open'),
      th('Corner'),
      th('Throw'),
      th('IFK'),
      th('DFK'),
      th('Pen') 
    )
  )
))


# set initial values and hpe do not return to
#values <- reactiveValues()
#values$playerID <- "BENTD"


### calc truegames played - plgameorder inc bench
print("b")
trueGames <- playerGame %>% 
  filter((START+subOn)>0) %>% 
  group_by(PLAYERID) %>% 
  dplyr::mutate(trueGameOrder=row_number())

# used in sp_birthplace  NB issue here aas this reduces allPlayers (which carries across )
# allPlayers <- playerGame %>% 
#   select(name,PLAYERID,COUNTRY) %>% 
#   unique()

## removed as  not sure needed and took ages to load
# standard map data for world
# mapData <- readOGR(dsn=".",
#                    layer = "ne_50m_admin_0_countries", 
#                    encoding = "UTF-8",verbose=FALSE)

print("c")
## used in year on year change
yrs <- unique(standings$season)
teams <- unique(standings$team)
allSeasonTeams <- data.frame(season=rep(yrs, length(teams)),team=rep(teams, length(yrs)))


### function extracted from doBy package which takes too long to load
subSeq <- function (x, item = NULL) {
  rrr <- rle(x)
  len <- rrr$lengths
  val <- rrr$values
  
  first <- last <- rep.int(NA, length(val))
  first[1] <- 1
  last [1] <- len[1]
  if (length(val)>1){
    for (kk in 2:length(val)){
      first[kk] <- last[kk-1]+1
      last [kk] <- last[kk-1]+len[kk]
    }
  }
  midp <- floor(first + len/2)
  
  ans <- cbind(first=first, last=last, slength=len, midpoint=midp)
  
  if (!is.null(item)) {
    iii <- val==item
    ans <- as.data.frame.matrix(ans[iii,,drop=FALSE], stringsAsFactors=FALSE)
    ans$value <- val[iii]
  } else {
    ans <- as.data.frame.matrix(ans, stringsAsFactors=FALSE)
    ans$value <- val
  }
  ans
}

## switched from manager ppg
managers[is.na(managers$Left),"Left"] <- as.Date(Sys.Date(), origin= '1970-01-01') #

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
  dplyr::summarise(sumPoints=sum(points),games=n(),ppg=round(sumPoints/games,2)) %>% 
  ungroup()




allManagerStints <- 
  managerGame %>% 
  select(name,ManagerTeam,Joined,Left) %>% 
  unique()



## artificial start date for those hired before PL existed
allManagerStints[allManagerStints$Joined<="1992-08-15","Joined"] <- "1992-08-15"




print("end global")