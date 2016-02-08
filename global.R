library(shiny)
library(shinydashboard)
library(httr)
library(rvest)
library(XML)
library(doBy) # uses MASS which has a eselect conflict with dplyr - need for sequences
library(dplyr) # this masks select from MASS, filter from stats and intersect etc from base
library(timelineR) # conflict with ggvis on add_axis so added to ggvis currently
library(ggvis)

library(RSQLite)
library(lubridate)
library(stringr)
library(markdown)
library(tidyr)
library(shinyBS)
library(scales)
library(ggplot2)
library(leaflet)
library(rCharts)
library(shinythemes)
library(DT)
library(readr)
library(ggmap)
library(rgdal)
library(choroplethr)
library(choroplethrMaps)
library(taucharts)
library(daff)
library(plotly)
#library(crosstalk) no longer needed as can now use event_data() in plotly
library(explodingboxplotR)
library(beeswarm) # just for test

library(addins)



positions <- read_csv("positions.csv") ##
playerGame <- readRDS("playerGame.rds")
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
playerGeos <- read_csv("playerGeos.csv")
goalSeqs <- readRDS("goalSeqs.rds")
goalSeqsClub <- readRDS("goalSeqsClub.rds")


teamCodes <- teamGames %>% 
  ungroup() %>% 
  select(TEAMNAME,TEAMID) %>% 
  unique()

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

    
playerChoice <- pgMini$PLAYERID
names(playerChoice) <- pgMini$name

teamsChoice <- sort(unique(playerGame$TEAMNAME))
teamsChoice_2 <- c("All Teams",teamsChoice)

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

hth<-data.frame(standings %>%
                  select(team,OppTeam:gameDate,venue,points,res))
hth$tmYrGameOrder <- NULL
## set up order for sequences (got to be prob after changing standings added team back in)
hth <-data.frame(hth %>%
                   group_by(team,OppTeam) %>%
                   arrange(gameDate) %>%
                   mutate(gameOrder=row_number()))



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

trueGames <- playerGame %>% 
  filter((START+subOn)>0) %>% 
  group_by(PLAYERID) %>% 
  mutate(trueGameOrder=row_number())

# used in sp_birthplace
allPlayers <- playerGame %>% 
  select(name,PLAYERID,COUNTRY) %>% 
  unique()


# standard map data for world
mapData <- readOGR(dsn=".",
                   layer = "ne_50m_admin_0_countries", 
                   encoding = "UTF-8",verbose=FALSE)


## used in year on year change
yrs <- unique(standings$season)
teams <- unique(standings$team)
allSeasonTeams <- data.frame(season=rep(yrs, length(teams)),team=rep(teams, length(yrs)))

print("end global")