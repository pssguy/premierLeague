
library(shiny)
library(shinydashboard)
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
library(shinythemes)
library(DT)
library(readr)

positions <- read_csv("positions.csv")
playerGame <- readRDS("playerGame.rds")
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





pgMini <- playerGame %>%
  select(PLAYERID,name) %>% 
  unique()

    
playerChoice <- pgMini$PLAYERID
names(playerChoice) <- pgMini$name

teamsChoice <- sort(unique(playerGame$TEAMNAME))
teamsChoice_2 <- c("All Teams",teamsChoice)

seasonChoice <- sort(unique(playerGame$season), decreasing = TRUE)
seasonChoice_2 <- c("All Seasons",seasonChoice)

currentYear <-max(standings$season)
currentRound <-max(standings[standings$season==currentYear,]$tmYrGameOrder)


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
