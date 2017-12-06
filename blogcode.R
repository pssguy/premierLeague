## unchanged starting lineup

sort(names(playerGame))

sort(names(teamGames))


playerGame %>% 
  filter(START>0&TEAMMATCHID==20000) %>% 
  select(TEAMNAME,gameDate,name) %>% 
  nest(name)
  

# A tibble: 1 x 3
# TEAMNAME   gameDate              data
# <chr>     <date>            <list>
#   1  Arsenal 1992-08-15 <tibble [11 x 1]>


test <-playerGame %>% 
  filter(START>0&TEAMNAME=="Arsenal") %>% 
  select(TEAMNAME,gameDate,name) %>% 
  nest(name) %>% 
  mutate(dataLast=lag(data)) %>% 
  filter(!is.na(dataLast)) %>% 
  head(2) %>% 
  ungroup() %>% 
  select(l1=data,l2=dataLast) 

try a dput
%>% 
  mutate(diff=length(setdiff(unlist(l1),unlist(l2))))

unlist(test[1,1])
unlist(test[1,2])

length(setdiff(unlist(test[1,1]),unlist(test[1,2])))


  mutate(changes=length(setdiff(unlist(data),unlist(dataLast))))  #0 no error thrown but it is incorrect
  

# Richarlson --------------------------------------------------------------


# manu 7 in row -----------------------------------------------------------

  

class(test)








temp <- test[2,]

length(setdiff(unlist(temp$data),unlist(temp$dataLast)))

# nope this is not constructed dight
x=list(c("a","b"),c("c","d"))
 y=list(c("b","c"),c("c","d"))

df <- as.data.frame(x,y)


## Week 2
# any team erlegated with 2 wins to start seasonÉ -------------------------

sort(names(standings))


standings %>% 
  filter(tmYrGameOrder==2&cumPts==6) %>% 
  arrange(desc(final_Pos)) %>% 
  select(final_Pos,season,team)

## could be poll question



# Kane not scoring in August ----------------------------------------------

sort(names(playerGame))
library(lubridate)
library(plotly)
library(forcats)
library(glue)

playerGame %>% 
  filter(PLAYERID=="KANEH"&mins>0) %>% 
  mutate(month=months(gameDate),monthOrder=month(gameDate)) %>% 
  select(gameDate,mins,Gls,month,monthOrder,name) %>% 
  group_by(month,monthOrder) %>% 
  summarise(apps=n(),mins=sum(mins),Gls=sum(Gls)) %>% 
  plot_ly(x=~fct_reorder(month, monthOrder),y=~Gls,
          hoverinfo="text",
          text=~paste0("Mins: ",mins,"<br>Goals: ",Gls)) %>% 
  add_markers(size=~mins) %>% 
  layout(xaxis=list(title=""),
         yaxis=list(title="EPL Goals"),
         title=glue("Premier League Goals by Month"), # ideally get name in there
         margin=list(b=60)
         
         ) %>%  config(displayModeBar = F,showLink = F)


# could cross-talk this


# venn diag robson kanu sub/redcard/scored --------------------------------

#new VennDiagram

sort(names(playerGame))

cds <- c("R","X","Z","P")

playerGame %>% 
  filter(subOn>0) #43171


playerGame %>% 
  filter(subOn>0&Gls>0) #2127

playerGame %>% 
  filter(subOn>0&CARD %in% cds) #62

playerGame %>% 
  filter(subOn>0&CARD %in% cds&Gls>0)  %>% #4
  select(name)


#think need this first as well
grid.newpage();

# A simple two-set diagram
venn.plot <- draw.pairwise.venn(2127, 62, 4, c("Scored", "Red Card"),fill=c("blue","red"));
grid.draw(venn.plot);
grid.newpage();

opta joe
4 - Hal Robson-Kanu is only the 4th player in Premier League history to score & be sent off after coming on as a substitute. Contrasts.

library(VennDiagram)

#think need this first as well
grid.newpage();

# A simple two-set diagram
venn.plot <- draw.pairwise.venn(100, 70, 30, c("First", "Second"),fill=c("blue","red"));
grid.draw(venn.plot);
grid.newpage();

# Same diagram as above, but without scaling
venn.plot <- draw.pairwise.venn(100, 70, 30, c("First", "Second"), scaled = FALSE);
grid.draw(venn.plot);
grid.newpage();

# A more complicated diagram Demonstrating external area labels
venn.plot <- draw.pairwise.venn(
  area1 = 100,
  area2 = 70,
  cross.area = 68,
  category = c("First", "Second"),
  fill = c("blue", "red"),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.pos = c(285, 105),
  cat.dist = 0.09,
  cat.just = list(c(-1, -1), c(1, 1)),
  ext.pos = 30,
  ext.dist = -0.05,
  ext.length = 0.85,
  ext.line.lwd = 2,
  ext.line.lty = "dashed"
);
grid.draw(venn.plot);
grid.newpage();


grid.newpage();

# A more complicated diagram Demonstrating external area labels
venn.plot <- draw.pairwise.venn(
  area1 = 2127,
  area2 = 62,
  cross.area = 4,
  category = c("Scored", "Dismissed"),
  fill = c("blue", "red"),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.pos = c(300, 105),
  cat.dist = 0.09,
  cat.just = list(c(-1, -1), c(1, 1)),
  ext.pos = 30,
  ext.dist = -0.05,
  ext.length = 0.85,
  ext.line.lwd = 2,
  ext.line.lty = "dashed"
);

# using subs --------------------------------------------------------------

Yesterday's win over Spurs was the first time in 50 games as Chelsea manager that Antonio Conte didn't use all three substitutes. Quirk.



# Rooney 200 goals --------------------------------------------------------

sort(names(playerGame))
sort(names(goals))


temp <- playerGame %>% 
  filter(LASTNAME=="Rooney") %>% 
  right_join(goals,by="PLAYER_MATCH") %>% 
  select(name,TIME) %>% 
  filter(!is.na(name)) %>% 
  group_by(TIME) %>% 
  tally() %>% 
  plot_ly(x=~TIME,y=~n)


playerGame %>% 
  group_by(PLAYERID,name) %>% 
  summarise(G=sum(Gls),A=sum(Assists),P=G+A,Mins=sum(mins)) %>% 
  arrange(desc(P)) %>% 
  ungroup() %>% 
  filter(PLAYERID!="OWNGOAL") %>%
  select(Player=name,Mins,Gls=G,`Assts(inc secondary)`=A,Pts=P) %>% 
 # head(10) %>% 
  DT::datatable(width=350,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# Bench costs -------------------------------------------------------------

playerGame %>% 
  filter(START==0&PLAYERID!="OWNGOAL") %>% 
  group_by(TEAMMATCHID,gameDate,TEAMNAME,Opponents) %>% 
  summarise(cost=sum(FEE,na.rm=T)) %>% 
  arrange(desc(cost))

playerGame %>% 
  filter(START==0&PLAYERID!="OWNGOAL"&TEAMMATCHID==42937) %>% 
  select(LASTNAME,FEE)

playerGame %>% 
  filter(START==0&PLAYERID!="OWNGOAL"&season=="2017/18"&TEAMNAME=="Man. City") %>% 
  arrange(LASTNAME,gameDate)



# aguero not scoring ------------------------------------------------------

playerGame %>% 
  filter(PLAYERID=="AGUEROS"&mins>0) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(Opponents) %>% 
  mutate(order=row_number()) %>% 
  filter(Gls>0) %>% 
  select(Opponents,gameDate,order,Gls) %>% 
  slice(1) %>% 
  arrange(desc(order))%>%
 DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))


# early 8 goals margin ----------------------------------------------------

sort(names(standings))
standings %>% 
  filter(tmYrGameOrder==2) %>% 
  arrange(desc(cumGD)) %>% 
  select (season,team,cumGD,cumGF,cumGA)


# unused factoid ----------------------------------------------------------

<p class="factoid">With Danilo replacing Sane on the bench, transfer fees for the seven non-starters dipped below the 200 million pound mark. Still the third most-expensive assembled in EPL history</p>
  

# man u sub goals ---------------------------------------------------------

sort(names(playerGame))

playerGame %>% 
  head(10) %>% 
  select(subOn)

test <-playerGame %>% 
  group_by(name,PLAYERID,TEAMNAME,season) %>% 
  filter(subOn>0&Gls>0&TEAMNAME=="Man. Utd.") %>% 
  tally() %>% 
  ungroup() %>% 
  select(name,season) %>% 
  unique() %>% 
  group_by(season) %>% 
  tally()

standings %>% 
  filter(MATCHID==2408)

Man United did not have a substitute score for then until their 78th Premier League game (Dion Dublin). They already have 4 from 3 different players this year(
  
  

# chicarito pc in 6ydbox --------------------------------------------------

sort(names(goals))  

table(goals$PLACE)



playerGame %>% 
  right_join(goals) %>% 
  select(name,PLAYERID,PLACE) %>% 
  #filter(PLAYERID=="HERNANJ") %>% 
  group_by(PLACE,PLAYERID) %>% 
  tally() %>% 
  spread(PLACE,n) %>% 
  replace_na(list(`6_Yd_Box` = 0, Long_Range = 0,Pen_Area = 0)) %>% 
  mutate(tot=`6_Yd_Box`+Long_Range+Pen_Area) %>% 
  filter(tot>38) %>% 
  mutate(pc=`6_Yd_Box`/tot) %>% 
  arrange(desc(pc))
  
# looks good need to filter out own goals


# TEAMS LOSING 1ST 3 GAMES ------------------------------------------------

sort(names(standings))
test <-standings %>% 
  filter(cumPts==0&tmYrGameOrder==3) %>% 
  select(season,team,final_Pos,cumGF) %>% 
  arrange(season,cumGF) %>% 
  select(Season=season,Team=team,`Final Pos`=final_Pos,`Goals For`=cumGF) %>%
   DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE)) 




# age by position ---------------------------------------------------------

sort(names(playerGame))
unique(playerGame$POSITION)

playerGame %>% 
  filter(START>0&!is.na(POSITION)) %>% 
  group_by(POSITION) %>% 
  summarise(avAge=mean(age,na.rm=TRUE))

# this does not seem to summarize by age
# playerGame %>% 
#   filter(START>0&!is.na(POSITION)&!is.na(age)) %>% 
#   group_by(POSITION) %>% 
#   summary()





# arsenal conceding goals -------------------------------------------------

library(doBy)

test <-temp %>% 
  #filter(goals>0) %>% 
  arrange(season) %>% 
  group_by(PLAYERID) %>% 
  # filter(PLAYERID=="BARTONJ") %>% 
  do(subSeq(.$x)) %>% 
  filter(value==1) %>% 
  filter(slength==max(slength,na.rm=TRUE)) %>% 
  arrange(desc(slength)) %>% 
  filter(PLAYERID!="OWNGOAL"&slength>=10) %>% 
  left_join(playerGame) %>% 
  ungroup() %>% 
  select(name,seasons=slength) %>%
  unique() %

standings %>% 
  arrange(tmGameOrder) %>% 
  filter(team=="Arsenal") %>% 
  select(GA,gameDate,tmGameOrder) %>% 
  mutate(lag1=lag(GA),lag2=lag(GA,2),threeGame=GA+lag1+lag2) %>% 
  filter(!is.na(threeGame)) %>% 
  arrange(desc(threeGame),desc(gameDate))



# hart conceding goals ----------------------------------------------------

sort(names(playerGame))
sort(names(standings))

hartGames <- playerGame %>% 
  filter(PLAYERID=="CECHP"&START>0) %>% 
  select(MATCHID,venue) %>% 
  left_join(standings) %>% 
  select(gameDate,GA) %>% 
  mutate(lag1=lag(GA),lag2=lag(GA,2),lag3=lag(GA,3),fourGame=GA+lag1+lag2+lag3) %>% 
  mutate(lag1=lag(GA),lag2=lag(GA,2),threeGame=GA+lag1+lag2) %>% 
  filter(!is.na(fourGame)) %>% 
  arrange(desc(fourGame),desc(gameDate))%>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

x <- c(1,3,5,2)

cumall(x)

cumany(x)

cummean(x)  



# Crouch scoring consec seasons --------------------------------------------------

sort(names(summary)) # has team as well as season

test <-summary %>% 
  mutate(Goals=(StGls+subGls)) %>% 
  filter(Goals>0) %>% 
  group_by(season,PLAYERID,name) %>% 
  summarize(totGls=sum(Goals)) %>% 
    ungroup() %>% 
#   arrange(season) %>% 
    group_by(PLAYERID,name) %>% 
   tally() %>% 
     arrange(desc(n))

Crouch 15th could be exceeded by Barry and Milner Rooney has already reached 16 and Defoe could surpass that with Bournemouth
top non home grown is Anelka on 13



# Posible followup --------------------------------------------------------


# The 33 most astounding stats from the Premier League season
# www.telegraph.co.uk/football/2017/05/22/33-astounding-stats-premier-league-season/
#   May 22, 2017 - An all-time low of 11.6 per cent of goals this season were scored from ... a 25-game unbeaten run between September 2016 and May 2017 ... Tottenham had the youngest starting XI, on average, over the course of the season, ..


# Time between premier league goals ---------------------------------------

#https://twitter.com/OptaJoe/status/901849399445381121

sort(names(playerGame))

playerGame %>% 
  filter(PLAYERID=="WOODC"&Gls>0) %>% 
  select(gameDate) %>% 
  mutate(prevDate=lag(gameDate,1),difftime(gameDate,prevDate))



playerGame %>% 
  filter(PLAYERID=="WOODC"&Gls>0) %>% 
  select(gameDate) %>% 
  mutate(prevDate=lag(gameDate,1),gap=difftime(gameDate,prevDate)) %>% 
  filter(!is.na(gap)) #1107


temp <-playerGame %>% 
  filter(Gls>0) %>% 
  select(gameDate,PLAYERID,name) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID) %>% 
 # filter(PLAYERID=="LACAZEA") %>% 
  mutate(prevDate=lag(gameDate,1),gap=difftime(gameDate,prevDate)) %>% 
  filter(!is.na(gap)) %>% 
  ungroup() %>% 
  arrange(desc(gap)) %>% 
  group_by(name,PLAYERID) %>% 
  slice(1) %>% 
  mutate(days=round(parse_integer(gap)/(60*60*24),1))

playerGame %>% 
  filter(PLAYERID=="DANI"&Gls>0) %>% 
  select(gameDate) %>% 
  mutate(prevDate=lag(gameDate,1),gap=difftime(gameDate,prevDate)) %>% 
  filter(!is.na(gap))

## time between goals and games played

x <-playerGame %>% 
  filter(Gls>0) %>% 
  select(gameDate,PLAYERID,name,POSITION) %>% 
  arrange(gameDate) %>% 
  group_by(name,PLAYERID,POSITION) %>% 
  # filter(PLAYERID=="LACAZEA") %>% 
  mutate(prevDate=lag(gameDate,1),gap=difftime(gameDate,prevDate)) %>% 
  filter(!is.na(gap)) %>% 
  ungroup() %>% 
  arrange(desc(gap)) %>% 
  group_by(name,PLAYERID,POSITION) %>% 
  slice(1) %>% 
  mutate(days=round(parse_integer(gap)/(60*60*24),1)) %>% 
  arrange(desc(days)) %>% 
  ungroup() 

y <- playerGame %>% 
  filter(mins>0) %>% 
  group_by(PLAYERID,name) %>% 
  summarise(games=n(),goals=sum(Gls)) %>% 
  right_join(x)

 x %>% 
  select(Player=name,position=POSITION,`Game 1`=prevDate,`Game 2`= gameDate,Days=days) %>%
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
 
 y %>% 
   plot_ly(x=~games,y=~days,
           hoverinfo="text",
           text=~paste0(name,
                        "<br>Games: ",games,
                        "<br>Goals: ",goals,
                        "<br>Max: ",days," days")) %>% 
             layout(title='Biggest time gap between goals scored by same Player',
                    xaxis=list("League Career Games"),
                      yaxis=list("Longest span between Goals")) %>%
             config(displayModeBar = F,showLink = F)
 
 y %>% 
   plot_ly(x=~games,y=~days) %>% 
   add_markers(size=~goals,
           hoverinfo="text",
           text=~paste0(name,
                        "<br>Games: ",games,
                        "<br>Goals: ",goals,
                        "<br>Max: ",days," days")) %>% 
   layout(title='Biggest time gap between goals scored by same Player',
          xaxis=list(title="League Career Games"),
          yaxis=list(title="Longest span between Goals")) %>%
   config(displayModeBar = F,showLink = F)
   
                      
           
           

# scoring in games to start season ----------------------------------------

# 3 - Sadio Mané is the 3rd player to score in the first 3 PL games of a season for Liverpool (Fowler in 94-95 & Sturridge in 13-14). Power.

 

# Palace pointless 0 goals first 4 games ----------------------------------

scored 1 gamein last 9 games
 
sort(names(standings))
 
df <-standings %>% 
  mutate(scoredin=ifelse(GF>0,1,0))

df <-df %>% 
  #select(team,gameDate,scoredin) %>% 
  arrange(gameDate) %>% 
  select(team,scoredin,gameDate) %>% 
  group_by(team) %>% 
  mutate(l1=lag(scoredin,1),
         l2=lag(scoredin,2),
         l3=lag(scoredin,3),
         l4=lag(scoredin,4),
         l5=lag(scoredin,5),
         l6=lag(scoredin,6),
         l7=lag(scoredin,7),
         l8=lag(scoredin,8)) %>% 
  filter(!is.na(l8)) %>% 
  mutate(tot=(scoredin+l1+l2+l3+l4+l5+l6+l7+l8) 
  ) %>% 
  filter(!is.na(tot)) %>% 
  arrange(tot)

##
#How many spells 4 games goalless and scoreless

df <-standings %>% 
  mutate(poor=ifelse(GF==0&res=="Loss",1,0))

df <-df %>% 
  #select(team,gameDate,scoredin) %>% 
  arrange(gameDate) %>% 
  select(team,poor,gameDate) %>% 
  group_by(team) %>% 
  mutate(l1=lag(poor,1),
         l2=lag(poor,2),
         l3=lag(poor,3),
         l4=lag(poor,4)
         ) %>% 
  filter(!is.na(l3)) %>% 
  mutate(tot=(poor+l1+l2+l3+l4) 
  ) %>% 
  filter(!is.na(tot)) %>% 
  arrange(tot)


palace only team two occasions ipswich 7 in all


# scoring first 3 appearances in league/for club --------------------------

sort(names(playerGame))

test <-playerGame %>% 
  filter((plGameOrderApp==1&Gls>0)&(plGameOrderApp==2&Gls>0)&(plGameOrderApp==3&Gls>0))

test <- playerGame %>% 
  filter((plGameOrderApp==1&Gls>0)&(plGameOrderApp==2&Gls>0))

test1 <- playerGame %>% 
  filter((plGameOrderApp==1&Gls>0)) %>% #200  
  pull(PLAYERID)
         
test2 <- playerGame %>% 
  filter((plGameOrderApp==2&Gls>0)) %>% 
  pull(PLAYERID)

test3 <- playerGame %>% 
  filter((plGameOrderApp==3&Gls>0)) %>% 
  pull(PLAYERID)

test4 <-intersect(test1,test2) #33

intersect(test3,test4)




is.integer(playerGame$plGameOrderApp)


# fewest apps to 80 golas -------------------------------------------------


80 - Fewest apps to 80 Premier League goals:
  @alanshearer - 102
@HKane - 120
@RvN1776 - 122
@aguerosergiokun - 128
@ThierryHenry - 132

## looks good just put do my specials cf but with plotly

player


# goals as teenager for manu u --------------------------------------------

rooney 15

temp <-playerGame %>% 
  filter(age<20) %>% 
  group_by(name,PLAYERID,TEAMNAME) %>% 
  summarise(tot=sum(Gls)) %>% 
  arrange(desc(tot)) %>%
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
  


# players used/players scoring --------------------------------------------

x <- playerGame %>% 
  filter(season=="2017/18") %>% 
  group_by(name,PLAYERID,TEAMNAME) %>% 
  summarise(tot=sum(mins)) %>% 
  filter(tot>0) %>% 
ungroup() %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
arrange(desc(n))


cf full years
x <-
  playerGame %>% 
    #filter(season==x) %>% 
    group_by(name,PLAYERID,TEAMNAME,season) %>% 
    summarise(tot=sum(mins)) %>% 
    filter(tot>0) %>% 
    ungroup() %>% 
    group_by(TEAMNAME,season) %>% 
    tally() %>% 
    arrange(desc(n))

x %>% 
  filter(TEAMNAME=="Liverpool") %>% 
  plot_ly(x=~season,y=~n) %>% 
  addbars(color=I('red'))

## ibe performance


# player scoring in first 4 games of season -------------------------------

sort(names(playerGame))

a <-playerGame %>% 
  filter((plYrGameOrderApp==1&Gls>0))
  select(PLAYERID,season,Gls)
  
  b <-playerGame %>% 
    filter((plYrGameOrderApp==2&Gls>0))
  select(PLAYERID,season,Gls)
  
  c <-playerGame %>% 
    filter((plYrGameOrderApp==3&Gls>0))
  select(PLAYERID,season,Gls)
  
  d <- a %>% 
    inner_join(b,by=c("season","PLAYERID"))


  c <-playerGame %>% 
    filter((plYrGameOrderApp==3&Gls>0))
  select(PLAYERID,season,Gls)
  
  e <- c %>% 
    inner_join(d,by=c("season","PLAYERID"))
  
  f <-playerGame %>% 
    filter((plYrGameOrderApp==4&Gls>0))
  select(PLAYERID,season,Gls)
  
  g <- f %>% 
    inner_join(e,by=c("season","PLAYERID")) %>% 
    filter(PLAYERID!="OWNGOAL")
  
# puncheon ----------------------------------------------------------------

playerGame %>% 
  filter(PLAYERID=="PUNCHEJ"&Gls>0) %>% 
  arrange(desc(gameDate)) %>% 
  select(gameDate)

playerGame %>% 
  filter(gameDate>as.Date("2016-05-15")&Gls>0) %>% 
  group_by(PLAYERID) %>% 
  summarise(ct=n(),totgls=sum(Gls)) %>% 
  arrange(desc(totgls))


# worst starts in EPL ------------------------------------------------------------

 last time 2 teams scoreless pointless after 4
sort(names(standings))

temp <-standings %>% 
  filter(tmYrGameOrder==4&cumPts==0) %>% 
  group_by(season) %>% 
  tally() %>% 
  arrange(desc(n))
 

First time 2 teams had 0 points after 4 games in PL seasonChoice
half of the other 8 were relegated


library(engsoccerdata)

pastYears <- england
currentYear<- england_current()
data <- rbind(pastYears,currentYear) %>% tbl_df()

home <- data %>% 
  select(team=home, opp=visitor, GF=hgoal, GA=vgoal,Season,tier,division,Date) %>% 
  mutate(venue="H")

glimpse(home)
away <- data %>% 
  select(team=visitor, opp=home, GF=vgoal, GA=hgoal,Season,tier,division,Date) %>% 
  mutate(venue="A")

df <- rbind(home,away) %>% 
  mutate(res=ifelse(GF>GA,"W",(ifelse(GF<GA,"L","D"))))

glimpse(df)

temp <- df %>% 
  group_by(team,Season,tier) %>% 
  arrange(Date) %>% 
  unique() %>% 
  mutate(gameOrder=row_number())  
  

x <-temp %>% 
  filter(gameOrder<5) %>% 
  mutate(losses=ifelse(res=="L",1,0)) %>% 
  group_by(Season,team,tier) %>% 
  summarise(l=sum(losses),cumGF=sum(GF)) %>% 
  filter(l==4&cumGF==0)
  

pa <-temp %>% 
  filter(team=="Plymouth Argyle") # looks like doubling up in 2016 at least maybe eng_current does this?
 

y <-temp %>% 
  filter(gameOrder<6) %>% 
  mutate(losses=ifelse(res=="L",1,0)) %>% 
  group_by(Season,team,tier) %>% 
  summarise(l=sum(losses),cumGF=sum(GF)) %>% 
  filter(l==5&cumGF==0)

# zabaleta/puncheons yellows --------------------------------------------------------

sort(names(playerGame))

temp <-playerGame %>% 
  arrange(desc(gameDate)) %>% 
  filter(LASTNAME=="Zabaleta")
no he didnt get one in 1 week(
  
) 
# used Puncheon in 
playerGame %>% 
  filter(PLAYERID=="ZABALEP"&mins>0) %>% 
  arrange(gameDate) %>% 
  select(name,Gls,CARD) %>% 
  mutate(booking=ifelse(!is.na(CARD),1,0)) %>% 
  mutate(cumGls=cumsum(Gls),cumCards=cumsum(booking)) %>% 
  mutate(apps=row_number()) %>% 
  plot_ly(x=~apps,y=~cumGls) %>% 
  add_lines(name="Goals") %>% 
  add_lines(x=~apps,y=~cumCards,name="Cards") 

temp <- playerGame %>% 
  filter(mins>0) %>% 
  arrange(gameDate) %>% 
  select(PLAYERID,name,Gls,CARD) %>%
  group_by(PLAYERID) %>% 
  mutate(booking=ifelse(!is.na(CARD),1,0)) %>% 
  mutate(cumGls=cumsum(Gls),cumCards=cumsum(booking)) %>% 
  mutate(apps=row_number()) %>%
  ungroup() %>% 
  arrange(desc(cumCards)) %>% 
  group_by(PLAYERID) %>%
  slice(1) %>% 
 ungroup() %>% 
  arrange(desc(cumCards))
 


# age of first game by club -----------------------------------------------

sort(names(playerGame)) 


playerGame %>% 
  filter(plTmGameOrder==1) %>% 
  filter(TEAMNAME=="Man. Utd.") %>% 
  arrange(gameDate) %>% 
 plot_ly(x=~gameDate,y=~age) %>% 
   add_markers(hoverinfo="text",
               text=~paste0(name,"<br>",age))
## look at cp number brought in total pl apps




# nEWCASTLE WINNIN 3 IN ROW? PREMIER --------------------------------------


# NO SCORING RUNS EVER/CURRENT/STARTING YEAR ------------------------------


# MAN CITY 6 HOW OFTEN AWAY -----------------------------------------------

sort(names(standings))

temp <- standings %>% 
  group_by(season,tmYrGameOrder) %>% 
  mutate(sumGF=sum(GF),pcGF=round(GF/sumGF,2)) %>% 
  arrange(desc(pcGF))
# good max man u 9/26 35
# man c currently 6/17 aslo 35 - but 2 games to come ended as 6é21 no big deal

standings %>% 
  filter(venue=="A"&(GF-GA)>5) # 11 times 9th 6-0 never by 

# CARDS ZABALETA PUNCHEON -------------------------------------------------

Puncheosn goals v cards over time

# oldest players ----------------------------------------------------------

# OptaJoeVerified account @OptaJoe  9h9 hours ago
# More
# 32 - Fernando Llorente (32y 201d) is the oldest outfield player to make a PL appearance for Spurs under Mauricio Pochettino. Experience.


# man city run of goals v watford without reply ---------------------------


# man city wins v watford -------------------------------------------------

#5 - Man City are only the 4th team in English top-flight history to win by 5+ goals in consecutive away games vs the same team. Rout.

library(engsoccerdata)
names(england)

head(england)

england %>% 
  arrange(Date) %>% 
  filter(tier==1) %>% 
  mutate(bigwin=ifelse((vgoal-hgoal)>4,1,0)) %>% 
  group_by(home,visitor)  %>% 
  mutate(lag1=lag(bigwin,1),tot=bigwin+lag1) %>% 
  filter(tot>1)


test <- england %>% 
  filter(home=="Derby County"&visitor=="Manchester City")





# teams not scoring in a round --------------------------------------------

temp <- standings %>% 
  filter(GF==0) %>% 
  group_by(tmYrGameOrder,season) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>=7) %>% 
  select(season,ct=n) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  tally()
## worth a tweet? maybe when differs


# Goals scored by time ----------------------------------------------------

sort(names(goals))
sort(names(standings))
sort(names(playerGame))

each <- playerGame %>% 
  select(PLAYER_MATCH,season,TEAMNAME) %>% 
  right_join(goals) %>% 
  select(season,TEAMNAME,TIME) %>% 
  filter(season=="2017/18")

all <- each %>% 
  group_by(TEAMNAME) %>% 
  tally()


each %>% 
  left_join(all) %>% 
  plot_ly(x=~jitter(TIME),y=~fct_reorder(TEAMNAME, n)) %>% 
  add_markers(hoverinfo="text",
              text=~paste0(TIME))

## looks plausible



# different golscorers ----------------------------------------------------

sort(names(playerGame))

pl_goals <-playerGame %>% 
  filter(season=="1992/93"&Gls>0) %>% 
  select(name,TEAMNAME,Gls) %>% 
  group_by(name,TEAMNAME) %>% 
  summarise(Goals=sum(Gls)) %>% 
  arrange(Goals)

tm_goals <- pl_goals %>% 
  group_by(TEAMNAME) %>% 
  summarise(tmGls=sum(Goals))
 

pl_goals %>% 
  left_join(tm_goals) %>% 
  #arrange(Goals) %>% no impact on plot
  plot_ly(x=~TEAMNAME,y=~Goals,color=~name,
          hoverinfo="text",
          text=~paste0(name,"<br>Goals: ",Goals)) %>% 
  add_bars(showlegend= FALSE) %>% 
  layout(barmode="stack")


plot_ly(x=~fct_reorder(month, monthOrder),y=~Gls,
        
        season <- "1992/93"
       theTitle<-paste0("Premier League Goals - ",season,"<br>Hover for Individual Player")
       #colours <- "12-class Paired"
        
        ## current;y alphabetical
     library(forcats)   
        pl_goals %>% 
          left_join(tm_goals) %>%  
          #arrange(Goals) %>% no impact on plot
          plot_ly(x=~Goals,y=~fct_reorder(TEAMNAME,tmGls),color=~name,colors="Set3",
                  hoverinfo="text",
                  text=~paste0(name,"<br>Goals: ",Goals)) %>% 
          add_bars(showlegend= FALSE) %>% 
          layout(barmode="stack",
                 margin=list(l=100,pad=5,t=50),
                 xaxis=list(title="Team Goals"),
                 yaxis=list(title=""),
                 title= theTitle)
        %>%  config(displayModeBar = F,showLink = F)
        
        ## add to premiersocccerstats
  


# Promoted Clubs winning three in succession ------------------------------
    
        ## from epl2018.rmd    
        
        positions <-  read_csv("data/positionSeqs.csv")
        
        promoted <- positions %>% 
          select(team,Season,tier) %>% 
          arrange(desc(Season)) %>% 
          group_by(team) %>% 
          mutate(prevYearTier=lead(tier)) %>% 
          filter(prevYearTier>tier&tier==1&Season>1991) #74 only 2 in 1995 when reducing 22 to 20
        
        ## excludes the 2017 three - need to update (not sure how arrived at could be engsoccerdata?)
        
        
        names(promoted)
        #Season
        
   sort(names(standings)) 
   
   sort(unique(standings$team))  # teams are diff names would need to qualify
   
   
   standings %>% 
     filter(season=="2017/18") %>% 
     select(team,res,gameDate) %>% 
     mutate(W=ifelse(res=="Win",1,0)) %>% 
     arrange(gameDate) %>% 
     group_by(team) %>% 
     mutate(lag1=lag(W,1),lag2=lag(W,2),wins=W+lag1+lag2) %>% 
     filter(wins==3)

   temp <-standings %>% 
      select(team,res,gameDate,season,tmYrGameOrder) %>% 
     mutate(W=ifelse(res=="Win",1,0)) %>% 
     arrange(gameDate) %>% 
     group_by(team,season) %>% 
     mutate(lag1=lag(W,1),lag2=lag(W,2),wins=W+lag1+lag2) %>% 
     filter(wins==3)


# goals as sub ------------------------------------------------------------

           summary %>% 
             group_by(season) %>% 
            summarise(start=sum(StGls,na.rm=T),sub=sum(subGls,na.rm=T),pc=round(100*sub/(sub+start),1)) %>% 
     plot_ly(x=~season, y=~pc) %>% 
     add_lines()

   temp <-summary %>% 
     filter(season=="2017/18") %>% 
     group_by(TEAMNAME) %>% 
     summarise(start=sum(StGls,na.rm=T),sub=sum(subGls,na.rm=T),pc=round(100*sub/(sub+start),1))
  5 of uniteds 
  
  
  temp <- summary %>% 
    filter(TEAMNAME=="Man. Utd.") %>% 
    group_by(season) %>% 
    summarise(start=sum(StGls,na.rm=T),sub=sum(subGls,na.rm=T),pc=round(100*sub/(sub+start),1))
  

# Palace run - may have something from last year book ---------------------

  # gap in GF after 6 games  ------------------------------------------------
  
  
  
  test <- standings %>% 
    filter(tmYrGameOrder==6) %>% 
    group_by(season) %>% 
    mutate(minGF=min(cumGF),diff=cumGF-minGF) %>%
    arrange(desc(diff)) %>% 
    slice(1) %>% 
    select(season,team,diff)  
  
  ## Is a record - could show gap - have a crosstalk comparison showing after so many games
  
  
  top <- standings %>% 
    filter(tmYrGameOrder==6) %>% 
    group_by(season) %>% 
    mutate(minGF=min(cumGF),diff=cumGF-minGF) %>%
    arrange(desc(diff)) %>% 
    slice(1) %>% 
    select(season,top=team,diff,max=cumGF) 
  
  bottom <- standings %>% 
    filter(tmYrGameOrder==6) %>% 
    group_by(season) %>% 
    mutate(maxGF=max(cumGF),diff=cumGF-maxGF) %>%
    arrange(diff) %>% 
    slice(1) %>% 
    select(season,bottom=team,min=cumGF) %>% 
    inner_join(top,by="season")
    
  library(forcats) # fct_reorder(TEAMNAME,tmGls)
  
  ## this separates out the segensts form markers - though everything is in right place
p <-  bottom %>% 
    plot_ly(color = I("gray80")) %>% 
    add_segments(x = ~min, xend = ~max, y = ~fct_reorder(season,diff), yend = ~fct_reorder(season,diff), showlegend = FALSE)  
p

 p %>%
    add_markers(y=~~fct_reorder(season,diff),x=~max,name="Prolific",color = I("blue"),
                hoverinfo="text",
                text=~paste(top,max)) %>% 
    add_markers(y=~~fct_reorder(season,diff),x=~min,name="Impotent",color = I("red"),
                hoverinfo="text",
                text=~paste(bottom,min))
  
  
  ## this separates out the segensts form markers - though everything is in right place
  bottom %>% 
    plot_ly(color = I("gray80")) %>% 
    add_segments(x = ~min, xend = ~max, y = ~fct_reorder(season,diff), yend = ~fct_reorder(season,diff), showlegend = FALSE)   %>%
    add_markers(y=~~fct_reorder(season,diff),x=~max,name="Prolific",color = I("blue"),
                hoverinfo="text",
                text=~paste(top,max)) %>% 
    add_markers(y=~~fct_reorder(season,diff),x=~min,name="Impotent",color = I("red"),
                hoverinfo="text",
                text=~paste(bottom,min))
  
  ## this works but orders by season 
  bottom %>% 
    plot_ly(color = I("gray80")) %>% 
    add_segments(x = ~min, xend = ~max, y = ~season, yend = ~season, showlegend = FALSE
                 ) %>%
      add_markers(y=~season,x=~max,name="Prolific",color = I("blue"),
                hoverinfo="text",
                text=~paste(top,max)) %>% 
    add_markers(y=~season,x=~min,name="Impotent",color = I("red"),
                hoverinfo="text",
                text=~paste(bottom,min)) %>% 
    layout(title="Most and least prolific teams after 6 Premier League games",
      xaxis=list(title="Goals Scored"),
           yaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
           
   # would like hoverinfo over line but would need intervening points to be interpolated (not worth it) 

# Gareth Barry apps -------------------------------------------------------

? ## cf Giggs - who was last player to appear form first round
    
    ## mins bit of estimate
    
    temp <- playerGame %>% 
    select(name,PLAYERID,mins) %>% 
    group_by(PLAYERID) %>% 
    summarize(tot=sum(mins,na.rm=T)) %>% 
    arrange(desc(tot)) %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
  ## do a cumulative of top 4 and next to barry currently
  
  players <- c("BARRYG","JAMESD","LAMPARF","GIGGSR","CARRICM")
  
  playerGame %>% 
    filter(PLAYERID %in% players) %>% 
    arrange(gameDate) %>% 
    group_by(PLAYERID) %>% 
    mutate(cummins=cumsum(mins)) %>% 
    plot_ly(x=~gameDate,y=~cummins, color=~name) %>% 
    add_lines() %>% 
    layout(title="Cumulative Minutes played by Leading Players",
           xaxis=list(title=""),
           yaxis=list(title="Cumulative Minutes '000")) %>%  config(displayModeBar = F,showLink = F)
           
  

# de bruyne 3 consec 2 assist games ---------------------------------------

playerGame %>% 
    filter(mins>0) %>% 
    arrange(gameDate) %>% 
    group_by(PLAYERID) %>% 
    mutate(val=(ifelse(Assists>1,1,0))) %>% 
    mutate(lag1=lag(val,1),lag2=lag(val,2),all=val+lag1+lag2) %>% 
    select(gameDate,name,PLAYERID,Assists,val,lag1,lag2,all) %>% 
    #filter(!is.na(all)) %>% 
    filter(all==3)

  ##     
## look at eriksens run
  
  library(RcppRoll)
  
  
 test <- playerGame %>% 
    filter(mins>0) %>% 
    arrange(gameDate) %>% 
    group_by(PLAYERID) %>% 
    select(name,PLAYERID,Assists) %>% 
    mutate(Ass=as.integer(Assists))
 
 str(test)
  
  
    roll_sum(test$Assists,18)
    
    df <- playerGame %>% 
      filter(mins>0) %>% 
      arrange(gameDate,PLAYERID) %>% 
      select(PLAYERID,name,Assists) %>% 
      group_by(PLAYERID) %>% 
      mutate(games=n()) %>% 
      filter(games>17)
    
    # 
    # myFun <- function(id){
    #   df %>% 
    #     filter(PLAYERID==id)
    #     roll_sum(Assists,18)
    # }
    # 
    # 
    # players <- unique(df$PLAYERID)
    # 
    # map_df(players,myFun)
    # 
    
    players <- unique(df$PLAYERID)
    
    get_points <- function(id) {
      career <-df %>% 
        filter(PLAYERID==id)
     print(id)
      
      
      # construct data.frame of results
      run <- roll_sum(career$Assists,n=18)
      data.frame(points=run,seq=1:length(run))
      
      
    }
    
    # apply the above function to all teams
    data <-map_df(players, get_points)
    
    
    df %>% 
      filter(PLAYERID=="BARLOWA")
    
    career <-df %>% 
      filter(PLAYERID=="ERIKSEC")
    
    # construct data.frame of results
    run <- roll_mean(career$Assists,n=18)
    data.frame(points=run,seq=1:length(run))
    
    get_runs <- function(x) {
      tm <-standings %>% 
        filter(team==x) %>% 
        arrange(tmGameOrder)
      
      # construct data.frame of results
      run <- roll_sum(tm$points,n=10)
      data.frame(points=run,seq=1:length(run),team=x)
      
      
    }
    
    # apply the above function to all teams
    data <-map_df(teams, get_runs)
    
    
    l=list(x=c("a","b","a","b","a","b"),y=c(1,4,3,3,7,0))
    
    df <- as_tibble(l)
    df
    
    names(data)
max(data$points)    #25


ret <- reprex({
  library(tibble)
  l=list(x=c("a","b","a","b","a","b"),y=c(1,4,3,3,7,0))
  
  df <- as_tibble(l)
  df
}, venue = "so")
ret

## stackoverflow alts

df <- playerGame %>% 
  filter(mins>0) %>% 
  arrange(gameDate,PLAYERID) %>% 
  select(PLAYERID,name,Assists) %>% 
  group_by(PLAYERID) %>% 
  mutate(games=n())

%>% 
  filter(games>17)

library(zoo)
library(dplyr)

library(tibble)
l = list(x = c("a", "b", "a", "b", "a", "b"), y = c(1, 4, 3, 3, 7, 0))

df <- as_tibble(l)

system.time(
 
df2 <-  df %>%
    group_by(x) %>%
  mutate(sum=c(NA,rollapply(y, width=2, sum)),seq=row_number()-1) %>%
    drop_na()
  )  #0.02    0.00    0.01

system.time(
df %>%
  group_by(x) %>%
  do(data_frame(x = unique(.$x), 
                sum = rollapplyr(.$y, width = 2, FUN = sum))) %>%
  mutate(seq = 1:n()) %>%
  ungroup()
) #user  system elapsed 
# 0.04    0.00    0.05 


library(RcppRoll); library(tidyverse)

n = 2
system.time(

df %>% 
  group_by(x) %>% 
  do(
    data.frame(
      sum = roll_sum(.$y, n), 
      seq = seq_len(length(.$y) - n + 1)
    )
  )
)
# user  system elapsed 
# 0.01    0.00    0.01 

system.time(
group_by(df, x) %>%
  mutate(sum = y + lead(y),
         seq = row_number()) %>%
  arrange(x) %>%
  ungroup %>%
  filter(complete.cases(.))
)
library(tidyverse)

df <- playerGame %>% 
  filter(mins>0) %>% 
  arrange(gameDate,PLAYERID) %>% 
  select(PLAYERID,name,Assists) %>% 
  group_by(PLAYERID) %>% 
  mutate(games=n()) %>% 
  filter(games>17)

system.time(
df2 <-  df %>%
  group_by(PLAYERID) %>%
  mutate(sum=c(NA,rollapply(Assists, width=17, sum)),seq=row_number()-1) %>%
  drop_na()
) 
)
#user  system elapsed 
#3.69    0.03    3.77 

  system.time(
    
    df2 <-  df %>%
      group_by(PLAYERID) %>%
      mutate(sum=c(NA,rollapply(Assists, width=17, sum)),seq=row_number()-1) %>%
      drop_na()
  ) 
  # Error in mutate_impl(.data, dots) : 
  #   Column `tot` must be length 32 (the group size) or one, not 17

system.time(
df2 <-  df %>%
    group_by(PLAYERID) %>%
    do(data_frame(x = unique(.$PLAYERID), 
                  sum = rollapplyr(.$Assists, width = 17, FUN = sum))) %>%
    mutate(seq = 1:n()) %>%
    ungroup()
) #user  system elapsed 
#33.17    0.19   34.03 


library(RcppRoll); library(tidyverse)

n = 17
system.time(
  
 df2 <- df %>% 
    group_by(PLAYERID) %>% 
    do(
      data.frame(
        sum = roll_sum(.$Assists, n), 
        seq = seq_len(length(.$Assists) - n + 1)
      )
    )
)

# Beckham 24 169

# user  system elapsed 
# 8.46    0.03    8.64 b
# user  system elapsed 
# 5.04    0.03    5.19 


n=17
system.time(
  df2 <-  df %>%
    group_by(PLAYERID) %>%
    do(data_frame(x = unique(.$PLAYERID), 
                  sum = rollapplyr(.$Assists, width = n, FUN = sum))) %>%
    mutate(seq = 1:n()) %>%
    ungroup()
)


system.time(
  df2 <-  df %>%
    group_by(PLAYERID) %>%
    do(data_frame(x = unique(.$PLAYERID), 
                  sum = roll_sum(.$Assists, width = n))) %>%
    mutate(seq = 1:n()) %>%
    ungroup()
)


n = 17
system.time(
  
  df2 <- df %>% 
    group_by(PLAYERID) %>% 
    do(
      data.frame(
        sum = roll_sum(Assists, n)
        
      ) 
    ) %>% 
  mutate(seq = 1:n()) %>%
)

system.time(
  
  df2 <- df %>% 
    group_by(PLAYERID) %>% 
    do(
      data.frame(
        sum = roll_sum(.$Assists, n)
        
      ) 
    ) %>% 
    mutate(seq = 1:n()) 
)


## data.table approach
library(data.table)
library(RcppRoll)
system.time(
  df[, .(sum = RcppRoll::roll_sum(Assists, n = 17L, fill = NA, align = "left"),
        seq = seq_len(.N)),
    keyby = .(PLAYERID)][!is.na(sum)]
)

l = list(x = c("a", "b", "a", "b", "a", "b"), y = c(1, 4, 3, 3, 7, 0))

df <- as_tibble(l)

l[, .(sum = RcppRoll::roll_sum(y, n = 2L, fill = NA, align = "left"),
      seq = seq_len(.N)),
  keyby = .(x)][!is.na(sum)]

## stackoverflow using data.table

library(data.table)
library(RcppRoll)

ptm <- proc.time() ## Start the clock


n=17
dplyr_result <- df %>% 
  group_by(PLAYERID) %>% 
  do(
    data.frame(
      sum = roll_sum(.$Assists, n), 
      seq = seq_len(length(.$Assists) - n + 1)
    )
  )
|========================================================0% ~0 s remaining     

dplyr_time <- proc.time() - ptm ## Stop the clock

## Using data.table instead ----------------------------------------------

library(data.table)

ptm <- proc.time() ## Start the clock

setDT(df) ## Convert l to a data.table
dt_result <- l[,.(sum = RcppRoll::roll_sum(Assists, n =n, fill = NA, align = "left"),
                  seq = seq_len(.N)),
               keyby = .(PLAYERID)][!is.na(sum)]

data.table_time <- proc.time() - ptm ## Stop the clock


library(tibble)
library(dplyr)
library(RcppRoll)
library(stringi) ## Only included for ability to generate random strings

## Generate data with arbitrary number of groups and rows --------------

rowCount   <- 100000
groupCount <- 10000
sumRows    <- 2L
set.seed(1)

l <- tibble(x = sample(stri_rand_strings(groupCount,3),rowCount,rep=TRUE),
            y = sample(0:10,rowCount,rep=TRUE))

## Using dplyr and tibble -----------------------------------------------

ptm <- proc.time() ## Start the clock

dplyr_result <- l %>% 
  group_by(x) %>% 
  do(
    data.frame(
      sum = roll_sum(.$y, sumRows), 
      seq = seq_len(length(.$y) - sumRows + 1)
    )
  )
|========================================================0% ~0 s remaining     

dplyr_time <- proc.time() - ptm ## Stop the clock

## Using data.table instead ----------------------------------------------

library(data.table)

ptm <- proc.time() ## Start the clock

setDT(l) ## Convert l to a data.table
dt_result <- l[,.(sum = RcppRoll::roll_sum(y, n = sumRows, fill = NA, align = "left"),
                  seq = seq_len(.N)),
               keyby = .(x)][!is.na(sum)]

data.table_time <- proc.time() - ptm ## Stop the clock


## Using dplyr and tibble -----------------------------------------------



sumRows <- 17L
ptm <- proc.time() ## Start the clock

dplyr_result <- df %>% 
  group_by(PLAYERID) %>% 
  do(
    data.frame(
      sum = roll_sum(.$Assists, sumRows), 
      seq = seq_len(length(.$Assists) - sumRows + 1)
    )
  )
|========================================================0% ~0 s remaining     

dplyr_time <- proc.time() - ptm ## Stop the clock

## Using data.table instead ----------------------------------------------

library(data.table)

ptm <- proc.time() ## Start the clock

setDT(df) ## Convert l to a data.table
dt_result <- l[,.(sum = RcppRoll::roll_sum(Assists, n = sumRows, fill = NA, align = "left"),
                  seq = seq_len(.N)),
               keyby = .(PLAYERID)][!is.na(sum)]

data.table_time <- proc.time() - ptm ## Stop the clock





> dplyr_time
user  system elapsed 
10.28    0.04   10.36 
> data.table_time
user  system elapsed 
0.35    0.02    0.36 

> all.equal(dplyr_result,as.tibble(dt_result))
[1] TRUE


# pss factoid -------------------------------------------------------------

## most complete games

playerGame %>% 
  filter(mins>=90) %>% 
  group_by(PLAYERID) %>% 
  tally() %>% 
  arrange(desc(n))



# number of goals scorers this year ---------------------------------------

sort(names(playerGame))

temp <- playerGame %>% 
  filter(PLAYERID!="OWNGOAL"&Gls>0) %>% 
  select(PLAYERID,TEAMNAME,season) %>% 
  unique() %>% 
group_by(TEAMNAME,season) %>% 
  tally()
 

# exploding boxplot pts after 7 games -------------------------------------

sort(names(standings))

test <-standings %>% 
  filter(tmYrGameOrder==7) %>% 
  #plot_ly(x=~season,y=~cumPts) %>% 
 # group_by(season) %>% 
  #summarise(range=max(cumPts)-min(cumPts)) %>% ## confirmed range 19 greatest
  exploding_boxplot(y="cumPts",group="season",color="season",label="team")



standings %>% 
  filter(tmYrGameOrder==7) %>% 
  mutate(year=str_sub(season,1,4)) %>% 
  exploding_boxplot(y="cumPts",group="year",color="year",label="team")



# repeating top 6 ---------------------------------------------------------


sort(names(standings))

standings %>% 
  filter(tmYrGameOrder==1&final_Pos<7&season<"2017/18") %>% 
  group_by(season) %>% 
  pull(team)


seasons <-standings %>% 
  filter(season<"2017/18") %>% 
  select(season) %>% 
  unique() %>% 
  arrange(season) %>% 
  pull(season)

x <- 1
seasons[x]

myFun <- function(x){
yr1 <- standings %>% 
  filter(tmYrGameOrder==1&final_Pos<7&season=="1992/93") %>% 
  pull(team)

yr2 <- standings %>% 
  filter(tmYrGameOrder==1&final_Pos<7&season=="1993/94") %>% 
  pull(team)

length(intersect(yr1,yr2))
}

seasons <-standings %>% 
  filter(season<"2017/18") %>% 
  select(season) %>% 
  unique() %>% 
  arrange(season) %>% 
  pull(season)

myFun <- function(x){
  yr1 <- standings %>% 
    filter(tmYrGameOrder==1&final_Pos<7&season==seasons[x]) %>% 
    pull(team)
  
  yr2 <- standings %>% 
    filter(tmYrGameOrder==1&final_Pos<7&season==seasons[x+1]) %>% 
    pull(team)
  
  length(intersect(yr1,yr2))
}

count=map_int(1:24,myFun)
df <- data.frame(season=seasons[2:25],count=count)
df


myFun <- function(x){
  yr1 <- standings %>% 
    filter(tmYrGameOrder==1&final_Pos<7&season==seasons[x]) %>% 
    pull(team)
  
  yr2 <- standings %>% 
    filter(tmYrGameOrder==1&final_Pos<7&season==seasons[x+1]) %>% 
    pull(team)
  
  length(intersect(yr1,yr2))
}

df <- data.frame(season=seasons[2:25],count=map_int(1:24,myFun))
df



# Burnley away points cf last year ----------------------------------------
## ctr Chelsea dropping at home





# Coutinho share from outside box - think did something similar -----------
now as many as inside - point to app in premiersoccerstats



# set minimum goals 
minGoals <- Place %>% 
  mutate(goals=(SixYd+PenArea+LongRange)) %>% 
  group_by(PLAYERID,name) %>% 
  summarise(tot=sum(goals)) %>% 
  #filter(tot>=input$pcPlGoals) %>% 
  filter(tot>=36) %>% 
  .$PLAYERID


df <- Place %>% 
  filter(PLAYERID %in% minGoals&PLAYERID!="OWNGOAL") %>% 
  mutate(goals=(SixYd+PenArea+LongRange)) %>% 
  group_by(PLAYERID,name) %>% 
  summarise(tot=sum(goals),lr=sum(LongRange),pc=round(100*lr/tot)) %>% 
  ungroup() %>% 
  arrange(desc(pc))  

df$jitpc <- jitter(df$pc, amount=0.2)
df$jittot <- jitter(df$tot, amount=0.2)


df %>% 
  plot_ly() %>% 
  add_markers(x = ~jittot, y = ~jitpc,  hoverinfo = "text",
              text = ~ paste(name,
                             "<br>Long Range: ",lr,
                             "<br>Total: ",tot,
                             "<br>PerCent: ",pc,"%")) %>%
  layout(hovermode = "closest",
         height = 400,
         autosize = F,
         #title=paste0(pcPlayerGoalsData()$category," as % of Premier League Goals"),
         title="Long Range as % of Premier League Goals",
         xaxis=list(title="Total Goals"),
         yaxis=list(title="% Long Range"
         )
  )  %>% 
  config(displayModeBar = F,showLink = F)




# Lukaku goalscoring ------------------------------------------------------

movina average in 1000 minute chunks


# not year of the ox ------------------------------------------------------

3rd longest drought for assists and second for goals 2021



# use of timetibble -------------------------------------------------------

library(tibbletime)

sort(names(standings))


standings <- as_tbl_time(standings, index = gameDate)

temp <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(gameDate) %>%  # need to be sure to do this first
  as_period(1~y, side = "end") %>% 
  select(gameDate,cumPts,position,final_Pos)


# 3 of last 4 seasons have been 6th


temp <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  arrange(gameDate) %>%  # need to be sure to do this first
  as_period(1~m, side = "end") %>% 
  select(gameDate,cumPts,position,final_Pos)
# should look to see if could add field for month

standings %>% 
  filter(team=="Crystal P") %>% 
  arrange(gameDate) %>%  # need to be sure to do this first
  as_period(1~m, side = "end") %>% 
  select(gameDate,cumPts,position,final_Pos) %>% 
  mutate(month=lubridate::month(gameDate)) %>% 
  filter(month==9)



# Man C goals for ---------------------------------------------------------

standings %>% 
  filter(tmYrGameOrder<=8) %>% 
  select(team,tmYrGameOrder,season,cumGF) %>% 
  arrange(season,tmYrGameOrder) %>% 
  group_by(season,team) %>% 
  plot_ly(x=~tmYrGameOrder,y=~cumGF,
          hoverinfo="text",
          text=~paste0(team,"<br>",season,"<br>",
                       cumGF," goals")) %>% 
  add_lines(color=I("lightgrey")) %>% 
  add_markers(data=subset(standings,season=="2017/18"),
            x=~jitter(tmYrGameOrder),y=~jitter(cumGF),color=I("blue"))

prev <- standings %>% 
  filter(tmYrGameOrder<=8&season<"2017/18") %>% 
  select(team,tmYrGameOrder,season,cumGF) %>% 
  arrange(season,tmYrGameOrder) %>% 
  group_by(season,team)

current <- standings %>% 
  filter(tmYrGameOrder<=8&season=="2017/18") %>% 
  select(team,tmYrGameOrder,season,cumGF) %>% 
  arrange(season,tmYrGameOrder) %>% 
  group_by(season,team)

manC <- standings %>% 
  filter(tmYrGameOrder<=8&season=="2017/18"&team=="Man. City") %>% 
  select(team,tmYrGameOrder,season,cumGF) %>% 
  arrange(season,tmYrGameOrder) %>% 
  group_by(season,team)

prev %>%        
plot_ly(x=~tmYrGameOrder,y=~cumGF,
        hoverinfo="text",
        text=~paste0(team,"<br>",season,"<br>",
                     cumGF," goals")) %>% 
  add_lines(color=I("lightgrey"), name="Prev Years") %>% 
  add_lines(data=current,x=~tmYrGameOrder,y=~cumGF,color=I("red"),name="2017/18") %>% 
  add_lines(data=manC,x=~tmYrGameOrder,y=~cumGF,color=I("blue"),name="Man c 17/18")

lowScorers <- standings %>% 
  filter(tmYrGameOrder==38) %>% 
  select(team,season,goals=cumGF) %>% 
  arrange(goals) %>%
                         DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


temp <-standings %>% 
  filter(team=="Man. City"&GF>6) %>% 
  arrange(gameDate)


# Lukaku flat track -------------------------------------------------------




#  Arsenal conceding away from home 2017 --------
library(stringr)

sort(names(standings))

temp <-standings %>% 
  filter(venue=="A") %>% 
  select(gameDate,GA,team) %>% 
  mutate(year=str_sub(gameDate,1,4)) %>% 
  group_by(year,team) %>% 
  summarise(tot=sum(GA)) %>% 
  arrange(desc(tot))

x <-standings %>% 
  mutate(year=str_sub(gameDate,1,4)) %>% 
  filter(year=="2011"&team=="Arsenal"&venue=="A") %>% 
  select(OppTeam,GA,GF)

y <-temp %>% 
  filter(team=="Arsenal") %>% 
  plot_ly(x=~year,y=~tot) %>% 
  
  
  



# mertesacker gap between goals -------------------------------------------

#follow up what already done?


# de bruyne assits --------------------------------------------------------

playerGame %>% 
  arrange(desc(Assists)) %>% 
  select(name, Assists,TEAMNAME,Opponents,gameDate,mins) %>% 
  filter(TEAMNAME=="Man. City")


# man city no loss in 16 --------------------------------------------------

library(RcppRoll)

test <- standings %>% 
  filter(team=="Man. City") %>% 
  arrange(gameDate) %>% 
  select(res) %>% 
  mutate(nl=ifelse(res=="Loss",0,1)) 


run <- roll_sum(test$nl,n=21) ## poss more def still some way short
df <- data.frame(run=run,seq=1:length(run))

# produce chart
df %>% 
  plot_ly(x=~seq,y=~run) %>% 
  add_lines()  


# mins before 1st goal score of season ------------------------------------


# oldest pplayer this year ------------------------------------------------

test <-playerGame %>% 
  filter(season=="2017/18"&mins>0) %>% 
  arrange(desc(age)) %>% 
  select(name,age,POSITION,TEAMNAME)

yep


# manager heatmaps --------------------------------------------------------

#redknapp comment in mail mourinho not many 3-2 wins

# need to do a sort of expand grid for manager

already in manager game

allGames <-managerGame %>% 
   group_by(ManagerID,name) %>% 
  tally() %>% 
  rename(all=n)


managerGame %>% 
  filter(GF==0&GA==0) %>%
  filter(ManagerID=="MourinhoJ")
  
managerGame %>% 
    filter(GF==0&GA==0) %>% 
  group_by(ManagerID,name) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  left_join(allGames) %>% 
  mutate(pc=round(100*n/all,1)) %>% 
  arrange(desc(n),desc(pc)) %>% 
  select(manager=name,`3-2`=n,all,`%`=pc) %>% 
 DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

# cf ferguson win %


managerGame %>% 
  filter(GF>GA) %>%
  filter(ManagerID=="MourinhoJ")
164/258 #63%

managerGame %>% 
  filter(GF>GA) %>%
  filter(ManagerID=="FergusonA")
528/810 #65%

# heatmap cf teams one

temp <- managerGame %>%
  filter(ManagerID=="MourinhoJ") %>%
   mutate(combo=paste0(GF,GA)) %>%
  group_by(combo) %>%
  tally()


allCombos <- expand.grid(
  data.frame(GF=0:9,GA=0:9)
) %>%
  mutate(combo=paste0(GF,GA)) #still a df with 100vals

#   test <- allCombos %>%
#     left_join(temp) # lots of NAs
#   
#   # seems pretty pointless renaming does same
#   test <- test %>%
#     mutate(count=(n))

test <- allCombos %>%
  left_join(temp) %>% 
  select(GF,GA,count=n)

# need to transform
Games <- t(matrix(test$count, nrow = 10, ncol = 10, byrow = TRUE,
                  dimnames = list(unique(test$GF),
                                  unique(test$GA))))


plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = Games, hoverinfo="z",
        colorscale='YIOrRd', reversescale=T,
        type = "heatmap") %>%
  layout(xaxis = list(title = "Goals Against"), 
         yaxis = list(title = "Goals For"))

## slection
select <-managerGame %>% 
  select(name,ManagerID) %>% 
  arrange(name) %>% 
  unique()

managerChoice <- select$ManagerID
names(managerChoice) <- select$name


# scoring against goalie --------------------------------------------------


sort(names(goals))

sort(names(playerGame))


# can just use playerGame

goalies <- playerGame %>% 
  filter(str_sub(POSITION,1,1)=="G"&mins>0) %>% 
  select(MATCHID,TEAMMATCHID,name,PLAYERID,gameDate,season)

sort(names(goalies))

test <-playerGame %>% 
  filter(mins>0) %>% 
  left_join(goalies, by="MATCHID") %>% 
  filter(TEAMMATCHID.x!=TEAMMATCHID.y)  %>% 
  select(player=name.x,TEAMNAME,Gls,mins,gameDate=gameDate.x,PLAYERID.x,PLAYERID.y,goalie=name.y,Opponents,season=season.x)

sort(names(test))

head(test)

temp <-
  test %>% 
  group_by(player,PLAYERID.x,PLAYERID.y,goalie) %>% 
  summarise(apps=n(),totMins=sum(mins),goals=sum(Gls)) %>% 
  ungroup()

# this takes a demobstrable time before ungrouping
rooney  <- temp %>% 
  filter(PLAYERID.x=="ROONEYX") %>% 
  filter(goals>0)

## courtois guess

test %>% 
  filter(PLAYERID.x=="ROONEYX"&PLAYERID.y=="SCHMEIK")

str(temp)

rooney %>% 
  plot_ly(x=~totMins,y=~goals,
          hoverinfo="text",
          text=~paste0(goalie,"(",goals,")")) %>% 
  layout(title="Rooney EPL goals by Opponent Goalie",
         xaxis=list(title=("minutes Played")),
           yaxis=list(title="Goals scored")) %>%  config(displayModeBar = F,showLink = F)
           
    
    
x <-    playerGame %>% 
      filter(PLAYERID=="ROONEYX"&Opponents=="Southampton")
  ) 


# most opp goalies

temp %>% 
  filter(goals>0) %>% 
  group_by(PLAYERID.x,player) %>% 
  tally() %>% 
  arrange(desc(n))


playerGame %>% 
  arrange(gameDate) %>% 
  filter(PLAYERID=="ROONEYX"&Gls>0) %>% 
  select(gameDate,Gls,Opponents)

# Record crowds -----------------------------------------------------------

could do changes by team and overall

# WBA late goals this yr (and palaces)


sort(names(goals))
sort(names(playerGame))



# goals conceded ----------------------------------------------------------

# 16 - This is the highest amount of goals Liverpool have conceded after nine top-flight games since the 1964-65 campaign (20). Sloppy.
# 
# could do for all premier league teams
# 
# Kane's goal was the 1,000th Liverpool have conceded in the Premier League
# 
# #LFC are the 6th team to concede that many

y <-playerGame %>% 
  group_by(Opponents) %>% 
  summarize(Goals=sum(Gls)) %>% 
  arrange(desc(Goals))
#1002
x <-playerGame %>% 
  filter(Opponents=="Liverpool") %>% 
  group_by(name,PLAYERID) %>% 
  summarize(Goals=sum(Gls)) %>% 
  arrange(desc(Goals))


z <-playerGame %>% 
  group_by(Opponents,TEAMNAME) %>% 
  summarize(Goals=sum(Gls)) %>% 
  arrange(desc(Goals))



# manager sackings --------------------------------------------------------

# de Boeuf - tactics did not match team skills
# shakespeare - probably not board choice team stll looked pretty but champions good
# Koeman - flawed spending on 
# 
# Bilic/Klopp/Hughes/Pulis all under scrutiny
# mainly because promoted clubs have all started better than anticipated


sort(names(managers))

library(lubridate)
temp <-managers %>% 
  filter(is.na(Caretaker)&Left!=("2017-10-23")) %>% 
  mutate(year=year(Left),month=month(Left),day=day(Left)) %>% 
  select(ManagerID,TeamID,name,year,month,day,Left) %>% 
  filter(month %in% 8:9|(month==10&day<24)) %>% 
  group_by(year) %>% 
  arrange(Left) %>% 
  mutate(order=row_number())

temp %>% 
  count(year) %>% 
  plot_ly(x=~year,y=~n) %>% 
  add_bars()
  
temp %>% 
  plot_ly(x=~Left,y=~order) %>% 
  
  ## ages of players
  
sort(names(summary))

ages <-summary %>% 
  filter(season=="2017/18"&PLAYERID!="OWNGOAL") %>% 
  mutate(age=(Sys.Date()-born)/365.25) %>% 
  select(name,TEAMNAME,mins,age)
  
ages %>% 
  filter(TEAMNAME=="Everton") %>% 
  plot_ly(x=~age,y=~mins,
          hoverinfo="text",
          text=~paste0(name,"<br>Age: ",round(age,0),
                       "<br>Mins: ",mins))

top11 <- ages %>% 
  arrange(desc(mins)) %>% 
  group_by(TEAMNAME) %>% 
  slice(1:11)

old <- top11 %>% 
  filter(age>32) %>% 
  mutate(age=round(age,1)) %>% 
  group_by(TEAMNAME) %>% 
  select(player=name,team=TEAMNAME,age,mins) %>% 
  
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# ages of team ------------------------------------------------------------

#Number in sweet spot 25-27 Everton
# contrast with promoted clubs



# Kanw assists per minute --------------------------------------------------




# mnc gls+assists ---------------------------------------------------------

sort(names(summary))


sort(names(playerGame))


playerGame %>% 
  group_by(PLAYERID,name,TEAMNAME,season) %>% 
  summarize(a=sum(Assists),b=sum(Gls),points=a+b) %>% 
  filter(season=="2017/18"&points>=7) %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
  arrange(desc(n))



# palaces no draw run -----------------------------------------------------


sort(names())

sort(names(standings))

library(RcppRoll)

cp <-standings %>% 
mutate(d=ifelse(res=="Draw",1,0)) %>% 
 arrange(gameDate) %>% 
  select(team,d) %>% 
  group_by(team) %>% 
  filter(team=="Crystal P")

x <- subSeq(test$d)
# Efficient windowed / rolling operations. Each function here applies an operation over a moving window of size n, with (customizable) weights specified through weights.

# not really appropriate


test <-standings %>% 
  mutate(d=ifelse(res=="Draw",1,0)) %>% 
  arrange(gameDate) %>% 
  select(team,d)

%>% 
  group_by(team)

%>% 
  summarise(maxD=do(subSeq(.$d)))


#This works alone
subSeq(cp$d)


standings %>% 
  mutate(d=ifelse(res=="Draw",1,0)) %>% 
  arrange(gameDate) %>% 
  select(team,d) %>% 
  group_by(team) %>% 
  filter(team=="Crystal P") %>% 
  do(subSeq(.$id))


y <-map_df(cp$d, function(x) subSeq(.d))


map_df(cp$d, ~ subSeq(.x)) this does not work just repeats value
#> $`Theon Greyjoy`
map(aliases, ~ paste(.x, collapse = " | "))
#> $`Theon Greyjoy`


my fun <- function(x) {
  maxRun <- test %>% 
    filter(team==x) %>% 
    do()
  
}

from existing code

D <-standings %>% 
  ungroup() %>% 
  filter(team=="Crystal P") %>% 
  arrange(tmGameOrder) %>% 
  select(res,tmGameOrder) %>% 
  mutate(cat=ifelse(res=="Draw",1,0)) %>% 
  do(subSeq(.$cat)) %>% 
  filter(value==0) %>% 
  arrange(desc(slength)) %>% 
  slice(1)

allteams <- unique(standings$team)

myFun <- function(x) {
  standings %>% 
    ungroup() %>% 
    filter(team==x) %>% 
    arrange(tmGameOrder) %>% 
    select(res,tmGameOrder) %>% 
    mutate(cat=ifelse(res=="Draw",1,0)) %>% 
    do(subSeq(.$cat)) %>% 
    filter(value==0) %>% 
    arrange(desc(slength)) %>% 
    slice(1)
  
}

tms <- unique(standings$team)

df <- map_df(tms,myFun)

df <- cbind(df,tms)



# biggest away draws ------------------------------------------------------

sort(names(teamGames))

# most pop away team

test <- teamGames %>% 
  filter(season=="2017/18") %>% 
  filter(TEAMNAME=="Liverpool")  %>% 
  inner_join(teamGames,by="MATCHID") %>% 
  filter(TEAMID.x==TEAMID.y) %>% 
  rename(Opponents=TEAMNAME.y,crowd=CROWD.x) %>% 
  select(team=TEAMNAME.x,Opponents,crowd) %>% 
  arrange(desc(crowd))



 teamGames %>% 
  #filter(season=="2016/17") %>% 
  filter(TEAMNAME=="Liverpool"&venue=="H")  %>% 
  inner_join(teamGames,by="MATCHID") %>% 
  filter(TEAMID.x!=TEAMID.y) %>% 
  rename(Opponents=TEAMNAME.y,crowd=CROWD.x) %>% 
   mutate(day=wday(gameDate.x,label=TRUE)) %>% 
  select(team=TEAMNAME.x,Opponents,crowd,season=season.x,day) %>% 
   plot_ly(x=~season,y=~crowd) %>% 
   add_markers(hoverinfo="text",
               text=~paste0(Opponents,"<br>",crowd,
                            "<br>", day)) %>%  config(displayModeBar = F,showLink = F)
               

sort(names(teamGames))
  

teamGames %>% 
  arrange(desc(CROWD))


prob <-teamGames %>% 
  filter(CROWD==9999&venue=="H")

## look at cummax crowd
cummax(c(3:1, 2:0, 4:2))

teamGames %>% 
  filter(venue=="H") %>% 
  arrange(gameDate,desc(CROWD)) %>% 
  mutate(maxcrowd=cummax(CROWD)) %>% 
  select(gameDate,CROWD,maxcrowd,TEAMNAME) %>% 
  filter(CROWD==maxcrowd) %>% 
  plot_ly(x=~gameDate,y=~maxcrowd ) %>% 
  add_markers(hoverinfo="text",
              text=~paste0(TEAMNAME)) %>% 
  add_lines(line = list(shape = "hv")) %>% 
  layout(showlegend=FALSE,
         title="Record Attendances at EPL matches<br>Hover for details",
         yaxis=list(title="Crowd Size"),
         xaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
         
 # max crowd 
teamGames %>% 
  filter(venue=="H") %>% 
  left_join(teamGames,by="MATCHID") %>% 
  filter(TEAMID.x!=TEAMID.y) %>% 
  rename(team=TEAMNAME.x,Opponents=TEAMNAME.y,crowd=CROWD.x,gameDate=gameDate.x) %>% 
  arrange(gameDate,desc(crowd)) %>% 
  mutate(maxcrowd=cummax(crowd)) %>% 
  select(gameDate,crowd,maxcrowd,team,Opponents) %>% 
  filter(crowd==maxcrowd) %>% 
  plot_ly(x=~gameDate,y=~maxcrowd,
          hoverinfo="text",
          text=~paste0(crowd,"<br>",team," v ",Opponents,
                       "<br>", gameDate)) %>% 
  add_markers() %>% 
  add_lines(line = list(shape = "hv")) %>% 
  layout(showlegend=FALSE,
         title="Record High Attendances at EPL matches<br>Hover for details",
         yaxis=list(title="Crowd Size"),
         xaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)


# min crowd 
teamGames %>% 
  filter(venue=="H") %>% 
  left_join(teamGames,by="MATCHID") %>% 
  filter(TEAMID.x!=TEAMID.y) %>% 
  rename(team=TEAMNAME.x,Opponents=TEAMNAME.y,crowd=CROWD.x,gameDate=gameDate.x) %>% 
  arrange(gameDate,crowd) %>% 
  mutate(mincrowd=cummin(crowd)) %>% 
  select(gameDate,crowd,mincrowd,team,Opponents) %>% 
  filter(crowd==mincrowd) %>% 
  plot_ly(x=~gameDate,y=~mincrowd,
          hoverinfo="text",
          text=~paste0(crowd,"<br>",team," v ",Opponents,
                       "<br>", gameDate)) %>% 
  add_markers() %>% 
  add_lines(line = list(shape = "hv")) %>% 
  layout(showlegend=FALSE,
         title="Record Low Attendances at EPL matches<br>Hover for details",
         yaxis=list(title="Crowd Size"),
         xaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
# not really worth putting up

## total wimbledon 1992/3
teamGames %>% 
  filter(venue=="H"&TEAMNAME=="Wimbledon"&season=="1992/93") %>% 
  summarize(tot=sum(CROWD))

test <-teamGames %>% 
  filter(venue=="H"&TEAMNAME=="Wimbledon"&season=="1992/93") 

  plot_ly(x=~gameDate, y=~CROWD,
          hoverinfo="text",
          text=~paste0(CROWD,"<br>",team," v ",Opponents,
                       "<br>", gameDate))
## interesting range
  

# start by man city points ------------------------------------------------


# man u late goals --------------------------------------------------------

  library(forcats)
  sort(names(goals))
  sort(names(standings))
  sort(names(playerGame))
  
  each <- playerGame %>% 
    select(PLAYER_MATCH,season,TEAMNAME) %>% 
    right_join(goals) %>% 
    select(season,TEAMNAME,TIME) %>% 
    filter(season=="2017/18")
  
  all <- each %>% 
    group_by(TEAMNAME) %>% 
    tally()
  
  
  each %>% 
    left_join(all) %>% 
    plot_ly(x=~jitter(TIME),y=~fct_reorder(TEAMNAME, n)) %>% 
    add_markers(hoverinfo="text",
                text=~paste0(TIME)) 
  ## maybe add as sub

# man city finally scorin in first 15 mins --------------------------------

  each %>% 
    filter(TIME<=15) %>% 
    pull(TEAMNAME) %>% 
    unique()

  

#  martial goals as sub ---------------------------------------------------

sort(names(summary))
  
subGls <- summary %>% 
    group_by(PLAYERID,name,TEAMNAME,season) %>% 
    summarise(gls=sum(subGls)) %>% 
    filter(gls>0)
  
  subGls %>% 
    arrange(desc(gls)) %>% 
    filter(season=="2017/18") # Martial leads with 4
  
teamSubgls <-  subGls %>% 
    group_by(TEAMNAME,season) %>% 
    summarise(totGls=sum(gls)) 


tem

    

# top 6 -------------------------------------------------------------------

sort(names(standings))


standings <- standings %>%
  arrange(season,tmYrGameOrder)

gameYears <- standings %>% 
  select(tmYrGameOrder,season) %>% 
  unique()



final <-c(1:6)

now <- standings %>% 
  filter(season=="2016/17"&tmYrGameOrder==10) %>% 
  select(team,position,final_Pos) %>% 
  arrange(position) %>% 
  head(6) %>% 
  pull(final_Pos)

df_in <- standings %>%
  arrange(season,tmYrGameOrder,position)

length(intersect(final,now)) #5


## OK now look at returning teams as well

myFun <- function(x,y) {
  # print(x)
  # print(y)
  now <- df_in %>% 
    filter(season==x&tmYrGameOrder==y) %>% 
    # select(team,position,final_Pos) %>% 
    # arrange(position) %>% 
    head(6) %>% 
    pull(final_Pos) 
  
  x <-length(intersect(final,now))
  #print(x)
  return(x)
  
}
  #pretty (slow)
  season=gameYears$season
  gameOrder=gameYears$tmYrGameOrder
  
  same <- map2_int(season,gameOrder,myFun)
  
  
same <- map2_int(gameYears$season,gameYears$tmYrGameOrder,myFun) #int 971 

l=list(season=season,gameorder=gameOrder,same=same)
df <- as.tibble(l)

standings %>% 
  filter(season=="2009/10"&tmYrGameOrder==1) %>% 
  arrange(position)

test <-df %>% 
  #ungroup() %>% 
  filter(season=="2009/10")

test %>% 
  plot_ly(x=~gameorder,y=~same) #length(test$same) #42 Error: Column `y` must be length 1 or 971, not 42!! length(test$gameOrder)

#speed up function


df <- as.data.frame(season=gameYears$season,gameOrder=gameYears$tmYrGameOrder,same=same)

myFun <- function(x,y) {
 
  # now <- df_in %>% 
  #   filter(season==x&tmYrGameOrder==y) %>% 
  #   head(6) %>% 
  #   pull(final_Pos) 
  
  teams <- df_in %>% 
    filter(season==x&tmYrGameOrder==y) %>% 
    head(6)
  
  now <- teams %>% 
    pull(final_Pos)
  
  club <- teams %>% 
    mutate(clubs=paste(team,collapse=", ")) %>% 
    pull(clubs)
  
  x <-length(intersect(final,now))
  
 
  
  
  temp_df<- data.frame(count=x,clubs=club)
  
  return(temp_df)
  
}
#pretty (slow)
season=gameYears$season[1:2]
gameOrder=gameYears$tmYrGameOrder[1:2]

same <- map2_df(season,gameOrder,myFun)


same <- map2_int(gameYears$season,gameYears$tmYrGameOrder,myFun) #int 971 

l=list(season=season,gameorder=gameOrder,same=same)
df <- as.tibble(l)

## testing listing

teams <- df_in %>% 
  filter(season=="2017/18"&tmYrGameOrder==9) %>% 
  head(6)

now <- teams %>% 
  pull(final_Pos)

club <- teams %>% 
  pull(team) %>% 
  mutate(clubs=paste(as.character(team), collapse=", "))
  as.list() %>%  nest()
  
  club <- teams %>% 
    mutate(clubs=paste(team,collapse=", ")) %>% 
    pull(clubs)

x <-length(intersect(final,now))

collapse string
paste(as.character(one), collapse=", ")

teams %>% 
  mutate(clubs=)


temp_df<- data.frame(count=x,clubs=club)


# ridgeplot ages ----------------------------------------------------------



# runs  of nos hsutouts - may have doen before ----------------------------


sort(names(standings))

temp <-standings %>% 
 # filter(team=="Crystal P") %>% 
  arrange(desc(gameDate))

temp <-standings %>% 
  # filter(team=="Crystal P") %>% 
  arrange(desc(gameDate)) %>% 
  mutate(bad=ifelse(GA>0,1,0))

myFun <- function(x) {
  temp <-standings %>% 
     filter(team==x) %>% 
    arrange(desc(gameDate)) %>% 
    mutate(bad=ifelse(GA>0,1,0))
  
y <-  roll_sum(temp$bad,n=18)
print(y)
return(y)
    
}

#run <- roll_sum(test$bad,n=18)

teams <- unique(standings$team)

res <- map(teams[1],myFun)
class(res) # list but looks like a vector
res <- map(teams[1],myFun) %>%  unlist()
class(res) # numeric
res <- map_int(teams[1],myFun) #Error: Result 1 is not a length 1 atomic vector but is still a vector


myFun <- function(x) {
  temp <-standings %>% 
    filter(team==x) %>% 
    arrange(desc(gameDate)) %>% 
    mutate(bad=ifelse(GA>0,1,0))
  
  #y <-  roll_sum(temp$bad,n=18)
  df <- data.frame(run=roll_sum(temp$bad,n=18),team=x)
  return(df)
  
}



teams <- unique(standings$team)  # gives an error as couple have teams have not had that long a run currently i.e.bri hud

teams <-standings %>% 
  group_by(team) %>% 
  tally() %>% 
  filter(n>37) %>%   # so have to have played full year
  pull(team)
  
res <- map_df(teams,myFun)

## so several teams have had 18 in row scored on




# coming from 2 goals down - also see ?358 articl -------------------------

# 5 - Cesar Azpilicueta has provided 5 assists for Alvaro Morata in the Premier League this term; the most by one player for a teammate. Duo.


# 1 - Chelsea have lost just one of their last 15 home PL games against Man United (in Oct 2012), winning nine and drawing five. Confident.

sort(names(standings))


# do with htmlwidget 
library(sparkline)
temp <- standings %>% 
  arrange(desc(gameDate)) %>% 
  filter(team=="Chelsea"&OppTeam=="Man. Utd."&venue=="H") %>% 
  mutate(GD=GF-GA) 

temp$GD

temp %>%
  .$GD %>%
  sparkline(type="bar")


df <- standings %>% 
  arrange(gameDate) %>% 
  filter(team=="Chelsea"&OppTeam=="Man. Utd."&venue=="H") %>% 
  mutate(GD=GF-GA) 

df %>%
  .$GD %>%
  sparkline(type="bar", width=200, height=120)

## look at any team they have played 16 at least

oppteams <- standings %>% 
  arrange(gameDate) %>% 
  filter(team=="Chelsea"&venue=="H") %>% 
  #group_by(OppTeam) %>% 
  count(OppTeam) %>% 
  filter(n>=16) %>% 
  pull(OppTeam)
  
  df <- standings %>% 
  arrange(desc(gameDate)) %>% 
  filter(team=="Chelsea"&OppTeam %in% oppteams&venue=="H") %>% 
  mutate(GD=GF-GA) %>% 
  group_by(OppTeam) %>% 
    select(OppTeam,points) %>% 
    slice(1:15) %>% 
    summarize(totPoints=sum(points))

  
  ## works but not that amazing
  
sort(names(assists))
sort(names(goals)) 
sort(names())


assister <- assists %>% 
  left_join(playerGame) %>% 
  select(name,PLAYERID,PLAYER_MATCH_GOAL,gameDate,Opponents,season)

assister %>% 
  filter(PLAYERID=="SILVAD2") #139 #32961
###
player <- "DEBRUYK"

assister %>% 
  filter(PLAYERID==player) #32

scorer <- goals %>% 
  left_join(playerGame) %>% 
  select(scorer=name,scorerID=PLAYERID,PLAYER_MATCH_GOAL) #26017

## sum for player 
scorerOrder <- assister %>% 
  filter(PLAYERID==player) %>% 
  left_join(scorer) %>% 
  group_by(scorer) %>% 
  tally() %>% 
  arrange(desc(n))




# assister %>% 
#   filter(PLAYERID==player) %>% 
#   arrange(gameDate) %>% 
#   mutate(order=row_number()) %>% 
#   left_join(scorer) %>% 
#   plot_ly(x=~order,y=~scorer) %>% 
#   add_markers() %>% 
#   layout(margin=list(l=120))

library(forcats)

# assister %>% 
#   filter(PLAYERID=="AZPILIC") %>% 
#   arrange(gameDate) %>% 
#   mutate(order=row_number()) %>% 
#   left_join(scorer) %>% # join individ
#   left_join(scorerOrder) %>% #join sum
#   plot_ly(x=~order,y=~fct_reorder(scorer, n),
#           hoverInfo="text",
#           text=~paste0(gameDate,
#                        "<br>v ",Opponents)) %>% 
#   add_markers() %>% 
#   layout(margin=list(l=120))

playerName<- assister %>% 
  filter(PLAYERID==player) %>% 
  head(1) %>% 
  pull(name)

library(glue)

assister %>% 
  filter(PLAYERID==player) %>% 
  arrange(gameDate) %>% 
  mutate(order=row_number()) %>% 
  left_join(scorer) %>% # join individ
  left_join(scorerOrder) %>% #join sum
  plot_ly(x=~order,y=~fct_reorder(scorer, n),
          hoverInfo="text",
          text=~paste0(gameDate,
                       "<br>v ",Opponents)) %>% 
  add_markers(color=~season, size=I(8)) %>% 
  layout(margin=list(l=120),
         title= glue("{playerName}'s Assists by Scorer"),
         xaxis=list(title="Goal Order"),
         yaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)


#3 assists since 2015/16

assists %>% 
  left_join(playerGame) %>% 
  select(name,PLAYERID,PLAYER_MATCH_GOAL,gameDate,Opponents,season) %>% 
  filter(season>"2014/15") %>% 
  count(PLAYERID, sort=TRUE) 


# Time ahead in game ------------------------------------------------------

# initially look at one game

sort(names(standings))


sort(names(teamGames))
sort(names(hth))

game <-teamGames %>% 
  filterTEAMMATCHID==21050) %>% 
  select(TEAMMATCHID,venue,GOALS,TEAMNAME,TEAMID,gameDate,season,MATCHID)

goalsFor <- game %>% 
  left_join(goals,by="TEAMMATCHID") %>% 
  pull(TIME)


mins <- 1:90
gamedf <- data.frame(mins=mins,goals=0) %>% 
  mutate(GF=ifelse(mins %in% goalsFor,1,0),TEAMID=TEAMID,TEAMMATCHID=TEAMMATCHID)

# have to think about scoring in minute 1


myFun <- function(x) {
  goalsFor <-teamGames %>% 
    filter(TEAMMATCHID==x) %>% 
    left_join(goals,by="TEAMMATCHID") %>% 
    pull(TIME)
  
  gamedf <- data.frame(mins=mins,goals=0) %>% 
    mutate(GF=ifelse(mins %in% goalsFor,1,0),TEAMMATCHID=x)
}


all <- map_df(teamGames$TEAMMATCHID[1:22],myFun) 

all <- as.tbl(all) %>% 
  left_join(teamGames) %>% 
  select(c(1,3,4,5)) 


joined <- all %>% 
  inner_join(all,by=c("mins"="mins","MATCHID"="MATCHID")) %>% 
  filter(TEAMMATCHID.x!=TEAMMATCHID.y) %>% 
  rename(GF=GF.x,TEAMMATCHID=TEAMMATCHID.x,GA=GF.y,OPPMATCHID=TEAMMATCHID.y) %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(diff=cumsum(GF)-cumsum(GA))

test <-joined %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(
    up = ifelse(diff>0,1,0),
    down=ifelse(diff<0,1,0),
    even=ifelse(diff==0,1,0)) %>% 
summarize(up=sum(up),down=sum(down),even=sum(even))

sort(names(standings))

teamGames %>% 
  filter(TEAMMATCHID==20008)

standings %>% 
  filter(MATCHID==1321)

## lets look at this year


thisYearGames <- teamGames %>% 
  filter(season=="2017/18") %>% 
  pull(TEAMMATCHID)

myFun <- function(x) {
  goalsFor <-teamGames %>% 
    filter(TEAMMATCHID==x) %>% 
    left_join(goals,by="TEAMMATCHID") %>% 
    pull(TIME)
  
  gamedf <- data.frame(mins=mins,goals=0) %>% 
    mutate(GF=ifelse(mins %in% goalsFor,1,0),TEAMMATCHID=x)
}


#all <- map_df(thisYearGames,myFun) 

allGames <- teamGames$TEAMMATCHID

all <- map_df(allGames,myFun)

all <- as.tbl(all) %>% 
  left_join(teamGames) %>% 
  select(c(1,3,4,5)) 

saveRDS(all,"scores.rds") # could get away with not saving matchid if problem only 193 takes 10mins to run


joined <- all %>% 
  inner_join(all,by=c("mins"="mins","MATCHID"="MATCHID")) %>% 
  filter(TEAMMATCHID.x!=TEAMMATCHID.y) %>% 
  rename(GF=GF.x,TEAMMATCHID=TEAMMATCHID.x,GA=GF.y,OPPMATCHID=TEAMMATCHID.y) %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(diff=cumsum(GF)-cumsum(GA))

saveRDS(joined,"scoreLines.rds")
joined <-readRDS("scoreLines.rds")

gameState <-joined %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(
    up = ifelse(diff>0,1,0),
    down=ifelse(diff<0,1,0),
    even=ifelse(diff==0,1,0)) %>% 
  summarize(up=sum(up),down=sum(down),even=sum(even))

sort(names(teamGames))
sort(names(gameState))


# teamGames %>% 
#   inner_join(gameState) %>% 
#   #filter(TEAMNAME=="Crystal P") %>%
#   group_by(TEAMNAME) %>% 
#   select(GOALS,up,down,even) %>% 
#   summarize(totup=sum(up),totdown=sum(down),toteven=sum(even))%>%
#                          DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  
teamGames %>% 
  inner_join(gameState) %>% 
  #filter(TEAMNAME=="Crystal P") %>%
  group_by(TEAMNAME,season) %>% 
  select(GOALS,up,down,even) %>% 
  mutate(upin=ifelse(up>0,1,0),downin=ifelse(down>0,1,0)) %>% 
  summarize(totup=sum(up),totdown=sum(down),toteven=sum(even),upin=sum(upin),downin=sum(downin))%>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))


## Man C slowish start were not ahead in first  -- mins
names(all)
names(gameState)

joined <- readRDS("scoreLines.rds")
notdown <-joined %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(
    
    good=ifelse(diff<0,0,1))  #1.8mill rows

test <- teamGames %>% 
  inner_join(notdown) %>% 
  arrange(gameDate) %>% 
  group_by(TEAMNAME) %>% 
  #select(good) %>% 
  do(subSeq(.$good))

mnc <-test %>% 
  filter(TEAMNAME=="Man. City") #700+90 to come their 3rd best

x <-test %>% 
  arrange(desc(last)) %>% 
  group_by(TEAMNAME) %>% 
  slice(1) %>% 
  filter(value==1)

bestever

y <- test %>% 
  arrange(desc(slength)) %>% 
  group_by(TEAMNAME) %>% 
  filter(value==1) %>% 
  slice(1)
   
  ## look at down by 2 and win

twodown <-joined %>% 
  group_by(TEAMMATCHID) %>% 
  mutate(
    twoDown=ifelse(diff<=-2,1,0)) %>% 
  filter(twoDown==1) %>% 
  select(TEAMMATCHID,MATCHID) %>% 
  unique() #4533 games

sort(names(standings))
sort(names(teamGames))

temp <-twodown %>% 
  left_join(teamGames) %>% 
  left_join(standings) %>% 
  select(team,GF,GA,gameDate,OppTeam,res,venue)


eve <-temp %>% 
  filter(team=="Everton")


table(temp$res)
# Draw Loss  Win 
# 231 4224   78 1.7% chance

#93% chance losing

c <- temp %>% 
  filter(res=="Win") %>% 
  group_by(team) %>% 
  tally()
  
## look at by season/club

##do a kind of trelliscope or lattice of mins ahead/level and behind each game


df <- read_rds("scoreLines.rds")
names(df) "mins"        "GF"          "TEAMMATCHID" "MATCHID"     "GA"         
[6] "OPPMATCHID"  "diff"       
sort(names(teamGames))

data <-df %>% 
  left_join(teamGames) %>% 
  #filter(MATCHID==1313) %>% 
  left_join(standings,by=c("MATCHID"="MATCHID","TEAMNAME"="team")) %>% #36mill
  select(team=TEAMNAME,season=season.x,gameDate=gameDate.x,mins,diff,tmYrGameOrder=tmYrGameOrder.x, GF=GF.y,GA=GA.y,OppTeam,final_Pos)



# names(tmYrs)
# 
# y <-standings %>% 
#   filter(MATCHID==1313)
# 
# z <-data %>% 
#   left_join(y, by=c("TEAMNAME"="team"))
# 
# names(standings)
# names(data)

test <-data %>% 
  filter(season=="2017/18"&team=="Crystal P") %>% 
mutate(
  up = if_else(diff>0,1,0),
  down=if_else(diff<0,1,0),
  even=if_else(diff==0,1,0)) %>% 
  group_by(TEAMMATCHID,tmYrGameOrder) %>% 
  summarize(up=sum(up),down=sum(down),even=sum(even)) %>% 
  gather(category,time,-c(TEAMMATCHID,tmYrGameOrder))
  

## team by game - but overall not by minute
test %>% 
  plot_ly(x=~tmYrGameOrder,y=~time) %>% 
  add_bars(color=~category) %>% 
  layout(barmode="stack",
         yaxis=list(title="Mins"),
         xaxis=list(title=""))


test2 <-data %>% 
  filter(season=="2007/08"&team=="Derby Co.") %>% 
  mutate(
    up = if_else(diff>0,1,0),
    down=if_else(diff<0,1,0),
    even=if_else(diff==0,1,0)) %>% 
  group_by(TEAMMATCHID,tmYrGameOrder) %>% 
  summarize(up=sum(up),down=sum(down),even=sum(even)) %>% 
  gather(category,time,-c(TEAMMATCHID,tmYrGameOrder))



test2 <-data %>% 
  filter(season=="2003/04"&team=="Arsenal") %>% 
  mutate(
    cat= case_when(
    diff>0 ~"up",
    diff==0 ~"even",
    diff<0 ~"down"
  ))


   ## looks good would be nice to add final score and opponents (perhaps add mod diff for opacity) 
    test2 %>%  
  plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
  add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
              hoverinfo="text",
              text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
    layout(barmode="stack",
         yaxis=list(title="Mins"),
         xaxis=list(title=""))

    
    ## subplots
    
  a<-  data %>% 
      filter(season=="2003/04"&team=="Arsenal") %>% 
      mutate(
        cat= case_when(
          diff>0 ~"up",
          diff==0 ~"even",
          diff<0 ~"down"
        ))
    
    
    ## looks good would be nice to add final score and opponents (perhaps add mod diff for opacity) 
  a_plot <- a %>%  
      plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
      add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
                  hoverinfo="text",
                  text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
      layout(barmode="stack",
             yaxis=list(title="Mins"),
             xaxis=list(title=""))
    
  b<-  data %>% 
    filter(season=="2004/05"&team=="Arsenal") %>% 
    mutate(
      cat= case_when(
        diff>0 ~"up",
        diff==0 ~"even",
        diff<0 ~"down"
      ))
  
  
  ## looks good would be nice to add final score and opponents (perhaps add mod diff for opacity) 
  b_plot <- b %>%  
    plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
    add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
                hoverinfo="text",
                text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
    layout(barmode="stack",
           yaxis=list(title="Mins"),
           xaxis=list(title=""))
  
  
  c<-  data %>% 
    filter(season=="2017/18"&team=="Arsenal") %>% 
    mutate(
      cat= case_when(
        diff>0 ~"up",
        diff==0 ~"even",
        diff<0 ~"down"
      ))
  
  
  ## looks good would be nice to add final score and opponents (perhaps add mod diff for opacity) 
  c_plot <- c %>%  
    plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
    add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
                hoverinfo="text",
                text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
    layout(barmode="stack",
           yaxis=list(title="Mins"),
           xaxis=list(title=""))
   
  subplot(a_plot,b_plot,c_plot,shareY= TRUE,shareX= TRUE,nrows=2) 
  
  
  a tibble with one list-column of plotly/ggplot2 objects.
  
  ## create a tibble with list of 
  
  l=list(plot=c(a_plot,b_plot,c_plot))
  t <- as_tibble(l) # this does not give correct result
  class(a_plot) #[1] "plotly"     "htmlwidget"
  
  t <- as_tibble(plots=list(a_plot,b_plot,c_plot))
  
  ## nesting example
  library(dplyr)
  as_tibble(iris) %>% nest(-Species) #goes from 150 diff values to 3dfs grouped by species
  as_tibble(chickwts) %>% nest(weight) # chickwts only has 2 values so here feed is group
  
  ## so by team (could also do by year)
  
 team_df<- as_tibble(tmYrs) %>%  nest(season)
#   team              data
#   <chr>            <list>
#     Chelsea <tibble [26 x 1]>
# 2    Man. Utd. <tibble [26 x 1]>
  
 
 ## get all the updown 
 
 allData <- data %>% 
   #filter(season=="2017/18"&team=="Arsenal") %>% 
   #group_by(season,team) %>% 
   mutate(
     cat= case_when(
       diff>0 ~"up",
       diff==0 ~"even",
       diff<0 ~"down"
     )) # tqkes say 20 secs could be interesting to cf v ifelse and data.table
 
 names(allData)
 library(stringr)
 theSeason <- paste0("13/14 (",
 allData %>%  
   filter(team=="Arsenal"&season=="2013/14") %>% 
   plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
   add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
               hoverinfo="text",
               text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
   layout(barmode="stack", showlegend=FALSE,
          title=~paste0(str_sub(season,3,7)," (",final_Pos,")"),
          yaxis=list(title="Mins"),
          xaxis=list(title=""))

 myPlotFun <- function(x,y) {
   
   allData %>%  
     filter(team==x&season==y) %>% 
     plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
     add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
                 hoverinfo="text",
                 text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
     layout(barmode="stack",
            yaxis=list(title="Mins"),
            xaxis=list(title=""))
   
 }
 
 myPlotFun <- function(y) {
   
   allData %>%  
     filter(team=="Arsenal"&season==y) %>% 
     plot_ly(x=~tmYrGameOrder,y=~mins) %>% 
     add_markers(color=~cat, symbol=I("square"), colors=c("black","orange","green"),opacity=0.5,
                 hoverinfo="text",
                 text=~paste0(OppTeam," ",GF,"-",GA))   %>% 
     layout(barmode="stack", showlegend=FALSE,
            title=~paste0(str_sub(season,3,7)," (",final_Pos,")"),
            yaxis=list(title="Mins"),
            xaxis=list(title=""))
   
 }
 
 
 tmYrs <- tmYrs %>% 
   arrange(team,season)
   
 
 arsenal <-tmYrs %>% filter(team=="Arsenal")
 
 plot_nested <- 
   arsenal %>%
   mutate(model = map(.$season, myPlotFun))
 
 # takes a little while
 glimpse(plot_nested)
 
 rows <- ceiling(nrow(arsenal)/5)  #6
 
 
p <- subplot(
plot_nested$model[1],
plot_nested$model[2])
p
 
 
 map2("Arsenal","2015/16",)
 
 
 economics_long %>%
   group_by(variable) %>%
   do(p = plot_ly(., x = ~date, y = ~value)) %>%
   subplot(nrows = NROW(.), shareX = TRUE)
 
 
 arsenal %>% 
   do(p=plot_ly(.,x = ~tmYrGameOrder,y = ~mins)) %>%
   subplot(nrows = 6, shareX = TRUE, shareY = TRUE)
 
 
plot_nested %>% 
   .$model %>%
   subplot(nrows = 6, shareX = TRUE, shareY = TRUE)
   
  if (require("gapminder")) {
    gapminder %>%
      group_by(country, continent) %>%
      nest()
    
    gapminder %>%
      nest(-country, -continent)
  }
  
  # also see http://ijlyttle.github.io/isugg_purrr/presentation.html#(19)

# man U gap from top fter 11 games ----------------------------------------

all <-
standings %>% 
  filter(tmYrGameOrder==11&position==1) %>% 
  select(season,leader=team,topPts=cumPts)

mnu <- standings %>% 
  filter(tmYrGameOrder==11&team=="Man. Utd.") %>% 
  select(season,team,cumPts,final_Pos) %>% 
  inner_join(all) %>% 
  mutate(diff=topPts-cumPts,title=ifelse(final_Pos==1,"Champs","Chumps")) %>% 
  mutate(diff=ifelse(diff==0,0.2,diff))


mnu %>% 
  plot_ly(x=~season,y=~diff) %>% 
  add_bars(color=~title,colors=c("red","blue"),opacity=0.5,
           hoverinfo="text",
           text=~paste0(leader," ",topPts,"pts")) %>% 
  layout(margin=list(b=100),yaxis=list(title="Point Deficit"),
         xaxis=list(title="")) %>%  config(displayModeBar = F,showLink = F)
         

# Rccproll of man us goalscoring ------------------------------------------

gls <- standings %>% 
  filter(team=="Man. Utd.") %>% 
  select(GF,gameDate) %>% 
  arrange(gameDate) %>% 
  pull(GF)

library(RcppRoll)
temp <-   data.frame(gls=roll_sum(gls,n=4))



# Moyes at 500 games ------------------------------------------------------
#(Alex Ferguson, Arsene Wenger, Harry Redknapp). Devotion.
# chart of cumulative points+ lines by team(or male points)


top500 <- c("FergusonA","WengerA","RedknappH","MoyesD")

managerGame %>% 
  group_by(ManagerID) %>% 
  tally() %>% 
  arrange(desc(n))

sort(names(managerGame))

managerGame %>% 
  filter(ManagerID %in% top500) %>% 
  group_by(ManagerID) %>% 
  mutate(cumPoints=cumsum(points),order=row_number()) %>% 
  plot_ly(x= ~order, y= ~cumPoints, color=~name) %>% 
  add_lines(hoverinfo="all") %>% 
  layout(title="Cumulative EPL points by longest serving managers",
         xaxis=list(title="Games managed"),
         yaxis=list(title="Points")) %>%  config(displayModeBar = F,showLink = F)
         
managerGame %>% 
  filter(ManagerID %in% top500) %>% 
  group_by(ManagerID) %>% 
  mutate(cumPoints=cumsum(points),order=row_number()) %>% 
  plot_ly(x= ~order, y= ~cumPoints, color=~name) %>% 
  add_markers(hoverinfo="x") %>% 
  layout(title="Cumulative EPL points by longest serving managers",
         xaxis=list(title="Games managed"),
         yaxis=list(title="Points")) %>%  config(displayModeBar = F,showLink = F)  



# win loss bt team NEW v MNU ----------------------------------------------


# Palace clean sheets - think may have looked alittle while ago -----------


# de bruyne ---------------------------------------------------------------

goal or assist run
275 hard t o believe

playerGame %>% 
  filter(season=="2017/18") %>% 
  group_by(PLAYERID) %>% 
  summarize(tot=sum(Assists)) %>% 
  arrange(desc(tot))

#az


def <-playerGame %>% 
  filter(POSITION=="Defender") %>% 
  group_by(PLAYERID,TEAMNAME) %>% 
  summarize(tot=sum(Assists)) %>% 
  arrange(desc(tot))




# young cross -------------------------------------------------------------

left footed
all 43 goals have been with right foot 


# something on pulis ------------------------------------------------------

No wins in 10

sort(names(managerTeam))
sort(names(standings))

head(managerTeam)

temp <-managerTeam %>% 
  left_join(teamCodes) %>% 
  right_join(standings,by=c("TEAMNAME"="team","theDate"="gameDate")) %>% 
  select(theDate,ManagerID,res) %>% 
  mutate(cat=ifelse(res=="Win",0,1))

temp %>% 
  arrange(theDate) %>% 
  group_by(ManagerID)

## prob not best should vectorize with data.table?
library(RcppRoll)
get_runs <- function(x) {
  df <- temp %>% 
    arrange(theDate) %>% 
    filter(ManagerID==x) 
  
  
  # construct data.frame of results
  run <- roll_sum(df$cat,n=10)
  data.frame(points=run,seq=1:length(run),manager=x)
  
  
}

# apply the above function to all teams
data <-map_df(managers$ManagerID, get_runs)
  

df <-temp %>% 
  arrange(theDate) %>% 
  filter(ManagerID=="PulisT") 

run <- roll_sum(df$cat,n=10)
run

get_runs <- function(x) {
  tm <-standings %>% 
    filter(team==x) %>% 
    arrange(tmGameOrder)
  
  # construct data.frame of results
  run <- roll_sum(tm$points,n=10)
  data.frame(points=run,seq=1:length(run),team=x)
  
  
}

# apply the above function to all teams
data <-map_df(teams, get_runs)

glimpse(data)



test <- managerTeam %>% 
  filter(ManagerID=="PulisT") %>%  #3850
  left_join(teamCodes) %>% 
  inner_join(standings,by=c("theDate"="gameDate","TEAMNAME"="team")) %>% # only key from left is retained
  select(theDate,ManagerID,res,TEAMNAME) %>% 
  #unique() %>% 
  filter(!is.na(res))
  mutate(cat=ifelse(res=="Win",0,1))
  
  
x <-  standings %>% 
    filter(season=="2017/18"&team=="West Brom") %>% 
    arrange(gameDate) %>% 
    select(gameDate,team,res)

y <- managerTeam %>% 
  filter(ManagerID=="PulisT") %>%  #3850
  left_join(teamCodes) %>% 
  filter(theDate>=as.Date("2017-08-01")) %>% 
  select(gameDate=theDate,team=TEAMNAME,ManagerID)

z <-x %>% 
semi_join(y)

# stil odd here
x <-  standings %>% 
  filter(season=="2016/17"&team=="West Brom") %>% 
  arrange(gameDate) %>% 
  select(gameDate,team,res)

y <- managerTeam %>% 
  filter(ManagerID=="PulisT") %>%  #3850
  left_join(teamCodes) %>% 
  filter(theDate>=as.Date("2016-08-01")) %>% 
  select(gameDate=theDate,team=TEAMNAME,ManagerID)

z <-x %>% 
  semi_join(y)


str(x)
str(y)

# morata ------------------------------------------------------------------

start compared with zolaécostaédrogba
slight amendment 



# man u4 goals ------------------------------------------------------------


tempAT <-standings %>% 
  filter(GF>3) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n==max(n))

tempAT %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes)) %>% 
  plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=I("lightgrey"), line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))

manU <- standings %>% 
  arrange(gameDate) %>% 
  filter(season=="2017/18"&team=="Man. Utd.") %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes))


tempAT %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes)) %>% 
  plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=I("lightgrey"), line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games")) %>% 
  add_lines(manU,x=~tmYrGameOrder,y=~count,color=I("red"), line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))  %>%
    layout(
      xaxis = list(range = c(1, 38)))
## problem as only part wau through season? how managed before
# https://plotly-book.cpsievert.me/a-case-study-of-housing-sales-in-texas.html could help
manU %>% 
  plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=I("red"), line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))
tempAT %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes)) %>% 
  plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=~team,colors=c("red","blue"), opacity=0.25, line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))

try adding manU to tempAT

best <- standings %>% 
  filter(GF>3) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n==max(n)) %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes))

manU <- standings %>% 
  filter(season=="2017/18"&team=="Man. Utd.") %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  #group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes))

all <- bind_rows(best,manU)  

all %>% 
plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=~team,colors=c("red","lightblue","black"),  line=list(shape= "hv"),
            hoverinfo="text",
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games")) %>% 
  add_lines(data=temp12,x=~tmYrGameOrder,y=~count,color=I("lightgrey"), line=list(shape= "hv"),
            hoverinfo="text", , name=~season,
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))

  plot_ly(data=temp12,x=~tmYrGameOrder,y=~count) %>% 
    add_lines(color=I("lightgrey"), line=list(shape= "hv"),
              hoverinfo="text", name="Other Round 12 leaders",
              text= ~paste0(team," ", season,"<br>",
                            count," after ",tmYrGameOrder," games")) %>% 
  add_lines(data=all,x=~tmYrGameOrder,y=~count,color=~team,colors=c("red","lightblue","black"),  line=list(shape= "hv"),
            hoverinfo="text", 
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games")) %>% 
    layout(xaxis=list(title="Games Played"),
           yaxis=list(title="Four goal Wins")) %>%  config(displayModeBar = F,showLink = F)
           
 



temp12 %>% 


LIV and man city lead with 11 in season

temp <-standings %>% 
  filter(GF>3) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  filter(team=="Man. Utd.")
man u already 5x a level not achieved in full season since 2012/13
an leads league mancity have 4


tempMNU <-standings %>% 
  filter(GF>3) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  filter(season=="2017/18"&team=="Man. Utd.")

temp12 <-standings %>% 
  filter(GF>3&tmYrGameOrder<=12) %>% 
  group_by(team,season) %>% 
  tally() %>% 
  ungroup() %>% # otherwise does not match overall n
  filter(n==max(n)) %>% 
  left_join(standings) %>% 
  arrange(gameDate) %>% 
  select(team,season,tmYrGameOrder,GF) %>% 
  group_by(team,season) %>%
  mutate(yes=ifelse(GF>3,1,0),count=cumsum(yes))
## also inc ars 2009/10,liv 16/17,mnc 11/12

temp12 %>% 
plot_ly(x=~tmYrGameOrder,y=~count) %>% 
  add_lines(color=I("lightgrey"), line=list(shape= "hv"),
            hoverinfo="text", , name=~season,
            text= ~paste0(team," ", season,"<br>",
                          count," after ",tmYrGameOrder," games"))


5x achieved after 12 games by few others in Liv last season only mangaed 2x in last subsequent 26
# most as sub

sort(names(playerGame))


playerGame %>% 
  filter(mins>0&START==0) %>% 
  group_by(PLAYERID,name) %>% 
 summarize(n=n(),mins=sum(mins)) %>% 
  arrange(desc(mins))

sort(names(summary))

## abandon this isnt right
summary %>% 
  filter(St==0&On>0) %>% 
  group_by(PLAYERID,name) %>% 
  summarize(tot=sum(On)) %>% 
ungroup() %>% 
  arrange(desc(tot)) %>% 
  select(name)



# adding team bookable ----------------------------------------------------

mutate(taskLink = paste0("<a href=\"", taskUrl, "\" target=\"_blank\">", task, "</a>")) %>%
  select(task,view=taskLink,topic,maintainer,email,updated) %>% 
  DT::datatable(width="100%",class='compact stripe hover row-border order-column',rownames=FALSE,escape = FALSE,
                options= list(paging = TRUE, searching = TRUE,info=FALSE,
                              columnDefs = list(list(visible=FALSE, targets=0))))

teamCodes %>% 
  right_join(teamSeason,by=c("TEAMNAME"="team")) %>% 
  select(team=TEAMNAME,bookmark) %>% 
  unique() %>% 
  filter(!is.na(bookmark)) %>% 
  mutate(Team=paste0("<a href=\"", bookmark, "\" target=\"_blank\">", team, "</a>")) %>% 
  select(Team) %>% 
  DT::datatable(width="100%",class='compact stripe hover row-border order-column',rownames=FALSE,escape = FALSE,
                options= list(paging = TRUE, searching = TRUE,info=FALSE,
                              columnDefs = list(list(visible=FALSE, targets=0))))

teamCodes %>% 
  right_join(teamSeason,by=c("TEAMNAME"="team")) %>% 
  select(team=TEAMNAME,bookmark) %>% 
  unique() %>% 
  filter(!is.na(bookmark)) %>% 
  mutate(Team=paste0("<a href=\"", bookmark, "\" target=\"_blank\">", team, "</a>")) %>% 
  select(Team)%>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,,escape = FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
names(teamSeason)


# Spurs furthest off top place --------------------------------------------


sort(names(standings))

topTeam <- standings %>% 
  filter(position==1) %>% 
  select(team,cumPts,season,tmYrGameOrder,gameDate)

standings %>% 
  filter(team=="Tottenham H") %>% 
  select(Tm=team,spursPts=cumPts,season,tmYrGameOrder) %>% 
  left_join(topTeam) %>% 
  mutate(behind=spursPts-cumPts) %>% 
  arrange(gameDate) %>% 
  plot_ly(x=~gameDate,y=~behind)

standings %>% 
  filter(team=="Tottenham H") %>% 
  select(Tm=team,spursPts=cumPts,season,tmYrGameOrder) %>% 
  left_join(topTeam) %>% 
  mutate(behind=spursPts-cumPts) %>% 
  arrange(gameDate) %>% 
  filter(tmYrGameOrder==15) %>% 
  plot_ly(x=~gameDate,y=~behind) %>% 
  add_bars() %>% 
  layout(title="Spurs deficit on leaders after 15 games of season",
         xaxis=list(title=""),
         yaxis=list(title="Points in arrears")) %>%  config(displayModeBar = F,showLink = F)
  

## looks good for specidic stage of season


## can revist if gets above 26
standings %>% 
  filter(team=="Tottenham H") %>% 
  select(Tm=team,spursPts=cumPts,season,tmYrGameOrder) %>% 
  left_join(topTeam) %>% 
  mutate(behind=spursPts-cumPts) %>% 
  arrange(gameDate) %>% 
  #filter(tmYrGameOrder==15) %>% 
  plot_ly(x=~gameDate,y=~behind) %>% 
  add_markers()



# Theo Walcott ------------------------------------------------------------

#ages 21-23  66 gls and assists to 2012/13 - at hsia ge inc prev years


# schedule difficulty -----------------------------------------------------

## last years team

old <- standings %>% 
  filter(season=="2016/17"&tmYrGameOrder==1) %>% 
  select(position=final_Pos,team) %>% 
  filter(position<18) 

new <- data.frame(team=c("Newcastle U","Brighton","Huddersfield"), position=c(18,19,20))

all <- bind_rows(old,new)
  


# for each team get value of opp they have faced to dae

chelsea <- standings %>% 
  filter(season=="2017/18"&team=="Chelsea") %>% 
  select(OppTeam) %>% 
  left_join(all,by=c("OppTeam"="team")) %>% 
  summarise(diff=mean(position))


myFun <- function(x) {
  
  ## remove x from all and then reorder
  
  change <- all %>% 
    filter(team!=x) %>% 
    mutate(order=row_number())
  
  standings %>% 
  filter(season=="2017/18"&team==x) %>% 
  select(OppTeam) %>% 
  left_join(change,by=c("OppTeam"="team")) %>% 
  summarise(diff=mean(order)) %>% 
    pull(diff)
}


difficulty=map_dbl(all$team,myFun)




df <- data.frame(team=all$team,difficulty=difficulty)

df %>% 
  arrange(difficulty) %>% 
  mutate(diff=row_number())
  

# Burnly 4 clean sheets in row almost --------------------------------------------

burnley <- standings %>% 
  filter(team=="Burnley") %>% 
  arrange(gameDate)

library(RcppRoll)

counts = roll_sum(burnley$GA,n=4)
df <- data.frame(goals=counts) %>% 
  mutate(order=row_number()) %>% 
  plot_ly(x=~order,y=~goals) %>% 
  add_lines()



# Everton conceding -------------------------------------------------------

24 in last 8 games worst start for 59 yraer

pickford

playerGame %>% 
  filter(PLAYERID=="PICKFOJ"&START>0&season=="2017/18")



playerGame %>% 
  filter(PLAYERID=="PICKFOJ"&START>0&season=="2016/17") %>% 
  select(TEAMMATCHID,MATCHID) %>% 
  left_join(standings) %>% # only matched bu matchid
  filter(team=="Sunderland") %>% 
  summarize(mean_GA=mean(GA)) # 1.724138


playerGame %>% 
  filter(PLAYERID=="PICKFOJ"&START>0&season=="2017/18") %>% 
  select(TEAMMATCHID,MATCHID) %>% 
  left_join(standings) %>% # only matched bu matchid
  filter(team=="Everton") %>% 
  summarize(mean_GA=mean(GA)) #2


# worst GA

sort(names(standings))


test <-standings %>% 
  group_by(season,team,final_Pos) %>% 
  summarise(mean_GA=mean(GA)) %>% 
  arrange(desc(mean_GA))



# are man city safe -------------------------------------------------------

standings %>% 
  filter(season>"1994/95"&final_Pos>17&tmYrGameOrder==38&cumPts>37) %>% 
  arrange(desc(cumPts)) %>% 
  select(season,team,points=cumPts) %>%
                         DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

# 2 home goals 4 in row ---------------------------------------------------

test <- standings %>% 
  arrange(gameDate) %>% 
  filter(team=="Crystal P"&venue=="H") %>% 
  select(gameDate,OppTeam,GF)
# yep never before


# Palace scoring away drought ---------------------------------------------

base it on player - not scoring - could have as a tables

# when did team score ingame
tmDroughtFun <- function(df) {
  expand.grid(c(1:90),df$TEAMMATCHID)
}

plDroughtFun <- function(df) {
  expand.grid(c(df$on:df$off),df$PLAYER_MATCH)
}

# need to add other column as are matching on TIME
gls <- goals %>% 
  mutate(scored=1)

sort(names(teamGames)) # has gmaeDate
# [1] "CROWD"         "gameDate"      "GOALS"         "MATCHID"       "REFEREE"       "season"       
# [7] "TEAMID"        "TEAMMATCHID"   "TEAMNAME"      "tmGameOrder"   "tmYrGameOrder" "venue" 

#Cp
games <- teamGames %>% 
  filter(TEAMNAME=="Crystal P"&venue=="A") %>% 
  ungroup() %>% 
  select(TEAMMATCHID,gameDate)
#names(games)

mins <- expand.grid(c(1:90),games$TEAMMATCHID)
names(mins) <- c("TIME","TEAMMATCHID")

mins <-mins %>% 
  left_join(games) %>% 
  select(TIME,TEAMMATCHID,gameDate) %>% 
  as.tbl()
names(gls)

# create gaps between goals
goalData <- mins %>% 
  left_join(gls)  %>% 
  select(TEAMMATCHID,TIME,gameDate,scored) %>% 
  arrange(gameDate,TIME) %>% 
  mutate(minOrder=row_number(),goal=ifelse(is.na(scored),0,1)) %>% 
  filter(goal==1|minOrder==max(minOrder)) %>% # to take account of current spell
  mutate(minslag=lag(minOrder),gap=minOrder-minslag)

# account for gap at beginning of career
goalData[1,]$gap <- goalData[1,]$minOrder

## current /max/max ths year
#currently 826 mins  - extend to all teams


# Rooney ------------------------------------------------------------------

#prob could look at the scorelines no that does not have players in

sort(names(playerGame))


rooney <- playerGame %>% 
  filter(PLAYERID=="ROONEYX") %>% 
  select(season,gameDate,mins,Gls) %>% 
  group_by(season) %>% 
  arrange(gameDate) %>% 
  mutate(cumMins=cumsum(mins),cumGls=cumsum(Gls))
  

maxMins <-rooney %>% 
  filter(season=="2017/18"&cumMins==max(cumMins)) %>% 
  select(cumMins) %>% 
  pull(cumMins)  #907


plDroughtFun <- function(df) {
  expand.grid(c(df$on:df$off),df$PLAYER_MATCH)
}

# need to add other column as are matching on TIME
gls <- goals %>% 
  mutate(scored=1)


  games <-playerGame %>% 
    filter(PLAYERID=="ROONEYX"&mins>0) %>% 
    select(PLAYERID,name,PLAYER_MATCH,START,on,offA,gameDate,TEAMMATCHID,season) %>% 
    mutate(on=as.integer(on),off=as.integer(offA)) %>% 
    mutate(on=ifelse(is.na(on),1,on),off=ifelse(is.na(off),90,off))
  
  # create list so can apply function above with purrr
  games_list <- split(games, 1:nrow(games))
  mins <-map_df(games_list,plDroughtFun)
  
  
  # rename columns and add gameDate
  names(mins) <- c("TIME","PLAYER_MATCH") 
  mins <-mins %>% 
    left_join(games) %>% 
    select(TIME,PLAYER_MATCH,gameDate,season)
  
  goalData <- mins %>% 
    left_join(gls)  %>% 
    select(PLAYER_MATCH,TIME,gameDate,scored,season) %>% 
    arrange(gameDate,TIME,season) %>% 
    group_by(season) %>% 
    mutate(minsPlayed=row_number(),goal=ifelse(is.na(scored),0,1),cumGoals=cumsum(goal))
  
  
 df <- goalData %>% 
    filter(minsPlayed==maxMins) %>% 
     arrange(season)
 
 df %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
 
 
 df %>% 
   plot_ly(x=~season,y=~cumGoals) %>% 
   add_bars()
    
    mutate(minOrder=row_number(),goal=ifelse(is.na(scored),0,1)) %>% 
    filter(goal==1|minOrder==max(minOrder)) %>% # to take account of current spell
    mutate(minslag=lag(minOrder),gap=minOrder-minslag)

## looks good just change look
    
    
Place %>% 
  group_by(PLAYERID) %>% 
  summarize(tot=sum(LongRange)) %>% 
  arrange(desc(tot))


# salahs start with liverpool ---------------------------------------------

sort(names(playerGame))


temp <-playerGame %>% 
  group_by(PLAYER_TEAM,name,TEAMNAME) %>% 
  filter(plTmGameOrder<=14&str_trim(name)!="Own Goal") %>% 
  summarize(tot=sum(Gls)) %>% 
  arrange(desc(tot))


# GIROUDS GOALS AS SUB ----------------------------------------------------

sort(names(summary))

top6 <- summary %>% 
    group_by(PLAYERID) %>% 
  summarize(totSub=sum(subGls),totSt=sum(StGls),pc=totSub/(totSub+totSt)) %>% 
  arrange(desc(totSub))

summary %>% 
  filter(PLAYERID=="GIROUDO") %>% 
  arrange(season) %>% 
  plot_ly(x=~season,y=~subGls)

# man u exactly 4 goals in season

standings %>% 
  group_by(season,team) %>% 
  filter(GF==4) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(team=="Man. Utd.")



# sterling goals ----------------------------------------------------------

playerGame %>% 
  filter(PLAYERID=="STERLIR") %>% 
  arrange(desc(age)) %>% 
  pull(age)  #22.94867


sort(names(playerGame))

playerGame%>% 
  filter(as.double(age)<23) %>% 
  mutate(points=Gls+Assists) %>% 
  group_by(PLAYERID) %>% 
  summarize(tot=sum(points)) %>% 
arrange(desc(tot))


glimpse(playerGame)


# mane scoring drought


# top assisters per min in prem -------------------------------------------

playerGame %>% 
  group_by(name,PLAYERID) %>% 
  summarize(m=sum(mins),a=sum(Assists)) %>% 
  filter(a>39) %>% 
  mutate(mpa=round(m/a)) %>% 
  arrange(mpa) %>% 
  ungroup() %>% 
  select(name,`mins`= mpa)%>%
                         DT::datatable(width =200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# Palace away for all clubs -----------------------------------------------



# Arsenal conceding 2 within 10 mins --------------------------------------

df <- read_rds("scoreLines.rds")
names(df) "mins"        "GF"          "TEAMMATCHID" "MATCHID"     "GA"         
[6] "OPPMATCHID"  "diff"       
sort(names(teamGames))

data <-df %>% 
  left_join(teamGames) %>% 
  #filter(MATCHID==1313) %>% 
  left_join(standings,by=c("MATCHID"="MATCHID","TEAMNAME"="team")) %>% #36mill
  select(team=TEAMNAME,season=season.x,gameDate=gameDate.x,mins,diff,tmYrGameOrder=tmYrGameOrder.x, GF=GF.y,GA=GA.y,OppTeam,final_Pos)

data %>% 
  filter(team=="Arsenal"&mins<=11&diff<=-2) %>% 
  group_by(TEAMMATCHID) %>% 
  slice(1)
## 3 occasions before this last game

# all4 since 16/4/2012 previous 3 all 	W Szczesny,

## all games going 2 down $ 112

downTwo <- data %>% 
  filter(team=="Arsenal"&diff<=-2) %>% 
  group_by(TEAMMATCHID) %>% 
  slice(1)

## should be all games 977
data %>% 
  filter(team=="Arsenal") %>% 
  group_by(TEAMMATCHID,gameDate) %>% 
  slice(1) %>% 
  select(TEAMMATCHID,gameDate) %>% 
  left_join(downTwo) %>% 
  arrange(gameDate) %>% 
  ungroup() %>% 
  mutate(mins=ifelse(is.na(team),0.1,mins),order=row_number()) %>% 
  plot_ly(x=~gameDate, y=~mins) %>% 
  add_bars()


## also most by calendat year

sort(names(downTwo))

downTwo %>% 
  mutate(year=as.character(year(gameDate))) %>% 
  group_by(year) %>% 
  tally() %>% 
  plot_ly(x=~year,y=~n,color=~year) %>% 
  add_bars(showlegend=FALSE) %>% # does not work - neither does showscale need to make year a character
  layout(title="EPL Games Arsenal have been two goals down in by calendar year",
         xaxis=list(title=""),
         yaxis=list(title="Count")) %>%  config(displayModeBar = F,showLink = F)
         



# MNC points after so many games + wins on trot ------------------------------------------

13 is a rec top tier single season - could look at other is detail

get their goals as winners etc 


# Tottenham in arrears  see Spurs furthest off top place----------------------------------------------------





# TOT assist/goals from subs ----------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(on>0&season=="2017/18") %>% 
  group_by(TEAMNAME) %>% 
  summarize(count=n(),mins=sum(mins),goals=sum(Gls),assists=sum(Assists),points=goals+assists) %>% 
  plot_ly(x=~mins,y=~jitter(points)) %>% 
  add_markers(hoverinfo="text",text=~paste0(
    "Goals: ",goals,
    "<br>Assists: ",assists,
    "<br>Mins: ",mins
  ),showlegend=FALSE) %>% 
  add_text(text=~TEAMNAME,showlegend=FALSE, hoverinfo="none") %>% 
  layout(title="Goals and Assists contributed by subs - 2017/18",
    xaxis=list(title="Minutes",rangemode="tozero"),
    yaxis=list(title="Goals and Assists")
    
    ) %>%  config(displayModeBar = F,showLink = F)
    
playerGame %>% 
  filter(on>0&season=="2017/18"&TEAMNAME=="Man. Utd.") %>% 
  group_by(PLAYERID) %>% 
  summarize(count=n(),mins=sum(mins),goals=sum(Gls),assists=sum(Assists),points=goals+assists)
  



# teams facing team off and def ability over rst of season ----------------



https://twitter.com/bankruptspurs/status/937258735331954688


  