# routine table -----------------------------------------------------------

# something on Wenger -----------------------------------------------------



## results - amy have to copy and run in console
hth %>% 
  filter(season=="2016/17"&gameDate>="2016-10-25"&gameDate<="2016-11-01") %>% ## may need to put in day later?
  filter(venue=="H") %>% 
  arrange(team) %>% 
  select(Home=team,GF,GA,Away=OppTeam) %>% 
  DT::datatable(rownames=FALSE,class='compact stripe hover row-border',colnames = c('', '', '', ''),
                options= list(paging = FALSE, searching = FALSE,info=FALSE,
                              columnDefs = list(list(width = '40%', targets = list(0,3)))),width=250,height=300)


# table
hth %>% 
  filter(season=="2016/17"&gameDate<="2016-11-01") %>% 
  group_by(team) %>% 
  mutate(W = ifelse(res=="Win",1,0),L = ifelse(res=="Loss",1,0),D = ifelse(res=="Draw",1,0)) %>%
  summarise(P=n(),Pts=sum(points),W=sum(W),D=sum(D),L=sum(L),GD=sum(GF)-sum(GA),GF=sum(GF)) %>% 
  arrange(desc(Pts),desc(GD),desc(GF),team) %>%
  DT::datatable(class='compact stripe hover row-border order-column',colnames = c('', 'P', 'Pts', 'W','D', 'L', 'GD','GF'),
                rownames=TRUE,options= list(paging = FALSE, searching = FALSE,info=FALSE),width=300)

#-------------------
## NB may not work given 99 also problem with purchases of teams when not in PL

library(tibble)
playerClub <- as_tibble(playerClub)
temp <-playerClub %>% #6621
  filter(JOINED>"1992-05-15"&FEE>0&!is.na(FEE)) #6071 by date 3205 down fee

years <- c(1992:2017)
## should just check this date in year
temp$season <- as.character(cut(temp$JOINED,  breaks=as.Date(paste(years,"-05-15",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))

max(temp$season)

cumFees <-temp %>% 
  arrange(JOINED) %>% 
  group_by(TEAMID) %>% 
  mutate(cumFee=1000*cumsum(FEE)) %>% 
  arrange(desc(cumFee)) %>% 
  ungroup()

a <- list(
  x = "1995-01-01",
  y = 99000000,
  text = "Pogba",
  xref = "x",
  yref = "y",
 showarrow = TRUE,
 #arrowhead = 7,
  ax = 0,
  ay = -20
)

topTeams <- c("ARS","BLB","NEW","LIV","EVE","CHL","MNC","LEE","ASV","MNU","TOT")

cumFees %>% 
  filter(TEAMID %in% topTeams) %>% 
  left_join(teamCodes) %>% 
  arrange(TEAMNAME) %>% 
  plot_ly(x=JOINED,y=cumFee,mode="lines", color=TEAMNAME, colors="Paired",
          hoverinfo="text", 
            text = paste(TEAMNAME,"<br>",JOINED,"<br>Total",round(cumFee/1000000,0),"m")) %>% 
  add_trace(x = c(min(JOINED), max(JOINED)), y= c(89000000, 89000000), mode = "lines", marker=list(color="blue"), name="Pogba") %>%
  layout(hovermode = "closest",
         title="Approximate Cumulative Fees paid by PL Club to 9th August 2016<br> Hover for details",
         xaxis=list(title=""),
         yaxis=list(title=""
         ),
         annotations = a
  )
  
  

## could look at top 6 spending teams of all time

?

test <-cumFees %>% 
    group_by(TEAMID) %>% 
  arrange(desc(cumFee)) %>% 
  slice(1)  %>% 
  arrange(desc(cumFee))


## line graph with Pogba in
## box graph of changes by year

# starting lineup change from year before ---------------------------------

sort(names(playerGame))

## first games of season
openingDay <- playerGame %>% 
  group_by(season,TEAMNAME) %>% 
  summarize(gameDate=min(gameDate)) %>% 
  unique() %>% 
  
  sort(openingDay$firstGame) # some first games 20th or 22nd ?? in 2011

openingDay everton 20th tot 22nd??  Everton	0 - 1	QPR  manu 3 tot 0.. manu had played earlier


test <- playerGame %>% #302279
  right_join(openingDay) %>% #7877
  filter(TEAMNAME=="Arsenal"&START>0) %>%   #264
  select(LASTNAME,PLAYERID,season)

seasons <- sort(unique(test$season))


#i <- 1

same <- integer()
yr <- character()
for(i in 1:(length(seasons)-1)) {
  print(i)
  same[i] <-  test %>% 
    filter(season==seasons[i]|season==seasons[i+1]) %>% 
    group_by(PLAYERID) %>% 
    tally() %>% 
    filter(n==2) %>% 
    nrow()
  yr[i] <- seasons[i+1]
  
}

## this works - just need to expand


# record over specific perios - create a moving average -------------------

sort(names(standings))


unique(standings$team)

cp <- standings %>% 
  ungroup() %>% 
  arrange(gameDate) %>% 
  filter(team=="Man. Utd.") %>% 
  select(gameDate,points,tmGameOrder,res) %>% 
  mutate(gameDate=as.character(gameDate))


stretch <- 21 # number of games in moving av
library(purrr)
i <- 1

av <- double()
date <- character()
res <- character()

for (i in 1:(nrow(cp)-(stretch-1))) {
  av[i] <- sum(cp[i:(i+stretch-1),"points"])/stretch
  date[i] <- as.character(cp[(i+stretch-1),"gameDate"]) #%>% unlist()
  res[i] <- cp[(i+stretch-1),"res"]
}

result <-map_chr(res,1)

df <- tibble(avPts=av,date=date,result=result)

df <- df %>% 
  mutate(pointColor=ifelse(result=="Draw","blue",ifelse(result=="Win","green","red")))
library(plotly)
df %>% 
  plot_ly(x=date,y=round(avPts,2),mode="markers",
          marker=list(color=pointColor),name=result)

## update plotly does purt colors in but looks wrong
df %>% 
  plot_ly(x=~date,y=~round(avPts,2),mode="markers",
          marker=list(color=~pointColor),name=result)

## look at line

df %>% 
  plot_ly(x=~date, y=~avPts, color=~pointColor)

#marker=list(color=result))
library(listviewer)

## av does not look quite right

jsonedit(date)

map_chr(res,1)



# first after first with final position -----------------------------------

## from standings but doing a 

sort(names(standings))

standings %>% 
  filter(tmYrGameOrder==1&position==1&season<"2016/17") %>% 
  select(final=final_Pos,team,season) %>% 
  plot_ly() %>% 
  add_markers(y = ~final,x=~season,color=~team,colors="Paired",
              hoverinfo="text",marker=list(size=10),
              text=~paste0(season,"<br>",team,"<br>Pos: ",final)) %>% 
  layout(
    title="Final positions of Teams Leading after Round 1 of PL",
    yaxis=list(range=c(18.5,0.5),title="Final Position"),
    xaxis=list(title="",categoryarray = ~season, categoryorder = "array"))


standings %>% 
  filter(tmYrGameOrder==1&position==1&season<"2016/17") %>% 
  select(final=final_Pos,team,season) %>% 
  plot_ly() %>% 
  add_markers(y = ~final,x=~season,color=~team,colors="Paired",
              hoverinfo="text",marker=list(size=10),
              text=~paste0(season,"<br>",team,"<br>Pos: ",final)) %>% 
  layout(
    title="Final positions of Teams Leading after Round 1 of PL",
    margin=list(b=60),
    yaxis=list(title="",range=c(18.5,0.5),title="Final Position"),
    xaxis=list(title=""))


# Walcott scoring in ## succsesive seasons --------------------------------

yrs <- tmYrs %>% 
  ungroup() %>% 
  select(season)  %>% 
  unique() %>% 
  arrange(season) %>% 
  .$season


sort(names(playerGame))

scorers <- playerGame %>% 
  ungroup() %>% 
  group_by(PLAYERID,name,season) %>% 
  summarize(goals=sum(Gls))

allScorers <- scorers$PLAYERID

totPoss <- expand.grid(season=yrs,PLAYERID=allScorers)

totPoss %>% 
  filter(PLAYERID=="BARTONJ")

temp <- totPoss  %>% 
  #filter(PLAYERID=="BARTONJ") %>% 
  left_join(scorers) %>% 
  mutate(sc=ifelse(is.na(goals),0,goals)) %>% 
  unique() %>% 
  mutate(x=ifelse(sc==0,0,1))

glimpse(temp)

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
  unique() %>% 
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE),width=300)




# Week 2 fastest to notch 2 goals -----------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(plGameOrder<=2&Gls>1&START==0) %>% 
  select(name,TEAMMATCHID,START,mins,gameDate)

playerGame %>% 
  filter(PLAYERID=="NOLITO")



# animation of fees by club this offseason --------------------------------

sort(names(playerClub))

dates <- seq.Date(from=as.Date("2016-06-01"), to=as.Date("2016-09-01"), by=1)

sort(names(standings))

teams <- tmYrs %>% 
  ungroup() %>% 
  filter(season=="2016/17") %>% 
  arrange(team) %>% 
  left_join(teamCodes,b=c("team"="TEAMNAME")) %>% 
  .$TEAMID

tmdates <- expand.grid(TEAMID=teams,date=dates)  #TEAMID is character



temp <- playerClub %>% 
  filter(JOINED>="2016-06-01"&FEE!=0&FEE!=99) %>% 
  select(date=JOINED,TEAMID,FEE,PLAYERID) %>% 
  right_join(tmdates) %>%  #1867 - expect 1860
  mutate(transfer=ifelse(is.na(FEE),0,FEE)) %>% 
  arrange(date) %>% 
  group_by(TEAMID) %>% 
  mutate(cumFee=cumsum(transfer)) %>% 
  left_join(allPlayers) %>% 
  select(date,cumFee,TEAMID,LASTNAME,transfer) 



library(plotly)
temp %>% 
  #filter(TEAMID=="CRP") %>% 
  plot_ly() %>% 
  add_lines(x=~date,y=~cumFee,color=~TEAMID, colors="Paired",
            hoverinfo="text", 
            text = ~paste(LASTNAME,"<br>",transfer/1000
                          )) %>% 
  layout(hovermode = "closest",
         title="2016 Summer Transfer Purchases by team",
         xaxis=list(title=""),
         yaxis=list(title="Cumulative Fee"
         )
  )
 
  
library(animation)

seq_along(dates)


i <- 25
saveGIF({
  
  for (i in seq_along(dates)) {
    
   # print(dates[i])
    
    g <- temp %>% 
      filter(date<=dates[i]) %>% 
      plot_ly() %>% 
      add_lines(x=~date,y=~cumFee,color=~TEAMID, colors="Paired")
    
    print(g)
  }
}, movie.name = "transfers.gif", interval = 0.2, ani.width = 700, ani.height = 600)

    
In find_magic() : ImageMagick not installed yet!

  library(installr)
install.ImageMagick()
# run again host of errors https://github.com/yihui/animation/issues/67 may have to run as administrator
ani.options(convert ="C:/Program Files/ImageMagick-7.0.2-Q16/convert.exe") # cannot see this file in windows explorer


    yearly <- filter(top20, year == i)
    
    g <- ggplot() + 
      geom_bar(data = yearly, aes(y = population, x = reorder(name_short, population), 
                                  fill = region, frame = year), stat = "identity") + 
      geom_text(data = yearly, aes(y = population + position_label, x = reorder(name_short, population), 
                                   label = poplabel), fontface = "bold") + 
      coord_flip() +
      scale_y_continuous(limits = c(0, 25000000), expand = c(0, 0)) + 
      scale_fill_manual(values = c("Northeast" = "#e41a1c", "Midwest" = "#377eb8", 
                                   "South" = "#4daf4a", "West" = "#984ea3")) + 
      theme_minimal(base_size = 16, base_family = "Tahoma") + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(), 
            plot.title = element_text(face = "bold"), 
            axis.text.x = element_blank(), 
            legend.position = "bottom") + 
      labs(y = "", 
           x = "", 
           fill = "", 
           caption = "Data source: Jonathan Schroeder, University of Minnesota | Chart by @kyle_e_walker", 
           title = paste0("20 largest US metro areas by population, ", as.character(i))) 
    
    print(g)
    
  }
  
  
}, movie.name = "metro_pop.gif", interval = 0.8, ani.width = 700, ani.height = 600)


saveGIF({
  
  for (i in seq(1790, 2010, 10)) {
    
    yearly <- filter(top20, year == i)
    
    g <- ggplot() + 
      geom_bar(data = yearly, aes(y = population, x = reorder(name_short, population), 
                                  fill = region, frame = year), stat = "identity") + 
      geom_text(data = yearly, aes(y = population + position_label, x = reorder(name_short, population), 
                                   label = poplabel), fontface = "bold") + 
      coord_flip() +
      scale_y_continuous(limits = c(0, 25000000), expand = c(0, 0)) + 
      scale_fill_manual(values = c("Northeast" = "#e41a1c", "Midwest" = "#377eb8", 
                                   "South" = "#4daf4a", "West" = "#984ea3")) + 
      theme_minimal(base_size = 16, base_family = "Tahoma") + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(), 
            plot.title = element_text(face = "bold"), 
            axis.text.x = element_blank(), 
            legend.position = "bottom") + 
      labs(y = "", 
           x = "", 
           fill = "", 
           caption = "Data source: Jonathan Schroeder, University of Minnesota | Chart by @kyle_e_walker", 
           title = paste0("20 largest US metro areas by population, ", as.character(i))) 
    
    print(g)
    
  }
  
  
}, movie.name = "metro_pop.gif", interval = 0.8, ani.width = 700, ani.height = 600)
  
# Wk 3 wins in 24 palace -------------------------------------------------------

allteams <- standings %>% 
  ungroup() %>% 
  group_by(team) %>% 
  
  arrange(gameDate) %>% 
  mutate(rn=row_number()) %>% 
  select(team,rn,res,season,gameDate,points,tmYrGameOrder)

tm <- allteams %>% 
  filter(team=="Man. Utd.")


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


### would make interesting subplot or trelliscope
## Need to do all at once

allteams <- standings %>% 
  ungroup() %>% 
  group_by(team) %>% 
  
  arrange(gameDate) %>% 
  mutate(rn=row_number()) %>% 
  select(team,rn,res,season,gameDate,points,tmYrGameOrder)

tm <- allteams %>% 
  filter(team=="Man. Utd.")


endDate=as.Date(character())
season=character()
run=integer()
points = integer()
round= integer()



currentTeams <- allteams %>% 
  filter(season=="2016/17") %>% 
  select(team) %>% 
  unique() %>% 
  arrange(team) %>% 
  .$team

j <- 1
i <- 1

for(j in seq_along(currentTeams)) {
  print(j)
  tm <- allteams %>% 
    filter(team==currentTeams[j])
 run <- integer()
  points <- integer()
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
  print("a")
  #df <- tibble(run=run,endDate=endDate,season=season,points=points,round=round)
  df <- tibble(run=run,points=points)# not sure what good round is
  df$team <- currentTeams[j]
  latest <- tail(df,1)
  print("b")
  if (j!=1) {
    dfAll <- rbind(dfAll,df)
    latestAll <- rbind(latestAll,latest)
  } else {
    dfAll <- df
    latestAll <- latest
  }
}
#df <- tibble(run=run,endDate=endDate,season=season,points=points,round=round)

p <- plot_ly() %>% 
  add_histogram(data=df,x=~points, , # alternative histnorm="percent",
                marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>%   # reduce width - might need to do bars or change layout?
  add_histogram(data=latest,x=~points, type="histogram",marker=list(color="red"),name="Current") %>%
  layout(barmode="overlay")


p <- dfAll %>% 
  group_by(team) %>% 
  plot_ly() %>% 
  add_histogram(x=~run,
                marker=list(color="lightblue"),opacity=0.7,name="PL all-time")

p <- subplot(p, nrows = 5)
p


library(plotly)
# Define xaxis and yaxis
p <- plot_ly(economics, x = date, y = uempmed, xaxis = "x1", yaxis = "y1")
p <- add_trace(p, x = date, y = unemploy, xaxis = "x1", yaxis = "y2")
p <- layout(p, showlegend = FALSE, yaxis = list(anchor = 'x', domain = c(0, 0.45)),
            yaxis2 = list(anchor = 'x', domain = c(0.55, 1), title = 'unemploy'))
p



p <- subplot(plot_ly() %>% 
  add_histogram(data=subset(dfAll,team=="Crystal P"),x=~run, # alternative histnorm="percent",
                marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>% 
  add_histogram(data=subset(dfAll,team=="Arsenal"),x=~run, # alternative histnorm="percent",
                marker=list(color="lightblue"),opacity=0.7,name="PL all-time")
)


# this works v basic though no team name, latest
p <- subplot(
plot_ly(data=subset(dfAll,team=="Crystal P"),x=~run, # alternative histnorm="percent",
       marker=list(color="lightblue"),opacity=0.7),
  plot_ly(data=subset(dfAll,team=="Arsenal"),x=~run, # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7),
nrows=2

) %>% layout(showlegend = FALSE)
p

i <- 3
p <- subplot(
  for(i in 1:seq_along(currentTeams)) {
  plot_ly(data=subset(dfAll,team==currentTeams[i]),x=~run, 
          marker=list(color="lightblue"),opacity=0.7)
  },
  nrows=5
  
) %>% layout(showlegend = FALSE)
p

p <- subplot(
  for(i in 1:seq_along(currentTeams)) {
    plot_ly(data=subset(dfAll,team==currentTeams[i]),x=~run, 
            marker=list(color="lightblue"),opacity=0.7)
  },
  nrows=5
  
) %>% layout(showlegend = FALSE)
p


toy example

library(plotly)
library(dplyr)

# construct data.frame
df <- tibble(x=c(3,2,3,5,5,5,2),y=c("a","a","a","b","b","b","b"))

# construct data.frame of last y values
  latest <- df %>% 
     group_by(y) %>% 
     slice(n())

# plot for one value of y (NB not sure why value for 3 appears?)
  p <- plot_ly() %>% 
  add_histogram(data=subset(df,y=="b"),x= ~x) %>%   
  add_histogram(data=subset(latest,y=="b"),x= ~x,marker=list(color="red")) %>%
  layout(barmode="overlay",showlegend=FALSE,title= ~y)
  p
  
  





p <- subplot(
  plot_ly(data=subset(dfAll,team=="Crystal P"),x=~run, # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7,name="PL all-time") %>% 
    plot_ly(data=subset(latest,team=="Crystal P"),x=~points, marker=list(color="red"),name="Current"),
  plot_ly(data=subset(dfAll,team=="Arsenal"),x=~run, # alternative histnorm="percent",
          marker=list(color="lightblue"),opacity=0.7,name="PL all-time"),
  nrows=2
) %>% layout(showlegend = FALSE)
  
p

p <- subplot(p, nrows = 2)

p <- subplot(
  plot_ly(economics, x = date, y = uempmed),
  plot_ly(economics, x = date, y = unemploy),
  margin = 0.05
) %>% layout(showlegend = FALSE)
p

#http://stackoverflow.com/questions/39197756/how-can-i-create-subplots-in-plotly-using-r-where-each-subplot-is-two-traces

library(plotly)
library(dplyr)

# construct data.frame
df <- tibble(x=c(3,2,3,5,5,5,2),y=c("a","a","a","b","b","b","b"))

# construct data.frame of last y values
latest <- df %>% 
  group_by(y) %>% 
  slice(n())

## may want to add box plot here?? to get red throught line



# plot for one value of y (NB not sure why value for 3 appears?)
p <- plot_ly() %>% 
  add_histogram(data=subset(df,y=="b"),x= ~x) %>%   
  add_histogram(data=subset(latest,y=="b"),x= ~x,marker=list(color="red")) %>%
  layout(barmode="overlay",showlegend=FALSE,title= ~y)
p

#http://stackoverflow.com/questions/39197756/how-can-i-create-subplots-in-plotly-using-r-where-each-subplot-is-two-traces (needed to replace x=x with x=~x)

N = nlevels(factor(df$y))
plot_list = vector("list", N)


a <- list(
  # x = m$wt,
  # y = m$mpg,
  text = this_y,
  xref = 50,
  yref = 50
  # showarrow = TRUE,
  # arrowhead = 7,
  # ax = 20,
  # ay = -40
)

for (i in 1:N) {
  this_y = levels(factor(df$y))[i]
  p <- plot_ly() %>% 
    add_histogram(data=subset(df,y==this_y), x=~x, marker=list(color="blue"),
              autobinx=F, xbins=list(start=0.5, end=6.5, size=1)) %>%   
    add_histogram( data=subset(latest,y==this_y), x = ~x, marker=list(color="red"), 
              autobinx=F, xbins=list(start=0.5, end=6.5, size=1)) %>%
    layout(barmode="overlay", showlegend=FALSE, title="Test",annotations= a) %>% 
  plot_list[[i]] = p
}

subplot(plot_list, nrows = 2,shareX=TRUE)

OR

for (i in 1:N) {
  this_y = levels(factor(df$y))[i]
  p <- df %>% 
    plot_ly() %>% 
    add_histogram(data=subset(df,y==this_y), x=~x, marker=list(color="blue"),
              autobinx=F, xbins=list(start=0.5, end=6.5, size=1)) %>%   
    add_histogram(type="histogram", data=subset(latest,y==this_y), x = ~x, marker=list(color="red"), 
              autobinx=F, xbins=list(start=0.5, end=6.5, size=1)) %>%
    layout(barmode="overlay", showlegend=FALSE, title=this_y)
  plot_list[[i]] = p
}

subplot(plot_list, nrows = 2)


## extend to dfAll

# construct data.frame of last y values
latestAll


names(dfAll) #[1] "run"    "points" "team" 

N = nlevels(factor(dfAll$team))
plot_list = vector("list", N) # set length to 20 all NULL

## calculate scale endpoint
endX = max(dfAll$run)+0.5

## for endY needd to do calcs

endY <- dfAll %>% 
  group_by(team,run) %>% 
  tally() %>% 
  arrange(n) %>% 
  tail(1) %>%
  .$n+0.5

i <- 1

for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- df %>% 
    plot_ly() %>% 
    add_histogram(data=subset(dfAll,team==this_y), x=~run, marker=list(color="lightblue"),
                  autobinx=F, xbins=list(start=0.5, end=endX, size=1)) %>%   
    add_histogram(data=subset(latestAll,team==this_y), x = ~run, marker=list(color="red"), 
                  autobinx=F, xbins=list(start=0.5, end=endX, size=1)) %>%
    layout(barmode="overlay", showlegend=FALSE, title="Wins in last 24 League Games"
  plot_list[[i]] = p
}
subplot(plot_list, nrows = 5, shareY= TRUE, shareX = TRUE)


for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- df %>% 
    plot_ly() %>% 
    add_histogram(data=subset(dfAll,team==this_y), x=~run, marker=list(color="lightblue"),
                  autobinx=F, xbins=list(start=0.5, end=endX, size=1)) %>%   
    add_histogram(type="histogram", data=subset(latestAll,team==this_y), x = ~run, marker=list(color="red"), 
                  autobinx=F, xbins=list(start=0.5, end=endX, size=1)) %>%
    layout(barmode="overlay", showlegend=FALSE, title=this_y)
  plot_list[[i]] = p
}
subplot(plot_list, nrows = 5,shareY= TRUE, shareX = TRUE))


### look at a bar equivalent

names(latestAll)
#[1] "run"    "points" "team" ## so run would be the one we want to link to 

latestAll<- latestAll %>% 
  rename(latestRun=run)

names(dfAll)

tmSummary <-dfAll %>% 
  group_by(run,team) %>% 
  tally() %>% 
  left_join(latestAll)


plot_ly() %>% 
  add_bars(data=subset(tmSummary,team=="Crystal P"), x=~run, y=~n, marker=list(color="lightblue")) %>% 
  add_bars(data=subset(tmSummary,team=="Crystal P"&run==latestRun), x=~run, y=~n, marker=list(color="red")) %>%
  layout(barmode="overlay", showlegend=FALSE, title="Crystal P- wins in  24 game sequences",
         xaxis=list(title="Wins"),
         yaxis=list(title="Count"))
                
N = nlevels(factor(dfAll$team))
plot_list = vector("list", N) 

for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- plot_ly() %>% 
    add_bars(data=subset(tmSummary,team==this_y), x=~run, y=~n, marker=list(color="lightblue")) %>% 
    add_bars(data=subset(tmSummary,team==this_y&run==latestRun), x=~run, y=~n, marker=list(color="red")) %>%
    layout(barmode="overlay", showlegend=FALSE, title="Wins in last 24 League Games - Hover plot for Team")
  plot_list[[i]] = p
}
subplot(plot_list, nrows = 5,shareY= TRUE, shareX = TRUE)
## this works


## no good hover just shows west ham
for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- plot_ly() %>% 
    add_bars(data=subset(tmSummary,team==this_y), x=~run, y=~n, marker=list(color="lightblue"),
             hoverinfo=text,text = ~paste(this_y,"<br>",run,"<br>",n)) %>% 
    add_bars(data=subset(tmSummary,team==this_y&run==latestRun), x=~run, y=~n, marker=list(color="red")) %>%
    layout(hovermode = "closest",barmode="overlay", showlegend=FALSE, title="Wins in last 24 League Games - Hover plot for Team")
  plot_list[[i]] = p
}
subplot(plot_list, nrows = 5,shareY= TRUE, shareX = TRUE)


# West Ham have scored in 15 successive PL matches; the longest su --------



# Last minute goals scored/conceded ---------------------------------------

sort(names(goals))
# [1] "GOALS"             "METHOD"            "PLACE"             "PLAY"             
# [5] "PLAYER_MATCH"      "PLAYER_MATCH_GOAL" "TEAMMATCHID"       "TIME"
sort(names(playerGame))

## 

lastFor <- goals %>% 
  left_join(playerGame) %>% 
  filter(TIME==90) %>% 
  select(TIME,TEAMMATCHID,MATCHID,TEAMNAME) %>% 

  group_by(TEAMNAME) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  select(For=n,TEAMNAME)


lastMin <- goals %>% 
  left_join(playerGame) %>% 
  filter(TIME==90) %>% 
  select(MATCHID,Scorer=TEAMNAME) %>% 
  left_join(teamGames) %>% 
  filter(Scorer!=TEAMNAME) %>% 
  select(TEAMNAME) %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
  select(Ag=n,TEAMNAME) %>% 
  left_join(lastFor) %>% 
  mutate(Ag=ifelse(is.na(Ag),0,Ag),For=ifelse(is.na(For),0,For))

library(rcdimple)
# look at R/oddsandends /suicide
# 
# also need to gathr
library(tidyr)

lastMin <-lastMin %>% 
  mutate(Ag=-Ag)

lastMin_gather <- lastMin %>% 
  gather(place,goals,-TEAMNAME)


## maybe simpler to look at mlb to get equivalnt columns


# py <- read_csv("~/R/mlb/problem.csv")
# py

#RD = TEAMNAME
# n=goals
#Result=place

theMax <- max(lastMin_gather$goals)
theMin <- min(lastMin_gather$goals)
title <- "90+ minute PL goals For and Against 1992-2016"

teamOrder <-sort(unique(lastMin_gather$TEAMNAME),decreasing = TRUE)

lastMin_gather %>%
  dimple(x = "goals", y = "TEAMNAME", group = "place", type = 'bar',height=1000,width=1000) %>%
  #dimple(x = "Population", y = "Age", group = "Gender", type = 'bar', storyboard = "Year") %>%
  yAxis(title="",type = "addCategoryAxis", orderRule = teamOrder) %>%
  xAxis(title="Goals ",type = "addMeasureAxis", overrideMax = theMax, overrideMin = theMin) %>%
  add_legend() %>%
  # add_title(html = "<h4 style='font-family:Helvetica; text-align: center;'>Results by Run Differential</h4>") %>%
  add_title(html = paste0("<h3 style='font-family:Helvetica; text-align: center;'>",title,"</h3>")) %>%
  tack(., options = list(
    chart = htmlwidgets::JS("
                              function(){
                              var self = this;
                              // x axis should be first or [0] but filter to make sure
                              self.axes.filter(function(ax){
                              return ax.position == 'x'
                              })[0] // now we have our x axis set _getFormat as before
                              ._getFormat = function () {
                              return function(d) {
                              return d3.format(',.0f')(Math.abs(d)) ;
                              };
                              };
                              // return self to return our chart
                              return self;
                              }
                              "))
  )








lastMin_gather %>%
   dimple(x = "goals", y = "TEAMNAME", group = "place", type = 'bar') %>%
  yAxis(title="Run Differential",type = "addCategoryAxis", orderRule = "goals") %>%  # unclear what this provides
  xAxis(title="Games",type = "addMeasureAxis", overrideMax = theMax, overrideMin = theMin) %>%
  add_legend() %>%
  # add_title(html = "<h4 style='font-family:Helvetica; text-align: center;'>Results by Run Differential</h4>") %>%
  add_title(html = paste0("<h3 style='font-family:Helvetica; text-align: center;'>",title,"</h3>")) %>%
  tack(., options = list(
    chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return d3.format(',.0f')(Math.abs(d)) ;
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            "))
    )




}) # cab add storyboard if by year (prob same in mlb)

## need to look at latest season where may not be for and against

lastFor <- goals %>% 
  left_join(playerGame) %>% 
  filter(TIME==90&season=="2016/17") %>% 
  select(TIME,TEAMMATCHID,MATCHID,TEAMNAME) %>% 
  
  group_by(TEAMNAME) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  select(For=n,TEAMNAME)

# after 3 rounds
# 1     2 Man. City
# 2     1 Crystal P
# 3     1    Hull C
# 4     1 Man. Utd.




lastAg <- goals %>% 
  left_join(playerGame) %>% 
  filter(TIME==90&season=="2016/17") %>% 
  select(MATCHID,Scorer=TEAMNAME) %>% 
  left_join(teamGames) %>% 
  filter(Scorer!=TEAMNAME) %>% 
  select(TEAMNAME) %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
  select(Ag=n,TEAMNAME) 

# Ag    TEAMNAME
# <int>       <chr>
#   1     1 Bournemouth
# 2     1      Hull C
# 3     1     Stoke C
# 4     1     Swansea
# 5     1  West Ham U

## so want 

base <- tmYrs %>% 
  filter(season=="2016/17")

%>% 
  left_join(lastFor) %>% 
  mutate(Ag=ifelse(is.na(Ag),0,Ag),For=ifelse(is.na(For),0,For))

base <- tmYrs %>% 
  rename(TEAMNAME=team) %>% 
  left_join(lastFor) %>% 
  left_join(lastAg) %>% 
  mutate(Ag=ifelse(is.na(Ag),0,Ag),For=ifelse(is.na(For),0,For)) %>% 
  filter(season=="2016/17")


library(rcdimple)
# look at R/oddsandends /suicide
# 
# also need to gathr
library(tidyr)

lastMin <-lastMin %>% 
  mutate(Ag=-Ag)

lastMin_gather <- lastMin %>% 
  gather(place,goals,-TEAMNAME)


# transfer stuff ----------------------------------------------------------

years <- c(1992:2016)
temp$season <- as.character(cut(temp$joined,  breaks=as.Date(paste(years,"-05-15",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))


temp <- playerClub

temp$start <- as.character(cut(temp$JOINED,  breaks=as.Date(paste(years,"-06-01",sep="")),  labels=paste(years[-length(years)],str_sub(years[-length(years)]+1,3,4),sep="/")))

library(lubridate)

temp$month <- month(temp$JOINED)
temp$day <- day(temp$JOINED)
temp$year <- year(temp$JOINED)


# another look at the sequence thing --------------------------------------

allteams <- standings %>% 
  ungroup() %>% 
  group_by(team) %>% 
  
  arrange(gameDate) %>% 
  mutate(rn=row_number()) %>% 
  select(team,rn,res,season,gameDate,points,tmYrGameOrder)

# tm <- allteams %>% 
#   filter(team=="Man. Utd.")


endDate=as.Date(character())
season=character()
run=integer()
points = integer()
round= integer()



currentTeams <- allteams %>% 
  filter(season=="2016/17") %>% 
  select(team) %>% 
  unique() %>% 
  arrange(team) %>% 
  .$team

# j <- 1
# i <- 1

for(j in seq_along(currentTeams)) {
  print(j)
  tm <- allteams %>% 
    filter(team==currentTeams[j])
  run <- integer()
  points <- integer()
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
  print("a")
  #df <- tibble(run=run,endDate=endDate,season=season,points=points,round=round)
  df <- tibble(run=run,points=points)# not sure what good round is
  df$team <- currentTeams[j]
  latest <- tail(df,1)
  print("b")
  if (j!=1) {
    dfAll <- rbind(dfAll,df)
    latestAll <- rbind(latestAll,latest)
  } else {
    dfAll <- df
    latestAll <- latest
  }
}

latestAll<- latestAll %>% 
  rename(latestRun=run)

names(dfAll)

tmSummary <-dfAll %>% 
  group_by(run,team) %>% 
  tally() %>% 
  left_join(latestAll)

# N = nlevels(factor(dfAll$team))
# plot_list = vector("list", N) # set length to 20 all NULL


## calculate scale endpoint
endX = max(dfAll$run)+0.5

## for endY needd to do calcs

endY <- dfAll %>% 
  group_by(team,run) %>% 
  tally() %>% 
  arrange(n) %>% 
  tail(1) %>%
  .$n+0.5


N = nlevels(factor(dfAll$team))
plot_list = vector("list", N) 
lab_list = vector("list", N) # ditto for labels

for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- plot_ly() %>% 
    add_bars(data=subset(tmSummary,team==this_y), x=~run, y=~n, marker=list(color="lightblue")) %>% 
    add_bars(data=subset(tmSummary,team==this_y&run==latestRun), x=~run, y=~n, marker=list(color="red")) %>%
    layout(barmode="overlay", showlegend=FALSE, title="Wins in last 24 League Games - Hover plot for Team")
  plot_list[[i]] = p
}
subplot(plot_list, nrows = 5,shareY= TRUE, shareX = TRUE)
## above is good without teamnames - need to redo title and get rid of run at bottom

plot_list = vector("list", N) 
lab_list = vector("list", N) 


## this works manually setting titlex and titley prob better way to do not in relation to papaer but cant be arsed
for (i in 1:N) {
  this_y = levels(factor(dfAll$team))[i]
  p <- plot_ly() %>% 
    add_bars(data=subset(tmSummary,team==this_y), x=~run, y=~n, marker=list(color="lightblue")) %>% 
    add_bars(data=subset(tmSummary,team==this_y&run==latestRun), x=~run, y=~n, marker=list(color="red")) %>%
    layout(barmode="overlay", showlegend=FALSE, title="Wins in last 24 League Games")
  plot_list[[i]] = p
  
  titlex = c(0.08,0.35,0.62,0.89,0.08,0.35,0.62,0.89,0.08,0.35,0.62,0.89,0.08,0.35,0.62,0.89,0.08,0.35,0.62,0.89)[i]
  titley = c(1.0,1.0,1.0,1.0,0.79,0.79,0.79,0.79,0.58,0.58,0.58,0.58,0.36,0.36,0.36,0.36,0.14,0.14,0.14,0.14)[i]
  
  # titlex = 0.5
  # titley = c(1.05, 0.45)[i]
  lab_list[[i]] = list(x=titlex, y=titley, text=this_y, 
                       showarrow=F, xref='paper', yref='paper', font=list(size=18))
  
}
subplot(plot_list, nrows = 5,shareY= TRUE, shareX = TRUE) %>%
  layout(annotations = lab_list) 

i <- 1
i <- 2
i <- 3


# wk_4 THREE Assist games ------------------------------------------------------

sort(names(playerGame))

## looks Ok but need to sort width on datatable
df <-playerGame %>% 
  filter(Assists>2&gameDate>="2011-08-27") %>% #or since matas first game
  group_by(name,PLAYERID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>2) %>% 
  select(name,count=n) %>%
  DT::datatable(width = "200px",class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))


playerGame %>% 
  filter(PLAYERID=="MATAJ") %>% 
  arrange(gameDate) 
  select(gameDate) #2011-08-27

 DT::datatable(data.frame(
    date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5
    
    
  ),options=list(width="200px"))
 
# something on west ham conceding 4 ---------------------------------------

would be nice to have coming from behind 2
 
 # for now concedinf4 at home
 
 
 sort(names(standings))
 
 ## add annotation and hoverinfo
# annotation 
 a <- list(
   x = 386,
   y = 6.0,
   text = "West Ham",
   # xref = "x",
   # yref = "y",
   showarrow = TRUE,
   arrowhead = 7,
   ax = 20,
   ay = -40
 )
 
 ## book
test <- standings %>% 
   ungroup() %>% 
   filter(venue=="H") %>% 
   select(team,GA) %>% 
   mutate(big=ifelse(GA>3,1,0)) %>% 
   group_by(team) %>% 
   summarize(tot=n(),bad=sum(big),pc=round(100*bad/tot,1)) %>% 
   arrange(desc(pc)) 
test %>% 
  plot_ly() %>% 
  add_markers(x=~tot,y=~pc) %>% 
  layout(annotations = a,
         title="Total and % Games conceding 4+ goals at Home",
         xaxis=list(title="Games played"),
         yaxis=list(title="Per Cent")) %>% 
  
  config(displayModeBar = F,showLink = F)


temp <- standings %>% 
  ungroup() %>% 
  filter(venue=="H"&team=="West Ham U"&GA>3) 

# one win v bradford

temp <- playerGame %>% 
  filter(MATCHID==3216) 5 diff scorers- one of four different times

playerGame %>% 
  filter(TEAMNAME=="West Ham U"&Gls>0&PLAYERID!="OWNGOAL") %>% 
  group_by(MATCHID) %>% 
  tally() %>% 
  arrange(desc(n))

temp <- playerGame %>% 
  filter(MATCHID==233)

# wk4 Kane to 50 premier goals cf others ----------------------------------

#fastest/youngest (owen and fowler well ahead)
# tap in


# chelsea players cf last year  -------------------------------------------

especially costa and hazard

# youngest player by manager wk 3----------------------------------------------


# Rooney away goals -------------------------------------------------------

3 in past 2 seasons - look at others + his direct free kicks

## take from main website

# manager tenures in PL
managerData <- reactive({
  
  req(input$manager)
  
  stints <- managers %>% 
    filter(name==input$manager) %>% 
    mutate(Left=as_date(ifelse(is.na(Left),Sys.Date(),Left)))
  
  
  # this gives all player games played for etc...
  #i <- 1
  for (i in 1:nrow(stints)) {
    
    tempdf <-playerGame %>% 
      filter(gameDate>=stints$Joined[i]&gameDate<=stints$Left[i]&TEAMNAME==stints$TEAMNAME[i])
    
    if (i!=1) {
      df <- bind_rows(df,tempdf)
    } else {
      df <- tempdf
    }
    
  }
  
  
  
  young<-df %>% 
    filter(mins>0&Gls>0) %>% #2992
    select(name,PLAYERID,age,gameDate,TEAMNAME,LASTNAME) %>% 
    arrange(gameDate,age) %>% 
    group_by(gameDate) %>% 
    slice(1) %>% 
    ungroup()  %>% 
    mutate(age = as.numeric(age),cumminAge=cummin(age)) %>% 
    group_by(name,PLAYERID) %>% 
    slice(1) %>% 
    mutate(label=paste0(LASTNAME," ",round(age,2))) %>% 
    rename(Team=TEAMNAME) %>% 
    mutate(alpha=ifelse(age==cumminAge,0.8,0.2)) %>% ## not working yet
    arrange(gameDate) 
  
  info=list(young=young)
  return(info)
  
})

output$managerPlayersAge <- renderPlot({
  
  
  req(managerData)
  title <- paste0("Players age when first scoring for ",input$manager," in Premier League")
  
  p <- ggplot(managerData()$young, aes(gameDate, round(age,2), label = label))
  
  
  p + geom_text_repel(aes(colour = factor(Team))) +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    ylab("Age")            + 
    ggtitle(title)    +
    #theme(legend.title=element_blank()) + 
    theme_fivethirtyeight() +  
    scale_color_fivethirtyeight() +
    theme(legend.title=element_blank()) 
  
})

plotlyOutput("managerPlayersAge")

```


# wk4 MOST EXPENSIVE STARTING LINEUPS -----------------------------------------

sort(names(playerGame))

matchFees <-playerGame %>% 
  filter(!is.na(FEE)&FEE!=99) %>% 
  group_by(MATCHID) %>% 
  summarise(totFee=sum(FEE)) %>% 
  arrange(desc(totFee))

pal <- c("#E41A1C","#377EB8")
pal <- c("lightblue","red")
playerGame %>% 
  filter(MATCHID==9600) %>% 
  arrange(FEE) %>% 
  mutate(shape=ifelse(START>0,1,0)) %>% 
  plot_ly() %>% 
    add_markers(x=~FEE/1000,y=~ name, color= ~TEAMNAME, colors= pal, symbol = ~ shape,
                hoverinfo="text",
                text=~paste(name,"<br>",FEE/1000,"mill.<br>",joined)) %>% 
  layout(margin=list(l=120),
         hovermode = "closest",
         title="Estimated transfer fees of players Man Utd v Man City",
         xaxis=list(title="Million Pounds"),
         yaxis=list(title="",categoryarray = ~name, categoryorder = "array")) %>% 

  config(displayModeBar = F,showLink = F)


## above in annual
## look at extending to games between?

sort(names(hth))

oppteam <-hth %>% 
  filter(team=="Man. Utd.") %>% 
  .$OppTeam %>% 
  unique() %>% 
  sort()

## look at scrolling option
hth %>% 
  filter(team=="Man. Utd."&OppTeam=="Man. City") %>% 
  arrange(desc(gameDate)) %>% 
  mutate(score=paste0(GF,"-",GA)) %>% 
  select(date=gameDate,score) %>% 
 DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
               selection='single',
               width=200,
               options= list(paging = TRUE, searching = FALSE,info=FALSE))


hth <-  hth %>% 
  filter(team=="Man. Utd."&OppTeam=="Man. City") %>% 
  #filter(team==input$team1_wk4&OppTeam==input$opp_wk4) %>% 
  arrange(desc(gameDate)) %>% 
  mutate(score=paste0(GF,"-",GA)) %>% 
  select(date=gameDate,score) 

print(nrow(hth))

hth %>% 
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,
                selection='single',
                width=200,
                options= list(paging = TRUE, searching = FALSE,info=FALSE))



matchFeesStarters <-playerGame %>% 
  filter(!is.na(FEE)&FEE!=99&START>0) %>% 
  group_by(MATCHID) %>% 
  summarise(totFee=sum(FEE)) %>% 
  arrange(desc(totFee)) %>% 
  plot_ly(x=~FEE,y=~)


matchFeesBench <-playerGame %>% 
  filter(!is.na(FEE)&FEE!=99&START==0) %>% 
  group_by(MATCHID) %>% 
  summarise(totFee=sum(FEE)) %>% 
  arrange(desc(totFee))


# poss - Sunderland have never won a Premier League game that Rodwell starts --------


# barry playing 600th game --played with

sort(names(playerGame))

playerGame %>% 
  group_by(PLAYERID) %>% 
  filter(mins>0) %>% 
  summarize(games=n(),totmins=sum(mins,na.rm=TRUE)) %>% 
  arrange(desc(totmins))


teamMatches <- playerGame %>% 
  filter(mins>0&PLAYERID=="BARRYG") %>% 
  .$TEAMMATCHID

temp <- playerGame %>% 
  filter(mins>0&TEAMMATCHID %in% teamMatches) %>% 
  group_by(PLAYERID,name) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  rename(games=n) %>% 
  ungroup() %>% 
  head(6) %>% 
  
  filter(PLAYERID!="BARRYG") %>%
  select(-PLAYERID) %>% 
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,width=200,options= list(paging = FALSE, searching = FALSE,info=FALSE))



# crosstalk example -------------------------------------------------------
library(countrycode)
  

countries <- countrycode_data

unique(countries$country.name) ## hit some probs eg United Kingdom


## lets initially look at places fee/season/team

## start off with no filtering

sort(names(playerClub))
sort(names(playerGeos))

playerInfo <- playerClub %>% 
  left_join(playerGeos, by=c('PLAYERID'='playerID')) %>% 
  mutate(lng=jitter(lon, amount=0.1),lat=jitter(lat, amount=0.1)) %>% 
  mutate(popup=PLAYERID) %>% 
  select(-lon)

glimpse(playerInfo)

library(crosstalk)
sd <- SharedData$new(playerInfo)

sd %>% 
 # select(lat,lng,popup) %>% 
leaflet() %>% 
  setView(9.998176, 14.531777, zoom = 2) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(popup=~popup)

## this seemed to help

devtools::install_github("rstudio/crosstalk") # was 0.2 worked with choropleth example
devtools::install_github("rstudio/leaflet@joe/feature/crosstalk-filter")

library(DT)



datatable(sd)
# Error in datatable(sd) : 
#   'data' must be 2-dimensional (e.g. data frame or matrix)


devtools::install_github("rstudio/DT@joe/feature/crosstalk")
library(DT)
datatable(sd) # works


devtools::install_github("ropensci/plotly@joe/feature/crosstalk")

library(plotly)
sd %>% 
  plot_ly() %>% 
  add_markers(x=~JOINED, y=~FEE)

library(crosstalk)
library(plotly)

set.seed(100)
sd <- SharedData$new(diamonds[sample(nrow(diamonds), 1000), ])

plot_ly(sd, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat) %>%
  layout(dragmode = "select")

diamonds <- diamonds[sample(nrow(diamonds), 1000), ]

# this works
plot_ly(diamonds, x = ~carat, y = ~price, text = ~paste("Clarity: ", clarity),
        mode = "markers", color = ~carat, size = ~carat) %>%
  layout(dragmode = "select")

plot_ly(sd$transform(as.data.frame), x = ~carat, y = ~price, text = ~paste("Clarity: ", clarity),
        mode = "markers", color = ~carat, size = ~carat) %>%
  layout(dragmode = "select")
# Error: First argument, `data`, must be a data frame. dt had worked above without transformation??
class(sd$transform(as.data.frame))  #SharedData" "R6"  ## same with the gapminer which worked with scatter


Warning in install.packages :
  package ‘d3scatter’ is not available (for R version 3.3.1)

devtools::install_github("jcheng5/d3scatter")

library(d3scatter)
d3scatter(sd$transform(as.data.frame), x = ~gdpPrcp, y = ~lifeExp, color = ~continent, x_label = "Income per person", y_label = "Life expectancy")

# library(crosstalk)
# library(leaflet)
# 
# sd <- SharedData$new(quakes)
# 
# leaflet(sd) %>% addMarkers()
# 
# 
# library(htmltools)
# library(crosstalk)
# library(leaflet)
# library(DT)
# 
# # Wrap data frame in SharedData
# sd <- SharedData$new(quakes)
# 
# # Create a filter input
# filter_slider("mag", "Magnitude", sd, column = ~mag, step = 0.1)
# 
# # Use SharedData like a dataframe with Crosstalk-enabled widgets
# leaflet(sd) %>% addTiles() %>% addMarkers()
# datatable(sd)


# wk5 goalscorers by team -------------------------------------------------

sort(names(playerGame))

temp <- playerGame %>% 
  group_by(name,PLAYERID,TEAMNAME,season) %>% 
  filter(Gls>0) %>% 
  select(name,PLAYERID,TEAMNAME,season) %>% 
  unique() %>% 
  group_by(TEAMNAME,season) %>%
  tally() %>% 
   
  arrange(desc(n)) %>% 
 # filter(season=="2016/17")
  filter(TEAMNAME=="Liverpool")


## wk5 west ham conceding 4 (2x in row) 3 3x - which is more common

## wk5 antonio scoring headers 11/13 % speed etc

sort(names(goals))

sort(names(playerGame))

apps %>% playerGame 
  filter(mins>0)

  ## start here for code
headers <- goals %>% 
  filter(METHOD=="Head") #4967

apps <- playerGame %>% 
filter(mins>0&PLAYERID!="OWNGOAL") %>% 
  select(name,PLAYERID,PLAYER_MATCH,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(PLAYERID) %>% 
  mutate(gameOrder=row_number()) %>% 
  right_join(headers) %>% 
  group_by(PLAYERID) %>% 
  arrange(gameOrder) %>% 
  mutate(headgame=row_number())

# confirm
apps %>% 
  filter(PLAYERID=="ANTONIM")

#issue with NAs
test <-playerGame %>% 
  filter(mins>0&PLAYERID!="OWNGOAL") %>% 
  select(name,PLAYERID,PLAYER_MATCH,gameDate) %>% 
  arrange(gameDate) %>% 
  group_by(PLAYERID) %>% 
  mutate(gameOrder=row_number()) %>% 
  right_join(headers)

## looks odd + one NA needs investigating
# apps %>% 
#   filter(PLAYERID=="CANTONE")

goals %>% 
  filter(METHOD=="Head"&PLAYERID=="OWNGOAL")

# look at those who have scored at least 10 - update to 11

temp <- apps %>% 
  filter(headgame==11)  #85

# biggets
tops <- apps %>% 
  group_by(PLAYERID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% # sheare 46 total took 113 crouch
  filter(!is.na(PLAYERID)) %>% 
  head(10) %>% 
  .$PLAYERID

tops <- c(tops,"ANTONIM")


apps %>% 
  filter(PLAYERID %in% tops)



apps %>% 
  filter(headgame==11&PLAYERID %in% tops) %>% 
  plot_ly() %>% 
 add_markers(x=~gameOrder,y=~name) %>% 
  
  layout(yaxis=list(title="",categoryarray = ~name, categoryorder = "array"),
         xaxis=list(title="",rangemode="tozero"),
         title="PL appearance in which 11th headed goals scored",
         margin=list(l=120)) %>% 
  
  config(displayModeBar = F,showLink = F)



## from this want games played at 11 


## wk5 everton best start for 38 years

## wk5 man u only 3 goal scorers

temp <- playerGame %>% 
  filter(season=="2016/17"&Gls>0) %>% 
  group_by(PLAYERID,TEAMNAME) %>% 
  tally() %>% 
  ungroup() %>% 
  select(TEAMNAME) %>% 
  group_by(TEAMNAME) %>% 
  tally()

## not that special man u have 4


## goals against at least one - do similar to west ham

sort(names(allMatches)) ##?? where did this come from its in updating sql


oppGames <- teamGames %>%
  select(MATCHID,GA=GOALS,OppTeam=TEAMNAME) %>%
  inner_join(teamGames)

allMatches <-oppGames[oppGames$TEAMNAME!=oppGames$OppTeam,]

allMatches <- allMatches %>%
  select(season,team=TEAMNAME,GF=GOALS,GA,gameDate,tmGameOrder,tmYrGameOrder,venue,MATCHID)
allMatches$points <- 3
allMatches[allMatches$GF==allMatches$GA,]$points <- 1
allMatches[allMatches$GF<allMatches$GA,]$points <- 0


## this gives all time wnba 34
df <- allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  filter(value==1) %>% 
  group_by(team) %>% 
  arrange(desc(slength)) %>% 
  slice(1)



## need current teams

currentTeams <- standings %>% 
  filter(season=="2016/17") %>% 
  .$team %>% 
  unique()


#max
max <-allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  group_by(team) %>% 
  filter(value==1) %>% 
  arrange(desc(slength)) %>% 
  slice(1) %>% 
  filter(team %in% currentTeams) %>% 
  select(team,max=slength)

#3 current
df <- allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  group_by(team) %>% 
  slice(1) %>% 
  filter(team %in% currentTeams) %>% 
  inner_join(max) %>% 
  mutate(run=(ifelse(value==1,slength,0))) %>% 
  arrange(desc(run)) %>% 
  select(team,run,max)%>%
 DT::datatable(width=250,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

test <-allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  group_by(team) %>% 
  filter(team=="Stoke C")
#max
max <-allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  group_by(team) %>% 
  filter(value==1) %>% 
  arrange(desc(slength)) %>% 
  slice(1) %>% 
  filter(team %in% currentTeams) %>% 
  select(team,max=slength)


stoke at 15 only Swindon and Leeds have longer runs do check sunderland

## histogram
df <-allMatches %>% 
  mutate(cat=ifelse(GA>0,1,0)) %>% 
  arrange(desc(gameDate)) %>% 
  group_by(team) %>% 
  do(subSeq(.$cat)) %>% 
  filter(team=="Stoke C"&value==1) %>% 
  group_by(slength) %>% 
  tally() 

df %>%   plot_ly() %>% 
  add_bars(data=df,x= ~slength, y= ~n, opacity=0.5) %>% 
  add_bars(data=tail(df,1),x= ~slength, y= ~n,color="red") %>% 
    layout()
  
## look also at 3 and 4 goals against
  
  ## 3
  df <- allMatches %>% 
    mutate(cat=ifelse(GA>2,1,0)) %>% 
    arrange(desc(gameDate)) %>% 
    group_by(team) %>% 
    do(subSeq(.$cat)) %>% 
    filter(value==1&slength>2) %>% #66
    group_by(team) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
   #filter(n>2) %>% 
    rename(count=n) %>% 
 DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  
  
  ##4
  
  allMatches %>% 
    mutate(cat=ifelse(GA>3,1,0)) %>% 
    arrange(desc(gameDate)) %>% 
    group_by(team) %>% 
    do(subSeq(.$cat)) %>% 
    filter(value==1&slength>1) %>% #54
    group_by(team) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>2) %>% 
    rename(count=n) %>% 
    DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
    group_by(team) %>% 
    slice(1)

 

#wk5 dbl digit gd after 5 games

sort(names(standings))

standings %>% 
  filter(tmYrGameOrder==5) %>% 
  arrange(cumGD) %>% 
  filter(cumGD<(-10)) %>% 
  select(team,season,GD=cumGD,finished=final_Pos) %>% 
  mutate(finished=ifelse(team=="Stoke C",NA,finished))%>%
DT::datatable(width=300,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


# wk6 arsenal not scoring v chelsea run -----------------------------------


## av age first 5 games/ man u mnc?

sort(names(playerGame))

sort(names(teamGames))

teams <- c("Man. City","Man. Utd.")

matches <- teamGames %>% 
  filter(season>"2014/15"&tmYrGameOrder<6&TEAMNAME %in% teams) %>% 
  .$TEAMMATCHID

unique(playerGame$TEAMNAME)

teams <- c("Man. City","Man. Utd.")
temp <-playerGame %>% 
  filter(season>"2014/15"&TEAMMATCHID %in% matches&START>0) %>% 
  select(TEAMNAME,season,age) %>% 
  group_by(TEAMNAME,season) %>% 
  summarize(avAge=mean(age,na.rm=T))
  

# Runs of cumulative goals eg have Palace ever scored 7 in consec  --------

sort(names(teamGames))

teamGames %>% 
  arrange(gameDate) %>% 
  select(TEAMNAME,GOALS,gameDate) %>% 
  mutate(lag1=lag(GOALS),sumGoals=GOALS+lag1) %>% 
  filter(TEAMNAME=="Crystal P") %>% 
  arrange(desc(sumGoals))
# first time in 

# 1  1994/95 Crystal P     3 1994-11-05     4        7
# 2  2014/15 Crystal P     4 2015-04-11     2        6


teamGames %>% 
  arrange(gameDate) %>% 
  select(TEAMNAME,GOALS,gameDate) %>% 
  mutate(lag1=lag(GOALS),lag2=lag(GOALS,2),sumGoals=GOALS+lag1+lag2) %>% 
  filter(TEAMNAME=="Crystal P") %>% 
  arrange(desc(sumGoals))

# teams with best and worst starts to PL this season ----------------------

sort(names(standings))

current_teams <- standings %>% 
  filter(season=="2016/17") %>% 
  .$team %>% 
  unique()

current_standings <- standings %>% 
  ungroup() %>% 
  filter(season=="2016/17"&tmYrGameOrder==5) %>% 
  select(team,position)

minMax_standings <- standings %>% 
  filter(team %in% current_teams&tmYrGameOrder==5) %>% 
  group_by(team) %>% 
  summarize(best=min(position),worst=max(position)) %>% 
  select(team,best,worst) %>% 
  left_join(current_standings)
  
 #  table though that dot plot might be best
  best <- minMax_standings %>% 
    filter(best==position) %>% 
    select(team,position)
  
  worst <- minMax_standings %>% 
    filter(worst==position) %>% 
    select(team,position)
  
  
  
  
  
    
 

# games lost after so many games ------------------------------------------

#West ham lost 5th game after---
  
  sort(names(standings))
  
  unique(standings$team)
  
  standings %>% 
    ungroup() %>% 
    arrange(gameDate) %>% 
    select(res,season,team,tmYrGameOrder) %>% 
    mutate(loss=ifelse(res=="Loss",1,0)) %>% 
    group_by(season,team) %>% 
      mutate(cumLoss=cumsum(loss)) %>% 
      filter(team=="West Ham U"&cumLoss==5) %>% 
      group_by(season) %>% 
      slice(1) %>% 
      plot_ly %>% 
    add_markers(x=~tmYrGameOrder,y=~season) %>% 
    layout(title="West Ham - games played when fifth loss suffered",
           xaxis=list(title="Games Played", rangemode = "tozero"),
           yaxis=list(title=""))
  
  
  
  
    

# % GOALS FROM DEFOE SINCE JOINED -----------------------------------------

sort(names(playerGame))

  # joined 2015-01-17
temp <-  playerGame %>% 
    filter(TEAMNAME=="Sunderland"&PLAYERID=="DEFOEJ")

## individ player goals


pl <- playerGame %>% 
  filter(gameDate>="2015-01-17") %>% 
  group_by(PLAYERID,TEAMNAME) %>% 
  summarize(totGls=sum(Gls)) 

li

tm <- playerGame %>% 
  ungroup() %>% 
  filter(gameDate>="2015-01-17"&Gls>0) %>% 
  group_by(TEAMNAME) %>% 
  summarize(tmGls=sum(Gls))

library(profvis)  # takes an awfully long time as.Posixct??
profvis(
playerGame %>% 
  filter(Gls>0) %>% 
  filter(gameDate>="2015-01-17") %>% ## this takes all time
  group_by(name,PLAYERID,TEAMNAME) %>% 
  summarize(totGls=sum(Gls)) %>% 
  left_join(tm) %>% 
  mutate(pc=round(100*totGls/tmGls,1)) %>% 
  arrange(desc(pc)) %>%
  ungroup() %>% 
  select(name,team=TEAMNAME,goals=totGls,team=tmGls,pc) %>% 
                         DT::datatable(width=400,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
)
system.time(
playerGame %>% 
  filter(Gls>0) %>% 
  filter(gameDate>="2015-01-17")
)

system.time(
  playerGame %>% 
    filter(Gls>0&gameDate>=as.Date("2015-01-17")) 
     )

playerGame %>% 
  filter(Gls>0) %>% 
  filter(gameDate>="2015-01-17")
)

class(playerGame$Gls)
class(playerGame$gameDate)

# accounting for goals scored by over 30s ---------------------------------

#defoe now in top 30 with 31 but missed 1.5? years in MLS

oldies <-playerGame %>% 
ungroup() %>% 
  filter(Gls>0&age>32.2793748574036) %>% 
  group_by(PLAYERID) %>% 
  summarize(tot=sum(Gls)) %>% 
  arrange(desc(tot))



# wk6 arsenal chelsea - perhapsmcumulative pt diff as a sparkline? --------

sort(names(hth))

library(sparkline)

data <-hth %>% 
  filter(team=="Chelsea"&OppTeam=="Arsenal") %>% 
  arrange(gameDate) %>% 
  mutate(GD=ifelse((GF==GA),0.1,GF-GA)) 

sparkline(
  data$GD,
  type="bar",
  width=180,height=60)


  
)

## prob with shiny

library(sparkline)
library(dplyr)

data <- data.frame(colA=c('a','a','a','b','b','b'),colB=c(1:6))

data %>% 
  filter(colA=="b") %>% 
  .$colB %>% 
  sparkline(type="bar")

## this works
library(shiny)

app <- shinyApp(
  ui = fluidPage(
    data <- data.frame(colA=c('a','a','a','b','b','b'),colB=c(1,2,6,3,5,4)),
    selectInput("choice", "choose", c("a","b")),
    sparklineOutput("plot")
  ),
  server = function(input, output) {
    output$plot <- renderSparkline({
      
      data %>% 
        filter(colA==input$choice) %>% 
        .$colB %>% 
        sparkline(type="bar")
      
    })
  }
)
runApp(app)



# CLEAN SHEETS BY TOP CLUB AFTER 7 GAMES ----------------------------------
sort(names(standings))

top <- standings %>% 
  filter(tmYrGameOrder==7&position==1) %>% 
  select(season,team)

temp <- standings %>% 
  filter(tmYrGameOrder<=7) %>% 
  inner_join(top)  %>% 
  mutate(cs=ifelse(GA==0,1,0)) %>% 
  group_by(season,team) %>% 
  summarize(tot=sum(cs)) %>% 
  arrange(tot)
  

# MNAU NOT BEATEN BOTTOM TEMA AT HOME -------------------------------------

## actually 19th - bit like one at bottom




# goals from corners ------------------------------------------------------

## did summary of man u



# walcott by month --------------------------------------------------------

library(lubridate)

sort(names(playerGame))

walcott <- playerGame %>% 
  filter(PLAYERID=="WALCOTT"&mins>0) %>% 
  select(gameDate,Gls,Assists,plYrGameOrderApp,season,mins) %>% 
  mutate(points=Gls+Assists, month=month(gameDate)) %>% 
  group_by(month) %>% 
  summarize(ppg=sum(points)*90/sum(mins),totpoints=sum(points)) %>% 
  plot_ly() %>% 
  add_markers(x=~month,y= ~ppg)


## look at adding trace for current season

playerGame %>% 
  filter(PLAYERID=="WALCOTT"&mins>0&season=="2016/17") %>% 
  select(gameDate,Gls,Assists,plYrGameOrderApp,season,mins) %>% 
  mutate(points=Gls+Assists, month=month(gameDate)) %>% 
  group_by(month) %>% 
  summarize(ppg=sum(points)*90/sum(mins),totpoints=sum(points)) %>% 
  plot_ly() %>% 
  add_markers(x=~month,y= ~ppg)

# stones clean sheets -----------------------------------------------------

sort(names(playerGame))
sort(names(standings))

stones <- playerGame %>% 
  filter(PLAYERID=="STONESJ"&START>0) %>% 
  select(team=TEAMNAME,MATCHID) 

sort(names(stones))  
   


 df <- standings %>% 
    
  select(team,OppTeam,gameDate,GA,MATCHID) %>% 
  unique() %>% 
    right_join(stones)
 
# ?? 2 in 17
 
 

# something on jon moss ---------------------------------------------------

sort(names(playerGame))
 
test <-
   playerGame %>% 
   group_by(MATCHID) %>% 
  filter(season=="2016/17") %>% 
   select(MATCHID,CARD) 

# table(test$CARD)
# 
# test[is.na(test$CARD),]
# 
# 
# test[is.na(test$CARD<"ZZ"),]$CARD <- "A"
# 
# table(test$CARD)
# 
# test %>% 
#   filter(is.null(CARD))

cardsByGame <-test %>% 
  mutate(CARD=ifelse(is.na(CARD),"  ",CARD)) %>% 
   mutate(red=ifelse(CARD=="P"|CARD=="R",1,0),yellow=ifelse(CARD=="Y",1,0)) %>% 
    summarize(reds=sum(red,na.rm=0),yellows=sum(yellow,na.rm=0),tot=reds+yellows) 



# df <-teamGames %>% 
#   filter(season=="2016/17") %>% 
#   left_join(cardsByGame) %>% 
#   group_by(REFEREE) %>% 
#   summarize(games=n(),R=sum(reds),Y=sum(yellows),all=sum(tot),pergame=round(all/games,1)) %>% 
#   arrange(desc(pergame))

teamGames %>% 
  filter(season=="2016/17") %>% 
  left_join(cardsByGame) %>% 
  group_by(REFEREE) %>% 
  plot_ly() %>% 
  add_boxplot(y= ~tot, x= ~REFEREE) %>% 
  layout(
    title= "Cards issued by Game by Referee",
    xaxis=list(title=""),
    yaxis=list(title="Count"),
    margin=list(r=70,b=100))
  )

# temp <-playerGame %>% 
#   filter(MATCHID==9627) %>% 
#   mutate(CARD=ifelse(is.na(CARD),"  ",CARD)) %>% 
#   select(LASTNAME,CARD)  #cabaye is na but not sue why
# 
#   
#   sum(df$games) #70 correct
#   
#   df %>% 
#     group_by(REFEREE) %>% 
#     mutate(cards=sum(R+Y)) %>% 
#     plot_ly() %>% 
#      add_bars(x=~REFEREE,y=~cards/games)
#   
#   
#   ### look at boxplot
#   
#   p <- plot_ly(diamonds, y = ~price, color = I("black"), 
#                alpha = 0.1, boxpoints = "suspectedoutliers")
#   p1 <- p %>% add_boxplot(x = "Overall")
#   p2 <- p %>% add_boxplot(x = ~cut)
#   subplot(
#     p1, p2, shareY = TRUE,
#     widths = c(0.2, 0.8), margin = 0
#   ) %>% hide_legend()
  

 plot_ly(df,y= ~ all,color = I("black"), 
                        alpha = 0.1, boxpoints = "suspectedoutliers") %>% 
   add_boxplot(x = ~ REFEREE)
  

# manager casualty --------------------------------------------------------



# matches between 1st and 2nd ------------------------------------------

  ## bit iffy at beginning of season
  
sort(names(standings))
  
  ty <- standings %>% 
    filter(season=="2016/17") #140 obs
  
 temp <- ty %>% 
    select(final_Pos,position,team,tmYrGameOrder,venue,MATCHID,res) %>% 
    arrange(tmYrGameOrder) %>% 
    group_by(team)  %>% 
      mutate(priorPos=lag(position)) %>% 
   filter(!is.na(priorPos))  #120 obs
 
x <- temp %>% 
   inner_join(temp,by=c("MATCHID"="MATCHID")) %>% 
  ungroup() %>% 
   filter(team.x!=team.y)
  
topTwo <-
  x %>% 
  filter((priorPos.x==1&priorPos.y==2)|(priorPos.x==1&priorPos.y==2))  ## looks good # 1 to date


## look over PLhistory

temp <- standings %>% 
  select(final_Pos,position,team,tmYrGameOrder,venue,MATCHID,res,season,gameDate,GF,GA) %>% 
  arrange(tmYrGameOrder) %>% 
  group_by(team,season)  %>% 
  mutate(priorPos=lag(position)) %>% 
  filter(!is.na(priorPos)) #18366

x <- temp %>% 
  inner_join(temp,by=c("MATCHID"="MATCHID")) %>% 
  ungroup() %>% 
  filter(team.x!=team.y)

topTwo <-
  x %>% 
  filter((priorPos.x==1&priorPos.y==2)|(priorPos.x==2&priorPos.y==1)) #51 did have dupes before
  
  table(topTwo$res.x)
  
  topTwo %>% 
    arrange(desc(gameDate.x)) %>% 
    select(season=season.x,round=tmYrGameOrder.x,top=team.x,venue=venue.x,GF=GF.x,GA=GA.x,second=team.y)%>%
    DT::datatable(width=400,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
  
  
  
  
# goals for by type -------------------------------------------------------

#in code
  output$teamGoalsFor <- DT::renderDataTable({
    print("gtf")
    df <- Goals_team_for[Goals_team_for$team==input$team_3,]
    df <- df[,c(-2)]
    df <-  df %>%
      arrange(desc(season))
    
    #DT::datatable(df,rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE), container = GF_format)
    
    DT::datatable(df,class='compact stripe hover row-border',rownames=FALSE,options= list(paging = FALSE, searching = FALSE, info=FALSE))
    
  })
  
  
  Goals_team_for %>% 
    filter(team=="Man. Utd.") %>% 
    arrange(desc(season)) %>%
    select(-team) %>% 
    DT::datatable(class='compact stripe hover row-border',rownames=FALSE,options= list(paging = TRUE, searching = FALSE, info=FALSE))
  
  
  
  
  
# TEAMS TO HAVE SCORED 6 --------------------------------------------------

sort(names(standings))
  
 temp <- standings %>% 
    filter(GF>5) %>% 
    group_by(team) %>% 
    select(tmGameOrder)  %>% 
    summarize(count=n(), first=min(tmGameOrder)) %>% 
    arrange(desc(count))  # bournemouth 24th
 
 unique(standings$team) #47
 
 totGames <- standings %>% 
   group_by(team) %>% 
   tally() %>% 
   left_join(temp) %>% 
   mutate(count=ifelse(is.na(count),0,count))
 
 ## good for chart
 standings %>% 
   filter(GF>5) %>% 
   arrange(tmGameOrder) %>% 
   group_by(team) %>% 
   select(tmGameOrder,team,gameDate,OppTeam,GF,GA)  %>% 
   
   mutate(order=row_number()) %>% 
   group_by(team) %>% 
   plot_ly() %>% 
   add_markers(~tmGameOrder,~order,color=~team) %>% 
   add_lines(~tmGameOrder,~order,color=~team,
             line = list(shape = "vhv"),
             hoverinfo = "text",
             text =  ~ paste("Game ",tmGameOrder,
                             "<br>",
                             gameDate,
                             "<br>",
                             team,
                             
               "<br>v ",
               OppTeam,
               "<br>",
               GF,
               "-",
               GA
             )
   )  %>%
   layout(
     hovermode = "closest")
  # by gamedate looks better    ?? but why no margin        
 standings %>% 
   filter(GF>5) %>% 
   arrange(tmGameOrder) %>% 
   group_by(team) %>% 
   select(tmGameOrder,team,gameDate,OppTeam,GF,GA)  %>% 
   
   mutate(order=row_number()) %>% 
   group_by(team) %>% 
   plot_ly() %>% 
   add_markers(~gameDate,~order,color=~team) %>% 
   add_lines(~gameDate,~order,color=~team, showlegend=FALSE,
             line = list(shape = "vhv"),
             hoverinfo = "text",
             text =  ~ paste("Game ",tmGameOrder,
                             "<br>",
                             gameDate,
                             "<br>",
                             team,
                             
                             "<br>v ",
                             OppTeam,
                             "<br>",
                             GF,
                             "-",
                             GA
             )
   )  %>%
   layout(height= 600, autosize=FALSE,width='50%',
     hovermode = "closest",
     title="Teams scoring 6 or more Goals in a Premier League game",
     xaxis=list(title=""),
     yaxis=list(title="Count")
   )
 
 
test <- standings %>% 
   filter(GF>5) %>% 
   arrange(desc(gameDate))

#26 teams have score 6 goals or more in PL since MAn Utd did it

## bournemouth goals

# Hull 17 goals conceded  in 4 game 9check not similar before) ------------


# SUN not one any first 8 two years in row (how many got relegated) ----------------------------------------
 #3 Bor hul 819th diff match up

sort(names(standings))

standings %>% 
  select(team,OppTeam) %>% 
  unique()  #1638


# example table
temp <- standings %>% 
  select(team,OppTeam) %>% 
   mutate(combo=ifelse(team<OppTeam,paste0(team,' v ',OppTeam),paste0(OppTeam,' v ',team))) %>% 
  select(combo) %>% 
  group_by(combo) %>% 
  tally() %>% 
  mutate(count=n/2) %>% 
  select(matchup=combo,count) %>% 
  arrange(count) 

temp %>%
                         DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=TRUE))

## looks good
temp %>% 
plot_ly() %>% 
  add_histogram(~count,nbinsx=50)

## liverpool now played all ever presents rather than everton 49 times

sort(names(standings))

## poss do +/- points with result as rug


sort(names(hth))






# southampton conceding time ----------------------------------------------


# burnley time taken to score away goal + 4wins in 40 in total ------------


# austin scoring rate -----------------------------------------------------


# number of saints players since they came up -----------------------------

sort(names(playerGame))


temp <- playerGame %>% 
  filter(season>"2011/12"&mins>0) %>% 
  group_by(TEAMNAME,PLAYERID) %>% 
  tally() %>% 
  ungroup() %>% 
  #filter(n>7) %>% ## makes quite diff - worth looking at
  group_by(TEAMNAME) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

# 1   Sunderland    84
# 2    Liverpool    70
# 3   West Ham U    69
# 4    West Brom    66
# 5      Chelsea    65
# 6  Aston Villa    62
# 7    Man. Utd.    61
# 8    Crystal P    59
# 9  Newcastle U    59
# 10 Southampton    59
## sunderland have used 84 players


playerGame %>% 
  filter(season>"2011/12"&mins>0) %>% 
  group_by(TEAMNAME,PLAYERID) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(TEAMNAME) %>% 
  filter(n)
  summarize(count=n()) %>% 
  arrange(desc(count))
  
  
  
  
  
# junior stanislas 10 asists and goals same game littlwe tricky--------------------------

temp <- playerGame %>% 
    arrange(gameDate) %>% 
    group_by(PLAYERID) %>% 
    filter(mins>0) %>% 
    select(PLAYERID,Gls,Assists) %>% 
    mutate(gameOrder=row_number(),cumGls=cumsum(Gls),cumAss=cumsum(Assists)) %>% 
    filter(cumGls==10)
  
  
  
  
  
# RODWELL never wiiner as starter -----------------------------------------


  
  
  
  
# goals by round - number of noc=score draws ------------------------------

  ## prob as with above is that tmYrGameOrder is not absolutley consistent
  
  17
  Sunderland
  Arsenal
  0
  0
  2016-04-24
  566
  34
  H
  9527
  1
  39
  57
  31
  -18
  38
  17
  Draw
  <table cellpadding='4' style='line-height:1'><tr> <th>Arsenal (H)</th></tr> <tr align='center'><td>2016-04-24</td></tr> <tr align='center'><td>0-0</td></tr> <tr align='center'><td>Pos: 17</td></tr> </table>
    1
  1600
  2015/16
  2
  Arsenal
  Sunderland
  0
  0
  2016-04-24
  921
  35
  A
  
  
  
  sort(names(standings))
  
temp <-  standings %>% 
    arrange(season,tmYrGameOrder) %>% 
    group_by(season,tmYrGameOrder) %>%
    #mutate(gameOrder=paste0(season,tmYrGameOrder)) %>% 
   # group_by(gameOrder) %>% 
  mutate(noscore=ifelse((GF==0&GA==0),1,0))

--


temp <-  temp %>% 
  filter(noscore==1)

%>% 
    summarize(totgls=sum(GF),ns=sum(noscore)) %>% 
  ungroup() %>% 
      mutate(gameOrder=row_number())

temp %>% 
  filter(season>"2014/15") %>% 
    plot_ly() %>% 
      add_bars(x=~gameOrder,y=~totgls)
  
  #13 gls before Subnday games - lowest 20 this far

# 16 in game 12 last year 
## x2 only 12 goals

## nil-nil draws
  

# stones conceding --------------------------------------------------------

sort(names(playerGame))

playerGame %>% 
  filter(PLAYERID=="STONESJ"&START>0) %>% 
  arrange(desc(gameDate)) %>% 
  inner_join(standings) %>% 
  select(GA,team,gameDate)
  
  
  
# mandanda letting goal in ------------------------------------------------



# coutinho goals outside area particularly from open play-------------------------------------------

outsideOpen <- goals %>% 
  filter(PLACE=="Long_Range"&PLAY=="Open") %>%  #2534
  left_join(playerGame)

outsideOpen %>% 
  filter(PLAYERID=="COUTINP") %>% 
  arrange(gameDate) %>% 
  select(gameDate,plGameOrderApp) #13th 2013-05-19 tot 11

# total 

outsideOpen %>% 
  group_by(PLAYERID) %>% 
  tally() %>% 
  arrange(desc(n))  #LAMPARD 23

outsideOpen %>% 
  filter(gameDate>="2013-05-19") %>% 
  group_by(PLAYERID,LASTNAME) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
    ungroup %>% 
  select(player=LASTNAME,goals=n) %>% 
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))






# southampton goals against -----------------------------------------------



# goals conceded in first minute ------------------------------------------

#man u vs games played in

sort(names(goals))
# [1] "GOALS"             "METHOD"            "PLACE"             "PLAY"              "PLAYER_MATCH"     
# [6] "PLAYER_MATCH_GOAL" "TEAMMATCHID"       "TIME" 

a <- goals %>% 
  filter(TIME==1) %>% 
  select(TEAMMATCHID) #120 out of 24938

sort(names(teamGames))

b <- a %>% 
  inner_join(teamGames) %>% 
  select(TEAMMATCHID,MATCHID)

c <- b %>% 
  inner_join(teamGames, by="MATCHID") %>% 
  filter(TEAMMATCHID.x!=TEAMMATCHID.y)


d <-c %>% 
  group_by(TEAMNAME) %>% 
  tally()

totGames <- teamGames %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
  rename(count=n) %>% 
  left_join(d) %>% 
  mutate(n=ifelse(is.na(n),0,n)) %>% 
  plot_ly() %>% 
  add_markers(~ count, ~jitter(n),
              hoverinfo = "text",
              text =  ~ paste0(TEAMNAME, "<br>Games: ", count, "<br>Count: ", n)
  ) %>%
  layout(
    hovermode = "closest",
  #  height = 250,
  #  autosize = F,
    title = "Times conceding in First Minute",
    xaxis = list(title = "Games played"),
    yaxis = list(title = "Count")
  ) %>%
  
  config(displayModeBar = F, showLink = F)

#120*100/24938
  

# Burnley record at Man U -------------------------------------------------

library(engsoccerdata)
library(tibble)
library(tidyverse)

df <- tbl_df(england)

sort(unique(df$home)) # "Manchester United" "Burnley"

library(doBy)


## this works Burnley 39 games since shutout 84 obs (will exclude those who scored on last visir
#- can obv extend to any other categories eg since win)
manHome <- df %>% 
  filter(home=="Manchester United") %>% 
  arrange(desc(Season)) %>% 
  group_by(visitor) %>% 
  mutate(shutout=ifelse(hgoal==0,1,0)) %>% 
 # filter(visitor=="Burnley") %>% 
  do(subSeq(.$shutout)) %>%
  ungroup() %>% 
  group_by(visitor) %>% 
  slice(1) 

#3 will need to look at updating - need to get engsoccerdata right

## 12011 takes a while (maybe need to do in update )
temp <- df %>% 
 # filter(home=="Manchester United") %>% 
  arrange(desc(Season)) %>% 
  group_by(home,visitor) %>% 
  mutate(shutout=ifelse(hgoal==0,1,0)) %>% 
  # filter(visitor=="Burnley") %>% 
  do(subSeq(.$shutout)) %>%
  ungroup() %>% 
  group_by(home,visitor) %>% 
  slice(1) 


## check looks good
temp %>% 
  filter(home=="Manchester United")

temp %>% 
  arrange(desc(slength)) %>% 
  group_by(home) %>% 
  slice(1) %>% 
  arrange(desc(slength)) %>% 
  select(home,away=visitor,games=slength)%>%
                         DT::datatable(width=470,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

#1         Blackburn Rovers   Birmingham City     1    57      57       29     0

test <- df %>% 
  filter(home=="Blackburn Rovers"&visitor=="Birmingham City")




# lukaku scoring all 7 games v west ham + 2 in cups -----------------------


# wilshere yet to record goal/assist ( need to do a minute thing)

## general scoring an assist by player (eg sterling)

sort(names(playerGame))

## make sure to use summarise not summarize
sterling <-playerGame %>% 
  filter(mins>0) %>% 
  group_by(PLAYERID,name,Opponents) %>% 
  summarise(sumMins=sum(mins),sumAssists=sum(Assists),sumGoals=sum(Gls),points=sumAssists+sumGoals) %>% 
  filter(PLAYERID=="STERLIR")
  # could also look at position when playing


# tot h fewest goals against after n games ----------------------------------


## sunderland start

sort(names(standings))

sunderland <-standings %>% 
  filter(team=="Sunderland"&tmYrGameOrder<11&season!="2016/17") %>% 
  ungroup()

sunderland2016 <-standings %>% 
  filter(team=="Sunderland"&tmYrGameOrder<11&season=="2016/17") %>% 
  ungroup()

others <-standings %>% 
  filter(team!="Sunderland"&tmYrGameOrder<11) %>% 
  ungroup()

sunderland %>% 
  group_by(season,team) %>% 
  plot_ly() %>% 
  add_lines(x= ~tmYrGameOrder, y=~cumPts) %>% 
  add_lines(data =sunderland2016, x= ~tmYrGameOrder, y=~cumPts, color="red", name="2016/17") %>% 
  add_lines(data =others, x= ~tmYrGameOrder, y=~cumPts, color="grey")


others %>% 
  group_by(season,team) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~cumPts) %>% 
  add_lines(color=I("grey")) %>% 
  add_lines(data =sunderland2016, x= ~tmYrGameOrder, y=~cumPts, color="red", name="2016/17") %>% 
  add_lines(data =others, x= ~tmYrGameOrder, y=~cumPts)


sort(names(others))
#OK
others %>% 
 # filter(season=="2016/17") %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~jitter(cumPts)) %>% 
  add_lines(color=I("grey"))


sunderland %>% 
  # filter(season=="2016/17") %>% 
  group_by(season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~jitter(cumPts)) %>% 
  add_lines(color=I("blue"))


others %>% 
  # filter(season=="2016/17") %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~jitter(cumPts)) %>% 
  add_lines(color=I("grey")) %>% 
  add_lines(x=sunderland$tmYrGameOrder,y=sunderland$cumPts,color=I("blue"))

others %>% 
  # filter(season=="2016/17") %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~jitter(cumPts)) %>% 
  add_lines(color=I("grey")) %>% 
  add_lines(x= ~sunderland$tmYrGameOrder,y=~sunderland$cumPts,color=I("blue"))


## look at setting color 

df <- standings %>% 
  filter(tmYrGameOrder<11) %>% 
  select(season,team,tmYrGameOrder,cumPts) %>% 
  mutate(lineColor=ifelse(team=="Sunderland","red","blue")) %>% 
  mutate(lineColor=ifelse(team=="Sunderland"&season=="2016/17","grey",lineColor)) 

sort(names(df))

df %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~cumPts) %>% 
  add_lines(color= ~lineColor)



standings %>% 
  filter(tmYrGameOrder<11) %>% 
  select(season,team,tmYrGameOrder,cumPts) %>% 
  mutate(lineColor=ifelse(team=="Sunderland","red","blue")) %>% 
  mutate(lineColor=ifelse(team=="Sunderland"&season=="2016/17","grey",lineColor)) %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~cumPts) %>% 
  add_lines(color= ~lineColor) %>%
  layout(
    xaxis=list(title="Games Played"),
    yaxis=list("Points")
  ) %>% 
  
  config(displayModeBar = F,showLink = F)

df <- read_csv("problem.csv")
df %>%
  ungroup() %>% 
  group_by(team, season) %>%
  plot_ly(x = ~ tmYrGameOrder, y =  ~ cumPts) %>%
  add_lines(color = ~ lineColor)  %>%
  layout(showlegend=FALSE,
    xaxis=list(title="Games Played"),
    yaxis=list(title="Points") ) %>% 
  config(displayModeBar = F,showLink = F)

  # group_by(team,season) %>% 
  # plot_ly(x= ~tmYrGameOrder, y=~cumPts) %>% 
  # add_lines(color= ~lineColor)

test <-df %>% 
  filter(tmYrGameOrder==10)

## toy example

library(plotly)
library(dplyr)
df <- tibble(team=c("A","A","A","A","B","B","B","B"),
             season=c(2000,2000,2001,2001,2000,2000,2001,2001),
             game=c(1,2,1,2,1,2,1,2),
             points=c(0,3,1,1,1,2,0,1),
             theColor=c("red","red","red","red","grey","grey","blue","blue"))

df %>%
#  arrange(desc(theColor)) %>% 
 group_by(team,season) %>%
  plot_ly(x=~game,y=~points,color= ~factor(theColor)) %>%  
       #  colors = c("red", "grey", "blue")) %>% 
  add_lines() %>% 
  layout(showlegend=FALSE)


# http://stackoverflow.com/questions/35601464/custom-colors-in-plotly updated for v4+


df <- data.frame(x = rep(LETTERS[1:5], 3), 
                 y = rexp(15, rate = 0.5),
                 z = c(rep("Adam", 5), rep("Arthur", 5), rep("Ford", 5)))  # these will be factors
#df <- arrange(df, desc(z))

plot_ly(df, 
        x = ~x, 
        y = ~y, 
        color = ~z, 
        colors = c("grey50", "blue", "red"), 
        type = "bar") %>% 
  layout(barmode = "stack")

# 
# ## this does produce result thogh not ideal
# cols <- c("red", "blue", "black")
# 
# df %>% 
#   group_by(team,season) %>% 
#   plot_ly(x=~game,y=~points,color=~theColor, colors=cols) %>% 
#   add_lines()

cols <- c("red", "blue", "black")

df %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~cumPts,color= ~lineColor,cols <- c("red", "blue", "black")) %>% 
  add_lines(alpha=0.5) %>% 
  layout(showlegend=F)

library(RColorBrewer)
df %>% 
  group_by(team,season) %>% 
  plot_ly(x= ~tmYrGameOrder, y=~jitter(cumPts),marker = list(color = brewer.pal(6, "Paired"))) %>% 
  add_lines()
marker = list(color = brewer.pal(6, "Paired"))

## no idea where grp comes from
basePlot <-  ggplot(others, aes(tmYrGameOrder, cumPts, group=season, color=grp))  +
  geom_line(aes(group=season, color=factor(grp))) +
  geom_line(data=df_prem, aes(gameOrder, cumG, group=Season, color=factor(grp))) +  
  geom_line(data=df_2016, aes(gameOrder, cumG, group=Season, color=factor(grp)), lwd=1.1)

#3 has issues of group by more than one category
basePlot <-  ggplot(others, aes(tmYrGameOrder, cumPts, group=season))  +
  geom_line(aes(group=season)) +
  geom_line(data=df_prem, aes(gameOrder, cumG, group=Season, color=factor(grp))) +  
  geom_line(data=df_2016, aes(gameOrder, cumG, group=Season, color=factor(grp)), lwd=1.1)


basePlot +
  
  xlab("Games Played") + ylab("Cumulative Goals") +
  scale_color_manual(values=c("gray80","#F9966B" , "blue")) +
  scale_x_continuous(breaks=seq(from=1,to=42, by=5)) +
  labs(title=theTitle) +
  theme(
    plot.title = element_text(hjust=0,vjust=1, size=rel(1.7)),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="gray65"),
    panel.grid.major.x = element_line(color="gray65"),
    panel.grid.minor = element_blank(),
    plot.background  = element_blank(),
    text = element_text(color="gray20", size=10),
    axis.text = element_text(size=rel(1.0)),
    axis.text.x = element_text(color="gray20",size=rel(1.5)),
    axis.text.y = element_text(color="gray20", size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5), vjust=0),
    axis.title.y = element_text(size=rel(1.5), vjust=1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
  


## united points in last n games



# liverpool 5 players with 7 points already -------------------------------


sort(names(playerGame))

playerGame %>% 
  filter(season=="2016/17") %>% 
  select(TEAMNAME,PLAYERID,name,Gls,Assists) %>% 
  mutate(points=Gls+Assists) %>% 
  group_by(TEAMNAME,name) %>% 
  summarise(totpoints=sum(points)) %>% 
  ungroup() %>% 
  filter(totpoints>6) %>% 
  group_by(TEAMNAME) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  rename(count=n,team=TEAMNAME) %>% 
                         DT::datatable(width=200,class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))

  # library(Lahman)
  # if (require("Lahman")) {
  #   batting_tbl <- tbl_df(Batting)
  #   tally(group_by(batting_tbl, yearID))
  #   tally(group_by(batting_tbl, yearID), sort = TRUE)
  #   
  #   # Multiple tallys progressively roll up the groups
  #   plays_by_year <- tally(group_by(batting_tbl, playerID, stint), sort = TRUE)
  #   tally(plays_by_year, sort = TRUE)
  #   tally(tally(plays_by_year))
  #   
  #   # This looks a little nicer if you use the infix %>% operator
  #   batting_tbl %>% group_by(playerID) %>% tally(sort = TRUE)
  #   
  #   # count is even more succinct - it also does the grouping for you
  #   batting_tbl %>% count(playerID)
  #   batting_tbl %>% count(playerID, wt = G)
  #   batting_tbl %>% count(playerID, wt = G, sort = TRUE)
  # }

## Liv 4 goals for


# Liv 4 goals for ---------------------------------------------------------



sort(names(standings))

temp <- standings %>% 
  group_by(team,season) %>% 
  filter(GF>3) %>% 
  tally()

temp %>% 
  arrange(season) %>% 
  filter(team=="Liverpool") %>% 
  plot_ly(x= ~season, y=~n) %>% 
  add_bars(color = I("red"), alpha=0.7) %>% 
  layout(
    title="Games with Liverpool scoring 4+ goals",
    xaxis=list(title=""),
    yaxis=list(title="Count"),
    margin=list(b=70)
  ) %>% 
  config(displayModeBar = F,showLink = F)


# palace goals for against ------------------------------------------------

sort(names(Goals_team_for))

Goals_team_for %>% 
  filter(season=="2016/17") %>% 
  arrange(desc(Head))
  