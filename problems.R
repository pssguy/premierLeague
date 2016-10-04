
# # standings chart by position SOLVED---------------------------------------------
# 
# df <-  standings %>% 
#   ungroup() %>% 
#   filter(tmYrGameOrder==11&position==1)
# 
# 
# theMin <- min(df$final_Pos) #1
# theMax <- max(df$final_Pos) #6
# df %>% 
#   
#   ggplot(aes(final_Pos)) +
#   geom_histogram(fill="blue",binwidth = 0.5, alpha=0.5, stat="count") + ##ggplot2_1.0.1.9003 Error: Unknown parameters: binwidth, bins, origin, right
#   # scale_x_continuous(breaks=df$final_Pos+0.2, labels=df$final_Pos)
#   # scale_x_continuous(breaks=standings$final_Pos+0.2, labels=standings$final_Pos)
#   scale_x_discrete(breaks=c(theMin:theMax)) +
#   theme_bw() +
#   xlab("Final Position") +
#   ylab("Seasons")
# 
# ## why ggplot2 
# 
# df %>% 
#   ggvis(~final_Pos) %>% 
#   layer_histograms(fill:="lightblue") %>% 
#   add_axis("x", title="Final League Position", format='d') %>% 
#   add_axis("y", title="Seasons")


# Baines too may games in season ------------------------------------------

# sort(names(playerGame))
# 
# 
# playerGame %>% 
#   filter(PLAYERID=="BAINESL") %>% 
#   group_by(gameDate) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))  #1 is max
# 
# playerGame %>% 
#   filter(PLAYERID=="BAINESL"&START>0&subOn>0) %>% 
#   group_by(gameDate) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) #2011-04-16     1 ## looks like add 1 to on column in access rather tha 1 gl 1 penalty
# 
# ## look at others
# 
# 
# playerGame %>% 
#   filter(START>0&subOn>0) %>% 
#   group_by(gameDate,PLAYERID,TEAMNAME,MATCHID) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n))
# 
# 
# # gameDate PLAYERID  TEAMNAME     n
# # (date)    (chr)     (chr) (int)
# # 1 2004-12-05  FREEDMD Crystal P     1
# # 2 2011-04-16  BAINESL   Everton     1
# # 3 2014-04-19  WHITTIP Cardiff C     1
# 
# 
# playerGame %>% 
#   filter(PLAYERID!="OWNGOAL") %>% 
#   group_by(gameDate,PLAYERID,name,MATCHID,TEAMNAME) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   arrange(desc(n)) 
# 
# 
# # gameDate PLAYERID     n
# # (date)    (chr) (int)
# # 1  1997-11-01  JOHANSM     2 # not clear issue
# # 2  1998-03-14   HOWEYS     2
# # 3  1998-05-10  HUGHESR     2
# # 4  2013-02-02  DASILVF     2 # ?
# # 5  2014-12-02   KEANEM     2
# # 6  1992-08-15  ABLETTG     1
# # 
# # gameDate PLAYERID            name MATCHID     n
# # (date)    (chr)           (chr)   (int) (int)
# # 1  1998-03-14   HOWEYS     Steve Howey     306     2 # wrong name
# # 2  1998-05-10  HUGHESR  Richard Hughes     387     2 # wrong name
# # 3  1992-08-15  ABLETTG     Gary Ablett    1317     1
# 
# 
# 
# # dubious goals way to get goalid -----------------------------------------
# 
# test <- goals %>% 
#   left_join(playerGame) %>% 
#   filter(PLAYERID=="TADICD") %>% 
#   select(PLAYER_MATCH_GOAL,PLAYER_MATCH,TIME,gameDate) %>% 
#   arrange(desc(gameDate))
# 
# 
# # checking loading - would like to reduce ---------------------------------
# 
# #library(profvis) required , after every line
# 
# Sys.time() # loading libraries takes 12
# #  library(shiny)
#   # etc
# 
# 
# # zaha yellows ------------------------------------------------------------
# 
# sort(names(playerGame))
# test <-playerGame %>% 
#   filter(PLAYERID=="ZAHAW") %>% 
#   select(gameDate,CARD)
# 
# ### input is correct not sure why calc is wrong neede to do an ungroup
# 
# 
# 
# # player at a glance not showing ------------------------------------------
# 
# data <- reactive({
#   
#   if(is.null(input$playerA)) return()
#   
#   
#   
#   basic <- summary %>%
#     filter(PLAYERID=="ABLETTG")
#   
#   teams <- length(unique(basic$TEAMNAME)) # 6
#   seasons <- length(unique(basic$season))
#   
#   
#   bySeason <- summary %>%
#     filter(PLAYERID=="ABLETTG") %>% 
#     ungroup() %>% 
#     group_by(season) %>% 
#     summarize(apps=sum(St+On), goals=sum(StGls+subGls),cards=sum(Y+R),assists=sum(Assists))
#   
#   
#   
#   max <-bySeason%>%
#     summarize(maxApps=max(apps),maxCards=max(cards),maxGoals=max(goals), maxAssists=max(assists))
#   
#   tot <-bySeason%>%
#     summarize(apps=sum(apps),goals=sum(goals),cards=sum(cards),assists=sum(assists)) 
#   
#   career <- cbind(max,tot)  %>% 
#     mutate(showApps=paste(apps,"-",maxApps),
#            showCards=paste(cards,"-",maxCards),
#            showGoals=paste(goals,"-",maxGoals),
#            showAssists=paste(assists,"-",maxAssists))
#   
#   
#   
#   
#   
#   
#   
#   info=list(teams=teams,seasons=seasons,career=career)
#   return(info)
#   
# })
# 
# output$appsBox <- renderInfoBox({
#   infoBox(
#     "Appearances",data()$career$showApps, icon = icon("futbol-o"), #user-times
#     color = "light-blue", subtitle = " Tot - Max(Year)"
#   )
# })
# output$teamsBox <- renderInfoBox({
#   infoBox(
#     "Teams",data()$teams, icon = icon("home"),
#     color = "light-blue"
#   )
# })
# output$goalsBox <- renderInfoBox({
#   infoBox(
#     "Goals",data()$career$showGoals, icon = icon("bullseye"),
#     color = "green", subtitle = " Tot - Max(Year)"
#   )
# })
# output$assistsBox <- renderInfoBox({
#   infoBox(
#     "Assists",data()$career$showAssists, icon = icon("heart"),
#     color = "green", subtitle = " Tot - Max(Year)"
#   )
# })
# output$cardsBox <- renderInfoBox({
#   infoBox(
#     "Cards",data()$career$showCards, icon = icon("book"),
#     color = "orange", subtitle = " Tot - Max(Year)"
#   )
# })
# output$seasonsBoxPlayer <- renderInfoBox({
#   
#   infoBox(
#     "Seasons",data()$seasons, icon = icon("calendar"),
#     color = "light-blue"
#   )
# })
# 
# 
# if (interactive()) {
#   library(shiny)
#   
#   ui <- dashboardPage(
#     dashboardHeader(title = "Dynamic boxes"),
#     dashboardSidebar(),
#     dashboardBody(
#       fluidRow(
#         box(width = 2, actionButton("count", "Count")),
#         infoBoxOutput("ibox"),
#         valueBoxOutput("vbox")
#       )
#     )
#   )
#   
#   server <- function(input, output) {
#     output$ibox <- renderInfoBox({
#       infoBox(
#         "Title",
#         input$count,
#         icon = icon("credit-card")
#       )
#     })
#     output$vbox <- renderValueBox({
#       valueBox(
#         "Title",
#         input$count,
#         icon = icon("credit-card")
#       )
#     })
#   }
# }
# runApp()
# 
# 
# 
# # No fees -----------------------------------------------------------------
# 
# feeMissing <-playerGame %>% 
#   filter(FEE==99) %>% 
#   select(name,TEAMNAME) %>% 
#   arrange(TEAMNAME) %>% 
#   unique()


# crosstalk playergamePCdets ----------------------------------------------

# probs <- read_csv("probs.csv")
# 
# clicked on danny murphy
# number was 287 417 apps% =51.6
# playerid= "DASILVF" which is 287th in probs and this does produce correct map
# 
# 
# 
# id <- probs[which(probs$count==632&probs$fullPC==61.2)]$PLAYERID ## 480
# 
# 
# probs$PLAYERID[which(probs$count==632)] #"GIGGSR"
# probs$PLAYERID[which(probs$fullPC==61.2)] [1] "GIGGSR"  "STRACHG"
# 
# id <- intersect(probs$PLAYERID[which(probs$count==326)],probs$PLAYERID[which(probs$fullPC==82.8)])



# plotly goals pc not showing ---------------------------------------------

# df <- read_csv("problem.csv")
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

# needed a unique name for input  SOLVED



# change the tauchart to a plotly -----------------------------------------

# prob <- read_csv("tauproblem.csv")
# 
# 
# 
# 
# 
# plot_ly(prob, x = season, y = Ppm, mode = "markers", hoverinfo = "text",
#         marker=list(size=Mins/10, sizemode="area"),
#         text = paste(
#                      "<br>Goals: ",Goals,
#                      "<br>Assists: ",Assists,
#                      "<br>Points: ",Points,
#                       "<br>Minutes: ",Mins
# )) %>%
#   layout(hovermode = "closest",
#          title="Points per 90 mins by Season",
#          xaxis=list(title=""),
#          yaxis=list(title="Points per 90 mins",rangemode="tozero"
#          )
#   )
# 
# was 
# df %>% 
#   tauchart() %>% 
#   tau_point("season","Ppm", size="2") %>% 
#   # tau_line("season","Ppm") %>% 
#   tau_tooltip(c("Goals","Assists")) %>% 
#   tau_guide_x(label="") %>% 
#   tau_guide_y(label ='Points per 90 mins')



# # timeline wto work in shiny ----------------------------------------------
# 
# df <- read_csv("problem.csv")
# 
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
#   height = 150 # should be enough for clearance
#   )
# 
# 
# library(shiny)
# library(timelineR)
# app <- shinyApp(
#   
#   
#   
#   ui = fluidPage(
#    
#    timelineOutput("timeline")
#   ),
#   server = function(input, output,session) {
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
#       })
#   }
# )
# runApp(app)
# 
# 
# app <- shinyApp(
#   ui = fluidPage(
#     numericInput("n", "n", 1),
#     plotOutput("plot")
#   ),
#   server = function(input, output) {
#     output$plot <- renderPlot( plot(head(cars, input$n)) )
#   }
# )
# 
# runApp(app)
# 
# 
# 
# # ISSUE WITH birthplace ---------------------------------------------------
# 
# df <- read_csv("problem.csv")
# 
# sort(names(df))
# 
# df %>% 
#   ggvis(~season,~pc) %>% 
#   layer_lines(stroke:="coral", strokeWidth:=3) %>% 
#   layer_points() %>% 
#   add_tooltip(all_values, "click") %>% 
#   handle_click(getbpSeason) %>%
#   set_options(width=500) %>% 
#   #  add_axis("y",title="% Mins played by English Born Players in BPL") %>% 
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 10
#   )),title = "")
# 
# 
# df %>% 
#   ggvis(~season,~pc,key:= ~id) %>% 
#   layer_lines(stroke:="coral", strokeWidth:=3) %>% 
#   layer_points() %>% 
#   add_tooltip(all_values, "click") %>% 
#   handle_click(getbpSeason) %>%
#   set_options(width=500) %>% 
#   #  add_axis("y",title="% Mins played by English Born Players in BPL") %>% 
#   add_axis("x", properties = axis_props(labels = list(
#     angle = 45, align = "left", fontSize = 10
#   )),title = "")



# crashing ----------------------------------------------------------------
# 
# theSeason <- "2015/16"
# pos4 <- standings %>%
#   filter(position==4&season==theSeason) %>%
#   select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)
# 
# ## looks like issue is with add_axis
# 
# df  <- data.frame(a=c(1,2),b=c(3,4))
# 
# df %>% 
#   ggvis(~a,~b) %>% 
#   ggvis::add_axis("y",title="Points")
# 
# # Error in add_axis(., "y", title = "Points") : 
# #   unused argument (title = "Points")
# 
# ## restart session
# 
# library(timelineR)
# library(ggvis)
# 
# 
# 
# # The following object is masked from ‘package:ggvis’:
# #   
# #   add_axis
# 
# 
# # swiytch result sequenc histogram to plotly ------------------------------
# 
# W <- read_csv("seqproblem.csv")
# 
# Win <- W %>% 
#   filter(value==0) %>% 
#   group_by(slength) %>% 
#   tally()
# 
# if (tail(W,1)$value==0) {
#   cond <- Win$slength == tail(W,1)$slength
# } else {
#   cond <- FALSE
# }
# 
# Win$opac<-0.1
# 
# glimpse(Win)
# 
# curr <- tail(W,1)$slength
# 
# Win <-Win %>% 
#   mutate(color=ifelse(slength==curr,"red","black"))
# #Win$color <- as.factor(Win$color)
# 
# Win <-Win %>% 
#   mutate(opacity=ifelse(slength==curr,1,0.5))
# 
# #plot_ly(Win,x=slength,y=n,type="bar",opacity=opacity)
# plot_ly(Win,x=slength,y=n,type="bar",color=color, showlegend=F,
#         hoverinfo="text",
#         text=paste("Run:",slength,"<br> Count:",n)) %>% 
#   layout(hovermode = "closest",
#                   xaxis=list(title="Run"),
#                   yaxis=list(title="Count"
#                             )
#   )
# 
# tail(W,1)$slength #2
# 
# # stackflow
# plotting.df = data.frame(names=c("a", "b", "c", "d", "e", "f", "g"),
#                          value=runif(7, min=0, max=2),
#                          label=c("y", "n", "y", "n", "n", "n", "n"),
#                          color=c("red", "black", "red", "black", "black", "black", "black"))
# plotting.df$names = factor(as.character(plotting.df$names), levels=as.character(plotting.df$names)[order(plotting.df$value, decreasing=TRUE)])
# plotting.df = plotting.df[order(plotting.df$value, decreasing=TRUE), ]
# plot_ly(plotting.df, type="bar", x=names, y=value, 
#         name="Comp a", 
#         hoverinfo="text", text=c(paste("Name:", plotting.df$names, 
#                                        "<br>Value:", signif(plotting.df$value, digits=3),
#                                        "<br>Label:", plotting.df$label)),
#         color=color)
# 
# glimpse(plotting.df)
# 
# current <- data.frame(slength=tail(W,1)$slength,n=Win$n[tail(W,1)$slength])
# 
# plot_ly(Win,x=slength,y=n,type="bar",opacity=Win$colNew) %>% ## the opacity has no impact
#   add_trace(current,x=slength,y=n,type="bar", opacity=1)
# 
# # try removing the 
# 
# Win <- Win[-curr,]
# 
# plot_ly(Win,x=slength,y=n,type="bar",opacity=0.5) %>% 
#   add_trace(current,x=slength,y=n,type="bar")
# 
# # plot_ly(topScorers, x = jitGoals, y = jitslength, mode = "markers", hoverinfo = "text",
# #         text = paste("Name:",name,"<br> Goals:",totGoals,"<br> Sequence:",slength)) %>%
# #   layout(hovermode = "closest",
# #          xaxis=list(title="Career Premier League goals"),
# #          yaxis=list(title="Best Consecutive Scoring Run in PL games for Club"
# #                    )
# #   )
# # 
# # })
# ggplot(Win, aes(x=slength,y=n)) +
#   geom_bar(data=subset(Win,cond==FALSE),stat="identity", width=0.7,  fill="blue", alpha=0.2)+
#   geom_bar(data=subset(Win,cond==TRUE),stat="identity", width=0.7, fill="blue") +
#   theme_bw() +
#   xlab("Sequence") +
#   ylab("Count") +
#   ggtitle("Winless")
# 
# glimpse(economics)
# 
# p <- plot_ly(economics,
#              type = "bar",       # all "scatter" attributes: https://plot.ly/r/reference/#scatter
#              x = date,               # more about scatter's "x": /r/reference/#scatter-x
#              y = uempmed,            # more about scatter's "y": /r/reference/#scatter-y
#              name = "unemployment",  # more about scatter's "name": /r/reference/#scatter-name
#              marker = list(          # marker is a named list, valid keys: /r/reference/#scatter-marker
#                color='#E3BA22'     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
#              ))
# 
# glimpse(Win)
# 
# 
# plot_ly(Win,
#         type = "bar",       # all "scatter" attributes: https://plot.ly/r/reference/#scatter
#         x = slength,               # more about scatter's "x": /r/reference/#scatter-x
#         y = n,            # more about scatter's "y": /r/reference/#scatter-y
#         #name = "unemployment",  # more about scatter's "name": /r/reference/#scatter-name
#         showlegend=F,
#         hoverinfo="text",
#         text=paste("Run:",slength,"<br> Count:",n)) %>% 
#         marker = list(          # marker is a named list, valid keys: /r/reference/#scatter-marker
#           color='#E3BA22'     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
#         )
# 
# library(plotly)
# df <- data.frame(x=c(1,2,3),y=c(6,3,4),opacity=c(1,0.2,1), color=c("#5a22e3","red","green"))
# 
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#         opacity=opacity,
#         marker = list(         
#           color='#5a22e3'    
#         )
#         )
# 
# plot_ly(df,
#         type="bar",
#         x=x,
#         y=y,
#         #group=y,
#         #opacity=opacity,
#         marker = list(         
#           color=color    
#         )
# )
# 
# 
# 
# # exploding boxplot -------------------------------------------------------
# 
# ## needed crosstalk branch to display
# library(shiny)
# library(explodingboxplotR)
# app <- shinyApp(
#   ui = fluidPage(
#     exploding_boxplotOutput("test", width = "100%", height = "400px")
#   ),
#   server = function(input, output) ({
#     
#   output$test <-  renderExploding_boxplot({
#     exploding_boxplot(
#       data.frame(
#         rowname = rownames(InsectSprays),
#         InsectSprays,
#         stringsAsFactors = FALSE
#       ),
#       y = "count",
#       group = "spray",
#       color = "spray",
#       label = "rowname"
#     )
#   
#   })
# })
# )
# 
# library(explodingboxplotR)
# library(shiny)
# library(beeswarm)
# 
# ui <- fluidPage(
#   fluidRow(
#     column(
#       width = 6,
#       exploding_boxplotOutput("explode1", height = 400)
#     ),
#     column(
#       width = 6,
#       plotOutput("beeswarm1", height = 400)
#     )
#   ),
#   fluidRow(
#     plotOutput("distplot1", height = 400)
#   )
# )
# 
# server <- function(input,output,session){
#   output$explode1 <- renderExploding_boxplot({
#     exploding_boxplot(
#       data.frame(
#         rowname = rownames(InsectSprays),
#         InsectSprays,
#         stringsAsFactors = FALSE
#       ),
#       y = "count",
#       group = "spray",
#       color = "spray",
#       label = "rowname",
#       crosstalk_group = "A"
#     )
#   })
#   
#   sd <- crosstalk::SharedData$new(InsectSprays, "spray", group = "A")
#   
#   output$beeswarm1 <- renderPlot({
#     df <- sd$data(TRUE)
#     if(any(is.na(df$selected_))) {
#       beeswarm(count~spray,df)
#     } else {
#       beeswarm(count~spray,subset(df, df$selected_ == TRUE))
#     }
#   })
#   
#   output$distplot1 <- renderPlot({
#     df <- sd$data(TRUE)
#     if(any(is.na(df$selected_))) {
#       plot(density(df$count))
#     } else {
#       plot(
#         density(subset(df, df$selected_ == TRUE)$count),
#         xlim = c(0,max(df$count))
#       )
#     }
#   })
# }
# 
# shinyApp(ui,server)
# 
# runApp(app)
# 
# 
# 
# # ggvis issues on current charts ------------------------------------------
# 
# ## due to conflict with timeliner
# 
# df <-  standings %>% 
#   ungroup() %>% 
#   filter(tmYrGameOrder==25&position==1)
# 
# df %>% 
#   ggvis(~final_Pos) %>% 
#   layer_histograms(fill:="lightblue") %>% 
#   ggvis::add_axis("x", title="Final League Position", format='d') %>% 
#   ggvis::add_axis("y", title="Seasons") %>% 
#   bind_shiny("st_position_chart")
# 
# ## plotly
# plot_ly(df , x=final_Pos, y=count,  type="bar"),
#         hoverinfo="text",
#         text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
#   add_trace(x=gameOrder,y=Assists, name="Assists (inc secondary)", type="bar",
#             hoverinfo="text",
#             text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder))  %>%
#   layout(hovermode = "closest", barmode="stack",
#          
#          xaxis=list(title=xTitle),
#          yaxis=list(title="Points"),
#          title=" Hover bar for details", titlefont=list(size=16)
#   )
# 
# plot_ly(ppgPlayer , x=gameOrder, y=Gls, name="Goals", type="bar",
#         hoverinfo="text",
#         text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder)) %>%
#   add_trace(x=gameOrder,y=Assists, name="Assists (inc secondary)", type="bar",
#             hoverinfo="text",
#             text=paste(TEAMNAME,"<br>v ",Opponents,"<br>",gameDate, "<br>Game ",gameOrder))  %>%
#   layout(hovermode = "closest", barmode="stack",
#          
#          xaxis=list(title=xTitle),
#          yaxis=list(title="Points"),
#          title=" Hover bar for details", titlefont=list(size=16)
#   )


# test <- playerGame %>%  # age has attributes
#   ungroup() %>% # this does not help
#   select(-age) %>% 
#   #filter(COUNTRY==input$country&season==input$teamYears)  %>%
#   filter(COUNTRY=="England"&season=="2015/16")  %>%
#   group_by(PLAYERID,name) %>%
#   select(Gls,Assists,mins) %>%
#   dplyr::summarize(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% ## issue is summarize not using group_by even though loaded last
#   filter(Points!=0) %>%
#   mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>%
#   ungroup()
# 
# 
# plot_ly(test, x = Points, y = Ppm, mode = "markers", hoverinfo = "text",
#         marker=list(size=Mins/10, sizemode="area"),
#         text = paste(name,
#                      "<br>Goals: ",Goals,
#                      "<br>Assists: ",Assists,
#                      "<br>Points: ",Points,
#                      "<br>Minutes: ",Mins
#         )) %>%
#   layout(hovermode = "closest",
#          title="Points (inc. secondary assists) per 90 mins by Season <br>
#          Zoom and Hover points for Details",
#          xaxis=list(title="Points in Season"),
#          yaxis=list(title="Points per 90 mins",rangemode="tozero"
#          )
#   ) %>%
#   config(displayModeBar = F)
# })
# 
# 
# test <- playerGame %>%  # age has attributes
#   ungroup() %>% # this does not help
#   select(-age) %>% 
#   #filter(COUNTRY==input$country&season==input$teamYears)  %>%
#   filter(COUNTRY=="England"&season=="2015/16")  %>%
#   group_by(PLAYERID,name) %>%
#   select(Gls,Assists,mins) # mins has loads of attributes
# 
# 
# test <- playerGame %>%  # age has attributes
#   ungroup() %>% # this does not help
#   select(-age) %>% 
#   #filter(COUNTRY==input$country&season==input$teamYears)  %>%
#   filter(COUNTRY=="England"&season=="2015/16")  %>%
#   select(Gls,Assists,mins,PLAYERID) %>% 
#   group_by(PLAYERID) 
# 
# str(test)
# 
# library(dplyr)
# df <- data.frame(id=c("a","a","a","b","b"), goals=c(0,2,3,1,1), stringsAsFactors = FALSE)
# str(df)
# 
# df %>% 
#   group_by(id) %>% 
#   summarize(tot=sum(goals)) ## no problem
# 
# 
# ## the global
# library(shiny)
# library(shinydashboard)
# library(httr)
# library(rvest)
# library(XML)
# library(doBy) # uses MASS which has a eselect conflict with dplyr - need for sequences
# #library(dplyr) # this masks select from MASS, filter from stats and intersect etc from base
# library(timelineR) # conflict with ggvis on add_axis so added to ggvis currently
# library(ggvis)
# 
# library(RSQLite)
# library(lubridate)
# library(stringr)
# library(markdown)
# library(tidyr)
# library(shinyBS)
# library(scales)
# library(ggplot2)
# library(leaflet)
# library(rCharts)
# library(shinythemes)
# library(DT)
# library(readr)
# library(ggmap)
# library(rgdal)
# library(choroplethr)
# library(choroplethrMaps)
# library(taucharts)
# library(daff)
# library(plotly)
# #library(crosstalk) no longer needed as can now use event_data() in plotly
# library(explodingboxplotR)
# library(beeswarm) # just for test
# #library(addins)
# library(dplyr)
# 
# df <- data.frame(id=c("a","a","a","b","b"), goals=c(0,2,3,1,1), stringsAsFactors = FALSE)
# str(df)
# 
# df %>% 
#   group_by(id) %>% 
#   summarize(tot=sum(goals))  # still works fine
# 
# sessionInfo()
# 
# In this case dplyr is first in attached packages
# 
# sessionInfo()
#   
# )


# checking for where problems on package upgradesmight occur --------------

library(reinstallr)
find_used_packages('DT', path='../') # Error in readLines(con) : cannot open the connection
getwd()
find_used_packages('DT', path='code')


show_package_stats(path = 'code', pattern = NULL)



# New season issues -------------------------------------------------------

# from server

if (input$sbMenu=="tm_playerSummary"|input$sbMenu=="tm_leaguePosition") {
  print("right start") # comes in intially ok
  print(input$teamA)
  yrs <- sort(unique(tmYrs[tmYrs$team==input$teamA,]$season),decreasing = T) # thinka bout + inc all
  #  updateSelectInput(session, "teamYears", choices = yrs)
  inputPanel(selectInput("teamYears",label=NULL,yrs, selected=yrs[1]))# - this was here and working
  #inputPanel(selectInput("teamYears",label=NULL,yrs,selected=values$teamYears))
}

yrs

1] "2004/05" "1993/94" "1999/00" "2005/06" "2003/04" "2008/09" "2011/12" "2006/07" "2012/13" "1994/95"
[11] "2007/08" "2001/02" "2014/15" "2009/10" "2013/14" "1992/93" "2002/03" "1995/96" "2015/16" "2000/01"
[21] "2010/11" "1998/99" "1997/98" "1996/97" "2016/17"

yrs <- sort(unique(tmYrs[tmYrs$team=="Arsenal",]$season),decreasing = T)
yrs
[1] "2016/17" "2015/16" "2014/15" "2013/14" "2012/13" "2011/12" "2010/11" "2009/10" "2008/09" "2007/08"
[11] "2006/07" "2005/06" "2004/05" "2003/04" "2002/03" "2001/02" "2000/01" "1999/00" "1998/99" "1997/98"
[21] "1996/97" "1995/96" "1994/95" "1993/94" "1992/93"

inputPanel(selectInput("teamYears",label=NULL,yrs, selected=yrs[1]))  ## looks good

graph <- standings %>%
  filter(team=="Arsenal"&season=="2016/17")
glimpse(graph) #class(graph)
#graph <- cbind(graph, id = seq_len(nrow(graph))) #Error: cannot convert object to a data frame if 1 value
theID <- seq_len(nrow(graph)) #class(id) integer
id_df <- data.frame(id=theID)

bind_cols(graph,id_df)
library(purrr)
map_df(graph,id) # NEEDS TO BE VECTOR

make id a DataFrame

graph <- graph %>% 
  mutate(id=row_number())



# just need to add a tt and res for fill
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- graph[graph$id == x$id,"tt" ]
  paste0(names(row), format(row), collapse = "<br />")
}

pos1 <- standings %>%
  filter(position==1&season==theSeason) %>%
  select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)


print("theseason")
print(theSeason)
pos4 <- standings %>%
  filter(position==4&season==theSeason) %>%
  select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)


# NEW SEASON BLUES --------------------------------------------------------


test <-standings %>% 
  filter(team=="Arsenal") %>% 
  group_by(season) %>% 
  filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
  ungroup()

summary <- test %>% 
  summarize(years=n(),bestPos=min(position),wortsPos=max(position),maxPoints=max(cumPts),minPoints=min(cumPts))

df <- read_csv("problem.csv")

maxSeason <-df %>% 
  group_by(final_Pos) %>% 
  tally()

theMax <- max(maxSeason$n)

cond <- df$season =="2016/17"
ggplot(df, aes(x=final_Pos)) +
  geom_histogram(data=subset(df,cond==FALSE), binwidth=0.5, fill="blue", alpha=0.2) +
  geom_histogram(data=subset(df,cond==TRUE), binwidth=0.5, fill="blue") +
  scale_x_continuous(breaks=df$final_Pos+0.25, labels=df$final_Pos) +
  # scale_y_continuous(breaks=pretty_breaks()) +
  scale_y_discrete(breaks= seq(0,theMax)) +
  theme_bw() +
  xlab("Position (2016/7 in bold)") +
  ylab("Seasons")


# plotly redoing ----------------------------------------------------------

Win <- read_csv("problemA.csv")

plot_ly(Win,x=~slength,
        y=~n,
        type="bar",
        marker=list(color='blue'),
        opacity=opacity,
        showlegend=F,
        group= ~slength, # could equally be slength
        hoverinfo="text",
        text=~paste("Run:",slength,"<br> Count:",n)) %>% 
  layout(hovermode = "closest", title= "Wins",
         xaxis=list(title="Run"),
         yaxis=list(title="Count"
         ))

Win %>% 
plot_ly() %>% 
   add_bars(data=Win,x=~slength,y=~n,opacity=~opacity,showlegend=F,
           #group= ~slength, # could equally be slength
           hoverinfo="text",
           text=~paste("Run:",slength,"<br> Count:",n)) %>% 
  layout(hovermode = "closest", title= "Wins",
         xaxis=list(title="Run"),
         yaxis=list(title="Count"
         ))


## this works other than hover info on the latest
  plot_ly() %>% 
  add_bars(data=subset(Win,opacity!=1.0),x=~slength,y=~n,showlegend=F,
           #group= ~slength, # could equally be slength
           hoverinfo="text",
           text=~paste("Run:",slength,"<br> Count:",n)) %>% 
  add_bars(data=subset(Win,opacity==1.0),x=~slength,y=~n,showlegend=F,
           #group= ~slength, # could equally be slength
           hoverinfo="text",
           text=~paste("Run:",slength,"<br> Count:",n)) %>% 
  layout(hovermode = "closest", title= "Wins",
         xaxis=list(title="Run"),
         yaxis=list(title="Count"
         ))
  
  ####
  Place <- readRDS("Place.rds")
  
  
  
  
# youngets player ---------------------------------------------------------

  apps <- summary %>% 
    #filter((St+On)>0) %>% 
    group_by(PLAYERID) %>% 
    summarize(Apps=(sum(St)+sum(On)))
  
  df <-  playerGame %>% 
    filter((START+subOn)>0&TEAMNAME=="Arsenal") %>% 
    group_by(gameDate) %>% 
    mutate(youngest=min(age)) %>% 
    filter(age==youngest) %>% 
    ungroup() %>% 
    arrange(gameDate) %>% 
    select(name,age,gameDate,PLAYERID) %>% 
    inner_join(apps)
  
  df$age <- as.numeric(df$age)
  
  df$id <- 1:nrow(df)
  
  df <-  df %>% 
    mutate(youngest=cummin(age)) %>% 
    filter(age==youngest) %>% 
    mutate(year=as.integer(str_sub(age,1,2))) %>% 
    mutate(days=floor(365.25*(age-year))) %>% 
    mutate(showAge=paste0(year," yrs ",days," days")) %>% 
    mutate(showApps=paste0(Apps," career Apps"))
  
  #print(glimpse(df))
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[x$id == df$id,c("name","showAge","gameDate","showApps") ]
    paste0(format(row), collapse = "<br />")
  }
  
  df   %>% 
    ggvis(~gameDate,~youngest, key:= ~id) %>% 
    layer_points(fill =~name) %>% 
    add_legend("fill", title="") %>% 
    ggvis::add_axis("x", title="") %>% 
    ggvis::add_axis("y", title="Age",title_offset=50) %>% 
    add_tooltip(all_values,"hover") %>% 
    set_options(height = 300) %>% 
    bind_shiny("sp_ageRecord")
  
  
  
  
# player images -----------------------------------------------------------
  thePlayer <- tolower(str_replace(playerName," ","-"))
  thePlayer <- "darren-bent"
  u <- paste0("http://www.premierleague.com/en-gb/players/profile.career-history.html/",thePlayer)
  
  # u ishttp://www.premierleague.com/en-gb/players/profile.career-history.html/darren-bent
  # https://www.premierleague.com/players/2098/Darren-Bent/overview is now format
  
  ## poss alternative google images
  
  library(rvest)
 d <- read_html("https://www.google.ca/search?q=google+images+darren+bent&source=lnms&tbm=isch")
  d %>% 
    html_attrs(".ivg-i:nth-child(1) .rg_ic")
  
  #//*[contains(concat( " ", @class, " " ), concat( " ", "ivg-i", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "rg_ic", " " ))]
  
  url <- "https://www.r-project.org"
  imgsrc <- read_html(url) %>%
    html_node(xpath = '//*/img') %>%
    html_attr('src')
    imgsrc #[1] "/Rlogo.png"
  
  
  url <- "https://www.google.ca/search?q=google+images+darren+bent&source=lnms&tbm=isch"
  imgsrc <- read_html(url) %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "ivg-i", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "rg_ic", " " ))]') %>%
    html_attr('src')
  imgsrc NA
  
  cab see 
  d %>% 
 html_nodes("div .rg_di rg_bx rg_el ivh-i a img src")
  
  x <-  d %>% 
    html_nodes("div") # no obvious candidate
  
  x[20:28]
  
  d %>% 
    html_nodes("div .rg_di") rg_bx rg_el ivh-i a img src")
  
  print(u)
  u.get<- GET(u)
  print("OK to here")
  u.content=content(u.get, as="text")
  u.html <- htmlParse(u.content)
  #    print(u.html)
  picLink <-xpathSApply(u.html, "//*/img[@class='heroimg']/@src")
  #unname(picLink)
  print(picLink)
  if (!is.null(picLink)) {
    src1 <- paste0("http://www.premierleague.com",unname(picLink))
  } else {
    src1 <- "http://www.premierleague.com/content/dam/premierleague/shared-images/site-furniture/players/-lsh.jpg"
  }
  print(src1) #1] "http://www.premierleague.com/content/dam/premierleague/shared-images/players/d/darren-bent/10738-lsh.jpg" is a picture
  tags$img(src=src1, width=300)


# look at final pos each year - looks far too complicated -----------------


# lost goals 8 of them ----------------------------------------------------

library(readr)
# goals1 <- read_csv("~/R/development/goals1.csv") #24776
# goals2 <- read_csv("~/R/development/goals2.csv") #24678


names(goals1)

names(goals2) adds goals and teammatchid
head(goals2) #GOALS might be order??

goals1 %>% 
  anti_join(goals2)


# A tibble: 8 × 6
  PLAYER_MATCH  TIME METHOD PLACE  PLAY PLAYER_MATCH_GOAL
  <int> <int>  <chr> <chr> <chr>             <int>
  1       391381    76      H     P     O              6088
  2       382698    60      R     O     O              5459
  3       665844    67      R     P     O             24335
  4       391366    54      R     G     O              5611
  5       673510    90      R     P     P             24947
  6       382693    21      L     G     O              5456
  7       673508    45      H     P     T             24948
  8       391361    59      R     P     C              5665

## need lookin g at but none of them are recent

tbl_playermatch is missing 673499 to 673516 team matchid 41316 and 41317 

in player_match 673480 begovic has missu=ing teammatch id 41315 or 41316
and 673516 has no teammatchid or playerteam

tbl matchteam does not have 15/6/7 nor 9043/4 match nor does match 


# sequence wins not correct in table --------------------------------------

df <- read_csv("problem.csv") # is resData()$W




write_csv(resData()$W,"problem.csv")
  # could put in reactive but not sure worthwhile
  long <- df %>% 
  filter(value==1) %>%  # need to restrict to wins first
  filter(slength==max(slength)) %>% ## 1 times won 14 in row 380-393
  tail(1)

 A tibble: 1 × 5
  first  last slength midpoint value
  <dbl> <dbl>   <dbl>    <dbl> <dbl>
  1   380   393      14      387     1


 tbl <-  standings %>% 
  ungroup() %>% 
  
  filter(team=="Arsenal")  # not ordered correctly

  
  if (input$seqVenue=="Home") {
  tbl <-  standings %>% 
  ungroup() %>% 
  filter(team==input$teamA&venue=="H")
  }  else if(input$seqVenue=="Away") {
  tbl <-  standings %>% 
  ungroup() %>% 
  filter(team==input$teamA&venue=="A")  
  } else {
  tbl <-  standings %>% 
  ungroup() %>% 
  
  filter(team==input$teamA)
  }
  
  tbl %>% 
  mutate(gameOrder=row_number()) %>% 
  filter(gameOrder>=long$first&gameOrder<=long$last) %>% 
  mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
  select(Opponents,Score,Date=gameDate) %>% 
  DT::datatable(class='compact stripe hover row-border',
  rownames=FALSE,
  
  options= list(paging = FALSE, searching = FALSE, info=FALSE,
  columnDefs = list(list(className = 'dt-center', targets = 1))))


tbl %>% 
arrange(tmGameOrder) %>% 
 # mutate(gameOrder=row_number()) %>% 
  filter(tmGameOrder>=long$first&tmGameOrder<=long$last) %>% 
  mutate(Score=paste0(GF,"-",GA),Opponents=ifelse(venue=="A",paste0("@ "," ",OppTeam),paste0("v ",OppTeam))) %>% 
  select(Opponents,Score,Date=gameDate) %>% 
  DT::datatable(class='compact stripe hover row-border',
  rownames=FALSE,
  
  options= list(paging = FALSE, searching = FALSE, info=FALSE,
  columnDefs = list(list(className = 'dt-center', targets = 1))))
  

# multiple issues early 2016/7 --------------------------------------------

dfTeamYear <- summary %>%
    filter(PLAYERID=="BENTD") %>% #BENTD
  mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
  select(Season=season,Age=age,Team = TEAMNAME,Apps=apps,St,On,Off,Bench,Mins=mins,Gls,Assists,Points,Pens,Y,R,OG,MP) %>% 
  ungroup() %>% 
  arrange(desc(Season)) %>% 
  select(-(c(name,LASTNAME,PLAYERID,born,left))) # has none of these in anywasy see select above???


playerGame %>% 
#ungroup() %>% does not make difference
    filter(PLAYERID=="BENTD") %>% 
  group_by(season,PLAYERID,name) %>% 
  select(Gls,Assists,mins) %>% 
  summarise(Goals=sum(Gls),Assists=sum(Assists),Points=Goals+Assists,Mins=sum(mins))%>% 
  filter(Points!=0) %>% 
  mutate(Gpm=90*Goals/Mins,Apm=90*Assists/Mins,Ppm=90*Points/Mins) %>% 
  ungroup() 



 test <-standings %>% 
    filter(team=="Arsenal") %>% 
  group_by(season) %>% 
  filter(tmYrGameOrder==max(tmYrGameOrder)) %>% 
  ungroup()
  
  summary <- test %>% 
  summarise(years=n(),bestPos=min(position),wortsPos=max(position),maxPoints=max(cumPts),minPoints=min(cumPts))
  
  
  mostGames <- playerGame %>% 
  filter(TEAMNAME==input$teamA) %>% 
  group_by(PLAYERID,name) %>% 
  filter((START+subOn)>0) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n))
  
  
  mostGoals <- playerGame %>% 
  filter(TEAMNAME==input$teamA&name!=" Own Goal") %>% 
  group_by(PLAYERID,name) %>% 
  summarize(sumGoals=sum(Gls)) %>% 
  ungroup() %>% 
  filter(sumGoals>0) %>% 
  arrange(desc(sumGoals))
  
  mostAssists <- playerGame %>% 
  filter(TEAMNAME==input$teamA) %>% 
  
  group_by(PLAYERID,name) %>% 
  summarize(sumAssists=sum(Assists)) %>% 
  ungroup() %>% 
  filter(sumAssists>0) %>% 
  arrange(desc(sumAssists))
  
  
  mostCards <- playerGame %>% 
  filter(TEAMNAME==input$teamA&CARD>"0") %>% 
  group_by(PLAYERID,name) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n))

## test summarize and summarise
library(tibble)
library(dplyr)
df <- tibble(x=c("a","a","b"),y= c( 1,2,4))

df %>% 
group_by(x) %>% 
summarize(ct=n(),tot=sum(y))  # works so maybe 


# wk4 issues --------------------------------------------------------------

df <- read_csv("problem.csv")


 plot <- df %>%
        plot_ly() %>%
        group_by(PLAYERID) %>% 
  add_lines(~gameOrder,~cumGoals, color=~name)


# WARD ASSISTS  -----------------------------------------------------------

sort(names(playerGame))

test <-playerGame %>% 
filter(PLAYERID=="WARDJ") # had missed entering initially not sure why?


---------


