## try setting reactive values here for keeping player
## needs to have an observeevent as well
values <- reactiveValues()
# values$playerID <- "BENTD" # no longer takes effect is this shiny ( no looks like putting front page on for player droughts is reason that stays
 values$TEAMNAME <- "Chelsea" # does take effect
 values$playerID <- "BENTD"
 values$managerID <- "Arsene Wenger"
# values$TEAMNAME <- NULL
 values$Opponents <- NULL
 values$MATCHID <- NULL

 values$teamYears <- NULL
 
 # https://blog.rstudio.org/2017/04/05/shiny-1-0-1/ other option for reactive values
 
shinyServer(function(input, output, session) {
  
  
  ## looking at specific team pages via 
  # this works
  # observeEvent(input$bookmark, {
  #   print(input$bookmark)
  # values$TEAMNAME <- input$bookmark 
  # values$playerID <- "OZILM"
  # })
  
  
  observeEvent(input$bookmark, {

    values$TEAMNAME <- input$bookmark
    values$playerID =
    case_when(
      input$bookmark=="Arsenal" ~  "OZILM", 
      input$bookmark=="Aston Villa" ~ "AGBONLG",
      input$bookmark=="Barnsley" ~  "READFERN", 
      input$bookmark=="Birmingham C" ~ "LARSSOS",
      input$bookmark=="Blackburn" ~  "SUTTONC", 
      input$bookmark=="Blackpool" ~ "ADAMC",
      input$bookmark=="Bolton" ~  "DAVIESK", 
      input$bookmark=="Bournemouth" ~ "KINGJ",
      input$bookmark=="Bradford C" ~  "BEAGRIP", 
      input$bookmark=="Brighton" ~ "GROSSP",
      input$bookmark=="Burnley" ~  "BARNESA", 
      input$bookmark=="Cardiff C" ~ "MUTCHJ",
      input$bookmark=="Charlton" ~  "EUELLJ", 
      input$bookmark=="Chelsea" ~ "HAZARDE",
      input$bookmark=="Coventry C" ~  "DUBLIND", 
      input$bookmark=="Crystal P" ~ "ZAHAW",
      input$bookmark=="Everton" ~  "BAINESL", 
      input$bookmark=="Derby Co." ~ "STURRID",
      input$bookmark=="Fulham" ~  "DEMPSEC", 
      input$bookmark=="Huddersfield" ~ "INCET",
      input$bookmark=="Hull C" ~  "HUDDLET", 
      input$bookmark=="Ipswich T" ~ "KIWOMYC",
      input$bookmark=="Leeds U" ~  "KEWELLH", 
      input$bookmark=="Leicester C" ~ "VARDYJ",
      input$bookmark=="Liverpool" ~  "COUTINP", 
      input$bookmark=="Man. City" ~ "AGUEROS",
      input$bookmark=="Man. Utd." ~  "CARRICM", 
      input$bookmark=="Middlesbro" ~ "DOWNINS",
      input$bookmark=="Newcastle U" ~  "SHEAREA", 
      input$bookmark=="Norwich C" ~ "HOOLAHW",
      input$bookmark=="Nottm Forest" ~  "STONES", 
      input$bookmark=="Oldham" ~ "SHARPG",
      input$bookmark=="Portsmouth" ~  "TAYLORM7", 
      input$bookmark=="QPR"  ~ "FERDINL",
      input$bookmark=="Reading" ~  "DOYLEK", 
      input$bookmark=="Sheff. Utd." ~ "GILLESK",
      input$bookmark=="Sheff. Wed." ~  "BRIGHTM", 
      input$bookmark=="Southampton" ~ "LETISSM",
      input$bookmark=="Stoke C" ~  "CROUCHP", 
      input$bookmark=="Sunderland"  ~ "PHILLIK",
      input$bookmark=="Swansea" ~  "SIGURDG", 
      input$bookmark=="Swindon T"  ~ "MONCURJ",
      input$bookmark=="Tottenham H" ~  "KANEH", 
      input$bookmark=="Watford" ~ "DEENEYT",
      input$bookmark=="West Brom" ~  "BRUNTC", 
      input$bookmark=="West Ham U" ~ "NOBLEM",
      input$bookmark=="Wigan Ath." ~  "RODALLH", 
      input$bookmark=="Wimbledon" ~ "EARLER",
      input$bookmark=="Wolves"  ~  "JARVISM" 
      
    )
    
  })
  
  
  # observeEvent(input$bookmark, {
  #   
  #   values$TEAMNAME <- input$bookmark 
  #  
  #   if(input$bookmark=="Arsenal") values$playerID <- "OZILM"
  #   if(input$bookmark=="Aston Villa") values$playerID <-  "AGBONLG",
  # })
  
  
  x <- 1:50
  case_when(
    x %% 35 == 0 ~ "fizz buzz",
    x %% 5 == 0 ~ "fizz",
    x %% 7 == 0 ~ "buzz",
    TRUE ~ as.character(x)
  )
  
  # Need to exclude the buttons from themselves being bookmarked -https://shiny.rstudio.com/articles/bookmarking-state.html
  setBookmarkExclude(c("b_1","b_2","b_3","b_4"))  # just b_4 for sidebar so saves whole site from first state

  # Trigger bookmarking with either button
  observeEvent(input$b_1, {
    session$doBookmark()
  })
  observeEvent(input$b_2, {
    session$doBookmark()
  })
  observeEvent(input$b_3, {
    session$doBookmark()
  })
  observeEvent(input$b_4, {
    session$doBookmark()
  })
  
  
  ## set up input menu in sidebar
  output$a <- renderUI({
    #     if (input$sbMenu=="players") {
    #       selectInput("playerA", "Player", playerChoice) 
    #     } else 
    if (input$sbMenu=="tm_playerSummary") { # has to be at menuSubItem if it exists
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    } else if (input$sbMenu=="sp_plGoalSeqs") {
      inputPanel(selectInput("teamC", label=NULL, teamsChoice_2))
    
    } else if (input$sbMenu=="tm_leaguePosition") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    }  else if (input$sbMenu=="tm_goals") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    }  else if (input$sbMenu=="tm_glance") { # like tm_goals should just be team input
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    }  else if (input$sbMenu=="tm_leaders") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    }  else if (input$sbMenu=="tm_hth") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    } else if (input$sbMenu=="tm_seqs") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
    } else if (input$sbMenu=="tm_heat") {
      inputPanel(selectInput("heatTeam", label=NULL,selected=values$TEAMNAME, teamsChoice))
    } else if (input$sbMenu=="tm_seqs_goals") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice))
      
      
      # manager ones
    } else if (input$sbMenu=="m_ppg") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice),
                 sliderInput("managerGames", label="Min games in Stint", min=1,max=100,value=5))
    } else if (input$sbMenu=="m_players") {
      print(values$managerID)
      inputPanel(selectInput("manager", label=NULL,selected=values$managerID, choices=managerChoice))
    } else if (input$sbMenu=="t_ppg") {
      inputPanel(selectInput("manager", label=NULL,selected=values$managerID, choices=managerChoice))
      # specials 
    } else if (input$sbMenu=="sp_playerByTeamPPG") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice),
                 selectInput("teamYears",label=NULL,seasonChoice)
                 )
     } else if (input$sbMenu=="sp_playerByCountryPPG") {
        inputPanel(selectInput("country", label=NULL,selected="England", countryChoice),
                   selectInput("teamYears",label=NULL,seasonChoice)
        )
        
      # player ones
    } else if (input$sbMenu=="pl_asstScorer") {
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
    } else if (input$sbMenu=="pl_career") {
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
    } else if (input$sbMenu=="pl_goals") {
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
    } else if (input$sbMenu=="pl_droughts") {
      print(" right menu")
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
    } else if (input$sbMenu=="pl_ppg") {
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
    } else if (input$sbMenu=="pl_glance") {
      print("pl_glance")
      #inputPanel(selectizeInput("playerA", label=NULL, c(Choose='',playerChoice),selected=values$playerID, options=list(maxOptions=10000)))
      #inputPanel(selectizeInput("playerA", label=NULL, choices =playerChoice,selected=values$playerID, options=list(maxOptions=10000)))
      inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
       } else if (input$sbMenu=="pl_seqs_goals") {
      #inputPanel(selectizeInput("playerA", label=NULL, c(Choose='',playerChoice),selected=values$playerID, options=list(maxOptions=10000)))
         inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
         }else if (input$sbMenu=="pl_opponent") {
           inputPanel(selectInput("playerA", label="Type Name and Select", choices =playerChoice,selected=values$playerID))
         } else if (input$sbMenu=="sp_comparisons") {
           inputPanel(selectInput("playerComps", label="Type Names and Select", choices =playerChoice,selected=c("BECKHAD","SCHOLEP","GIGGSR"), multiple= TRUE),
                      radioButtons("compCategory", label="Category", choices=c("Goals","Assists","Points"),selected="Points",inline=TRUE),
                      radioButtons("compTime", label=NULL, choices=c("Apps","Age","Date"),inline=TRUE),
           actionButton(inputId="compBtn",label="Compare Players"))
         }
    
  })
  
 
  # this no longer changes and defaults toADrian anyways or what appears first? eg front page
  observeEvent(input$playerA,{
    print("enter observeEvent(input$playerA,{")
    values$playerID <- input$playerA
    print(values$playerID)
  })
  
  # this changes with every diff entry as expected 
  observeEvent(input$teamA,{
    print("enter observeEvent(input$teamA,{")
    values$TEAMNAME <- input$teamA
    print(values$TEAMNAME)
    
  }) # if add once = TRUE the reverst back to default afeter one change - prob dont want
  
  
  # observeEvent(input$teamYears,{
  #   print("enter observeEvent(input$teamA,{")
  #   values$teamYears <- input$teamYears
  # 
  #   
  # })
#   
#   observeEvent(input$playerB,{
#     print("enter input$PlayerA in observeEvent")
#     print(input$playerB)
#     
#     updateSelectizeInput(session,"playerA", "Player", playerChoice,selected=input$playerB,   options=list(maxOptions=10000))
#   })
  
  # Not sure how this can be applied to all
  output$teamYear_ui <- renderUI({
    if (input$sbMenu=="tm_playerSummary"|input$sbMenu=="tm_leaguePosition") {
      print("right start") # comes in intially ok
      print(input$teamA)
      yrs <- sort(unique(tmYrs[tmYrs$team==input$teamA,]$season),decreasing = T) # thinka bout + inc all
    #  updateSelectInput(session, "teamYears", choices = yrs)
      inputPanel(selectInput("teamYears",label=NULL,yrs, selected=yrs[1]))# - this was here and working
      #inputPanel(selectInput("teamYears",label=NULL,yrs,selected=values$teamYears))
    } else if (input$sbMenu=="pl_goals"){
      #print("here u are")
    }
    else { # looks promising
      #print("here u are")
      #selectInput("teamYears",label=NULL,yrs, selected=yrs[length(yrs)]) # need to readdress
      #  return()
    }
  })
  
  output$teamYear_cpc <- renderUI({
    
      yrs <- sort(unique(tmYrs[tmYrs$team==input$team_cpc,]$season),decreasing = T)
      
      inputPanel(selectInput("teamYear_cpc",label=NULL,yrs, selected=yrs[1]))
      
  })
  
  # finishing position - need to exclude 42 game seasons
  output$teamYear_fp <- renderUI({
    
    yrs <- sort(unique(tmYrs[tmYrs$team==input$team_fp,]$season),decreasing = T)
    noGoYears <- c("1992/93","1993/94","1994/95","2017/18") # cannot have incomplete years
    yrs <-  setdiff(yrs,noGoYears)
    
    inputPanel(selectInput("teamYear_fp",label=NULL,yrs, selected=yrs[1]))
    
  })
  
  output$teamYear_fp_front <- renderUI({
    
    yrs <- sort(unique(tmYrs[tmYrs$team==input$team_fp_front,]$season),decreasing = T)
    noGoYears <- c("1992/93","1993/94","1994/95","2017/18")  #cannot have incomplete years
    yrs <-  setdiff(yrs,noGoYears)
    
    inputPanel(selectInput("teamYear_fp_front",label=NULL,yrs, selected=yrs[1]))
    
  })
  
  # may be redundant - anyway is duplicate causing problem
  output$c <- renderUI({
    if (input$sbMenu=="st_round") {
      inputPanel(selectInput("seasonA",label=NULL,seasonChoice))
    }
  })
  
  ## may be able to simplify games inputs
  output$standings_ui <- renderUI({ 
    if (input$sbMenu=="st_round") {
      print("enter st_roundUI")
      print(currentRound)
      if (input$seasonA<"1995/96") {
        inputPanel(numericInput("gamesA","Games Played",min=1,max=42,step=1,value=42))
      } else {
        inputPanel(numericInput("gamesA","Games Played",min=1,max=38,step=1,value=currentRound))  
      }
    } else if (input$sbMenu=="st_position") {
               inputPanel(numericInput("positionA","League Position",min=1,max=22,step=1,value=1),
                          numericInput("gamesB","Games Played",min=1,max=42,step=1,value=currentRound)
                          )
    } else if (input$sbMenu=="st_team") {
      inputPanel(selectInput("teamA", label=NULL,selected=values$TEAMNAME, teamsChoice),
                 numericInput("gamesC","Games Played",min=1,max=42,step=1,value=currentRound)
      )
    }  else if (input$sbMenu=="st_date") {
      inputPanel(dateInput("dateA","Enter Date (pending updates)",value=Sys.Date(), startview="year", min="1992-08-15", max=Sys.Date())
      )
    }
  })
      #dateInput("date_1","Enter Date",value="2014-01-01", startview="year"),
#       if (input$seasonA<"1995/96") {

  
  
  ## years available by team
  output$tmSeasonChoice <- renderUI({
    
    if (!is.null(input$team_3)) {
      tm <- input$team_3
    } else {
      tm <- "Arsenal"
    }
    
    "plain text"
    
    
  })
  
  output$tmSeasonChoice_2 <- renderUI({
    
    if (!is.null(input$team_3)) {
      tm <- input$team_3
    } else {
      tm <- "Arsenal"
    }
    
    
    
    tmYrChoice <- tmYrs[tmYrs$team==tm,]$season
    
    selectInput("season_a","",tmYrChoice,selected="2014/15")
  })
  
  ## need to decide whether to do some of this in global
  goalsData <- reactive({
    thePlayer==input$player
    print(thePlayer)  ##
    #  thePlayer <- "HENRYT" needed to do when row_number() was no longer any good had to add vector
    #     temp <-playerGame %>%
    #       filter(PLAYERID==thePlayer&(START+subOn)>0) %>%
    #       arrange(gameDate) %>%
    #       mutate(gameOrder=row_number())
    
    df <- playerGame %>%
      
      filter(PLAYERID==thePlayer&(START+subOn)>0) %>%
      arrange(gameDate) %>%
      mutate(gameOrder=row_number(LASTNAME)) %>%
      select(TEAMMATCHID,PLAYERID,name,PLAYER_MATCH,gameOrder,season,gameDate) %>%
      left_join(goals) %>% ## ? glsTeamSeason
      filter(!is.na(TIME)) %>%
      arrange(gameOrder,TIME) %>%
      mutate(goalOrder=row_number(LASTNAME))
    
    print(df)
    info=list(df=df)  
    return(info)
    
  })
  

  
  
  ## this is from https://demo.shinyapps.io/029-row-selection/ works but nt after sorting and is just row
  ## + has combo of rows
  
  # callback = "function(table) {
  #  table.on('click.dt', 'tr', function() {
  #  $(this).toggleClass('selected');
  # Shiny.onInputChange('rows',
  # table.rows('.selected').indexes().toArray());
  # });
  # }"
  
  
  
  #)
  
  # just a check
  output$rows_out <- renderText({
    paste(c('You selected these rows on the page:', input$row),
          collapse = ' ')
  })
  
  # just a check
  output$teamYearRow_out <- renderText({
    paste(c('You selected these rows on the page:', input$teamYearRow),
          collapse = ' ')
  })
  
  # observe({
  #   input$teamYearRow
  #   print(input$teamYearRow)
  #   if (is.null(input$teamYearRow))  return()
  #   print("again")
  #   print(input$teamYearRow) # gets here fine
  #   if (!is.null(input$teamYearRow)) updateTabsetPanel(session, inputId="tsp_Players", selected="panel1") #, selected= "panel1" made no diff
  #   
  # })
  
  
  
  # ## need to action re datatable
  output$oneYear <- renderDataTable({
    
    if (is.null(input$row)) {
      chosenRow <- 1
    } else {
      chosenRow <- input$row+1
    }
    
    print(chosenRow)
    
    if (!is.null(input$player)) {
      thePlayer <- input$player
    } else {
      thePlayer=="ROONEYX"
    }
    
    tbl <- data.frame(summary %>%
                        filter(PLAYERID==thePlayer))
    tbl <- tbl  %>%    arrange(desc(season))
    
    print(tbl)
    
    year <- tbl[as.character(chosenRow)==row.names(tbl),]$season
    team <- tbl[as.character(chosenRow)==row.names(tbl),]$TEAMNAME
    
    pg<-  playerGame %>%
      filter(PLAYERID==thePlayer&season==year&TEAMNAME==team) %>%
      arrange(desc(gameDate)) %>%
      select(Date=day,Opponents,At=venue,St=st,On=on,Off=off,Gls,Assts=Assists,Card=CARD)
    
    
  },options = list(paging = FALSE, searching = FALSE,info=FALSE,
                   columnDefs= list(columnDefs=list(width="30%",columnDefs.targets= 0),list(className="rt",targets=list(4,5,6,7)))) # not working 
  )  
  
  # 
  # ### team summary all time
  output$teamAllTime <- renderDataTable({
    print("enter table")
    if (!is.null(input$team_3)) {
      theTeam <- input$team_3
    } else {
      theTeam=="Arsenal"
    }
    
    if (!is.null(input$withClub)) {
      withClub <- input$withClub
    } else {
      withClub=="All"
    }
    
    print(withClub)
    
    
    if (withClub=="All") {
      tbl <- summary %>%
        filter(TEAMNAME==theTeam) %>%
        
        mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
        select(Player=name,Age=age,Apps=apps,St,On,Off,Bench,Goals=Gls,Pens,Assists,Points,Y,R,OG,MP)
    } else {
      tbl <- summary %>%
        filter(TEAMNAME==theTeam&is.na(LEFT)) %>%
        
        mutate(apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(season,1,4)),byear=as.integer(str_sub(born,1,4)),age=janAge-byear) %>%
        select(Player=name,Age=age,Apps=apps,St,On,Off,Bench,Goals=Gls,Pens,Assists,Points,Y,R,OG,MP)
    }
    
    
    tbl <- data.frame(tbl)
    
    print(head(tbl))
    
    tbl <- tbl[,-(1:3)]
    # tbl <- tbl[,-(3)]
    print(head(tbl))
    allTime <- tbl %>%
      group_by(PLAYERID,Player) %>%
      summarise(Apps=sum(Apps),St=sum(St),On=sum(On),Off=sum(Off),Bench=sum(Bench),Goals=sum(Goals),
                Pens=sum(Pens),Assists=sum(Assists),Points=sum(Points),Y=sum(Y),R=sum(R),OG=sum(OG),MP=sum(MP)) 
    
    print(head(allTime))
    
    allTime <- data.frame(allTime)
    allTime <- allTime[,-(1)] # get rif of PLAYERID
    allTime
  }
  ,options= list(paging = FALSE, searching = FALSE,
                 # columnDefs=list(width="20%",columnDefs.targets= 0), # looks good at least once
                 order=list(c(1,'desc'),c(2,'desc'),c(5,'desc')),
                 columnDefs= list(columnDefs=list(width="20%",columnDefs.targets= 0),list(className="rt",targets=list(1,2,3,4,5,6,7,8,9,10,11,12,13))))
  
  )
  
  # prob should put this and above table into a reactievas some processing is same
  
  
  
  
  ## Playing for two teams
  dualTeamData <-reactive({
    
    #   print("enter theData")
    #   print(input$teamA)
    
    if (!is.null(input$teamA)) {
      teamA <- input$teamA
    } else {
      teamA <- "Everton"
    }
    if (!is.null(input$teamB)) {
      teamB <- input$teamB
    } else {
      teamB <- "Liverpool"
    }
    
    teams <- c(teamA,teamB)
    # print(teams)
    # glimpse(playerGame)
    col1_id <- playerGame %>% 
      filter(TEAMNAME == teamA&PLAYERID!="OWNGOAL") %>% 
      select(PLAYERID) %>% 
      distinct()
    
    #print(col1_id)
    
    col2_id <- playerGame %>% 
      filter(TEAMNAME == teamB&PLAYERID!="OWNGOAL") %>% 
      select(PLAYERID) %>% 
      distinct()  
    
    a <- col1_id$PLAYERID
    b <- col2_id$PLAYERID
    players <-intersect(a,b) 
    # print(col2_id)
    # print(intersect(col1_id,col2_id))
    # players <-intersect(col1_id,col2_id)$PLAYERID
    
    # print(players)
    # print("? players")
    narrow <- summary %>%
      filter(PLAYERID %in% players&TEAMNAME %in% teams) %>%
      #select(PLAYERI,name,TEAMNAME,St,On) %>%
      group_by(PLAYERID,name,TEAMNAME) %>%
      summarise(apps=(sum(St)+sum(On))) 
    #print(narrow)
    narrow <- data.frame(narrow)
    narrow <- narrow[,c(-1)]
    
    # widen
    
    wide <- narrow  %>% spread(TEAMNAME,apps)
    colnames(wide)[1] <- ""
    print("wide")
    print(wide)
    info=list(wide=wide)
    return(info)
  })
  
  output$twoTeams <- renderDataTable({dualTeamData()$wide},options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))
  
  
  ## teamLeaders
  
  
  
  ### standings
  ## weeks at each position ?club or league
  
  
  # this starts working then stops
  # try the method in example in package - does not cover filtering by input
  # observe({
  #   print("enter weekPos")
  #   print(input$team_4)
  #   print(input$season_4)
  #   if (input$season_4=="All Seasons"){
  #   standings %>%
  #     filter(team==input$team_4) %>%
  #     ggvis(~position) %>%
  #     bind_shiny('weekPos')
  #   } else {
  #     standings %>%
  #       filter(team==input$team_4&season==input$season_4) %>%
  #       ggvis(~position) %>%
  #       bind_shiny('weekPos')
  #   }
  #})
  
  
  
  
  
  
  
  
  
  ## or as but ggplot maybe best to get away from histogram with ggplot
  ## this ignores the games played
  output$weekPos <- renderPlot({
    print("enter weekPos")
    print(input$team_4b)
    print(input$season_4b)
    if (input$season_4b=="All Seasons"){
      standings %>%
        filter(team==input$team_4b) %>%
        group_by(position) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=position,y=count)) + geom_bar(stat="identity", width= .5)
    } else {
      standings %>%
        filter(team==input$team_4b&season==input$season_4b) %>%
        group_by(position) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=position,y=count)) + geom_bar(stat="identity")
    }
  })
  
  
  ## add leaders after this nmber of games
  
  
  
  
  
  
  
  ## FP - leading scorers etc for games played
  
  ### player sequence
  
  
  
  ### goals
  
  
  
#   output$goalDistribution <- renderPlot({
#     goals %>%
#       left_join(playerGame, by="PLAYER_MATCH") %>%
#       filter(PLAYERID==input$player) %>%
#       select(METHOD,PLACE,PLAY,plGameOrderApp)  %>%
#       ggplot(aes(x=METHOD,y=PLACE))+
#       geom_point()+
#       geom_jitter(aes(colour=PLAY),position=position_jitter(width=0.25, height=0.25))+
#       scale_y_discrete(limits=c("6_Yd_Box","Pen_Area","Long_Range"),labels=c("6yd Box","Pen Area","Long Range"))+
#       scale_colour_brewer(palette="Set1")+
#       ggtitle("Method, Place and Play of Goals Scored")
#     
#   })
#   
  
  
  
  # results heatmap
  
  output$resultsSummary <- renderChart({
    
    temp <- standings %>%
      filter(team==input$team_3) %>%
      mutate(combo=paste0(GF,GA)) %>%
      group_by(combo) %>%
      tally()
    
    
    allCombos <- expand.grid(
      data.frame(GF=0:9,GA=0:9)
    ) %>%
      mutate(combo=paste0(GF,GA))
    
    test <- allCombos %>%
      left_join(temp)
    
    #test[is.na(test$n),]$n <- 0
    test <- test %>%
      mutate(count=(n)) # poss relate to max value??
    
    d1 <-  dPlot(
      GA ~ GF
      ,data = test
      ,type = "bar"
      ,yAxis = list( type = "addCategoryAxis" )
      ,colorAxis = list(
        type = "addColorAxis"
        ,colorSeries = 'n'
        ,palette = c('white','red')
        ,outputFormat = "0.01%"
      )
      ,height = 400
      ,width = 400
      ,barGap = 0
    )
    d1$addParams(dom = 'resultsSummary')
    #d1$events = list(
    #  click = "#! function() { Shiny.onInputChange('info',{GF:this.GF, GA:this.GA}) } !#")
    #d1$addEventHandler = list(click="#! function(e) { Shiny.onInputChange('infoX',{console.log(e.xValue)}) } !#")
    return(d1)
  })
  
  output$scorelines <- renderDataTable({
    
    standings %>%
      filter(team==input$team_3&GF==2&GA==2) %>%
      arrange(desc(gameDate)) %>%
      select(Season=season,Date=gameDate,Opponents=OppTeam,Venue=venue)
    
  })
  
  
  ## wil this work if they need stuff from reactive???
  ## players by club single year
  source("code/teamYear.R", local=TRUE)
  source("code/teamLeagueStandings.R", local=TRUE)
  source("code/lineups.R", local=TRUE)
  #   ## front page current Leaders
  #   source("code/currentLeaders.R", local=TRUE)
  #   ## leading scorers by season/team/number of games
  #   source("code/topScorers.R", local=TRUE)
  #   ## team leaders by year goals assists etc
  source("code/teamLeaders.R", local=TRUE)
  #   # Goals For and Ag
  source("code/teamGoals.R", local=TRUE)
  # #   source("code/teamGoalsFor.R", local=TRUE)  ## do not show these they will confuse
  # #   source("code/teamGoalsDiff.R", local=TRUE)
  #   ## player by year
  # source("code/careerTots.R", local=TRUE) rolled into career
  source("code/career.R", local=TRUE)
  #   
  #   
  source("code/goalSummary.R", local=TRUE)
  source("code/headToHead.R", local=TRUE)
  #   
  source("code/standings.R", local=TRUE)
  #   
  source("code/playerSeqs.R", local=TRUE)
  #   
  source("code/playerGoals.R", local=TRUE)
  # source("code/goalDistribution.R", local=TRUE)
  #   
  source("code/goalFirsts.R", local=TRUE)
  #   
  #   source("code/playerWith.R", local=TRUE)
  # 
  source("code/specials/scoredOn.R", local=TRUE)
  source("code/specials/resultsByGameSpan.R", local=TRUE)
  
  source("code/playerAtAGlance.R", local=TRUE)
  source("code/playerPix.R", local=TRUE)
  source("code/playerBirthplace.R", local=TRUE)
  source("code/playerWiki.R", local=TRUE)
  source("code/teamAtAGlance.R", local=TRUE)
  source("code/teamPix.R", local=TRUE)
  source("code/birthChoropleth.R", local=TRUE)
  source("code/topLineup.R", local=TRUE)
  source("code/playerTransfers.R", local=TRUE)
  source("code/teamTwitter.R", local=TRUE)
  source("code/teamSequences.R", local=TRUE)
  source("code/teamSequencesGoals.R", local=TRUE)
  source("code/headlines.R", local=TRUE)
  source("code/playerMilestones.R", local=TRUE)
  source("code/playerGoalDistribution.R", local=TRUE)
  source("code/playerByOpponent.R", local=TRUE)
  source("code/sp_youngest.R", local=TRUE)
  source("code/sp_leadingGoalscorers.R", local=TRUE)
 # source("code/sp_birthplace.R", local=TRUE)
  source("code/playerComparisons.R", local=TRUE)
  source("code/allPlayerGoalSeqs.R", local=TRUE)
  source("code/specials/twoClubs.R", local=TRUE)
  source("code/specials/yearOnYearChanges.R", local=TRUE)
  source("code/specials/tmGoalsSince.R", local=TRUE)
  source("code/teamHeatMap.R", local=TRUE)
  source("code/specials/pcFullGames.R", local=TRUE)
  source("code/standingLeaders.R", local=TRUE)
  source("code/standingsBoxplot.R", local=TRUE)
  source("code/specials/pcPlayerGoals.R", local=TRUE)
  source("code/specials/deficitsOvercome.R", local=TRUE)
  source("code/managerPPG.R", local=TRUE)
  source("code/player_ppg.R", local=TRUE)
  source("code/specials/playerByTeamPPG.R", local=TRUE)
  source("code/specials/playerByCountryPPG.R", local=TRUE)
  source("code/specials/cardsPerClub.R", local=TRUE)
  source("code/specials/finishingPos.R", local=TRUE)
  source("code/managerPlayersAge.R", local=TRUE)
  source("code/specials/vTopClubs.R", local=TRUE)
  source("code/playerDroughts.R", local=TRUE)
  source("code/assistScorerCombo.R", local=TRUE)
 # source("code/bookmark.R", local=TRUE)
  
  ##  observeevent for clicking on a row and jumping to a players
  ## record 
  
  observeEvent(input$mostGames_rows_selected,{
    s = as.integer(input$mostGames_rows_selected)
    values$playerID <- teamData()$mostGames$PLAYERID[s]
    updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  })
  
  observeEvent(input$mostGoals_rows_selected,{
    s = as.integer(input$mostGoals_rows_selected)
    values$playerID <- teamData()$mostGoals$PLAYERID[s]
    updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  })
  
  observeEvent(input$mostAssists_rows_selected,{
    s = as.integer(input$mostAssists_rows_selected)
    values$playerID <- teamData()$mostAssists$PLAYERID[s]
    updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  })
  
  observeEvent(input$mostCards_rows_selected,{
    s = as.integer(input$mostCards_rows_selected)
    values$playerID <- teamData()$mostCards$PLAYERID[s]
    updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  })
  
  ### need to get this working
  # observeEvent(input$teamYear_rows_selected,{
  #   s = as.integer(input$teamYear_rows_selected)
  #   values$playerID <- teamData()$teamYear$PLAYERID[s]
  #   updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  # })
  
  ## looking to go from heatmap table to showing scorers - may be better elsewhere
  
  observeEvent(input$heatTable_rows_selected,{
    s = as.integer(input$heatTable_rows_selected)
    print(s)
    print(glimpse(heatData()$df))
    values$MATCHID <- heatData()$df$MATCHID[s]
    print(values$MATCHID)
    #updateTabItems(session, inputId="sbMenu", selected="pl_glance")
  })
  
  
  ## add a fixed notification
  
  id <- NULL
  
  observe({
    
    
    id <<- showNotification(
      "Tweet Handle @pssguy",
      duration = NULL, 
      closeButton = TRUE,
      type = "message"
    )
  })
  
  
  ## wan to re-address
#   observeEvent(input$teamYear_rows_selected,{
#     s = as.integer(input$teamYear_rows_selected)
#     print("printing s")
#     
# tbl <-    data.frame(summary %>%
#                  filter(TEAMNAME==input$teamA&season==input$teamYears)) #%>%
#                  
# #                  mutate(pos=str_sub(POSITION,1,1),apps=St+On,Gls=StGls+subGls,Pens=startPens+subPens,Points=Gls+Assists,janAge=as.integer(str_sub(as.character(season),1,4)),byear=as.integer(str_sub(as.character(born),1,4)),age=janAge-byear) %>%
# #                  select(PLAYERID,Player=name,pos,Age=age,Apps=apps,St,On,Off,Bench,Mins=mins,Goals=Gls,Pens,Assists,Points,Y,R)) -> tbl
# #     
# print(tbl$PLAYERID) # cazorla is 27t
#     print(s) # All Single default hit row 2 but s= 27(perhaps the sorting). However did give CAZORLS  which was what was after
#    print(tbl$PLAYERID[s])
#     values$playerID <- tbl$PLAYERID[s]
#     updateTabItems(session, inputId="sbMenu", selected="pl_glance")
#   })
  
}) # end




