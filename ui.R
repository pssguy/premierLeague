dashboardPage(title="BPL",
  skin = "yellow",
  dashboardHeader(title = "Barclays Premier League", titleWidth = 300),
  
  dashboardSidebar(
    includeCSS("custom.css"),
    #selectInput("teamA", "Team", teamsChoice),
    uiOutput("a"),
    uiOutput("teamYear_ui"),
    uiOutput("c"),
    uiOutput("standings_ui"),
    uiOutput("position_ui"),
   # uiOutput("tests_ui"),
    
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem("Front Page",tabName="frontPage"),
      
      menuItem(
        "Teams", tabName = "teams",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "tm_glance"),
        menuSubItem("Player Summary",tabName = "tm_playerSummary"),
        menuSubItem("League Position",tabName = "tm_leaguePosition"),
        menuSubItem("Goals",tabName = "tm_goals"),
        menuSubItem("Team Leaders",tabName = "tm_leaders"),
        menuSubItem("Head to Head",tabName = "tm_hth"),
        menuSubItem("Sequences-Results",tabName = "tm_seqs")#,
        #menuSubItem("Sequences-Goals",tabName = "tm_seqs_goals")
      ),
      
      
      menuItem(
        "Players", tabName = "players",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "pl_glance"),
        menuSubItem("Career Summary", tabName = "pl_career"),
        menuSubItem("Goal Details", tabName = "pl_goals"),
        menuSubItem("Sequences-Goals",tabName = "pl_seqs_goals"),
        menuSubItem("By Opposition",tabName = "pl_opponent")
      ),
        
      menuItem(
        "Standings", tabName = "standings",icon = icon("table"),
        menuSubItem("By Round", tabName = "st_round"),
        menuSubItem("By Position", tabName = "st_position"),
        menuSubItem("By Team", tabName = "st_team"),
        menuSubItem("By Date", tabName = "st_date")
      ),
      
      
 
      
      
      menuItem(
        "Specials", tabName = "specials",
        
        menuSubItem("Birthplace",tabName = "sp_birthplace", selected=T),
        menuSubItem("Leading GoalScorers",tabName = "sp_goalScorers"),
        menuSubItem("Results By Game Span",tabName = "sp_resSpan"),
        menuSubItem("Scored On",tabName = "sp_scoredOn"),
        menuSubItem("Youngest Players",tabName = "sp_youngest")
        
        
      ),
      menuItem(
        "Other Dashboards",
        menuSubItem("Climate",href = "https://mytinyshinys.shinyapps.io/climate"),
        menuSubItem("Cricket",href = "https://mytinyshinys.shinyapps.io/cricket"),
        menuSubItem("MainlyMaps",href = "https://mytinyshinys.shinyapps.io/mainlyMaps"),
        menuSubItem("MLB",href = "https://mytinyshinys.shinyapps.io/mlbCharts"),
        menuSubItem("WikiGuardian",href = "https://mytinyshinys.shinyapps.io/wikiGuardian"),
        menuSubItem("World Soccer",href = "https://mytinyshinys.shinyapps.io/worldSoccer")
      ),
      
      
      menuItem("Info", tabName = "info", icon = icon("info")),
      
      menuItem("", icon = icon("twitter-square"),
               href = "https://twitter.com/pssGuy"),
      menuItem("", icon = icon("envelope"),
               href = "mailto:agcur@rogers.com")
      
    )
  ),
  dashboardBody(
    tabItems(
      #tabItem("standings"),
      
      ## Front Page
      tabItem("frontPage",
              fluidRow(
              column(width=4,
                     box(width=12,
                         status = "warning",solidHeader = TRUE,title = "Not Just Another EPL site ",
                         collapsible = T,collapsed =F,
                         includeMarkdown("frontPage.md")
                     ),
                     
              box(width=12,
                status = "success",solidHeader = TRUE,title = "Current Team Sequences",
                collapsible = T,collapsed =T,
                DT::dataTableOutput("teamSeqCurrent")
              ),
              box(width=12,
                  status = "success",solidHeader = TRUE,title = "Player Milestones",
                  collapsible = T,collapsed =T,
                  DT::dataTableOutput("playerMilestones")
              ),
              box(width=12,
                  status = "success",solidHeader = TRUE,title = "Twitter Feed",
                  collapsible = T,collapsed =T,
                  tags$body(includeScript("twitter.js"),
                            
                            
                            a("Soccer", class="twitter-timeline",
                              width="320",
                              href="https://twitter.com/pssGuy/timelines/524678699061641216",
                              "data-widget-id" = "524686407298596864",
                              "data-chrome" ="nofooter transparent noheader")
                            )
                  
              ),
              box(width=12,
                  status = "success",solidHeader = TRUE,title = "What's New",
                  collapsible = T,collapsed =T,
                  includeMarkdown("whatsNew.md")
              )
              ),
              column(width=8,
                     box(width=12,
                         status = "success",solidHeader = TRUE,title = "Highlighted App",
                         collapsible = T,collapsed =F,
                         helpText("Reece Oxford made an impressive debut as West Ham's youngest ever Premier League Player but his predecessor
                                  never played an EPL game again. Click for Team. Hover for Details"),
                         selectInput("teamC", label=NULL,teamsChoice,selected="West Ham U" , width=150),
                         ggvisOutput("ageRecord")
                     ),
              box(width=12,
                  status = "success",solidHeader = TRUE,title = "Team Leaders (Ties not shown)",
                  collapsible = T,collapsed =T,
                  DT::dataTableOutput("teamLeadersCurrent")
              )
              )
              )  
              ),
      
      
      ## Player Section
      tabItem(
        "players",
        box(
          status = "warning",solidHeader = TRUE,title = "test image",
          
          htmlOutput("playerPix")
        )
        
        
      ),
      
      tabItem("tm_glance",
fluidRow(
 tabBox(
#    tabPanel("Where in the World (click for details)",
#             leafletOutput("teamLeaflet")), # height='90%', width='90%' produces blank area also seemed to impact other tables on page!!!!!
   tabPanel("Squad Photo",htmlOutput("squadPhoto"))
 #tabPanel("Where in the World",plotOutput("birthChoropleth")),
# tabPanel("Where in the World (click for details)",helpText("Available Soons")) 
 #
),
box(title="EPL Finishing Positions",width=4,height=375,solidHeader = TRUE,status = 'success',

plotOutput("seasonsHist")
),
box(title="Top LineUp",width=2,solidHeader = TRUE,status = 'success',
    h5(textOutput("lineupCount")),
    hr(),
textOutput("lineupText"))


),
              
fluidRow(
  
  box(title="Most Apps (click row for player)",width=3,solidHeader = TRUE,status = 'success',
      DT::dataTableOutput("mostGames")
  ),
  box(title="Most Goals",width=3,solidHeader = TRUE,status = 'success',
      DT::dataTableOutput("mostGoals")
  ),
  box(title="Most Assists (max 2 per goal)",width=3,solidHeader = TRUE,status = 'success',
      DT::dataTableOutput("mostAssists")
  ),
  box(title="Most Cards",width=3,solidHeader = TRUE,status = 'success',
      DT::dataTableOutput("mostCards")
  )
)
),
      
      
      tabItem("tm_playerSummary",
              
              fluidRow(
                box(
                  width = 12,status = "success",solidHeader = TRUE,title = "Player Summary ",
                  inputPanel(
                   
                    radioButtons(
                      "withClub","Players",choices = c("All","Current"),inline = TRUE
                    ),
                    radioButtons(
                      "seasons","Seasons",choices = c("All","Single"), selected = "Single",inline = TRUE
                    )
                  ),
                  DT::dataTableOutput('teamYear')
                )
                
              )),
      
      
      tabItem("tm_leaguePosition",
              
              fluidRow(
                box(
                  width = 6,
                  status = "success",solidHeader = TRUE,title = "Positon by Round. Hover points for Result, click for lineup",
                  ggvisOutput("posGraph")
                ),
                box(
                  width = 6,
                  status = "success",solidHeader = TRUE,title = "Team Lineup",
                  DT::dataTableOutput("lineup")
                )
              )),
      
      tabItem(
        "tm_goals",
        box(
          title = "Team Goals For and Against", solidHeader = TRUE,status = 'success',
          width = 12,
          radioButtons("tmGoals",'',c("For","Against","Difference"), inline =
                         TRUE),
          DT::dataTableOutput("teamGoalsFor")
        )
        ## need to add some charts
        
      ),
      
      tabItem(
        "tm_leaders",
        box(
          title = "Team Leaders. Ties are not currently shown", solidHeader = TRUE,status =
            'success',
          width = 12,
          
          DT::dataTableOutput("teamLeaders")
        )
        ## need to add some charts
        
      ),
      tabItem("tm_hth",
              fluidRow(
                column(
                  width = 7,
                  
                  box(
                    title = "Head to Head Click for fixture details", solidHeader = TRUE,status = 'success',
                    width = 12,
                    
                    DT::dataTableOutput("hthTable")
                  )
                ),
                
                column(
                  width = 5,
                  
                  box(
                    title = "Fixtures", solidHeader = TRUE,status = 'success',
                    width = 12, collapsible = T,collapsed =F,
                    
                    DT::dataTableOutput("hthFixtures")
                  )
                )
              )),
#       
#       tabItem(
#         "tm_seqs",
#         radioButtons(
#           "seqVenue","Venue",choices = c("All","Home","Away"),inline = TRUE
#         ),
# fluidRow(column(width=2,plotOutput("tm_wins")),
#          column(width=2,plotOutput("tm_noWins")),
#          column(width=2,plotOutput("tm_draws")),
#          column(width=2,plotOutput("tm_noDraws")),
#          column(width=2,plotOutput("tm_losses")),
#          column(width=2,plotOutput("tm_noLosses"))
# ),

tabItem(
  "tm_seqs",
  box(width=12, height= 350,title = "Result Sequences - Most Recent in Bold",
      solidHeader = TRUE,status = 'success',
      collapsible = TRUE, collapsed = FALSE,
  radioButtons(
    "seqVenue","Venue",choices = c("All","Home","Away"),inline = TRUE
  ),
  fluidRow(column(width=2,plotOutput("tm_wins")),
           column(width=2,plotOutput("tm_noWins")),
           column(width=2,plotOutput("tm_draws")),
           column(width=2,plotOutput("tm_noDraws")),
           column(width=2,plotOutput("tm_losses")),
           column(width=2,plotOutput("tm_noLosses"))
  )),
fluidRow(column(width=4,
         box(width=12,title = "Most Recent Record Run - Wins",
             solidHeader = TRUE,status = 'success',
             collapsible = TRUE, collapsed = TRUE,
             DT::dataTableOutput("tmWinSeq"))
),
column(width=4,
       box(width=12,title = "Most Recent Record Run - Draws",
           solidHeader = TRUE,status = 'success',
           collapsible = TRUE, collapsed = TRUE,
           DT::dataTableOutput("tmDrawSeq"))
),
column(width=4,
       box(width=12,title = "Most Recent Record Run - Defeats",
           solidHeader = TRUE,status = 'success',
           collapsible = TRUE, collapsed = TRUE,
           DT::dataTableOutput("tmLossSeq"))
)
),
fluidRow(column(width=4,
                box(width=12,title = "Most Recent Record Run - No Wins",
                    solidHeader = TRUE,status = 'success',
                    collapsible = TRUE, collapsed = TRUE,
                    DT::dataTableOutput("tmNoWinSeq"))
),
column(width=4,
       box(width=12,title = "Most Recent Record Run - No Draws",
           solidHeader = TRUE,status = 'success',
           collapsible = TRUE, collapsed = TRUE,
           DT::dataTableOutput("tmNoDrawSeq"))
),
column(width=4,
       box(width=12,title = "Most Recent Record Run - Undefeated",
           solidHeader = TRUE,status = 'success',
           collapsible = TRUE, collapsed = TRUE,
           DT::dataTableOutput("tmNoLossSeq"))
)
)
      ),
tabItem(
  "tm_seqs_goals",
  radioButtons(
    "seqVenueB","Venue",choices = c("All","Home","Away"),inline = TRUE
  ),
  fluidRow(column(width=3,plotOutput("tm_goalFor"))
#   fluidRow(column(width=2,plotOutput("tm_goalFor")),
#            column(width=2,plotOutput("tm_noWins")),
#            column(width=2,plotOutput("tm_draws")),
#            column(width=2,plotOutput("tm_noDraws")),
#            column(width=2,plotOutput("tm_losses")),
#            column(width=2,plotOutput("tm_noLosses"))
  ),
  fluidRow(column(width=6,
                  box(width=12,title = "Most Recent Record Run - Goals For",
                      solidHeader = TRUE,status = 'success',
                      collapsible = TRUE, collapsed = TRUE,
                      DT::dataTableOutput("tmSeqGF"))
  ))
#   column(width=4,
#          box(width=12,title = "Most Recent Record Run - Draws",
#              solidHeader = TRUE,status = 'success',
#              collapsible = TRUE, collapsed = TRUE,
#              DT::dataTableOutput("tmDrawSeq"))
#   ),
#   column(width=4,
#          box(width=12,title = "Most Recent Record Run - Defeats",
#              solidHeader = TRUE,status = 'success',
#              collapsible = TRUE, collapsed = TRUE,
#              DT::dataTableOutput("tmLossSeq"))
#   )
#   ),
#   fluidRow(column(width=4,
#                   box(width=12,title = "Most Recent Record Run - No Wins",
#                       solidHeader = TRUE,status = 'success',
#                       collapsible = TRUE, collapsed = TRUE,
#                       DT::dataTableOutput("tmNoWinSeq"))
#   ),
#   column(width=4,
#          box(width=12,title = "Most Recent Record Run - No Draws",
#              solidHeader = TRUE,status = 'success',
#              collapsible = TRUE, collapsed = TRUE,
#              DT::dataTableOutput("tmNoDrawSeq"))
#   ),
#   column(width=4,
#          box(width=12,title = "Most Recent Record Run - Undefeated",
#              solidHeader = TRUE,status = 'success',
#              collapsible = TRUE, collapsed = TRUE,
#              DT::dataTableOutput("tmNoLossSeq"))
#   )
  
),
      ## Standings section
      tabItem(
        "st_round",
        box(
          width = 6,
          title = "Standings",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("st_round")
        )
      ),
      tabItem(
        "st_position",
        box(
          width = 6,
          title = "Standings",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("st_position")
        ),
        box(
          width = 6,
          title = "Standings",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          plotOutput("st_position_chart")
        )
      ),
tabItem(
  "st_team",
  box(
    width = 6,
    title = "Standings By Team By Round",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("st_team")
  )
),
tabItem(
  "st_date",
  box(
    width = 4,
    title = "Standings on Chosen Date",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("st_dateNow")
  ),
  box(
    width = 4,
    title = "Standings Rest of Season",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("st_dateLater")
  ),
  box(
    width = 4,
    title = "Standings Full Year",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("st_dateSeason")
  )
),
      
      
      
      ### Players section
      
      tabItem(
        "pl_glance",
        
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,title = "In Action",solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = FALSE,
              htmlOutput("playerPic", height=250)
            )
          ),
          column(
            width = 3,
            box(
              width = 12,title = "Birth Place",solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = FALSE,
              leafletOutput("playerBirthplace", height = 250)
            )
          ),
          column(
            width = 5,
            box(
              width = 12,title = "Permanent Transfers (hover for details)",solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = FALSE,
             tauchartsOutput("playerTransfers_tau", height="250px")
            )
          )
          
          
        ),
      #  hr(),
        
        fluidRow(
          column(width = 3,
                 infoBoxOutput("teamsBox", width = 12)),
          
          column(width = 3, offset =1,
                 infoBoxOutput("seasonsBoxPlayer", width = 12)),
          
          column(width = 3, offset =1,
                 infoBoxOutput("appsBox", width = 12))
          
        ),
        fluidRow(
          column(width = 3,
                 infoBoxOutput("goalsBox", width = 12)),
          column(width = 3,offset =1,
                 infoBoxOutput("assistsBox", width = 12)),
          column(width = 3,offset =1,
                 infoBoxOutput("cardsBox", width = 12))
        ),
      fluidRow(
        column(width = 5,offset=3,
               box(
          width = 12,title = "Wikipedia (includes non-EPL data)",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          uiOutput("playerWiki")
        )
        )
      )
        
        
      ),
      
      
      
      tabItem(
        "pl_career",
        fluidRow(column(width=7,
        box(
          width = 12,title = "Game Summaries. Point size relates to Goals/Assists. Hover for details. Values of 99 indicate time of substitution unknown",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          ggvisOutput("careerChart")
        )),
        column(width=5,
        box(
          width = 12,title = "Points per 90 Minutes Hover for details",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
        #  ggvisOutput("pointsByYearChart")
          tauchartsOutput("pointsByYearChart")
        )
        )
        ),
        
        box(
          width = 12,title = "By Club",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput("career")
        )
        ,
        box(
          width = 12,title = "By Season",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          
          DT::dataTableOutput("careerYear")
        )
      ),
      
      tabItem(
        "pl_goals",
        box(
          width = 12,title = "Goal Summary Table",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("goalSummary")
        ),
        fluidRow(column(width=7,
        box(
          width = 12,title = "Goals By Year",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          
          radioButtons(
            "method","",choices = c("Method","Place","Play"), inline = TRUE
          ),
          
          ggvisOutput("playerGoals")
        )
        ),
        column(width=5,
               box(
                 width = 12,title = "Goals Distribution (Points are jittered)",solidHeader = TRUE,status = 'success',
                 collapsible = TRUE, collapsed = TRUE,
                 
                 
                 plotOutput("playerGoalDistribution")
               )
  
        )
        )
        ),
        tabItem(
          "pl_seqs_goals",
          box(width=12, height= 400,title = "Scoring Sequences - Most Recent in Bold",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = FALSE,
          radioButtons(
            "seqPlVenue","Venue",choices = c("All","Home","Away"),inline = TRUE
          ),
          fluidRow(
            column(width=3,plotOutput("gameGoal")),
          column(width=3,plotOutput("gameNoGoal")),
column(width=3,plotOutput("gameGoalStarter")),
column(width=3,plotOutput("gameNoGoalStarter"))
)),

fluidRow(
  box(width=6,
      title = "Individual Goal Sequences By Date - All Games",solidHeader = TRUE,status = 'success',
      collapsible = TRUE, collapsed = TRUE,
      plotOutput("gameGoalSeq")
      ),
  box(width=6,
      title = "Individual Goal Sequences By Date - As Starter",solidHeader = TRUE,status = 'success',
      collapsible = TRUE, collapsed = TRUE,
      plotOutput("gameGoalSeqStarter")
     )
  
)
          ),

tabItem(
  "pl_opponent",   
  box(
    width = 7,title = "Summary By Opponent - click for Game Info",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("playerByOpponent")
     ),
  box(
    width = 5,title = "Games By Selected Opponent",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    DT::dataTableOutput("plOpponentSummary")
  )
  
),
      ## specials

tabItem("sp_goalScorers", 
        box(width=12,
            status = "success",solidHeader = TRUE,title = "Leading Goalscorers. Amend minimum goals as required. Teams can be de(selected) via legend. Hover for details",
            collapsible = T,collapsed =F,
            helpText("Elite Goalscorers are always difficult to find which made home-grown Harry Kane's 21 league goals scored last year such a fairy tale
                     "),
            div(style = "display:inline-block",numericInput("goalA", label="Min Goals",value=20,min=1,max=40,width=80)),
           # div(style = "display:inline-block",inputPanel(selectInput("teamE", label=NULL, teamsChoice))),
           div(style = "display:inline-block",selectInput("teamE", label="Select Teams", c("All Teams",teamsChoice), selected=c("All Teams"), multiple = T, width=150)), 
           tauchartsOutput("leadingGoalscorers_tau")
            )),


tabItem("sp_resSpan", 
        box(width=12,
            status = "success",solidHeader = TRUE,title = "Results By Game Span (takes a few seconds)",
            collapsible = T,collapsed =F,
            sliderInput("spanA", label="Game Span",value=10,min=2,max=20),
            radioButtons("spanVenue", label=NULL, choices=c("All","Home","Away"),inline =T),
            radioButtons("result", label=NULL, choices=c("Win","Loss","Draw","No Win","No Loss","No Draw"),inline=T),
            
           # div(style = "display:inline-block",sliderInput("spanA", label="Game Span",value=10,min=2,max=20),selectInput("spanVenue", label="NULL", choices=c("All","Home","Away"))),
           #  div(style = "display:inline-block",selectInput("spanVenue", label="NULL", choices=c("All","Home","Away"))),
           # div(style = "display:inline-block",selectInput("teamE", label="Select Teams", c("All Teams",teamsChoice), selected=c("All Teams"), multiple = T, width=150)), 
            DT::dataTableOutput("resByGameSpan")
        )),

tabItem("sp_youngest", 
        box(width=12,
                           status = "success",solidHeader = TRUE,title = "Youngest Players",
                           collapsible = T,collapsed =F,
                           helpText("Reece Oxford made an impressisve debut as West Ham's youngest ever Premier League Player but his predecessor
                                  never played an EPL game again. Click for Team. Hover for Details"),
                           selectInput("teamD", label=NULL,teamsChoice,selected="West Ham U" , width=150),
                           ggvisOutput("sp_ageRecord")
)),
      tabItem("sp_scoredOn",
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Teams Scored On. Click for Details", solidHeader = TRUE,status = 'success',
                    width = 12,
                    DT::dataTableOutput("scoredOn")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Goals by Opponent", solidHeader = TRUE,status = 'success',
                    width = 12,
                    DT::dataTableOutput("scoredOn_dets")
                  )
                )
              ),
              fluidRow(column(
                width = 8,offset = 2,
                
                box(
                  title = "Hover for Opponent.     Points are jittered to aid viewing", solidHeader = TRUE,status = 'success',
                  width = 12,
                  DT::dataTableOutput("scoredOnChart")
                )
              ))),
      
      
tabItem("sp_birthplace", 
        
        ggvisOutput("sp_birthplaceChart")
        ),   
      
      
      
 
      
      tabItem("info", includeMarkdown("info.md"))
      
      
      
      
      
      
      
    ) # tabItems
  ) # body
) # page
