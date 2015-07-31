

dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Premier League"),
  
  dashboardSidebar(
    #selectInput("teamA", "Team", teamsChoice),
    uiOutput("a"),
    uiOutput("teamYear_ui"),
    uiOutput("c"),
    uiOutput("standings_ui"),
    uiOutput("position_ui"),
   # uiOutput("tests_ui"),
    
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Teams", tabName = "teams",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "tm_glance"),
        menuSubItem("Player Summary",tabName = "tm_playerSummary", selected = TRUE),
        menuSubItem("League Position",tabName = "tm_leaguePosition"),
        menuSubItem("Goals",tabName = "tm_goals"),
        menuSubItem("Team Leaders",tabName = "tm_leaders"),
        menuSubItem("Head to Head",tabName = "tm_hth"),
        menuSubItem("Sequences",tabName = "tm_seqs")
      ),
      
      
      
      menuItem(
        "Standings", tabName = "standings",icon = icon("table"),
        menuSubItem("By Round", tabName = "st_round"),
        menuSubItem("By Position", tabName = "st_position")
      ),
      
      
      menuItem(
        "Players", tabName = "players",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "pl_glance"),
        menuSubItem("Career Summary", tabName = "pl_career"),
        menuSubItem("Goal record", tabName = "pl_goals")
        
      ),
      
      
      menuItem(
        "Specials", tabName = "specials",
        menuSubItem("Scored On",tabName = "sp_scoredOn")
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
      tabItem("standings"),
      tabItem(
        "players",
        box(
          status = "warning",solidHeader = TRUE,title = "test image",
          
          htmlOutput("playerPix")
        )
        
        
      ),
      
      tabItem("tm_glance",
fluidRow(
 # box(width=3,htmlOutput("teamPic")),
  box(width=6,htmlOutput("squadPhoto")),
  
#   box(
#     status = "warning",solidHeader = TRUE,title = "test image",
#     
#     htmlOutput("teamPic")
#   ),
box(title="EPL Finishing Positions",width=3,solidHeader = TRUE,status = 'success',
ggvisOutput("seasonsHist")
),
#box(width=3,htmlOutput("teamPic")), # a logo really
box(width=3,plotOutput("birthChoropleth"))
),
              
fluidRow(
  
  box(title="Most Appearances",width=3,solidHeader = TRUE,status = 'success',
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
                  width = 12,status = "success",solidHeader = TRUE,title = "Player Summary",
                  inputPanel(
                    # uiOutput("teamYear_ui"),
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
                    width = 12, collapsible = T,collapsed =T,
                    
                    DT::dataTableOutput("hthFixtures")
                  )
                )
              )),
      
      tabItem(
        "tm_seqs",
        box(
          title = "Maybe look at index.rmd", solidHeader = TRUE,status =
            'success',
          width = 12
          
        )
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
              htmlOutput("playerPic")
            )
          ),
          column(
            width = 3,
            box(
              width = 12,title = "Birth Place",solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = FALSE,
              leafletOutput("playerBirthplace", height = 200)
            )
          ),
          column(
            width = 5,
            box(
              width = 12,title = "Wikipedia",solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("playerWiki")
            )
          )
          
          
        ),
      #  hr(),
        
        fluidRow(
          column(width = 3,
                 infoBoxOutput("teamsBox", width = 12)),
          
          column(width = 3,
                 infoBoxOutput("seasonsBox", width = 12)),
          
          column(width = 3,
                 infoBoxOutput("appsBox", width = 12))
        ),
        fluidRow(
          column(width = 3,
                 infoBoxOutput("goalsBox", width = 12)),
          column(width = 3,
                 infoBoxOutput("assistsBox", width = 12)),
          column(width = 3,
                 infoBoxOutput("cardsBox", width = 12))
        )
        
        
        
      ),
      
      
      
      tabItem(
        "pl_career",
        box(
          width = 12,title = "Game Summaries. Point size relates to Goals/Assists. Hover for details. Values of 99 indicate time of substitution unknown",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          ggvisOutput("careerChart")
        )
        ,
        box(
          width = 12,title = "Career",solidHeader = TRUE,status = 'success',
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
          collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput("goalSummary")
        ),
        box(
          width = 12,title = "Goal Charts",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          
          radioButtons(
            "method","",choices = c("Method","Place","Play"), inline = TRUE
          ),
          
          ggvisOutput("playerGoals")
        )
        ,
        #         box(
        #           width = 12,title = "Goal Distribution",solidHeader = TRUE,status = 'success',
        #           collapsible = TRUE, collapsed = TRUE,
        #           plotOutput("goalDistribution")
        #         ),
        box(
          width = 12,title = "Goal Sequences",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = TRUE,
          plotOutput("bestRunA")
        ),
        box(
          width = 6,title = "Goal Firsts",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("goalFirsts")
        )
        
      ),
      
      ## specials
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
      
      
      
      
      
      
 
      
      tabItem("info", includeMarkdown("info.md"))
      
      
      
      
      
      
      
    ) # tabItems
  ) # body
) # page
