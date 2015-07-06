




dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Premier League"),
  
  dashboardSidebar(
    uiOutput("a"),
    uiOutput("teamYear_ui"),
    
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Teams", tabName = "teams",icon = icon("table"),
        menuSubItem("Player Summary",tabName = "tm_playerSummary"),
        menuSubItem("League Position",tabName = "tm_leaguePosition"),
        menuSubItem("Goals",tabName = "tm_goals"),
        menuSubItem("Team Leaders",tabName = "tm_leaders"),
        menuSubItem("Head to Head",tabName = "tm_hth"),
        menuSubItem("Sequences",tabName = "tm_seqs")
      ),
      
      
      
      menuItem("Standings", tabName = "standings",icon = icon("table")),
      menuItem("Players", tabName = "players",icon = icon("table")),
      menuItem(
        "Specials", tabName = "specials",
        menuSubItem("Scored On",tabName = "sp_scoredOn", selected = TRUE)
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
      tabItem("test"),
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
                    title = "Head to Head", solidHeader = TRUE,status = 'success',
                    width = 12,
                    
                    DT::dataTableOutput("hthTable")
                  )
                ),
                
                column(
                  width = 5,
                  
                  box(
                    title = "Fixtures", solidHeader = TRUE,status = 'success',
                    width = 12,
                    
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
      
      ## specials
      tabItem(
        "sp_scoredOn",
        fluidRow(column(
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
            title = "", solidHeader = TRUE,status = 'success',
            width = 12,
            DT::dataTableOutput("scoredOn_dets")
          )
        )),
        fluidRow(
          column(width=8,offset=2,
                 
                 box(
                   title = "Hover for Opponent.     Points are jittered to aid viewing", solidHeader = TRUE,status = 'success',
                   width = 12,
                   DT::dataTableOutput("scoredOnChart")
                 )      
                 )
        )
        
        
      ),
      
      
      
      
      
      ### Players section
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
      #
      #
      #                            inputPanel(
      #                              selectInput("team_3","",teamsChoice, selected="Arsenal"),
      #
      #                              uiOutput('tmSeasonChoice')
      #                            )
      #       ),
      
      
      tabItem("info", includeMarkdown("info.md"))
      
      
      
      
      
      
      
    ) # tabItems
  ) # body
) # page
