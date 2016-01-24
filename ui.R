dashboardPage(
  title = "BPL",
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
      
      menuItem("Front Page",tabName = "frontPage"),
      menuItem("Managers",tabName = "managers"),
      
      menuItem(
        "Teams", tabName = "teams",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "tm_glance"),
        menuSubItem("Player Summary",tabName = "tm_playerSummary"),
        menuSubItem("League Position",tabName = "tm_leaguePosition"),
        menuSubItem("Goals",tabName = "tm_goals"),
        menuSubItem("Team Leaders",tabName = "tm_leaders"),
        menuSubItem("Head to Head",tabName = "tm_hth"),
        menuSubItem("Scoreline Heatmap",tabName = "tm_heat"),
        menuSubItem("Sequences-Results",tabName = "tm_seqs")#,
        #menuSubItem("Sequences-Goals",tabName = "tm_seqs_goals")
      ),
      
      
      menuItem(
        "Players", tabName = "players",icon = icon("table"),
        menuSubItem("At A Glance", tabName = "pl_glance"),
        menuSubItem("By Opposition",tabName = "pl_opponent"),
        menuSubItem("Career Summary", tabName = "pl_career"),
        menuSubItem("Goal Details", tabName = "pl_goals"),
        menuSubItem("Points per Game", tabName = "pl_ppg"),
        menuSubItem("Sequences-Goals",tabName = "pl_seqs_goals")
       
      ),
      
      menuItem(
        "Standings", tabName = "standings",icon = icon("table"),
        menuSubItem("All Seasons", tabName = "st_boxplot"),
        menuSubItem("By Round", tabName = "st_round"),
        menuSubItem("By Position", tabName = "st_position"),
        menuSubItem("By Team", tabName = "st_team"),
        menuSubItem("By Date", tabName = "st_date"),
        menuSubItem("Leaders by Season", tabName = "st_leaders")
      ),
      
      
      
      
      
      menuItem(
        "Specials", tabName = "specials",
        menuSubItem("Best Goal Sequences",tabName = "sp_plGoalSeqs"),
      #  menuSubItem("Birthplace",tabName = "sp_birthplace"),
        menuSubItem("Deficits Overcome",tabName = "sp_deficits"),
        menuSubItem("Games Since Goal Tally",tabName = "sp_tmGoalsSince"),
        menuSubItem("Leading GoalScorers",tabName = "sp_goalScorers"),
        menuSubItem("Percent Full Games",tabName = "sp_pcFullGames"),
        menuSubItem("Player % Goals by Category",tabName = "sp_pcPlayerGoals"),
        menuSubItem("Played for 2 clubs",tabName = "sp_twoClubs"),
        menuSubItem("Player Comparisons",tabName = "sp_comparisons"),
        menuSubItem("Results By Game Span",tabName = "sp_resSpan"),
        menuSubItem("Scored On",tabName = "sp_scoredOn"),
        # menuSubItem("Year on Year Changes",tabName = "sp_yearOnYear"),
        menuSubItem("Youngest Players",tabName = "sp_youngest")
        
        
      ),
      tags$hr(),
      menuItem(
        text = "",href = "https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"
      ),
      tags$hr(),
      
      tags$body(
        a(
          class = "addpad",href = "https://twitter.com/pssGuy", target = "_blank",img(src =
                                                                                        "images/twitterImage25pc.jpg")
        ),
        a(
          class = "addpad2",href = "mailto:agcur@rogers.com", img(src = "images/email25pc.jpg")
        ),
        a(
          class = "addpad2",href = "https://github.com/pssguy",target = "_blank",img(src =
                                                                                       "images/GitHub-Mark30px.png")
        ),
        a(
          href = "https://rpubs.com/pssguy",target = "_blank",img(src = "images/RPubs25px.png")
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      ## Front Page
      tabItem("frontPage",
              fluidRow(
                column(
                  width = 4,
                  box(
                    class = "information",
                    width = 12,
                    status = "warning",solidHeader = TRUE,title = "Not Just Another Soccer site ",
                    collapsible = T,collapsed = F,
                    includeMarkdown("frontPage.md")
                  ),
                  
                  box(
                    width = 12,
                    status = "success",solidHeader = TRUE,title = "Current Team Sequences",
                    collapsible = T,collapsed = F,
                    DT::dataTableOutput("teamSeqCurrent")
                  ),
                  box(
                    width = 12,
                    status = "success",solidHeader = TRUE,title = "Player Milestones",
                    collapsible = T,collapsed = F,
                    DT::dataTableOutput("playerMilestones")
                  ),
                  box(
                    width = 12,
                    status = "success",solidHeader = TRUE,title = "Twitter Feed",
                    collapsible = T,collapsed = T,
                    tags$body(
                      includeScript("twitter.js"),
                      
                      
                      a(
                        "Soccer", class = "twitter-timeline",
                        width = "320",
                        href = "https://twitter.com/pssGuy/timelines/524678699061641216",
                        "data-widget-id" = "524686407298596864",
                        "data-chrome" = "nofooter transparent noheader"
                      )
                    )
                    
                  ),
                  box(
                    width = 12, class = "information",
                    status = "success",solidHeader = TRUE,title = "What's New",
                    collapsible = T,collapsed = T,
                    includeMarkdown("whatsNew.md")
                  )
                ),
                column(
                  width = 8,
 
                  box(width=12,
                    solidHeader = TRUE,status = 'warning',title="Latest App - Goals and Assists by Game",
                    inputPanel(selectInput("playerppg", label="Type Name and Select", choices =playerChoice,selected="VARDYJ")),
                   # footer = "Hover points for detail",
                    plotlyOutput("player_ppg_hl")
                  ),
                  
                  box(
                    width = 12,
                    status = "success",solidHeader = TRUE,title = "Team Leaders (Ties not shown)",
                    collapsible = T,collapsed = F,
                    DT::dataTableOutput("teamLeadersCurrent")
                  )
                )
              )),
      
      #managers
      tabItem("managers",
              box(title="Manager's Average points per game - Hover bar for more information",
                  status = "success",
              ggvisOutput("managerPPGbyTeam"),
              textOutput("liverpool")
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
      
      tabItem(
        "tm_glance",
        fluidRow(
          #           tabBox(#    tabPanel("Where in the World (click for details)",
          #             #             leafletOutput("teamLeaflet")), # height='90%', width='90%' produces blank area also seemed to impact other tables on page!!!!!
          #             tabPanel("Squad Photo",htmlOutput("squadPhoto"),
          #             #tabPanel("Where in the World",plotOutput("birthChoropleth")),
          #             # tabPanel("Where in the World (click for details)",helpText("Available Soons"))
          #             #),
          #             box(
          #               title = "EPL Finishing Positions",width = 4,height = 375,solidHeader = TRUE,status = 'success',
          #
          #               plotOutput("seasonsHist")
          #             ),
          #             box(
          #               title = "Top LineUp",width = 2,solidHeader = TRUE,status = 'success',
          #               h5(textOutput("lineupCount")),
          #               hr(),
          #               textOutput("lineupText")
          #             )
          #
          #
          #           ))
          
          
          
          
          box("Squad Photo",htmlOutput("squadPhoto")),
          
          box(
            title = "EPL Finishing Positions",width = 4,height = 375,solidHeader = TRUE,status = 'success',
            
            plotOutput("seasonsHist")
          ),
          box(
            title = "Top LineUp",width = 2,solidHeader = TRUE,status = 'success',
            h5(textOutput("lineupCount")),
            hr(),
            textOutput("lineupText")
          )
          
          
          
        ),
        
        fluidRow(
          box(
            title = "Most Apps (click row for player)",width = 3,solidHeader = TRUE,status = 'success',
            DT::dataTableOutput("mostGames")
          ),
          box(
            title = "Most Goals",width = 3,solidHeader = TRUE,status = 'success',
            DT::dataTableOutput("mostGoals")
          ),
          box(
            title = "Most Assists (max 2 per goal)",width = 3,solidHeader = TRUE,status = 'success',
            DT::dataTableOutput("mostAssists")
          ),
          box(
            title = "Most Cards",width = 3,solidHeader = TRUE,status = 'success',
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
                    width = 12, collapsible = T,collapsed = F,
                    
                    DT::dataTableOutput("hthFixtures")
                  )
                )
              )),
      
      
      tabItem(
        "tm_heat",
        
        
        box(
          title = "Heatmap", solidHeader = TRUE,status = 'success',
          width = 6,
          
          # # revamped version of plotly - but seems to work ok without any change?
          # plotlyOutput("x", width = 400, height = 250, inline = T),
          # htmltools::div(style = "display:inline-block", plotlyOutput("x", width = 400, height = 250)),


          plotlyOutput("heatResults"),
          h4("Click on a cell in the heatmap to display table of results")
          
        ),
        box(
          title = "Results by Scoreline",
          footer="Click row for Goalscorer timeline",
          solidHeader = TRUE,status = 'success',
          width = 6,
          uiOutput("heatHeader"),
          DT::dataTableOutput("heatTable")
          
        ),
        box(
          solidHeader = TRUE,status = 'success',
          title="Goal Timeline",
         # collapsible = T,collapsed = F,
          height=200,
          
          
          d3kit_timelineOutput("matchScorers")
          
        )
      ),
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
        box(
          width = 12, height = 550,title = "Result Sequences - Most Recent in bold - Hover bar for details",
          solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          radioButtons(
            "seqVenue","Venue",choices = c("All","Home","Away"),inline = TRUE
          ),
          fluidRow(
            column(width = 2,plotlyOutput("tm_wins")),
            column(width = 2,plotlyOutput("tm_noWins")),
            column(width = 2,plotlyOutput("tm_draws")),
            column(width = 2,plotlyOutput("tm_noDraws")),
            column(width = 2,plotlyOutput("tm_losses")),
            column(width = 2,plotlyOutput("tm_noLosses"))
          )
        ),
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - Wins",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmWinSeq")
            )
          ),
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - Draws",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmDrawSeq")
            )
          ),
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - Defeats",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmLossSeq")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - No Wins",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmNoWinSeq")
            )
          ),
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - No Draws",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmNoDrawSeq")
            )
          ),
          column(
            width = 4,
            box(
              width = 12,title = "Most Recent Record Run - Undefeated",
              solidHeader = TRUE,status = 'success',
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("tmNoLossSeq")
            )
          )
        )
      ),
      #         tabItem(
      #           "tm_seqs_goals",
      #           radioButtons(
      #             "seqVenueB","Venue",choices = c("All","Home","Away"),inline = TRUE
      #           ),
      #           fluidRow(column(width = 3,plotOutput("tm_goalFor")),
      #                    #   fluidRow(column(width=2,plotOutput("tm_goalFor")),
      #                    #            column(width=2,plotOutput("tm_noWins")),
      #                    #            column(width=2,plotOutput("tm_draws")),
      #                    #            column(width=2,plotOutput("tm_noDraws")),
      #                    #            column(width=2,plotOutput("tm_losses")),
      #                    #            column(width=2,plotOutput("tm_noLosses"))),
      #                    fluidRow(column(
      #                      width = 6,
      #                      box(
      #                        width = 12,title = "Most Recent Record Run - Goals For",
      #                        solidHeader = TRUE,status = 'success',
      #                        collapsible = TRUE, collapsed = TRUE,
      #                        DT::dataTableOutput("tmSeqGF")
      #                      )
      #                    ))
      #                    #   column(width=4,
      #                    #          box(width=12,title = "Most Recent Record Run - Draws",
      #                    #              solidHeader = TRUE,status = 'success',
      #                    #              collapsible = TRUE, collapsed = TRUE,
      #                    #              DT::dataTableOutput("tmDrawSeq"))
      #                    #   ),
      #                    #   column(width=4,
      #                    #          box(width=12,title = "Most Recent Record Run - Defeats",
      #                    #              solidHeader = TRUE,status = 'success',
      #                    #              collapsible = TRUE, collapsed = TRUE,
      #                    #              DT::dataTableOutput("tmLossSeq"))
      #                    #   )
      #                    #   ),
      #                    #   fluidRow(column(width=4,
      #                    #                   box(width=12,title = "Most Recent Record Run - No Wins",
      #                    #                       solidHeader = TRUE,status = 'success',
      #                    #                       collapsible = TRUE, collapsed = TRUE,
      #                    #                       DT::dataTableOutput("tmNoWinSeq"))
      #                    #   ),
      #                    #   column(width=4,
      #                    #          box(width=12,title = "Most Recent Record Run - No Draws",
      #                    #              solidHeader = TRUE,status = 'success',
      #                    #              collapsible = TRUE, collapsed = TRUE,
      #                    #              DT::dataTableOutput("tmNoDrawSeq"))
      #                    #   ),
      #                    #   column(width=4,
      #                    #          box(width=12,title = "Most Recent Record Run - Undefeated",
      #                    #              solidHeader = TRUE,status = 'success',
      #                    #              collapsible = TRUE, collapsed = TRUE,
      #                    #              DT::dataTableOutput("tmNoLossSeq"))
      #                    #   )
      #
      #           ),
      ## Standings section
      tabItem(
        "st_boxplot",
        box(
          width = 6, height = 600,
          collapsed = F,collapsible = T,
          title = "Points Range by Games played - Click plot for Season standings",solidHeader = TRUE,status = 'success',
          sliderInput(
            "st_boxGames","Games Played", min = 1,max = 42, value = currentRound
          ),
          plotlyOutput("st_BoxAll")
        ),
        box(
          width = 6,height = 600,
          collapsed = F,collapsible = T,
          
          plotlyOutput("st_BoxSeason")
        )#,
#         box(
#          # width = 6,height = 600,
#           #collapsed = F,collapsible = T,
#           #exploding_boxplotOutput("test", width = "100%", height = "400px"),
#           exploding_boxplotOutput("st_explodingBoxAll")
#         )
      ),
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
          title = "Final League Standings",solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          ggvisOutput("st_position_chart")
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
        "st_leaders",
        box(
          width = 6,
          title = "Number of leadership changes by Year",
          footer = "Only shows changes after full round of games",
          solidHeader = FALSE,
          collapsible = FALSE, collapsed = FALSE,
          plotlyOutput("st_topChanges")
        ),
        box(
          width = 6,
          title = "Progress of teams that topped League - hover chart for results
          ",solidHeader = FALSE,
          collapsible = FALSE, collapsed = FALSE,
          plotlyOutput("st_topChangesWeekly")
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
              htmlOutput("playerPic", height = 250)
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
              tauchartsOutput("playerTransfers_tau", height = "250px")
            )
          )
          
          
        ),
        #  hr(),
        
        fluidRow(
          column(width = 3,
                 infoBoxOutput("teamsBox", width = 12)),
          
          column(
            width = 3, offset = 1,
            infoBoxOutput("seasonsBoxPlayer", width = 12)
          ),
          
          column(
            width = 3, offset = 1,
            infoBoxOutput("appsBox", width = 12)
          )
          
        ),
        fluidRow(
          column(width = 3,
                 infoBoxOutput("goalsBox", width = 12)),
          column(
            width = 3,offset = 1,
            infoBoxOutput("assistsBox", width = 12)
          ),
          column(
            width = 3,offset = 1,
            infoBoxOutput("cardsBox", width = 12)
          )
        ),
        fluidRow(column(
          width = 5,offset = 3,
          box(
            width = 12,title = "Wikipedia (includes non-EPL data)",solidHeader = TRUE,status = 'success',
            collapsible = TRUE, collapsed = TRUE,
            uiOutput("playerWiki")
          )
        ))
        
        
      ),
      
      
      
      tabItem(
        "pl_career",
        fluidRow(column(
          width = 7,
          box(
            width = 12,title = "Game Summaries. Point size relates to Goals/Assists. Hover for details. Values of 99 indicate time of substitution unknown",solidHeader = TRUE,status = 'success',
            collapsible = TRUE, collapsed = FALSE,
            ggvisOutput("careerChart")
          )
        ),
        column(
          width = 5,
          box(
            width = 12,title = "Points per 90 Minutes Hover for details",solidHeader = TRUE,status = 'success',
            footer = "Hover for details. Circle size reflects minutes played",
            collapsible = TRUE, collapsed = FALSE,
            #  ggvisOutput("pointsByYearChart")
            plotlyOutput("pointsByYearChart")
          )
        )),
        
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
        fluidRow(column(
          width = 7,
          box(
            width = 12,title = "Goals By Year",solidHeader = TRUE,status = 'success',
            collapsible = TRUE, collapsed = TRUE,
            
            radioButtons(
              "method","",choices = c("Method","Place","Play"), inline = TRUE
            ),
            
            ggvisOutput("playerGoals")
          )
        ),
        column(
          width = 5,
          box(
            width = 12,title = "Goals Distribution (Points are jittered)",solidHeader = TRUE,status = 'success',
            collapsible = TRUE, collapsed = TRUE,
            
            
            plotOutput("playerGoalDistribution")
          )
          
        ))
      ),

tabItem(
  "pl_ppg",
  box(
    width = 12,title = "Goals and Assists by Game - Hover for details",solidHeader = TRUE,status = 'success',
    collapsible = TRUE, collapsed = FALSE,
    plotlyOutput("player_ppg")
  )
  ),


      tabItem(
        "pl_seqs_goals",
        box(
          width = 12, height = 400,title = "Scoring Sequences - Most Recent in Bold",
          solidHeader = TRUE,status = 'success',
          collapsible = TRUE, collapsed = FALSE,
          radioButtons(
            "seqPlVenue","Venue",choices = c("All","Home","Away"),inline = TRUE
          ),
          fluidRow(
            column(width = 3,plotOutput("gameGoal")),
            column(width = 3,plotOutput("gameNoGoal")),
            column(width = 3,plotOutput("gameGoalStarter")),
            column(width = 3,plotOutput("gameNoGoalStarter"))
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Individual Goal Sequences By Date - All Games",solidHeader = TRUE,status = 'success',
            collapsible = TRUE, collapsed = TRUE,
            plotOutput("gameGoalSeq")
          ),
          box(
            width = 6,
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
      
      tabItem(
        "sp_goalScorers",
        box(
          width = 12,
          status = "success",solidHeader = TRUE,title = "Leading Goalscorers. Amend minimum goals as required. Teams can be de(selected) via legend. Hover for details",
          collapsible = T,collapsed = F,
          helpText(
            "Elite Goalscorers are always difficult to find which made home-grown Harry Kane's 21 league goals scored last year such a fairy tale
            "
          ),
          div(
            style = "display:inline-block",numericInput(
              "goalA", label = "Min Goals",value = 20,min = 1,max = 40,width = 80
            )
          ),
          # div(style = "display:inline-block",inputPanel(selectInput("teamE", label=NULL, teamsChoice))),
          div(
            style = "display:inline-block",selectInput(
              "teamE", label = "Select Teams", c("All Teams",teamsChoice), selected =
                c("All Teams"), multiple = T, width = 150
            )
          ),
          tauchartsOutput("leadingGoalscorers_tau")
          )
    ),
    
    
    tabItem(
      "sp_resSpan",
      box(
        width = 12,
        status = "success",solidHeader = TRUE,title = "Results By Game Span (takes a few seconds)",
        collapsible = T,collapsed = F,
        sliderInput(
          "spanA", label = "Game Span",value = 10,min = 2,max = 20
        ),
        radioButtons(
          "spanVenue", label = NULL, choices = c("All","Home","Away"),inline = T
        ),
        radioButtons(
          "result", label = NULL, choices = c("Win","Loss","Draw","No Win","No Loss","No Draw"),inline =
            T
        ),
        
        # div(style = "display:inline-block",sliderInput("spanA", label="Game Span",value=10,min=2,max=20),selectInput("spanVenue", label="NULL", choices=c("All","Home","Away"))),
        #  div(style = "display:inline-block",selectInput("spanVenue", label="NULL", choices=c("All","Home","Away"))),
        # div(style = "display:inline-block",selectInput("teamE", label="Select Teams", c("All Teams",teamsChoice), selected=c("All Teams"), multiple = T, width=150)),
        DT::dataTableOutput("resByGameSpan")
      )
    ),
    tabItem(
      "sp_twoClubs",
      box(
        width = 4,
        status = "success",solidHeader = TRUE,title = "Played for Two Clubs",footer =
          "On Squad appeances include those as non-playing sub",
        collapsible = T,collapsed = F,
        selectInput(
          "twoTeams","Select two teams",choices = teamsChoice, selected = c("Chelsea","Tottenham H"), multiple =
            T
        ),
        radioButtons("twoTeamsApp","",c("Appeared","On Squad"), inline = T),
        
        
        DT::dataTableOutput("twoClubs")
      )
    ),
    
    tabItem(
      "sp_youngest",
      box(
        width = 12,
        status = "success",solidHeader = TRUE,title = "Youngest Players",
        collapsible = T,collapsed = F,
        helpText(
          "Reece Oxford made an impressisve debut as West Ham's youngest ever Premier League Player but his predecessor
          never played an EPL game again. Click for Team. Hover for Details"
        ),
        selectInput(
          "teamD", label = NULL,teamsChoice,selected = "West Ham U" , width = 150
        ),
        ggvisOutput("sp_ageRecord")
        )
    ),
    tabItem(
      "sp_comparisons",
      box(
        width = 12,
        status = "success",solidHeader = TRUE,title = "Offensive Output Comparisons",
        footer = "Unlike official Statistics, up to two assists are allowed per goal",
        collapsible = T,collapsed = F,
        
        ggvisOutput("sp_comparisons")
      )
    ),
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
    
    
    
    tabItem(
      "sp_plGoalSeqs",
      #   box(
      #     title = "Best Goal Scoring Sequence (click for player)", solidHeader = TRUE,status = 'success',
      #     footer ="Data is jittered to make players more easily identifiable",
      #     width = 6#,
      #   #  ggvisOutput("allPlayerGoalSeqs")
      #
      #   ),
      box(
        title = "Best Goal Scoring Sequence (hover for player details, zoom and pan)", solidHeader = TRUE,status = 'success',
        footer = "Data is jittered to make players more easily identifiable",
        width = 6,
        plotlyOutput("allPlayerGoalSeqs_plotly")
        
      )
    ),
    
    tabItem(
      "sp_birthplace",
      box(
        title = "% Minutes Played by Country", solidHeader = TRUE,status = 'success',
        width = 6,
        div(
          style = "display:inline-block; padding-right: 20px;",selectInput("bp_Teams",NULL,teamsChoice_2, width =
                                                                             '150px')
        ),
        div(
          style = "display:inline-block; padding-right: 20px;",selectInput(
            "bp_Country",NULL,countryChoice, width = '150px', selected = "England"
          )
        ),
        div(
          style = "display:inline-block",radioButtons("bp_fullScale","Full Scale", c("No","Yes"), inline =
                                                        T)
        ),
        ggvisOutput("sp_birthplaceChart")
      ),
      box(
        title = "By Team", solidHeader = TRUE,status = 'success',
        width = 3,
        DT::dataTableOutput("sp_bpTeamsbyYear")
      ),
      box(
        title = "", solidHeader = TRUE,status = 'success',
        width = 3,
        DT::dataTableOutput("sp_bpPlayersbyTeamsbyYear")
      )
      
    ),
    
    tabItem(
      "sp_yearOnYear",
      selectInput(
        "yronyrTeam","Select teams",choices = teamsChoice,selected = c("Arsenal","Chelsea","Man. Utd.","Liverpool"), multiple =
          T
      ),
      numericInput(
        "yronyrRound","Games",value = currentValue,min = 1,max = 42, width = 80
      ),
      radioButtons(
        "yronyrCat","Category",c("Points","Position","GF","GA","GD"), inline = T
      ),
      box(
        title = "Year on Year Changes - Hover points for Details - Click on Legend to Remove/Show Team", solidHeader = TRUE,status = 'success',
        width = 6,
        plotlyOutput("sp_yearOnYear")
      )
    ),
    
    tabItem(
      "sp_tmGoalsSince",
      
      
      box(
        title = "Goals per Game", solidHeader = TRUE,status = 'success',
        width = 6,
        
        div(
          style = "display:inline-block; padding-right: 20px;",radioButtons(
            "goalsSinceCat","Minimum Goals",c("Goals For","Goals Ag","Goals in Game"), inline =
              T
          )
        ),
        div(
          style = "display:inline-block", numericInput(
            "goalsSinceCount","Goals",value = 5,min = 1,max = 15, width = 80
          )
        ),
        
        DT::dataTableOutput("tmGoalsSince")
      )
    ),
    
    tabItem(
      "sp_pcFullGames",
      
      
      box(
        title = "Percent Full Games - zoom and hover for details, Click for Game Details Chart", solidHeader = TRUE,status = 'success',
        width = 6,
        #     div(
        #       style = "display:inline-block; padding-right: 20px;",
        #     selectInput("sp_pcFullGamsTeams","Choose",teamsChoice_2),width=120),
        #     div(
        #       style = "display:inline-block",  sliderInput("sp_pcFullGames","Appearances",min=1,max=500,value=50)
        #     ),
        selectInput("sp_pcFullGamsTeams","Choose",teamsChoice_2,width = 150),
        sliderInput(
          "sp_pcFullGames","Minimum Appearances",min = 1,max = 500,value = 50
        ),
        
        
        plotlyOutput("pcFullGames")
      ),
      box(
        title = "Player Appearances - Hover point for Details", solidHeader = TRUE,status = 'success',
        width = 6,
        
        plotlyOutput("pcFullGamesDets")
      )
      
    ),
    
    tabItem(
      "sp_pcPlayerGoals",
      
      
      box(
        title = "Proportion of Player Goals by Category", footer = "Pan and Zoom. Points are jittered for clarity. Hover points for detail",
        solidHeader = TRUE,status = 'success',
        inputPanel(
          sliderInput(
            "pcPlGoals","Minimum Goals",min = 1,max = 250,value = 50
          ),
          radioButtons(
            "pcPlGoalsCat","Choose category",c(
              "Long Range","Pen Area","6yd Box",
              "Open Play","Corner","Throw In","Indirect FK","Direct FK","Penalty","Right Foot","Left Foot","Header"
            ), inline = T
          )
        ),
        plotlyOutput("pcPlayerGoals")
      )
    ),
    
    tabItem(
      "sp_deficits",
      
      
      box(
        solidHeader = FALSE,status = 'success',
        footer = "Hover points for detail",
        plotlyOutput("relegationOvercome")
      )
    ),
    
    
    tabItem("info", includeMarkdown("info.md"))
    
    
    
    
    
    
    
      ) # tabItems
      ) # body
  ) # page
