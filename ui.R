shinyUI(
 # navbarPage("myTinyShinys",inverse=TRUE,collapsible=TRUE,footer="could put some stuff here",#theme="simplex.min.css", completely threw layout so will defer
             navbarPage("myTinyShinys",inverse=TRUE,theme = shinytheme("cosmo"),collapsible=TRUE,           
             tabPanel("Front",
                      fluidRow(
                        column(4,
                               h4("About PremierSoccerStats"),
                               includeMarkdown("about.md"),
                               br(),
                               tags$body(includeScript("twitter.js"),
                                tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
                             
                               a("Soccer", class="twitter-timeline",
                                 href="https://twitter.com/pssGuy/timelines/524678699061641216",
                                 "data-widget-id" = "524686407298596864",
                                 "data-chrome" ="nofooter transparent noheader"))
                        ),
        
                        column(4,includeMarkdown("frontPage.md")),

column(4,
       h4("Leaders by Club at current stage of season"),
        inputPanel(
         radioButtons("category_2","Category",c("Goals","Assists","Cards"),inline=TRUE),
         radioButtons("season_2","Period",c("Current","Last Yr","Record"),inline=TRUE)
        ),
       DT::dataTableOutput('currentLeaders') #milestones - prob chart is best or random carousel
)

                      )),
             
             tabPanel("Teams",
                      inputPanel(
                        selectInput("team_3","",teamsChoice, selected="Arsenal"),
                      
                       uiOutput('tmSeasonChoice'),
                        helpText(" could add overall record best runs")
                      ) ,
                      tabsetPanel("tsp_Teams",
                                  
#                                   tabPanel("Player Summary",
#                                            fluidRow( column(6,h6("Single Season"),DT::dataTableOutput('teamYear'),textOutput("teamYearRow_out")), # this is just a test
#                                                             column(6,
#                                                                    fluidRow(
#                                                                      column(3,h6("AllTime")),
#                                                                      column(3, radioButtons("withClub","Players",choices=c("All","Current")))
#                                                                    ),
#                                                                      
#                                                                    dataTableOutput('teamAllTime'))),
#                                            bsTooltip("teamYear", "age: At Jan 1st of season, Assists: Unofficial but WAY better, Points= Assists+Goals, OG:Own Goal, MP:Missed Penalty(limited data)", "top")#, 
#                                           
#                                   ),
                                  tabPanel("Player Summary",
                                           inputPanel(
                                           radioButtons("withClub","Players",choices=c("All","Current"),inline = TRUE),
                                           radioButtons("seasons","Seasons",choices=c("All","Single"), selected="Single",inline = TRUE)
                                           ),
                                           DT::dataTableOutput('teamYear'),
                                           textOutput("teamYearRow_out"),
                                         
                                                     
                                           bsTooltip("teamYear", "age: At Jan 1st of season, Assists: Unofficial but WAY better, Points= Assists+Goals,
                                                     OG:Own Goal, MP:Missed Penalty(limited data)", "top")
                      ),
                                           
                                  
                                  
                                        
                                  tabPanel("League Position",
                                           helpText("    Additional lines are for 1st, 4th and final relegation postion. Hover points for game data. downloadable as SVG or PNG"),
                                          
                                           fluidRow(
                                             column(6,ggvisOutput("posGraph")),
                                             column(4,dataTableOutput("lineup"),offset=2)
                                             )
                                           
                                           
                                  ),
#                                   tabPanel("Goal Details",
#                                            inputPanel(radioButtons("goalType"),"Select",c("Goals For", "Goals Ag","Goal Diff"))
#                                            fluidRow(
#                                              column(6,h6("Goals For"),DT::dataTableOutput('teamGoalsFor')),
#                                              column(6,h6("Goals Against"),DT::dataTableOutput('teamGoalsAg'))
#                                            )
#                                   ),
navbarMenu("Goal Details",
           tabPanel("Goals For", inputPanel(DT::dataTableOutput('teamGoalsFor'))),
           tabPanel("Goals Ag", inputPanel(DT::dataTableOutput('teamGoalsAg'))),
           tabPanel("Goals Diff", inputPanel(DT::dataTableOutput('teamGoalsDiff')))
),
           
                                  tabPanel("Result Summaries",
                                           fluidRow(
                                             column(6,h6("Hover square for details of that scoreline"),showOutput("resultsSummary", "dimple")),
                                             column(6,dataTableOutput('scorelines'))
                                           )
                                  ),
                                  tabPanel("Team Leaders",
                                           
                                           DT::dataTableOutput('teamLeaders'),
                                           bsTooltip("teamLeadersbs","Only shows one, if ties")
                                           ),

navbarMenu("Head to Head",
           tabPanel("Table", inputPanel(DT::dataTableOutput('hthTable'))),
           tabPanel("Chart", ggvisOutput("hthGraph"))
          
),


#                                   tabPanel("Head to Head",
#                                            fluidRow(
#                                              column(6,DT::dataTableOutput('hthTable')),
#                                              column(6,helpText("Hover for Team. Some obscuration for small number of games"),
#                                                     ggvisOutput("hthGraph"))
#                                            )
#                                            ),
                                  
                                  tabPanel("Sequences")
                                  
                      )),
             tabPanel("Players",
                     
                      inputPanel(
                      selectInput("player","Select or enter Name",playerChoice,selected="ROONEYX"),
                      helpText(" could add photo basic bio total games played (rank)")
                      ),
               tabsetPanel(id="tsp_Players",
                tabPanel(title="Apps Summary",value="panel1",
                    fluidRow(
                      bsTooltip("career", "Age: At Jan 1st of season, Assists: Unofficial but WAY better, Points= Assists+Goals, OG:Own Goal, MP:Missed Penalty(limited data)", "top"), 
                      column(6,DT::dataTableOutput('careerTots'),
                             h6("When sorted by season descending, click on row for Game data"),
                             DT::dataTableOutput('career')), 
                      column(4,DT::dataTableOutput('oneYear'),offset=1)#
                    )
                ),
#                 tabPanel("Goals x Game"#,
#                         # ggvisOutput('careerGoals') # need to get up
#                 ),
#                 tabPanel("Season Leaders"
#                        #  dataTableOutput('teamLeaders')
#                          
#                 ),


#                 tabPanel("Goal Summary",
#                          fluidRow(
#                          column(5,DT::dataTableOutput('goalSummary')),
#                          column(7,
#                            radioButtons("method","",choices=c("Method","Place","Play")),     
#                                 ggvisOutput('playerGoals'))
#                          )
#                 ),
navbarMenu("Goal Summary",
           tabPanel("Table", inputPanel(DT::dataTableOutput('goalSummary'))),
           tabPanel("Chart",
                    inputPanel(
                      radioButtons("method","",choices=c("Method","Place","Play"))
                    ),
                    ggvisOutput("playerGoals"))
           
),
                tabPanel("Sequences",
                         fluidRow(
                           column(3,ggvisOutput("bestRun")),
                           column(4,ggvisOutput("worstRun"), offset = 2)
                         ),
                         fluidRow(
                           column(4,dataTableOutput("plBestSeqGl")),
                           column(4,dataTableOutput("plWorstSeqGl"), offset =1)
                ),
                fluidRow(
                  column(4,ggvisOutput("bestAssRun")),
                  column(4,ggvisOutput("worstAssRun"))
                ),
                fluidRow(
                  column(6,dataTableOutput("plBestSeqAss")),
                  column(6,dataTableOutput("plWorstSeqAss"))
                )
                
                ),
                tabPanel("Goal Firsts",
                         fluidRow(
                           column(3,
                                  h5("Appearances taken to reach Goal by Type"),
                                  DT::dataTableOutput("goalFirsts"),offset = 1),
                           column(6,plotOutput("goalDistribution"), offset = 1)
                         )
                               
                )
#                 tabPanel("Opposition"
#                          
#                 ),
#                 tabPanel("Played With"
#                          
#                 ),
#                 tabPanel("Transfer Trail"
#                          
#                 )
               )
             ),
                                                                   
             tabPanel("Standings",
                       tabsetPanel(id="q",
                                   
                                  # navbarMenu("Latest",
                                              tabPanel("By Round", inputPanel(numericInput("games_4a","Games Played",min=1,max=42,step=1,value=currentRound),
                                                                           selectInput("season_4a","Season",seasonChoice_2,selected="2014/15")
                                                                           ),
                                                       fluidRow(column(4,DT::dataTableOutput('standings')))
                                              ),
                                  tabPanel("By Position", inputPanel(numericInput("position_4a","League Position",min=1,max=22,step=1,value=1),
                                                                     numericInput("games_4b","Games Played",min=1,max=42,step=1,value=currentRound)
                                                                     ),
                                           fluidRow(column(4,DT::dataTableOutput('leaders')))
                                  ),
                                  tabPanel("Team Standing", inputPanel(selectInput("team_4a","Team",teamsChoice)),
                                           fluidRow(column(4,DT::dataTableOutput('teamStanding')))    
                                  ),
                                 

                                  tabPanel("Current Form",
                                           inputPanel(
                                           numericInput("games_5","Number Games",min=1,max=currentRound,step=1,value=currentValue)
                                           ),
                                           fluidRow(column(4,DT::dataTableOutput('currentForm')))
                                  ),
                                  tabPanel("Specific Date",
                                           dateInput("date_1","Enter Date",value="2014-01-01", startview="year"),
                                           fluidRow(
                                             column(3,h6("At Chosen Date"),DT::dataTableOutput('datetableNow')),
                                             column(3,h6("Remainder of Season"),DT::dataTableOutput('datetableRest'),offset=1),
                                             column(3,h6("Final Table"),DT::dataTableOutput('datetableYear'),offset=1)
                                           )
                                           
                                  ),
                                  tabPanel("Weeks x Position",
                                           inputPanel(
                                             selectInput("team_4b","Select Team",teamsChoice, selected="Arsenal"),
                                             selectInput("season_4b","Select Season(s)",seasonChoice_2,selected="2014/15", multiple=T)
                                             
                                           ),
#                                            uiOutput('weekPos_ui'),
#                                       ggvisOutput('weekPos')   
fluidRow(column(6,
helpText("Number of weeks spent at specific League Position, by club"),
                                      plotOutput('weekPos') 
))                              
                                  )
)
)
#))
#                                   tabPanel("Mini Leagues"
#                                            
#                                   )
                      
#                      )
 #                     )
,

# some issues on occasion
tabPanel("MiniApps", tags$iframe(src="https://pssguy.shinyapps.io/pssBlog/blog.Rmd", width = "1000", height="600", seamless="seamless")),

navbarMenu("mts Sites",
           tabPanel(a("Premier League", href="https://mytinyshinys.shinyapps.io/premierLeague")),
           tabPanel(a("Sports", href="https://mytinyshinys.shinyapps.io/sports")),
           tabPanel(a("Science", href="https://mytinyshinys.shinyapps.io/science")),
           tabPanel(a("Socio-Economic", href="https://mytinyshinys.shinyapps.io/socioEconomic")),
           tabPanel(a("Diversions", href="https://mytinyshinys.shinyapps.io/diversions"))
           
           )
               


))


