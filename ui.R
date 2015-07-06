

dashboardPage(skin="yellow",
  dashboardHeader(title = "Premier League"),
  
  dashboardSidebar(
    uiOutput("a"),
    uiOutput("teamYear_ui"),
   
    
    sidebarMenu(id = "sbMenu",
      
      menuItem("Teams", tabName = "teams",icon = icon("table"),
               menuSubItem("Player Summary",tabName = "tm_playerSummary"),
               menuSubItem("League Position",tabName = "tm_leaguePosition"),
               menuSubItem("Test",tabName = "test")
               ),
      
      
      
      menuItem("Standings", tabName = "standings",icon = icon("table")),
      menuItem("Players", tabName = "players",icon = icon("table")),
      
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
       tabItem("players",
               box(
                 status="warning",solidHeader = TRUE,title="test image",
 
             htmlOutput("playerPix")   
              )
               
               
               ),
       tabItem("test"),
       tabItem("tm_leaguePosition",
#                fluidRow(
#                  box(
#                    status="warning",solidHeader = TRUE,title="Select Team and Season",
#                    inputPanel(
#                      selectInput("team_3","",teamsChoice, selected="Arsenal"),
#                      uiOutput('tmSeasonChoice_2'))
#                  )
#                ),
               fluidRow(
                 box( width=6,
                   status="success",solidHeader = TRUE,title="Positon by Round. Hover points for Result",
                   ggvisOutput("posGraph")
                 ),
                 box( width=6,
                      status="success",solidHeader = TRUE,title="Positon by Round. Hover points for Result",
                      DT::dataTableOutput("lineup") # not sure why this is here
                 )
               )
       ),
       tabItem("tm_playerSummary",
               ## should be able to dispense with this
#                fluidRow(
#                  box(
#                      status="warning",solidHeader = TRUE,title="Select Team and Season",
#                      inputPanel(
#                      selectInput("team_3","",teamsChoice, selected="Arsenal"),
#                      uiOutput('tmSeasonChoice'))
#                  )
#                  ),
               fluidRow(
                 box(width=12,status="success",solidHeader = TRUE,title="Player Summary",
                     inputPanel(
                      # uiOutput("teamYear_ui"),
                       radioButtons("withClub","Players",choices=c("All","Current"),inline = TRUE),
                       radioButtons("seasons","Seasons",choices=c("All","Single"), selected="Single",inline = TRUE)
                     ),
                     DT::dataTableOutput('teamYear')
                     )

               )       
       ),
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

