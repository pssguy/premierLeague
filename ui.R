

dashboardPage(skin="yellow",
  dashboardHeader(title = "Premier League"),
  
  dashboardSidebar(
    
   
    
    sidebarMenu(
      
      menuItem("Teams", tabName = "teams",icon = icon("table"),
               menuSubItem("Player Summary",tabName = "playerSummary"),
               menuSubItem("Test",tabName = "test")
               ),
      
      
      
      menuItem("Standings", tabName = "standings",icon = icon("table")),
#      menuItem("Players", tabName = "players",icon = icon("table")),
      
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
       tabItem("test"),
       tabItem("playerSummary",
               fluidRow(
                 box(
                     status="warning",solidHeader = TRUE,title="Select Team and Season",
                     inputPanel(
                     selectInput("team_3","",teamsChoice, selected="Arsenal"),
                     uiOutput('tmSeasonChoice'))
                 )
                 ),
               fluidRow(
                 box(width=12,status="success",solidHeader = TRUE,title="Player Summary",
                     inputPanel(
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

