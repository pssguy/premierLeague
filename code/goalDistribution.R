
# problem even when ghardcoding
# Error in eval(substitute(expr), envir, enclos) : 
#   incorrect length (0), expecting: 23670
# though all test to end look good

output$goalDistribution <- renderPlot({
  print("enter goal distribution 1")
  print(input$playerA)
#   if (is.null(input$playerA))
#     return()
  print("enter goal distribution 2")
  
  print(glimpse(goals))
  print(glimpse(playerGame))
  
df <-  goals %>%
    left_join(playerGame, by = "PLAYER_MATCH") %>%
    filter(PLAYERID == "SHEAREA")

print(glimpse(df))
df <- data.frame(df)

thePlot <-df %>%
    select(METHOD,PLACE,PLAY,plGameOrderApp)  %>%
    ggplot(aes(x = METHOD,y = PLACE)) +
    geom_point() +
    geom_jitter(aes(colour = PLAY),position = position_jitter(width = 0.25, height =
                                                                0.25)) +
    scale_y_discrete(
      limits = c("6_Yd_Box","Pen_Area","Long_Range"),labels = c("6yd Box","Pen Area","Long Range")
    ) +
    scale_colour_brewer(palette = "Set1") +
    ggtitle("Method, Place and Play of Goals Scored") 

print(str(thePlot))

thePlot
})
