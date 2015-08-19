output$playerGoalDistribution <- renderPlot({
  goals %>%
    left_join(playerGame, by="PLAYER_MATCH") %>%
    filter(PLAYERID==input$playerA) %>%
    select(METHOD,PLACE,PLAY,plGameOrderApp)  %>%
    ggplot(aes(x=METHOD,y=PLACE))+
    geom_point()+
    geom_jitter(aes(colour=PLAY),position=position_jitter(width=0.25, height=0.25))+
    scale_y_discrete(limits=c("6_Yd_Box","Pen_Area","Long_Range"),labels=c("6yd Box","Pen Area","Long Range"))+
    scale_colour_brewer(palette="Set1")+
    ggtitle("Method, Place and Play of Goals Scored") +
    xlab("Method") +
    ylab("Play") +
    labs(colour="Place") +
    theme_bw()
  
})