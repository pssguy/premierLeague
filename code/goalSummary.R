output$goalSummary <- DT::renderDataTable({
  
  df <-unique(allGoalsPlayer[allGoalsPlayer$PLAYERID==input$player,])
  df <- data.frame(df)
  df <- df[,-1]
  
  
  if (nrow(df)>1) {
    career <- df %>%
      summarise(Tot=sum(Tot),Right=sum(Right),Left=sum(Left),Head=sum(Head),
                SixYd=sum(SixYd),PenArea=sum(PenArea),LongRange=sum(LongRange),
                Open=sum(Open),Corner=sum(Corner),Throw=sum(Throw),IFK=sum(IFK),
                DFK=sum(DFK),Pen=sum(Pen))
    career$Season <- "Career"
    df <- rbind(df,career)
  } else {
    df <- df # otherwis dos not show?
  }
  
  df <- df %>%
         arrange(desc(Season))
  
  
  DT::datatable(df,options= list(paging = FALSE, searching = FALSE, ordering=FALSE,info=FALSE))
}

)