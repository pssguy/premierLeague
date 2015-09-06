output$goalSummary <- DT::renderDataTable({
  
  df <-
    unique(allGoalsPlayer[allGoalsPlayer$PLAYERID == input$playerA,]) %>%
    ungroup() %>%
    select(-PLAYERID)
  
#   #print("df length")
#   #print(nrow(df))
#   #print(glimpse(df)) #does have 1 obs all0 so should show up
  
  
  if (nrow(df) > 1) {
    career <- df %>%
      summarise(
        Tot = sum(Tot),Right = sum(Right),Left = sum(Left),Head = sum(Head),
        SixYd = sum(SixYd),PenArea = sum(PenArea),LongRange = sum(LongRange),
        Open = sum(Open),Corner = sum(Corner),Throw = sum(Throw),IFK =
          sum(IFK),
        DFK = sum(DFK),Pen = sum(Pen)
      )
    career$Season <- "Career"
    df <- rbind(df,career)
  } else {
    df <- df # otherwis dos not show?
  }
  
  df <- df %>%
    arrange(desc(Season)) %>%  # this puts career at top usefully
    DT::datatable(class='compact stripe hover row-border',
      rownames = FALSE,container = PL_format,options = list(
        paging = FALSE, searching = FALSE, ordering = FALSE,info = FALSE
      )
    )  %>% formatStyle(
      'Season',
      target = 'row',
      backgroundColor = styleEqual(c('Career'), c('lightgreen'))
    )
})