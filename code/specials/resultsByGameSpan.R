## triggerred by Palaces 8 in 10 away span



output$resByGameSpan <- DT::renderDataTable({
  
  print(input$spanVenue)
  print(input$spanA)
  print(input$result)
  
  x <- integer()
  
  if(input$spanVenue=="Away") {
  allteams <- standings %>% 
    ungroup() %>% 
    group_by(team) %>% 
    filter(venue=="A") %>% 
    arrange(gameDate) %>% 
    mutate(rn=row_number()) %>% 
    select(team,rn,res) #9026
  } else if(input$spanVenue=="Home") {
    allteams <- standings %>% 
      ungroup() %>% 
      group_by(team) %>% 
      filter(venue=="H") %>% 
      arrange(gameDate) %>% 
      mutate(rn=row_number()) %>% 
      select(team,rn,res)
  } else {
    allteams <- standings %>% 
      ungroup() %>% 
      group_by(team) %>% 
    
      arrange(gameDate) %>% 
      mutate(rn=row_number()) %>% 
      select(team,rn,res)
  }
  teamChoice <- unique(allteams$team)
  num <- input$spanA-1
  
 if (input$result=="Win") {
  for (j in 1:length(teamChoice)) {
    cp <- allteams %>% 
      filter(team==teamChoice[j])
    for(i in 1:(nrow(cp)-num)) {
      print(i)
      x[i] <-cp %>% 
        filter(between(rn,i,i+num)&res=="Win") %>% 
        nrow()
    }
    tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
    x <- NULL
    if (j!=1) {
      df <- rbind(df,tempdf)
    } else {
      df <- tempdf
    }
  }
 } else if  (input$result=="No Win") {
   
   for (j in 1:length(teamChoice)) {
     cp <- allteams %>% 
       filter(team==teamChoice[j])
     for(i in 1:(nrow(cp)-num)) {
       print(i)
       x[i] <-cp %>% 
         filter(between(rn,i,i+num)&res!="Win") %>% 
         nrow()
     }
     tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
     x <- NULL
     if (j!=1) {
       df <- rbind(df,tempdf)
     } else {
       df <- tempdf
     }
   }
 } else if  (input$result=="Loss") {
   
   for (j in 1:length(teamChoice)) {
     cp <- allteams %>% 
       filter(team==teamChoice[j])
     for(i in 1:(nrow(cp)-num)) {
       print(i)
       x[i] <-cp %>% 
         filter(between(rn,i,i+num)&res=="Loss") %>% 
         nrow()
     }
     tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
     x <- NULL
     if (j!=1) {
       df <- rbind(df,tempdf)
     } else {
       df <- tempdf
     }
   }
 } else if  (input$result=="No Loss") {
   
   for (j in 1:length(teamChoice)) {
     cp <- allteams %>% 
       filter(team==teamChoice[j])
     for(i in 1:(nrow(cp)-num)) {
       print(i)
       x[i] <-cp %>% 
         filter(between(rn,i,i+num)&res!="Loss") %>% 
         nrow()
     }
     tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
     x <- NULL
     if (j!=1) {
       df <- rbind(df,tempdf)
     } else {
       df <- tempdf
     }
   }
 } else if  (input$result=="Draw") {
   
   for (j in 1:length(teamChoice)) {
     cp <- allteams %>% 
       filter(team==teamChoice[j])
     for(i in 1:(nrow(cp)-num)) {
       print(i)
       x[i] <-cp %>% 
         filter(between(rn,i,i+num)&res=="Draw") %>% 
         nrow()
     }
     tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
     x <- NULL
     if (j!=1) {
       df <- rbind(df,tempdf)
     } else {
       df <- tempdf
     }
   }
 } else if  (input$result=="No Draw") {
   
   for (j in 1:length(teamChoice)) {
     cp <- allteams %>% 
       filter(team==teamChoice[j])
     for(i in 1:(nrow(cp)-num)) {
       print(i)
       x[i] <-cp %>% 
         filter(between(rn,i,i+num)&res!="Draw") %>% 
         nrow()
     }
     tempdf <- data.frame(team=teamChoice[j],max=max(x),min=min(x))
     x <- NULL
     if (j!=1) {
       df <- rbind(df,tempdf)
     } else {
       df <- tempdf
     }
   }
 }
   
   
   
   
   print(input$result)
   
   
  df %>% 
    arrange(desc(max)) %>% 
    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  
})