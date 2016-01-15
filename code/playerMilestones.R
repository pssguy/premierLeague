## reaching certain number of apps - goals,asssists
## if a fixed table, then prob best in updatingSQL yep thats where it is


output$playerMilestones <- DT::renderDataTable({
#   print("enter playerMilestones")
#   print(glimpse(milestones))
milestones %>% 
  DT::datatable(rownames=FALSE,class='compact stripe hover row-border',
                colnames=c('','Category','Count'),
                options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
}) 