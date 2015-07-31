

output$birthChoropleth <- renderPlot({
  if(is.null(input$teamA)) return()
  print("enter choropleth")
  test <-playerGame %>% 
    filter(TEAMNAME==input$teamA&(START+subOn)>0) %>% 
    group_by(COUNTRY) %>% 
    tally() %>% 
    ungroup() %>% 
    arrange(desc(n))
  
  
  
  test[test$COUNTRY=="England",]$COUNTRY <- "united kingdom"
  test[test$COUNTRY=="Wales",]$COUNTRY <- "united kingdom"
  test[test$COUNTRY=="Scotland",]$COUNTRY <- "united kingdom"
  test[test$COUNTRY=="N. Ireland",]$COUNTRY <- "united kingdom"
  
chart <-  test %>% 
    group_by(COUNTRY) %>% 
    summarize(ct=sum(n)) %>% 
    mutate(pc=round(100*ct/sum(ct),1),region=tolower(COUNTRY)) %>% 
    rename(value=pc) %>% 
    select(region,value) %>% 
    unique(.) 

# print(glimpse(chart))
# write_csv(chart,"problem.csv")
# df <- read_csv("problem.csv")
country_choropleth(df,num_colors=1)
   
},width=200,height=200)