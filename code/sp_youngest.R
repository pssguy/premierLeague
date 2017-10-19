# latest App - subsequently switched to specials?? - decid on how names go

observe({
  #if (is.null(input$teamC)) return()

  req(input$teamD)

  ## easiest to put here 190ms so not an issue
  apps <- summary %>%
    ungroup() %>%  #safer
    group_by(PLAYERID) %>%
    summarise(Apps=(sum(St)+sum(On))) ## summarize looks like different end product




  df <-  playerGame %>%
    filter((START+subOn)>0&TEAMNAME==input$teamD) %>%
    #filter((START+subOn)>0&TEAMNAME=="Arsenal") %>%
    group_by(gameDate) %>%
    mutate(youngest=min(age)) %>%
    filter(age==youngest) %>%
    ungroup() %>%
    arrange(gameDate) %>%
    select(name,age,gameDate,PLAYERID) %>%
    inner_join(apps)

  df$age <- as.numeric(df$age)

  df$id <- 1:nrow(df)

  df <-  df %>%
    mutate(youngest=cummin(age)) %>%
    filter(age==youngest) %>%
    mutate(year=as.integer(str_sub(age,1,2))) %>%
    mutate(days=floor(365.25*(age-year))) %>%
    mutate(showAge=paste0(year," yrs ",days," days")) %>%
    mutate(showApps=paste0(Apps," career Apps"))

  #print(glimpse(df))

  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[x$id == df$id,c("name","showAge","gameDate","showApps") ]
    paste0(format(row), collapse = "<br />")
  }

  df   %>%
    ggvis(~gameDate,~youngest, key:= ~id) %>%
    layer_points(fill =~name) %>%
    add_legend("fill", title="") %>%
    ggvis::add_axis("x", title="") %>%
    ggvis::add_axis("y", title="Age",title_offset=50) %>%
    add_tooltip(all_values,"hover") %>%
    set_options(height = 300) %>%
    bind_shiny("sp_ageRecord")

})

## first pass - needs work

# youngData <- reactive({
#  
#   print("enter reactive")
#   
#   req(input$teamD)
#   
#   print(input$teamD)
#   
#   ## easiest to put here 190ms so not an issue
#   apps <- summary %>% 
#     ungroup() %>%  #safer
#     group_by(PLAYERID) %>% 
#     summarise(Apps=(sum(St)+sum(On))) ## summarize looks like different end product
#   
#   
#   
#   
#   df <-  playerGame %>% 
#     filter((START+subOn)>0&TEAMNAME==input$teamD) %>% 
#     #filter((START+subOn)>0&TEAMNAME=="Arsenal") %>% 
#     group_by(gameDate) %>% 
#     mutate(youngest=min(age)) %>% 
#     filter(age==youngest) %>% 
#     ungroup() %>% 
#     arrange(gameDate) %>% 
#     select(name,age,gameDate,PLAYERID) %>% 
#     inner_join(apps)
#   
#   df$age <- as.numeric(df$age)
#   
#   df$id <- 1:nrow(df)
#   
#   df <-  df %>% 
#     mutate(youngest=cummin(age)) %>% 
#     filter(age==youngest) %>% 
#     mutate(year=as.integer(str_sub(age,1,2))) %>% 
#     mutate(days=floor(365.25*(age-year))) %>% 
#     mutate(showAge=paste0(year," yrs ",days," days")) %>% 
#     mutate(showApps=paste0(Apps," career Apps"))
#   
#   #print(glimpse(df))
#   
#   # all_values <- function(x) {
#   #   if(is.null(x)) return(NULL)
#   #   row <- df[x$id == df$id,c("name","showAge","gameDate","showApps") ]
#   #   paste0(format(row), collapse = "<br />")
#   # }
#   # 
#   # df   %>% 
#   #   ggvis(~gameDate,~youngest, key:= ~id) %>% 
#   #   layer_points(fill =~name) %>% 
#   #   add_legend("fill", title="") %>% 
#   #   ggvis::add_axis("x", title="") %>% 
#   #   ggvis::add_axis("y", title="Age",title_offset=50) %>% 
#   #   add_tooltip(all_values,"hover") %>% 
#   #   set_options(height = 300) %>% 
#   #   bind_shiny("sp_ageRecord")
#   
#   info=list(df=df)
#   return(info)
#   
# })
# 
# sp_ageRecord <- renderPlotly({
#   
#   print("enter plotly")
#   
#   youngData()$df %>% 
#     plot_ly(~gameDate,~youngest)
#   
# })
#   
