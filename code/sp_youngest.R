## latest App - subsequently switched to specials?? - decid on how names go

observe({
  #if (is.null(input$teamC)) return()
  
  req(input$teamC)
  
  ## easiest to put here
  apps <- summary %>% 
    #filter((St+On)>0) %>% 
    group_by(PLAYERID) %>% 
    summarize(Apps=(sum(St)+sum(On)))
  
  df <-  playerGame %>% 
    filter((START+subOn)>0&TEAMNAME==input$teamD) %>% 
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
    add_axis("x", title="") %>% 
    add_axis("y", title="Age",title_offset=50) %>% 
    add_tooltip(all_values,"hover") %>% 
    set_options(height = 300) %>% 
    bind_shiny("sp_ageRecord")
  
})