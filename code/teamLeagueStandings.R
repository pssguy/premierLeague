## Problem
observe({

  req(input$teamA)
  req(input$teamYears)

  # if (is.null(input$teamA)) return()
  # if (is.null(input$teamYears)) return()
  #   print(input$teamA)
  #   print(input$teamYears)

  theTeam <- input$teamA
  theSeason <- input$teamYears
#   print("team and season")
#   print(theTeam) # theTeam <-  "Arsenal"
#   print(theSeason)
  graph <- standings %>%
    filter(team==theTeam&season==theSeason)


  graph <- cbind(graph, id = seq_len(nrow(graph)))

  # just need to add a tt and res for fill
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- graph[graph$id == x$id,"tt" ]
    paste0(names(row), format(row), collapse = "<br />")
  }

  pos1 <- standings %>%
    filter(position==1&season==theSeason) %>%
    select(season,leader=team,tmYrGameOrder,lpos=position,lpoints=cumPts)


  print("theseason")
  print(theSeason)
  pos4 <- standings %>%
    filter(position==4&season==theSeason) %>%
    select(season,euro=team,tmYrGameOrder,epos=position,epoints=cumPts)

  ## need to vary in shiny
  if (theSeason >"1994/95") {
    pos18 <- standings %>%
      filter(position==17&season==theSeason) %>%
      select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
  } else if (theSeason=="1994/95") {
    pos18 <- standings %>%
      filter(position==18&season==theSeason) %>%
      select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
  } else {
    pos18 <- standings %>%
      filter(position==19&season==theSeason) %>%
      select(season,rel=team,tmYrGameOrder,rpos=position,rpoints=cumPts)
  }
  #str(pos1)
  pos1 <- data.frame(pos1) # no good otherwise
  pos4 <- data.frame(pos4)
  pos18 <- data.frame(pos18)



  graph %>%
    inner_join(pos1) %>%
    inner_join(pos4) %>%
    inner_join(pos18) %>%
    ggvis(~tmYrGameOrder,~cumPts,key := ~id) %>%
    layer_lines() %>%
    layer_points(fill = ~res) %>%
    layer_lines( ~tmYrGameOrder,~lpoints,stroke := "green") %>%
    layer_lines(~tmYrGameOrder,~epoints,stroke := "blue") %>%
    layer_lines(~tmYrGameOrder,~rpoints,stroke := "red") %>%
    add_tooltip(all_values, "hover") %>%
    handle_click(getLineups) %>%
    ggvis::add_axis("y",title="Points") %>%
    ggvis::add_axis("x",title="Games Played. Lines also shown for 1st,4th and final Relegation Position",format='d') %>%
    add_legend("fill",title="") %>%
    set_options(width = "auto", height = 400, resizable=FALSE) %>%
    bind_shiny('posGraph')



})
