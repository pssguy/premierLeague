

output$teamTwitter <- renderUI({
  
 HTML(a("Fun", class="twitter-timeline",
  href="https://twitter.com/pssGuy/timelines/530058458880409600",
  "data-widget-id" = "530058992483958785",
  "data-chrome" ="nofooter transparent noheader"))

})



# This works direct in ui
# a("Fun", class="twitter-timeline",
#   href="https://twitter.com/pssGuy/timelines/530058458880409600",
#   "data-widget-id" = "530058992483958785",
#   "data-chrome" ="nofooter transparent noheader")


## a("Fun", class="twitter-timeline",
# href="https://twitter.com/pssGuy/timelines/530058458880409600",
# "data-widget-id" = "530058992483958785",
# "data-chrome" ="nofooter transparent noheader")
# shows Fun as does putting tags$a at front



# HTML(a("Fun", class="twitter-timeline",
#        href="https://twitter.com/pssGuy/timelines/530058458880409600",
#        "data-widget-id" = "530058992483958785",
#        "data-chrome" ="nofooter transparent noheader"))

# shows
# 
# a list(class = "twitter-timeline", href = "https://twitter.com/pssGuy/timelines/530058458880409600", `data-widget-id` = "530058992483958785", `data-chrome` = "nofooter transparent noheader") list("Fun")




# output$teamTwitter <- renderText({
#   
#   a("Fun", class="twitter-timeline",
#               href="https://twitter.com/pssGuy/timelines/530058458880409600",
#               "data-widget-id" = "530058992483958785",
#               "data-chrome" ="nofooter transparent noheader")
#   
# })