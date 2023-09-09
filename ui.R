ui <- fluidPage(
  
  # Application title
  titlePanel("NCAA Football"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Team", choices = teams),
      selectInput("week", "Week", selected = 1, choices = setNames(1:15, paste0("Week ", 1:15)))
    ),
    
    mainPanel(
      #tableOutput("games"),
      uiOutput("games")
    )
  )
)