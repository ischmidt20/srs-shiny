ui <- fluidPage(
  titlePanel("NCAA Simple Rating System"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Team", selected = selectedTeam, choices = teams),
      selectInput("week", "Week", selected = selectedWeek, choices = setNames(1:15, paste0("Week ", 1:15))),
      dateInput("date", "Date")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Football",
          fluid = TRUE,
          uiOutput("games")
        ),
        tabPanel(
          "Basketball",
          fluid = TRUE,
          uiOutput("games2")
        )
      )
    )
  )
)
