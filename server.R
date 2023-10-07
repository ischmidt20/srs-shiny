server <- shinyServer(function(input, output, session) {
  
  teams = read.csv('teams.csv')$home
  games = readGames()
  refreshResult = updateScheduleDay()
  selectedWeek = refreshResult$currentWeek
  games[row.names(refreshResult$games), c('week', 'date', 'status', 'statusLong', 'home', 'away')] = refreshResult$games[, c('week', 'date', 'status', 'statusLong', 'home', 'away')]
  inv = getInv(games)
  
  getGames = eventReactive(c(input$team, input$week), {
    importance = getImportance(games, input$team)
    games[games$include == TRUE, 'importance'] = importance
    games %>%
      filter(week == input$week) %>%
      return()
  })
  
  panel = function(game) {
    
    if (is.na(game$importance) | (game$importance == 0)) {
      leftPanel = column(5,
        fluidRow(paste0(game$away, " @")),
        fluidRow(game$home),
        style = "padding-left:15px"
      )
      midPanel = column(2, fluidRow(0), offset = 1)
    } else if (game$importance > 0) {
      leftPanel = column(5,
        fluidRow(paste0(game$away, " @"), style = backgroundRed),
        fluidRow(game$home, style = backgroundGreen),
        style = "padding-left:15px"
      )
      midPanel = column(
        2,
        tagList(tags$br(), fluidRow(substring(sprintf("%.6f", abs(game$importance)), 2))),
        offset = 1
        )
    } else {
      leftPanel = column(5,
        fluidRow(paste0(game$away, " @"), style = backgroundGreen),
        fluidRow(game$home, style = backgroundRed),
        style = "padding-left:15px"
      )
      midPanel = column(2, fluidRow(substring(sprintf("%.6f", abs(game$importance)), 2)), offset = 1)
    }
    
    wellPanel(
      fluidRow(
        leftPanel,
        midPanel,
        column(4, game$statusLong),
      ), style = "padding-top:0px;padding-bottom:0px;padding-left:15px"
    )
    
  }
  
  output$games = renderUI({
    games = getGames() %>%
      arrange(
        status,
        desc(abs(importance))
        )
    lapply(1:nrow(games), function(x) panel(games[x,]))
  })
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
})