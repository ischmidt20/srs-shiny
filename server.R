server <- shinyServer(function(input, output, session) {
  teams <- sort(union(read.csv("teamsFootball.csv")$home, read.csv("teamsBasketball.csv")$home))
  gamesFootball <- readGames("Football")
  refreshResult <- updateScheduleDayFootball()
  selectedWeek <- refreshResult$currentWeek
  gamesFootball[row.names(refreshResult$games), c("week", "date", "status", "statusLong", "home", "away")] <- refreshResult$games[, c("week", "date", "status", "statusLong", "home", "away")]
  invFootball <- getInv(gamesFootball)

  gamesBasketball <- readGames("Basketball")
  refreshResult <- updateScheduleDayBasketball()
  selectedWeek <- refreshResult$currentWeek
  gamesBasketball[row.names(refreshResult$games), c("week", "date", "status", "statusLong", "home", "away")] <- refreshResult$games[, c("week", "date", "status", "statusLong", "home", "away")]
  invBasketball <- getInv(gamesBasketball)

  getGamesFootball <- eventReactive(c(input$team, input$week), {
    importance <- getImportance(gamesFootball, input$team, invFootball)
    gamesFootball[gamesFootball$include == TRUE, "importance"] <- importance
    gamesFootball %>%
      filter((week == input$week)) %>%
      return()
  })

  getGamesBasketball <- eventReactive(c(input$team, input$date), {
    importance <- getImportance(gamesBasketball, input$team, invBasketball)
    gamesBasketball[gamesBasketball$include == TRUE, "importance"] <- importance
    gamesBasketball %>%
      filter((week == input$date)) %>%
      return()
  })

  panel <- function(game) {
    if (is.na(game$importance) | (game$importance == 0)) {
      leftPanel <- column(5,
        fluidRow(paste0(game$away, " @")),
        fluidRow(game$home),
        style = "padding-left:15px"
      )
      midPanel <- column(2, fluidRow(0), offset = 1)
    } else if (game$importance > 0) {
      leftPanel <- column(5,
        fluidRow(paste0(game$away, " @"), style = backgroundRed),
        fluidRow(game$home, style = backgroundGreen),
        style = "padding-left:15px"
      )
      midPanel <- column(
        2,
        tagList(tags$br(), fluidRow(substring(sprintf("%.6f", abs(game$importance)), 2))),
        offset = 1
      )
    } else {
      leftPanel <- column(5,
        fluidRow(paste0(game$away, " @"), style = backgroundGreen),
        fluidRow(game$home, style = backgroundRed),
        style = "padding-left:15px"
      )
      midPanel <- column(2, fluidRow(substring(sprintf("%.6f", abs(game$importance)), 2)), offset = 1)
    }

    wellPanel(
      fluidRow(
        leftPanel,
        midPanel,
        column(4, game$statusLong),
      ),
      style = "padding-top:0px;padding-bottom:0px;padding-left:15px"
    )
  }

  output$games <- renderUI({
    games <- getGamesFootball() %>%
      arrange(
        status,
        desc(abs(importance))
      )
    lapply(seq_len(nrow(games)), function(x) panel(games[x, ]))
  })

  output$games2 <- renderUI({
    games <- getGamesBasketball() %>%
      arrange(
        status,
        desc(abs(importance))
      )
    lapply(seq_len(nrow(games)), function(x) panel(games[x, ]))
  })

  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
})
