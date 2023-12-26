library(shiny)
library(jsonlite)
library(dplyr)
library(igraph)
library(lubridate)

selectedTeam <- "California"

processGamesBasketball <- function(games) {
  games$statusLong <- games$status$type$shortDetail
  games$status <- games$status$type$state
  games$date <- with_tz(as_datetime(games$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"), "US/Eastern")
  games$week <- trunc(games$date, unit = "day")
  games[, c("home", "away")] <- lapply(games$competitions, function(x) x$competitors[[1]]$team$location) %>% do.call(rbind, .)
  games <- games[!(duplicated(games$id) | (games$away == "TBD") | (games$home == "TBD")), ]
  row.names(games) <- games$id
  games <- games[order(games$date), c("week", "date", "status", "statusLong", "home", "away")]
  return(games)
}

# Run daily
updateScheduleSeasonBasketball <- function() {
  teams <- read.csv("teamsBasketball.csv")$home
  dates <- format(seq(as.Date("2023/11/1"), as.Date("2024/4/1"), by = "day"), "%Y%m%d")
  games <- mapply(function(date) fromJSON(paste0("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?dates=", date, "&groups=50&limit=357"))$events, dates, SIMPLIFY = FALSE) %>%
    bind_rows() %>%
    processGamesBasketball()
  teams <- count(games, home, sort = TRUE) %>%
    filter(n >= 4) %>%
    arrange(home) %>%
    select(home)
  teams %>% write.csv("teamsBasketball.csv", row.names = FALSE)
  games$include <- (games$away %in% teams$home) & (games$home %in% teams$home)
  games %>% write.csv("gamesBasketball.csv")
}

updateScheduleDayBasketball <- function() {
  gamesWeek <- fromJSON(paste0("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?groups=50&limit=357"))$events %>%
    processGamesBasketball()
  return(list(currentWeek = gamesWeek$week[1], games = gamesWeek))
}

processGamesFootball <- function(games) {
  games$week <- games$week$number
  games$statusLong <- games$status$type$shortDetail
  games$status <- games$status$type$state
  games$date <- with_tz(as_datetime(games$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"), "US/Eastern")
  games[, c("home", "away")] <- lapply(games$competitions, function(x) x$competitors[[1]]$team$location) %>% do.call(rbind, .)
  games <- games[!(duplicated(games$id) | (games$away == "TBD") | (games$home == "TBD")), ]
  row.names(games) <- games$id
  games <- games[order(games$date), c("week", "date", "status", "statusLong", "home", "away")]
  return(games)
}

# Run daily
updateScheduleSeasonFootball <- function() {
  teams <- read.csv("teamsFootball.csv")$home
  df <- expand.grid(1:15, c("80", "81"))
  games <- mapply(function(week, group) fromJSON(paste0("http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?dates=2023&week=", week, "&groups=", group, "&limit=300"))$events, df$Var1, df$Var2, SIMPLIFY = FALSE) %>%
    bind_rows() %>%
    processGamesFootball()
  teams <- count(games, home, sort = TRUE) %>%
    filter(n >= 4) %>%
    arrange(home) %>%
    select(home)
  # teams %>% write.csv('teamsFootball.csv', row.names = FALSE)
  games$include <- (games$away %in% teams$home) & (games$home %in% teams$home)
  games %>% write.csv("gamesFootball.csv")
}

updateScheduleDayFootball <- function() {
  gamesWeek <- lapply(c("80", "81"), function(group) fromJSON(paste0("http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?groups=", group, "&limit=300"))$events) %>%
    bind_rows() %>%
    processGamesFootball()
  return(list(currentWeek = gamesWeek$week[1], games = gamesWeek))
}

# Run upon load
readGames <- function(sport) {
  games <- read.csv(paste0("games", sport, ".csv"), row.names = 1) %>% mutate(status = factor(status, levels = c("in", "pre", "post")))
  games$date <- strptime(games$date, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern")
  if (sport == "Basketball") {
    games$week <- strptime(games$week, format = "%Y-%m-%d")
  }
  return(games)
}

getInv <- function(games) {
  games$teamA <- games[, c("home", "away")] %>% apply(1, min)
  games$teamB <- games[, c("home", "away")] %>% apply(1, max)
  graph <- games %>%
    filter(include) %>%
    group_by(teamA, teamB) %>%
    count() %>%
    rename(weight = n) %>%
    graph_from_data_frame(directed = FALSE)
  L <- laplacian_matrix(graph, weights = NULL, sparse = FALSE)
  n <- nrow(L)
  inv <- solve((L[, 1:(n - 1)] - L[, n])[1:(n - 1), ])
  inv <- inv %>%
    rbind(-colSums(inv)) %>%
    cbind(0)
  rownames(inv)[n] <- rownames(L)[n]
  colnames(inv)[n] <- colnames(L)[n]
  return(inv)
}

getImportance <- function(games, team, inv) {
  importances <- games %>%
    filter(include == TRUE) %>%
    select(c(home, away)) %>%
    lapply(function(x) inv[team, x])
  return(importances$home - importances$away)
}

backgroundRed <- "background-color:rgba(192, 0, 0, 0.5)"
backgroundGreen <- "background-color:rgba(112, 173, 71, 0.5)"

teams <- sort(union(read.csv("teamsFootball.csv")$home, read.csv("teamsBasketball.csv")$home))
gamesFootball <- readGames("Football")
refreshResult <- updateScheduleDayFootball()
selectedWeek <- refreshResult$currentWeek
gamesFootball[row.names(refreshResult$games), c("week", "date", "status", "statusLong", "home", "away")] <- refreshResult$games[, c("week", "date", "status", "statusLong", "home", "away")]
invFootball <- getInv(gamesFootball)

gamesBasketball <- readGames("Basketball")
refreshResult <- updateScheduleDayBasketball()
gamesBasketball[row.names(refreshResult$games), c("week", "date", "status", "statusLong", "home", "away")] <- refreshResult$games[, c("week", "date", "status", "statusLong", "home", "away")]
invBasketball <- getInv(gamesBasketball)
