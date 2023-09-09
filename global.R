library(shiny)
library(jsonlite)
library(dplyr)
library(igraph)
library(lubridate)

# Run daily
updateSchedule = function() {
  df = expand.grid(1:15, c('80', '81'))
  games = mapply(function(week, group) fromJSON(paste0('http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?dates=2023&week=', week, '&groups=', group, '&limit=300'))$events, df$Var1, df$Var2) %>% bind_rows()
  games$week = games$week$number
  games$statusLong = games$status$type$shortDetail
  games$status = games$status$type$state
  games$date = with_tz(as_datetime(games$date, format = '%Y-%m-%dT%H:%MZ', tz = 'UTC'), 'US/Eastern')
  games[, c('home', 'away')] = lapply(games$competitions, function(x) x$competitors[[1]]$team$location) %>% do.call(rbind, .)
  games = games[!(duplicated(games$id) | (games$away == 'TBD') | (games$home == 'TBD')),]
  games = games[order(games$date), c('id', 'week', 'date', 'status', 'statusLong', 'home', 'away')]
  teams = count(games, home, sort = TRUE) %>% filter(n >= 4) %>% arrange(home) %>% select(home)
  games$include = (games$away %in% teams$home) & (games$home %in% teams$home)
  # teams %>% write.csv('teams.csv', row.names = FALSE)
  games %>% write.csv('games.csv', row.names = FALSE)
  return(games)
}

# Run upon load
readGames = function() {
  games = read.csv('games.csv') %>% mutate(status = factor(status, levels = c('in', 'pre', 'post')))
  return(games)
}

getInv = function(games) {
  games$teamA = games[, c('home', 'away')] %>% apply(1, min)
  games$teamB = games[, c('home', 'away')] %>% apply(1, max)
  graph = games %>% filter(include) %>% group_by(teamA, teamB) %>% count() %>% rename(weight = n) %>% graph_from_data_frame(directed = FALSE)
  L = laplacian_matrix(graph, weights = NULL, sparse = FALSE)
  n = nrow(L)
  inv = solve((L[,1:(n-1)] - L[,n])[1:(n-1),]) 
  inv = inv %>% rbind(-colSums(inv)) %>% cbind(0)
  rownames(inv)[n] = rownames(L)[n]
  colnames(inv)[n] = colnames(L)[n]
  return(inv)
}

getImportance = function(games, team) {
  importances = games %>% filter(include == TRUE) %>% select(c(home, away)) %>% lapply(function(x) inv[team, x])
  return(importances$home - importances$away)
}

backgroundRed = "background-color:rgba(192, 0, 0, 0.5)"
backgroundGreen = "background-color:rgba(112, 173, 71, 0.5)"

games = updateSchedule()
games = readGames()
inv = getInv(games)
teams = games %>% filter(include) %>% pull(home) %>% unique() %>% sort()