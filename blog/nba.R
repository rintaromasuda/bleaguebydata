if (!require(rjson)) {
  install.packages("rjson")
  library(rjson)
}

# Get game log to get Game IDs

url <- "https://stats.nba.com/stats/teamgamelog?DateFrom=&DateTo=&LeagueID=&Season=2018-19&SeasonType=Regular+Season&TeamID=1610612739"
res <- rjson::fromJSON(file = url)

col.names <- res[[3]][[1]][[2]]
num.games <- length(res[[3]][[1]][[3]])

df.games <- data.frame(matrix(nrow = 0, ncol = length(col.names)))
colnames(df.games) <- col.names
for (i in 1:num.games) {
  df <- data.frame(t(res[[3]][[1]][[3]][[i]]))
  colnames(df) <- col.names
  df.games <- rbind(df.games, df)
}

# Access each game

