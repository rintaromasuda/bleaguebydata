if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if (!require(rjson)) {
  install.packages("rjson")
  library(rjson)
}

teamId <- 1610612744

# Get game log (a.k.a. schedule) to get Game IDs
url <- paste0("https://stats.nba.com/stats/teamgamelog",
              "?DateFrom=&DateTo=&LeagueID=&Season=2018-19&SeasonType=Regular+Season",
              "&TeamID=",
              as.character(teamId))
              
res <- rjson::fromJSON(file = url)

colNames <- res$resultSets[[1]]$headers
numGames <- length(res$resultSets[[1]]$rowSet)

df.games <- data.frame()
for (i in 1:numGames) {
  arrayRow <- as.character(res$resultSets[[1]]$rowSet[[i]])
  df <- as.data.frame(matrix(arrayRow, nrow = 1),
                      stringsAsFactors = FALSE)
  colnames(df) <- colNames
  df.games <- rbind(df.games, df)
}

# Add game index
df.games$Game_ID <- as.character(df.games$Game_ID)
df.games <- df.games %>%
  arrange(Game_ID) %>%
  mutate(Game_Index = row_number())

# Access each game
df.boxscore <- data.frame()
for (gameId in df.games$Game_ID) {
  url <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                "?EndPeriod=1&EndRange=0",
                "&GameID=",
                gameId,
                "&RangeType=0&StartPeriod=1&StartRange=0")
  res <- rjson::fromJSON(file = url)
  
  colNames <- res$resultSets[[1]]$headers
  
  for (i in 1:length(res$resultSets[[1]]$rowSet)) {
    arrayRow <- as.character(res$resultSets[[1]]$rowSet[[i]])
    df <- as.data.frame(matrix(arrayRow, nrow = 1),
                        stringsAsFactors = FALSE)
    colnames(df) <- colNames
    df.boxscore <- rbind(df.boxscore, df)
  }
}

df.output <- merge(df.boxscore,
                   df.games[, c("Game_ID", "Team_ID", "Game_Index")],
                   by.x = c("GAME_ID", "TEAM_ID"),
                   by.y = c("Game_ID", "Team_ID"))

ConvertMinStrToDec <- function(min_str) {
  Convert <- function(item) {
    min <- as.numeric(item[1])
    min <- min + as.numeric(item[2]) / 60
    round(min, 2)
  }
  
  ls <- sapply(stringr::str_split(min_str, ":"), Convert)
  return(ls)
}

df.output$MIN_NUM <- ConvertMinStrToDec(df.output$MIN)
df.output[is.na(df.output$MIN_NUM),]$MIN_NUM <- 0

ggplot() +
  geom_tile(data = df.output,
            aes(x = Game_Index,
                y = PLAYER_NAME,
                fill = MIN_NUM)) +
  ylab("") +
  xlab("nth Game") +
  ggtitle("Miniutes Played over Games - Golden State Warriors") +
  scale_fill_continuous(high = "navy", low = "white", guide_legend(title = "MIN")) +
  scale_x_continuous(breaks = seq(5, 82, by = 5)) +
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 1, size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "white", colour = "white")
  )
