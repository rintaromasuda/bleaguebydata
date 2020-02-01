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

if(!require(httr)) {
  install.packages("httr")
  library(httr)
}

if(!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

Sys.setlocale(locale = "English")

teamId <- 1612709918

requiredHeaders <- add_headers("Referer" = "http://stats.nba.com",
                               "User-Agent" = "RScript/1.0",
                               "x-nba-stats-origin" = "stats",
                               "x-nba-stats-token" = "true"
                               )

# Get game log (a.k.a. schedule) to get Game IDs
url <- paste0("https://stats.nba.com/stats/teamgamelog",
              "?DateFrom=&DateTo=&LeagueID=20&Season=2019-20&SeasonType=Regular+Season",
              "&TeamID=",
              as.character(teamId))

httpResponse = GET(url, requiredHeaders, accept_json())
res <- content(httpResponse)

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

df.games$GameDate <- as.Date(df.games$GAME_DATE,format="%b %d,%Y")

# Add game index
df.games$Game_ID <- as.character(df.games$Game_ID)
df.games <- df.games %>%
  arrange(GameDate) %>%
  mutate(Game_Index = row_number())

# Access each game
df.boxscore <- data.frame()
for (gameId in df.games$Game_ID) {
  url <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                "?EndPeriod=1&EndRange=0",
                "&GameID=",
                gameId,
                "&RangeType=0&StartPeriod=1&StartRange=0")
  print(url)
  httpResponse = GET(url, requiredHeaders, accept_json())
  res <- content(httpResponse)

  colNames <- res$resultSets[[1]]$headers

  for (i in 1:length(res$resultSets[[1]]$rowSet)) {
    print(i)
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
df.output$MIN_CLASS <- cut(df.output$MIN_NUM,
                           c(-Inf, 0, 5, 10, 20, 30, 40, Inf),
                           labels = c("0", "<=5", "<=10", "<=20", "<=30", "<=40", ">40"))
df.output$STARTER <- ifelse(df.output$START_POSITION == "", FALSE, TRUE)

ggplot() +
  geom_tile(data = df.output,
            aes(x = Game_Index,
                y = PLAYER_NAME,
                fill = MIN_CLASS),
            width = 0.8,
            height = 0.8) +
  ylab("") +
  xlab("nth Game") +
  ggtitle("Miniutes Played - Texas Legends") +
  scale_fill_manual(guide_legend(title = "MIN"), values = c("white",
                                                            "gray90",
                                                            "skyblue1",
                                                            "springgreen1",
                                                            "springgreen3",
                                                            "darkgreen",
                                                            "red1")) +
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

ggsave("YudaiBaba_Legends.jpg", width = 6, height = 9)

ggplot() +
  geom_bar(data = subset(df.output, PLAYER_ID == 1629819),
           aes(x = Game_Index,
               y = as.integer(PTS)),
           fill = "blue",
           stat = "identity") +
  geom_line(data = subset(df.output, PLAYER_ID == 1629819),
           aes(x = Game_Index,
               y = MIN_NUM),
           size = 1,
           color = "red") +
  geom_point(data = subset(df.output, PLAYER_ID == 1629819),
            aes(x = Game_Index,
                y = MIN_NUM),
            size = 3,
            color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Minutes Played")) +
  labs(y = "Points Made",
       x = "Nth Game of Texas Legends",
       title = "Yudai Baba in 2019-20 Season at NBA G League",
       subtitle = "Points Made and Minutes Played") +
  theme_bw() +
  theme(
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave("YudaiBaba_PtsAndMin.jpg", width = 6, height = 6)

