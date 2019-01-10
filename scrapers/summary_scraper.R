devtools::install_github("rintaromasuda/bleaguer", force = TRUE)
library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

remDr <- RSelenium::remoteDriver(remoteServerAddr = "40.115.154.189",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

df.result <- data.frame()
scraped.games <- unique(b.games.summary$ScheduleKey)
exception.games <- c(4090)
irregular.games <- c()

season <- "2018-19"
df.games <- subset(b.games, Season == season)

for (idx in seq(1:nrow(df.games))) {
  key <- df.games[idx, "ScheduleKey"]
  if (key %in% df.result$ScheduleKey | key %in% exception.games | key %in% scraped.games) {
    print(paste("Skip->", key))
    next
  }

  homeTeamId <- df.games[idx, "HomeTeamId"]
  awayTeamId <- df.games[idx, "AwayTeamId"]

  url.detail <- paste("https://www.bleague.jp/game_detail/?ScheduleKey=",
                      as.character(key),
                      sep = ""
                      )
  Sys.sleep(1)
  print(url.detail)
  remDr$navigate(url.detail)
  pageSource <- remDr$getPageSource()
  html.detail <- read_html(pageSource[[1]])
  tables.game <- html_table(html.detail)

  # Page check

  # Table for quarter and overtime scores. We do NOT support overtime 5th and later.
  df.qtscore <- subset(tables.game[[1]], X2 != "EX1" & X2 != "EX2" & X2 != "EX3" & X2 != "EX4")
  df.otscore <- subset(tables.game[[1]], X2 == "EX1" | X2 == "EX2" | X2 == "EX3" | X2 == "EX4")

  # Skip the game if they don't get quaters (typically Championship game 3s don't have them)
  #if (nrow(subset(df.qtscore, (X2 == "1Q" | X2 == "2Q" | X2 == "3Q" | X2 == "4Q" | X2 == "F"))) != 5) {
  #  irregular.games <- append(irregular.games, key)
  #  next
  #}

  if (nrow(subset(df.qtscore, (X2 == "1Q" | X2 == "2Q" | X2 == "3Q" | X2 == "4Q" | X2 == "F"))) != 5) {
    df.qtscore[1, "X2"] <- "1Q"
    df.qtscore[2, "X2"] <- "2Q"
    df.qtscore[3, "X2"] <- "3Q"
    df.qtscore[4, "X2"] <- "4Q"
  }

  # Fill 0 for overtimes that didn't happen
  for (i in seq(1:4)) {
    ot <- paste("EX", as.character(i), sep = "")
    if (nrow(subset(df.otscore, X2 == ot)) <= 0) {
      df <- data.frame(X1 = 0, X2 = ot, X3 = 0)
      df.otscore <- rbind(df.otscore, df)
    }
  }

  # Table for team summary stats
  df.stats <- tables.game[[2]]

  # Table for team common stats (we add them to both home and away)
  df.common <- tables.game[[3]]
  df.common <- cbind(df.common[, "X2"], df.common)
  colnames(df.common) <- c("X1", "X2", "X3")

  # Combine them all
  df.combined <- rbind(df.qtscore, df.otscore)
  df.combined <- rbind(df.combined, df.stats)
  df.combined <- rbind(df.combined, df.common)

  num.rows <- 34 # If the page has correct info, it should be this number of cols

  if (nrow(df.combined) == 34) {
    # Unpivot data for home team
    df.home <- data.frame(
      key,
      homeTeamId,
      df.combined[1, "X1"],
      df.combined[2, "X1"],
      df.combined[3, "X1"],
      df.combined[4, "X1"],
      df.combined[5, "X1"],
      df.combined[6, "X1"],
      df.combined[7, "X1"],
      df.combined[8, "X1"],
      df.combined[9, "X1"],
      df.combined[10, "X1"],
      df.combined[11, "X1"],
      df.combined[12, "X1"],
      df.combined[13, "X1"],
      df.combined[14, "X1"],
      df.combined[15, "X1"],
      df.combined[16, "X1"],
      df.combined[17, "X1"],
      df.combined[18, "X1"],
      df.combined[19, "X1"],
      df.combined[20, "X1"],
      df.combined[21, "X1"],
      df.combined[22, "X1"],
      df.combined[23, "X1"],
      df.combined[24, "X1"],
      df.combined[25, "X1"],
      df.combined[26, "X1"],
      df.combined[27, "X1"],
      df.combined[28, "X1"],
      df.combined[29, "X1"],
      df.combined[30, "X1"],
      df.combined[31, "X1"],
      df.combined[32, "X1"],
      df.combined[33, "X1"],
      df.combined[34, "X1"]
    )

    # Unpivot data for away team
    df.away <- data.frame(
      key,
      awayTeamId,
      df.combined[1, "X3"],
      df.combined[2, "X3"],
      df.combined[3, "X3"],
      df.combined[4, "X3"],
      df.combined[5, "X3"],
      df.combined[6, "X3"],
      df.combined[7, "X3"],
      df.combined[8, "X3"],
      df.combined[9, "X3"],
      df.combined[10, "X3"],
      df.combined[11, "X3"],
      df.combined[12, "X3"],
      df.combined[13, "X3"],
      df.combined[14, "X3"],
      df.combined[15, "X3"],
      df.combined[16, "X3"],
      df.combined[17, "X3"],
      df.combined[18, "X3"],
      df.combined[19, "X3"],
      df.combined[20, "X3"],
      df.combined[21, "X3"],
      df.combined[22, "X3"],
      df.combined[23, "X3"],
      df.combined[24, "X3"],
      df.combined[25, "X3"],
      df.combined[26, "X3"],
      df.combined[27, "X3"],
      df.combined[28, "X3"],
      df.combined[29, "X3"],
      df.combined[30, "X3"],
      df.combined[31, "X3"],
      df.combined[32, "X3"],
      df.combined[33, "X3"],
      df.combined[34, "X3"]
    )

    # Change column names for unpivotted data
    colNames <- c("ScheduleKey", "TeamId", df.combined$X2)
    colnames(df.home) <- colNames
    colnames(df.away) <- colNames

    # Add them to the result
    df.result <- rbind(df.result, df.home)
    df.result <- rbind(df.result, df.away)
  } else {
    # Handle irregular pages here
    irregular.games <- append(irregular.games, key)
  }
}

df.result.bk <- df.result

df.result$Q1 <- df.result$`1Q`
df.result$Q2 <- df.result$`2Q`
df.result$Q3 <- df.result$`3Q`
df.result$Q4 <- df.result$`4Q`

df.result$OT1 <- df.result$EX1
df.result$OT2 <- df.result$EX2
df.result$OT3 <- df.result$EX3
df.result$OT4 <- df.result$EX4

df.result$PTS <- df.result$F

df.result$F2GM <- df.result$`2 Points FGM`
df.result$F2GA <- df.result$`2 Points FGA`
df.result$F3GM <- df.result$`3 Points FGM`
df.result$F3GA <- df.result$`3 Points FGA`
df.result$FTM <- df.result$`Free-ThrowsM`
df.result$FTA <- df.result$`Free-ThrowsA`

df.result$OR <- df.result$`Offensive Rebounds`
df.result$DR <- df.result$`Defensive Rebounds`
df.result$TR <- df.result$`Total Rebounds`

df.result$AS <- df.result$Assist

df.result$TO <- df.result$Turnover

df.result$ST <- df.result$Steals

df.result$BS <- df.result$Blocks

df.result$F <- df.result$Fouls

df.result$PtsFastBreak <- df.result$`Fast Break Points`
df.result$PtsBiggestLead <- df.result$`Biggest Lead`
df.result$PtsInPaint <- df.result$`Points in the Paint`
df.result$PtsFromTurnover <- df.result$`Points From Turnover`
df.result$PtsSecondChance <- df.result$`Second Chance Points`

df.result$BiggestScoringRun <- df.result$`Biggest Scoring Run`

df.result$LeadChanges <- df.result$`Lead Changes`
df.result$TimesTied <-  df.result$`Times Tied`

df.output <- df.result %>%
  select(
    "ScheduleKey",
    "TeamId",
    "PTS",
    "Q1",
    "Q2",
    "Q3",
    "Q4",
    "OT1",
    "OT2",
    "OT3",
    "OT4",
    "F2GM",
    "F2GA",
    "F3GM",
    "F3GA",
    "FTM",
    "FTA",
    "OR",
    "DR",
    "TR",
    "AS",
    "TO",
    "ST",
    "BS",
    "F",
    "PtsBiggestLead",
    "PtsInPaint",
    "PtsFastBreak",
    "PtsSecondChance",
    "PtsFromTurnover",
    "BiggestScoringRun",
    "LeadChanges",
    "TimesTied"
  )

fileName <- paste("games_summary_", as.character(season), ".csv", sep = "")
write.csv(df.output, fileName, fileEncoding = "UTF-8", row.names = FALSE, quote = FALSE)

irregular.games
