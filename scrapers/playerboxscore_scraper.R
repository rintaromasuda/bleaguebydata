Sys.setlocale(locale = 'Japanese')

library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
}

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}

remDr <- RSelenium::remoteDriver(remoteServerAddr = "bleaguer-selenium1",
                                 port = 4444L,
                                 browserName = "chrome")
remDr$open()

df.result <- data.frame()
scraped.games <- unique(b.games.boxscore$ScheduleKey)
exception.games <- c(4090)
irregular.games <- c()

season <- "2018-19"
df.games <- read_csv("games.csv",
                     cols(
                       ScheduleKey = col_integer(),
                       Season = col_factor(),
                       EventId = col_integer(),
                       Date = col_character(),
                       Arena = col_character(),
                       Attendance = col_integer(),
                       HomeTeamId = col_integer(),
                       AwayTeamId = col_integer()),
                     col_names = TRUE,
                     locale = readr::locale(encoding = "UTF-8"))

for (idx in seq(1:nrow(df.games))) {

  key <- df.games[idx,]$ScheduleKey
  if (key %in% df.result$ScheduleKey | key %in% exception.games | key %in% scraped.games) {
    print(paste("Already done. Skipping->", key))
    next
  }

  homeTeamId <- df.games[idx,]$HomeTeamId
  awayTeamId <- df.games[idx,]$AwayTeamId
  eventId <- df.games[idx,]$EventId

  url.detail <- paste("https://www.bleague.jp/game_detail/?ScheduleKey=",
                      as.character(key),
                      "&TAB=B",
                      sep = ""
                      )
  print(url.detail)

  try.count <- 1
  try.success <- FALSE
  try.threshold <- 60
  # Post-season Game 3s in 2016-17 and 2017-18 have less tables
  expected.table.count <- ifelse(eventId %in% c(300,400,800), 9, 13)
  while (try.count <= try.threshold) {
    remDr$navigate(url.detail)
    pageSource <- remDr$getPageSource()
    html.boxscore <- read_html(pageSource[[1]])
    tables.boxscore <- html_table(html.boxscore)
    # Check the page and leave if it's good
    if ((length(tables.boxscore) >= expected.table.count) &&
        (nrow(tables.boxscore[[4]]) > 0) &&
        (nrow(tables.boxscore[[5]]) > 0)) {
      try.success <- TRUE
      break
    } else {
      Sys.sleep(0.5)
      print(paste("Retry the page load...(", try.count, ")", sep = ""))
      try.count <- try.count + 1
    }
  }

  if (!try.success) {
    print(paste("Insufficient tables->", key))
    irregular.games <- append(irregular.games, key)
    next
  }

  # Read player URLs and name separately as just reading the tables don't give us them
  urls.home.players <- html.boxscore %>%
    html_nodes("#game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(2) > table > tbody > tr > td:nth-child(3) > a") %>%
    html_attr("href")

  urls.away.players <- html.boxscore %>%
    html_nodes("#game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(4) > table > tbody > tr > td:nth-child(3) > a") %>%
    html_attr("href")

  names.home.players <- html.boxscore %>%
    html_nodes("  #game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(2) > table > tbody > tr > td:nth-child(3) > a > span.for-pc") %>%
    html_text(trim = TRUE)

  names.away.players <- html.boxscore %>%
    html_nodes("  #game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(4) > table > tbody > tr > td:nth-child(3) > a > span.for-pc") %>%
    html_text(trim = TRUE)

  # Get IDs out of URLs and trim the names
  startStr <- "PlayerID="
  ids.home.players <- substring(urls.home.players,
                           regexpr(startStr, urls.home.players) + nchar(startStr))
  ids.away.players <- substring(urls.away.players,
                                regexpr(startStr, urls.away.players) + nchar(startStr))
  names.home.players <- gsub(" ", "", names.home.players) # Hankaku
  names.home.players <- gsub("　", "", names.home.players) # Zenkaku
  names.away.players <- gsub(" ", "", names.away.players) # Hankaku
  names.away.players <- gsub("　", "", names.away.players) # Zenkaku

  # Total boxscore tables
  table.home.total <- tables.boxscore[[4]]
  table.away.total <- tables.boxscore[[5]]

  # Removing summary rows at the bottom
  table.home.total <- table.home.total[!is.na(table.home.total$`#`),]
  table.away.total <- table.away.total[!is.na(table.away.total$`#`),]

  # Validate row numbers
  if ((nrow(table.home.total) != length(names.home.players)) |
      (nrow(table.home.total) != length(ids.home.players)) |
      (nrow(table.away.total) != length(names.away.players)) |
      (nrow(table.away.total) != length(ids.away.players))) {
    print(paste("Data row num mis-match->", key))
    irregular.games <- append(irregular.games, key)
    next
  }

  # Replacing names and adding more columns
  table.home.total$PLAYER <- names.home.players
  table.away.total$PLAYER <- names.away.players

  table.home.total$PlayerId <- ids.home.players
  table.away.total$PlayerId <- ids.away.players

  table.home.total$ScheduleKey <- key
  table.away.total$ScheduleKey <- key
  table.home.total$TeamId <- homeTeamId
  table.away.total$TeamId <- awayTeamId

  df.total <- rbind(table.home.total, table.away.total)
  df.total$BoxType <- "Total"

  df.result <- rbind(df.result, df.total)
}

df.result.bk <- df.result

df.result$Number <- df.result$`#`
df.result$StarterBench <- ifelse(df.result$S == "〇", "Starter", "Bench")
df.result$Player <- df.result$PLAYER
df.result$Position <- df.result$PO
df.result$F3GM <- df.result$`3FGM`
df.result$F3GA <- df.result$`3FGA`
df.result$MIN.STR <- df.result$MIN
df.result$MIN <- bleaguer::ConvertMinStrToDec(df.result$MIN.STR)

df.output <- df.result[,c(
                       "ScheduleKey",
                       "TeamId",
                       "BoxType",
                       "PlayerId",
                       "Player",
                       "Number",
                       "Position",
                       "StarterBench",
                       "MIN",
                       "MIN.STR",
                       "PTS",
                       "FGM",
                       "FGA",
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
                       "BSR",
                       "F",
                       "FD",
                       "DUNK",
                       "EFF"
                       )]

fileName <- paste("games_boxscore_", as.character(season), ".csv", sep = "")
write.csv(df.output, fileName, fileEncoding = "UTF-8", row.names = FALSE, quote = FALSE)
