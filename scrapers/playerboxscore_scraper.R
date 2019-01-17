devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
}

remDr <- RSelenium::remoteDriver(remoteServerAddr = "40.115.154.189",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

df.result <- data.frame()
scraped.games <- c()
exception.games <- c(4090)
irregular.games <- c()

season <- "2018-19"
df.games <- subset(b.games, Season == season)

for (idx in seq(1:nrow(df.games))) {
  
  key <- df.games[idx,]$ScheduleKey
  if (key %in% df.result$ScheduleKey | key %in% exception.games | key %in% scraped.games) {
    print(paste("Already done. Skipping->", key))
    next
  }

  homeTeamId <- df.games[idx,]$HomeTeamId
  awayTeamId <- df.games[idx,]$AwayTeamId

  url.detail <- paste("https://www.bleague.jp/game_detail/?ScheduleKey=",
                      as.character(key),
                      "&TAB=B",
                      sep = ""
                      )
  print(url.detail)
  
  remDr$navigate(url.detail)
  pageSource <- remDr$getPageSource()
  html.boxscore <- read_html(pageSource[[1]])
  
  tables.boxscore <- html_table(html.boxscore)
  if (length(tables.boxscore) < 13) {
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

# fileName <- paste("games_summary_", as.character(season), ".csv", sep = "")
# write.csv(df.output, fileName, fileEncoding = "UTF-8", row.names = FALSE, quote = FALSE)

#game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(2) > table > tbody > tr:nth-child(1) > td:nth-child(3) > a