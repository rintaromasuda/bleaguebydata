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
  if (key %in% df.result$ScheduleKey | key %in% exception.games) {
    print(paste("Already done. Skipping->", key))
    next
  }

  homeTeamId <- df.games[idx,]$HomeTeamId
  awayTeamId <- df.games[idx,]$AwayTeamId

  url.detail <- paste("https://www.bleague.jp/game_detail/?ScheduleKey=",
                      as.character(key),
                      "&TAB=P",
                      sep = ""
                      )
  print(url.detail)

  try.count <- 1
  try.success <- FALSE
  try.threshold <- 60
  # Post-season Game 3s in 2016-17 and 2017-18 have less tables
  #expected.table.count <- ifelse(eventId %in% c(300,400,800), 9, 13)
  while (try.count <= try.threshold) {
    remDr$navigate(url.detail)
    pageSource <- remDr$getPageSource()
    html.pbyp <- read_html(pageSource[[1]])
    #tables.boxscore <- html_table(html.boxscore)
    # Check the page and leave if it's good
    #if ((length(tables.boxscore) >= expected.table.count) &&
    #    (nrow(tables.boxscore[[4]]) > 0) &&
    #    (nrow(tables.boxscore[[5]]) > 0)) {
      try.success <- TRUE
      break
    #} else {
    #  Sys.sleep(0.5)
    #  print(paste("Retry the page load...(", try.count, ")", sep = ""))
    #  try.count <- try.count + 1
    #}
  }

  if (!try.success) {
    print(paste("Insufficient tables->", key))
    irregular.games <- append(irregular.games, key)
    next
  }

  array.periods <- html.pbyp %>%
    html_nodes("#game__playbyplay__inner > ul.playbyplay_contents.playbyplay_contents_text > li") %>%
    html_children()

  print(array.periods)    
}
