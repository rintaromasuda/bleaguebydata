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

# if (!require(dplyr)) {
#   install.packages("dplyr")
#   library(dplyr)
# }

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
  
  key <- df.games[idx, "ScheduleKey"]
  if (key %in% df.result$ScheduleKey | key %in% exception.games | key %in% scraped.games) {
    print(paste("Already done. Skipping->", key))
    next
  }

  homeTeamId <- df.games[idx, "HomeTeamId"]
  awayTeamId <- df.games[idx, "AwayTeamId"]

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
  
  print(names.home.players)
  print(names.away.players)
}

# fileName <- paste("games_summary_", as.character(season), ".csv", sep = "")
# write.csv(df.output, fileName, fileEncoding = "UTF-8", row.names = FALSE, quote = FALSE)

#game__boxscore__inner > ul.boxscore_contents > li.select > div:nth-child(2) > table > tbody > tr:nth-child(1) > td:nth-child(3) > a