if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

result <- data.frame()

for (year in c(2016, 2017)) {
  for (league in c(1, 2)) {
    url_top <- paste("https://www.bleague.jp/stats/",
                     "?tab=",
                     as.character(league),
                     "&year=",
                     as.character(year),
                     sep = "")
    print(url_top)
    html_top <- read_html(url_top, encoding = "utf-8")
    
    urls_player <- html_top %>%
      html_nodes("#tbl-player > tbody > tr > td:nth-child(3) > a") %>%
      html_attr("href")
    urls_player <- paste(urls_player, "&year=", as.character(year), sep = "")
    
    names_player <- html_top %>%
      html_nodes("#tbl-player > tbody > tr > td:nth-child(3) > a") %>%
      html_text(trim = TRUE)

    startStr <- "PlayerID="
    endStr <- "&"
    ids_player <- substring(urls_player,
                            regexpr(startStr, urls_player) + nchar(startStr),
                            regexpr(endStr, urls_player) - nchar(endStr))

    index <- 0
    for (url in urls_player) {
      index = index + 1
      html_player <- read_html(url, encoding = "utf-8")
      tables_player <- html_table(html_player)
      df_game <- tables_player[[4]]
      df_game$PLAYER <- names_player[index]
      df_game$PID <- ids_player[index]
      df_game$YEAR <- year
      df_game$LEAGUE <- league
      result <- rbind(result, df_game)
    }
  }
}

write.csv(result, file = "player_games.csv")

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
readr::write_excel_csv(result, "player_games_excel.csv")
