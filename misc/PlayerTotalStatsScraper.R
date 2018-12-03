if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

result <- data.frame()

for (year in c(2018)) {
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
  
    startStr <- "PlayerID="
    ids_player <- substring(urls_player,
                            regexpr(startStr, urls_player) + nchar(startStr),
                            nchar(urls_player))
    
    names_team <- html_top %>%
      html_nodes("#tbl-player > tbody > tr >td:nth-child(1) > a") %>%
      html_text(trim = TRUE)

    tables_total <- html_table(html_top)
    df_player <- tables_total[[1]]
    
    df_player$ID <- c(ids_player, rep("-1", nrow(df_player) - length(ids_player)))
    
    df_player$YEAR <- year
    df_player$LEAGUE <- league
    
    result <- rbind(result, subset(df_player, PLAYER != "PLAYER"))
  }
}

write.csv(result, file = "PlayerTotalStats_201819_20181201.csv")

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
readr::write_excel_csv(result, "PlayerTotalStats_201819_20181201_Excel.csv")
