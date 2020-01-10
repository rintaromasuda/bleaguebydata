if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

library(bleaguer)

df_teams <- subset(b.teams, Season == b.current.season)

df_result <- data.frame()

for(row in 1:nrow(df_teams)) {
  name_team <- df_teams[["NameLong"]][row]
  league_team <- df_teams[["League"]][row]
  url_team <- paste0("https://www.bleague.jp/club_detail/?TeamID=", df_teams[["TeamId"]][row])

  tryCatch(html_team <- read_html(url_team, encoding = "utf-8"),
           error = function(e) {
             print(paste("Error in", name_team))
             Sys.sleep(3000)
             stop
           },
           finally=print(paste(name_team, "has started")))

  urls_player <- html_team %>%
    html_nodes("#contents_inner > div > article > section:nth-child(4) > div > ul > li > a") %>%
    html_attr("href")

  for(url_player in urls_player) {
    tryCatch(html_player <- read_html(url_player, encoding = "utf-8"),
             error = function(e) {
               print(paste("Error in", url_player))
               Sys.sleep(3000)
               stop
             },
             finally=print(paste(url_player, "has finished")))

    startStr <- "PlayerID="
    id_player <- substring(url_player,
                           regexpr(startStr, url_player) + nchar(startStr))

    name_player <- html_player %>%
      html_node("#contents_inner > div > article > header > div > h2") %>%
      html_text()

    position_player <- html_player %>%
      html_node("#contents_inner > div > article > header > div > p.position") %>%
      html_text()

    tables_player <- html_table(html_player)
    table_demo <- tables_player[[1]]

    player_school <- table_demo[1, c("X2")]
    player_hometown <- table_demo[2, c("X2")]

    player_birthday_raw <- table_demo[3, c("X2")]
    player_birthday <- as.Date(player_birthday_raw, "%Y年%m月%d日")
    #player_age <- floor(age_calc(player_birthday, Sys.Date(), units = "years"))

    player_height_raw <- table_demo[4, c("X2")]
    player_height <- as.numeric(gsub("cm", "", player_height_raw))

    player_weight_raw <- table_demo[5, c("X2")]
    player_weight <- as.numeric(gsub("kg", "", player_weight_raw))

    player_nationality <- table_demo[6, c("X2")]

    # Result
    df_player <- data.frame(
      Team = name_team,
      League = league_team,
      Name = name_player,
      Position_Raw = position_player,
      School = player_school,
      Hometown = player_hometown,
      Birthday_Raw = player_birthday_raw,
      Birthday = player_birthday,
      Height_Raw = player_height_raw,
      Height = player_height,
      Weight_Raw = player_weight_raw,
      Weight = player_weight,
      Nationality = player_nationality,
      PlayerId = id_player
    )

    df_result <- rbind(df_result, df_player)
  }
}

df_result$BirthYear <- as.numeric(format(df_result$Birthday,'%Y'))

df_target <- subset(df_result, BirthYear %in% c(1996, 1984))
for(row in 1:nrow(df_target)) {
  team <- df_target[["Team"]][row]
  name <- df_target[["Name"]][row]
  bday <- df_target[["Birthday_Raw"]][row]
  line <- paste0(team, " ", name, "（", bday, "）")
  print(line)
}
