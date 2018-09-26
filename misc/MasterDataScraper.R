if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(eeptools)) {
  install.packages("eeptools")
  library(eeptools)
}

names_team <- c(
  # East
  "–kŠC“¹",
  "H“c",
  "“È–Ø",
  "a’J",
  "A“Œ‹ž",
  "ç—t",
  # Center
  "VŠƒ",
  "•xŽR",
  "‰¡•l",
  "ìè",
  "ŽO‰“",
  "ŽO‰Í",
  # West
  "–¼ŒÃ‰®D",
  "Ž ‰ê",
  "‘åã",
  "‹ž“s",
  "•Ÿ‰ª",
  "—®‹…"
)

urls_team <- c(
  "https://www.bleague.jp/club_detail/?TeamID=702",
  "https://www.bleague.jp/club_detail/?TeamID=693",
  "https://www.bleague.jp/club_detail/?TeamID=703",
  "https://www.bleague.jp/club_detail/?TeamID=726",
  "https://www.bleague.jp/club_detail/?TeamID=706",
  "https://www.bleague.jp/club_detail/?TeamID=704",
  "https://www.bleague.jp/club_detail/?TeamID=695",
  "https://www.bleague.jp/club_detail/?TeamID=696",
  "https://www.bleague.jp/club_detail/?TeamID=694",
  "https://www.bleague.jp/club_detail/?TeamID=727",
  "https://www.bleague.jp/club_detail/?TeamID=697",
  "https://www.bleague.jp/club_detail/?TeamID=728",
  "https://www.bleague.jp/club_detail/?TeamID=729",
  "https://www.bleague.jp/club_detail/?TeamID=698",
  "https://www.bleague.jp/club_detail/?TeamID=700",
  "https://www.bleague.jp/club_detail/?TeamID=699",
  "https://www.bleague.jp/club_detail/?TeamID=753",
  "https://www.bleague.jp/club_detail/?TeamID=701"
)

df_team <- data.frame(
  Team = names_team,
  Url = urls_team,
  stringsAsFactors = FALSE
)
df_team$Team <- as.factor(df_team$Team)

for(row in 1:nrow(df_team)) {
  name_team <- df_team[row, "Team"]
  url_team <- df_team[row, "Url"]

  html_team <- read_html(url_team, encoding = "utf-8")
  
  urls_player <- html_team %>%
    html_nodes("#contents_inner > div > article > section:nth-child(4) > div > ul > li > a") %>%
    html_attr("href")
  
  for(url_player in urls_player) {
    html_player <- read_html(url_player, encoding = "utf-8")
    
    name_player <- html_player %>%
      html_node("#contents_inner > div > article > header > div > h2") %>%
      html_text()
    
    tables_player <- html_table(html_player)
    table_demo <- tables_player[[1]]
    
    player_school <- table_demo[1, c("X2")]
    player_hometown <- table_demo[2, c("X2")]
    
    player_birthday_raw <- table_demo[3, c("X2")]
    player_birthday <- as.Date(player_birthday_raw, "%Y”N%mŒŽ%d“ú")
    player_age <- floor(age_calc(player_birthday, Sys.Date(), units = "years"))

    player_height_raw <- table_demo[4, c("X2")]
    player_height <- as.numeric(gsub("cm", "", player_height_raw))
 
    player_weight_raw <- table_demo[5, c("X2")]
    player_weight <- as.numeric(gsub("kg", "", player_weight_raw))

    player_nationality <- table_demo[6, c("X2")]

    # Result
    df_player <- data.frame(
      Name = name_player,
      School = player_school,
      Hometown = player_hometown,
      Birthday_Raw = player_birthday_raw,
      Birthday = player_birthday,
      Age = player_age,
      Height_Raw = player_height_raw,
      Height = player_height,
      Weight_Raw = player_weight_raw,
      Weight = player_weight,
      Nationality = player_nationality      
    )
    
    print(df_player)
  }
}



