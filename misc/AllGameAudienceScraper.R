if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

names_team_201718 <- c(
  "北海道",
  "島根",
  "栃木",
  "SR渋谷",
  "A東京",
  "千葉",
  "新潟",
  "富山",
  "横浜",
  "川崎",
  "三遠",
  "三河",
  "名古屋D",
  "滋賀",
  "大阪",
  "京都",
  "西宮",
  "琉球"
)

urls_team_201718 <- c(
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=702",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=720",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=703",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=726",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=706",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=704",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=695",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=696",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=694",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=727",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=697",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=728",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=729",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=698",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=700",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=699",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=718",
  "https://www.bleague.jp/schedule/?tab=1&year=2017&event=2&club=701"
)

names_team_201819 <- c(
  "北海道",
  "秋田",
  "栃木",
  "SR渋谷",
  "A東京",
  "千葉",
  "新潟",
  "富山",
  "横浜",
  "川崎",
  "三遠",
  "三河",
  "名古屋D",
  "滋賀",
  "大阪",
  "京都",
  "福岡",
  "琉球"
)

urls_team_201819 <- c(
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=702",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=693",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=703",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=726",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=706",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=704",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=695",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=696",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=694",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=727",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=697",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=728",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=729",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=698",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=700",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=699",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=753",
  "https://www.bleague.jp/schedule/?tab=1&year=2018&event=2&club=701"
)

names_b2_team_201819 <- c(
  "青森",
  "仙台",
  "山形",
  "福島",
  "茨城",
  "群馬",
  "東京Z",
  "八王子",
  "金沢",
  "信州",
  "FE名古屋",
  "西宮",
  "奈良",
  "島根",
  "広島",
  "香川",
  "愛媛",
  "熊本"
)

urls_b2_team_201819 <- c(
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=708",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=692",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=710",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=711",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=712",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=713",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=715",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=749",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=750",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=716",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=717",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=718",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=719",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=720",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=721",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=722",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=723",
  "https://www.bleague.jp/schedule/?tab=2&year=2018&event=7&club=724"
)

#names_team <- names_team_201718
#urls_team <- urls_team_201718
names_team <- names_team_201819
urls_team <- urls_team_201819
#names_team <- names_b2_team_201819
#urls_team <- urls_b2_team_201819

result_summary <- data.frame()

for(idx_team in 1:length(names_team)) {
  name <- names_team[idx_team]
  url <- urls_team[idx_team]
  
  html_team <- read_html(url, encoding = "utf-8")
  
  tryCatch(
    html_team <- read_html(url, encoding = "utf-8"),
    error = function(e) { stop(paste("Error in", name)) },
    finally=print(paste(name, "has started."))
  )
  
  urls_game <- html_team %>%
    html_nodes("#round_list > dd > ul > li > div.gamedata_left > div.data_link > div.state_link.btn.report > a") %>%
    html_attr("href")
  
  num_games = length(urls_game)
  
  for(idx_game in 1:num_games) {
    url_game <- urls_game[idx_game]
    
    # Access each game
    Sys.sleep(1)
    html_game <- read_html(url_game, encoding = "utf-8")
    
    startStr <- "ScheduleKey="
    id_schedule <- substring(url_game,
                            regexpr(startStr, url_game) + nchar(startStr))
    
    date_game <- html_game %>%
      html_nodes("#game__top__inner > div.date_wrap > p:nth-child(2) > span") %>%
      html_text()
    
    name_home <- html_game %>%
      html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home.win > div.team_name > p.for-sp") %>%
      html_text()
    
    # If NULL, get the team name from another class
    if (identical(name_home, character(0))) {
      name_home <- html_game %>%
        html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home > div.team_name > p.for-sp") %>%
        html_text()      
    }
    
    name_away <- html_game %>%
      html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away.win > div.team_name > p.for-sp") %>%
      html_text()
    
    # If NULL, get the team name from another class
    if (identical(name_away, character(0))) {
      name_away <- html_game %>%
        html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away > div.team_name > p.for-sp") %>%
        html_text()        
    }
    
    print(paste("Game #", idx_game, name_home, "vs.", name_away))
    
    
    if(name_home == name) {
      col_teamA <- 1
      col_teamB <- 3
      name_teamA <- name_home
      name_teamB <- name_away
      isHome_teamA <- TRUE
      isHome_teamB <- FALSE
    } else if(name_away == name) {
      col_teamA <- 3
      col_teamB <- 1
      name_teamA <- name_away
      name_teamB <- name_home
      isHome_teamA <- FALSE
      isHome_teamB <- TRUE
    } else {
      stop(paste("Invalid team name:", name))
    }

    df_AB <- data.frame(
      GAMEIDX = idx_game,
      DATE = date_game,
      SHEDULEKEY = id_schedule,
      TEAM.A = name_teamA,
      TEAM.B = name_teamB,
      ISHOME.A = isHome_teamA,
      ISHOME.B = isHome_teamB
    )

    attendance <- html_game %>%
      html_nodes("#game__top__inner > div.place_wrap > p.Attendance") %>%
      html_text()
    
    df_AB$ATTENDANCE_RAW <- attendance
    
    result_summary <- rbind(result_summary, df_AB)
  }
}

write.csv(result_summary, file = "B1_201819_11setsu_Attendance.csv")
