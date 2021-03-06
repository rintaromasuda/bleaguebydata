if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
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

remDr <- RSelenium::remoteDriver(remoteServerAddr = "40.115.154.189",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()

result_summary <- data.frame()
result_boxscore <- data.frame()

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
#  if(num_games != 60) {
#    stop(paste("# of games has to be 60 but", num_games))
#  }
  
  for(idx_game in 1:num_games) {
    url_game <- urls_game[idx_game]
    remDr$navigate(url_game)
    
    maxAttempt <- 50
    numAttempt <- 1
    while(numAttempt < maxAttempt) {
      Sys.sleep(3)
      pageSource <- remDr$getPageSource()
      html_game <- read_html(pageSource[[1]])
      tables_game <- html_table(html_game)
      if(length(tables_game) >= 5) {
        if (nrow(tables_game[[4]]) > 0 & nrow(tables_game[[5]]) > 0) {
          chk <- tables_game[[1]]
          if (nrow(subset(chk, X2 == "F")) > 0) {
            break
          }
          else {
            print("Try getPageSource() again #3...")
            numAttempt <- numAttempt + 1         
          }
        }
        else {
          print("Try getPageSource() again #1...")
          numAttempt <- numAttempt + 1         
        }
      }
      else{
        print("Try getPageSource() again #2...")
        numAttempt <- numAttempt + 1
      }
    }
    
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

    # Row bind the score table and the advanced stats table
    table_scoreqf <- subset(tables_game[[1]], X2 != "EX1" & X2 != "EX2")
    table_scoreex <- subset(tables_game[[1]], X2 == "EX1" | X2 == "EX2" | X2 == "EX3")
    table <- rbind(table_scoreqf, tables_game[[2]])    
        
    getTeamStats <- function(colIdx) {
      df <- data.frame(
        table[1, colIdx],
        table[2, colIdx],
        table[3, colIdx],
        table[4, colIdx],
        table[5, colIdx],
        table[6, colIdx],
        table[7, colIdx],
        table[8, colIdx],
        table[9, colIdx],
        table[10,colIdx],
        table[11,colIdx],
        table[12,colIdx],
        table[13,colIdx],
        table[14,colIdx],
        table[15,colIdx],
        table[16,colIdx],
        table[17,colIdx],
        table[18,colIdx],
        table[19,colIdx],
        table[20,colIdx],
        table[21,colIdx],
        table[22,colIdx],
        table[23,colIdx],
        table[24,colIdx],
        table[25,colIdx],
        table[26,colIdx],
        table[27,colIdx],
        table[28,colIdx])
      return(df)
    }
  
    colnames <- gsub(" ", "", toupper(table$X2))
    
    df_teamA <- getTeamStats(col_teamA)
    names(df_teamA) <- paste(colnames, ".A", sep = "")
    
    df_teamB <- getTeamStats(col_teamB)
    names(df_teamB) <- paste(colnames, ".B", sep = "")
    
    df_AB <- cbind(df_teamA, df_teamB)
    df_AB$GAMEIDX <- idx_game
    df_AB$DATE <- date_game
    df_AB$SHEDULEKEY <- id_schedule
    df_AB$TEAM.A <- name_teamA
    df_AB$TEAM.B <- name_teamB
    df_AB$ISHOME.A <- isHome_teamA
    df_AB$ISHOME.B <- isHome_teamB

    attendance <- html_game %>%
      html_nodes("#game__top__inner > div.place_wrap > p.Attendance") %>%
      html_text()
    
    df_AB$ATTENDANCE_RAW <- attendance
        
    if(is.na(table_scoreex[1, col_teamA])) {
      df_AB$EX1.A <- 0
      df_AB$EX1.B <- 0     
    }
    else {
      df_AB$EX1.A <- table_scoreex[1, col_teamA]
      df_AB$EX1.B <- table_scoreex[1, col_teamB]
    }
 
    if(is.na(table_scoreex[2, col_teamA])) {
      df_AB$EX2.A <- 0
      df_AB$EX2.B <- 0     
    }
    else {
      df_AB$EX2.A <- table_scoreex[2, col_teamA]
      df_AB$EX2.B <- table_scoreex[2, col_teamB]
    }
    
    if(is.na(table_scoreex[3, col_teamA])) {
      df_AB$EX3.A <- 0
      df_AB$EX3.B <- 0     
    }
    else {
      df_AB$EX3.A <- table_scoreex[3, col_teamA]
      df_AB$EX3.B <- table_scoreex[3, col_teamB]
    }
    
    result_summary <- rbind(result_summary, df_AB)
    
    ## For box score
    table_box_home <- tables_game[[4]]
    table_box_away <- tables_game[[5]]
    
    table_box_home$SHEDULEKEY <- id_schedule
    table_box_away$SHEDULEKEY <- id_schedule
    table_box_home$TEAM <- ifelse(isHome_teamA, name_teamA, name_teamB)
    table_box_away$TEAM <- ifelse(isHome_teamA, name_teamB, name_teamA)
        
    table_box <- rbind(table_box_home, table_box_away)
    
    result_boxscore <- rbind(result_boxscore, table_box)
  }
}

write.csv(result_summary, file = "B1_201819_11setsu_Summary.csv")
write.csv(result_boxscore, file = "B1_201819_11setsu_BoxScore.csv")
