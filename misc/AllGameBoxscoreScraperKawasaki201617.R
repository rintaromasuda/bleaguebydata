if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

names_team <- c(
  "ìè"
)

urls_team <- c(
  "https://www.bleague.jp/schedule/?tab=1&year=2016&event=2&club=727"
)

result <- data.frame()

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
    Sys.sleep(1)
    
    url_game <- urls_game[idx_game]
    tryCatch(
      html_game <- read_html(url_game, encoding = "utf-8"),
      error = function(e) { stop(paste("Error in", url_game)) }
    )
    
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
    
    # Row bind the score table and the advanced stats table
    tables_game <- html_table(html_game)
    table <- rbind(tables_game[[1]], tables_game[[2]])
    
    if(name_home == name) {
      col_teamA <- 1
      col_teamB <- 3
      name_teamA <- name_home
      name_teamB <- name_away
    } else if(name_away == name) {
      col_teamA <- 3
      col_teamB <- 1
      name_teamA <- name_away
      name_teamB <- name_home
    } else {
      stop(paste("Invalid team name:", name))
    }
    
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
    df_AB$TEAM.A <- name_teamA
    df_AB$TEAM.B <- name_teamB
    
    result <- rbind(result, df_AB)
  }
}

write.csv(result, file = "all_games_201617_Kawasaki.csv")

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
readr::write_excel_csv(result, "all_games_201718_excel.csv")

