# Returns a JSON object by accessing www.bleague.jp
getJson <- function(scheduleKey) {
  url <- paste0(
    "https://www.bleague.jp/game_detail/?ScheduleKey=",
    as.character(scheduleKey)
  )
  page <- xml2::read_html(url, encoding = "utf-8")
  script_tags <- page %>%
    rvest::html_nodes("script") %>%
    rvest::html_text()

  for (tag in script_tags) {
    if (grepl("_contexts_s3id.data", tag, ignore.case = TRUE)) {
      jsonStr <- stringr::str_extract(tag, "\\{\".*\"\\}")
    }
  }
  
  jsonObj <- jsonlite::fromJSON(jsonStr)
  return(jsonObj)
}

# Returns a processed Play by Play data frame
getPlayByPlay <- function(jsonObj) {
  df_pbyp <- jsonObj$PlayByPlays
  
  df_pbyp <- within(df_pbyp, {
    RestMinInPeriod <- bleaguer::ConvertMinStrToDec(RestTime)
    PastMinInPeriod <- ifelse(Period <= 4, (10 - RestMinInPeriod), (5 - RestMinInPeriod))
    PastMinInGame <- ifelse(Period <= 4, (((Period - 1) * 10) + PastMinInPeriod), (40 + ((Period - 5) * 5) + PastMinInPeriod))
  })

  return(df_pbyp)
}

# Returns a home-away combined Boxscore data frame
getBoxScore <- function(jsonObj) {
  df_home <- jsonObj$HomeBoxscores
  df_home$HomeAway <- 1

  df_away <- jsonObj$AwayBoxscores
  df_away$HomeAway <- 2

  df_boxscore <- rbind(df_home, df_away)
  df_boxscore$TeamId <- df_boxscore$TeamID
  df_boxscore$TeamID <- NULL
  df_boxscore$PlayerId <- df_boxscore$PlayerID
  df_boxscore$PlayerID <- NULL

  return(df_boxscore)
}

# Returns a timeline data out of Play by Play and Boxscore
getTimeline <- function(df_pbyp) {
  df_result <- data.frame()

  homeOnCourt <- list()
  awayOnCourt <- list()
  homeTeamId <- 0
  awayTeamId <- 0
  lastPeriod <- 1
  
  # Just in case, order Play by Play with No
  df_pbyp %<>%
    arrange(No) %>%
    as.data.frame()
  
  for(rowNum in 1:nrow(df_pbyp)){
    row <- df_pbyp[rowNum,]
    lastPeriod <- row$Period
    
    if(row$ActionCD1 == "86"){
      # Player IN
      newItem <- data.frame(
        Type = "In",
        TeamId = row$TeamID,
        PlayerId = row$PlayerID1,
        PastMinInGame = row$PastMinInGame
      )
      df_result <- rbind(df_result, newItem)
      
      if(row$HomeAway == 1){
        homeOnCourt <- append(homeOnCourt, row$PlayerID1)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
        
        if(homeTeamId == 0){
          homeTeamId <- row$TeamID
        }
      }else{
        awayOnCourt <- append(awayOnCourt, row$PlayerID1)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
        if(awayTeamId == 0){
          awayTeamId <- row$TeamID
        }
      }
    }else if(row$ActionCD1 == "87"){
      # Player OUT
      newItem <- data.frame(
        Type = "Out",
        TeamId = row$TeamID,
        PlayerId = row$PlayerID1,
        PastMinInGame = row$PastMinInGame
      )
      df_result <- rbind(df_result, newItem)
      
      if(row$HomeAway == 1) {
        homeOnCourt <- homeOnCourt[homeOnCourt != row$PlayerID1]
      }else{
        awayOnCourt <- awayOnCourt[awayOnCourt != row$PlayerID1]
      }
    }else if(row$ActionCD1 == "89"){
      # Player OUT and IN together
    }
  }
  
  # Add last OUT for the players on the court
  lastMin <- 40 + ((lastPeriod - 4) * 5)
  for(onCourt in homeOnCourt){
    newItem <- data.frame(
      Type = "Out",
      TeamId = homeTeamId,
      PlayerId = onCourt,
      PastMinInGame = lastMin
    )
    df_result <- rbind(df_result, newItem)
  }
  
  for(onCourt in awayOnCourt){
    newItem <- data.frame(
      Type = "Out",
      TeamId = awayTeamId,
      PlayerId = onCourt,
      PastMinInGame = lastMin
    )
    df_result <- rbind(df_result, newItem)
  }
  
  return(df_result)
}
