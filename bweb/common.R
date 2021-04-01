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

  return(df_boxscore)
}

# Returns a timeline data out of Play by Play and Boxscore
getTimeline <- function(df_pbyp, df_box) {
  df_result <- data.frame()

  homeOnCourt <- list()
  awayOnCourt <- list()
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
        TeamName = row$TeamNameJ,
        PlayerId = row$PlayerID1,
        PastMinInGame = row$PastMinInGame
      )
      df_result <- rbind(df_result, newItem)
      
      if(row$HomeAway == 1) {
        homeOnCourt <- append(homeOnCourt, row$PlayerID1)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- append(awayOnCourt, row$PlayerID1)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else if(row$ActionCD1 == "87"){
      # Player OUT
      newItem <- data.frame(
        Type = "Out",
        TeamId = row$TeamID,
        TeamName = row$TeamNameJ,
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
      # TBA
    }
  }
  
  # Add last OUT for the players on the court
  lastMin <- 40 + ((lastPeriod - 4) * 5)
  print(homeOnCourt)
  print(awayOnCourt)
  for(onCourt in homeOnCourt){
    newItem <- data.frame(
      Type = "Out",
      TeamId = "",
      TeamName = "",
      PlayerId = onCourt,
      PastMinInGame = lastMin
    )
    df_result <- rbind(df_result, newItem)
  }
  
  for(onCourt in awayOnCourt){
    newItem <- data.frame(
      Type = "Out",
      TeamId = "",
      TeamName = "",
      PlayerId = onCourt,
      PastMinInGame = lastMin
    )
    df_result <- rbind(df_result, newItem)
  }
  
  return(df_result)
}

getTimelineData <- function(df_pbyp, df_boxscore){
  df <- df_pbyp %>% arrange(DataNo) %>% as.data.frame()
  
  df_result <- data.frame()
  
  homeOnCourt <- list()
  awayOnCourt <- list()
  lastPeriod <- 1
  
  for(rowNum in 1:nrow(df)){
    row <- df[rowNum,]
    
    lastPeriod <- row$Period
    
    if(row$ActionCd == "86"){
      player_in <- str_extract(row$PlayerData, "#[0-9]+")
      df_data <- data.frame(
        Team = row$Team,
        Number = player_in,
        Type = "In",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)
      
      if(row$Team == home.teamName) {
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else if(row$ActionCd == "89"){
      splitted <- str_split(row$PlayerData, " ")[[1]]
      player_out <- str_extract(splitted[1], "#[0-9]+")
      player_in <- str_extract(splitted[4], "#[0-9]+")
      
      df_data <- data.frame(
        Team = row$Team,
        Number = player_out,
        Type = "Out",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)
      
      df_data <- data.frame(
        Team = row$Team,
        Number = player_in,
        Type = "In",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)
      
      if(row$Team == home.teamName) {
        homeOnCourt <- homeOnCourt[homeOnCourt != player_out]
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- awayOnCourt[awayOnCourt != player_out]
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else{
      # Ignore
    }
  }
  
  lastMin <- 40
  if(lastPeriod > 4){
    lastMin <- lastMin + ((lastPeriod - 4) * 5)
  }
  
  # Add last OUT for the players on court
  for(onCourt in homeOnCourt){
    df_data <- data.frame(
      Team = home.teamName,
      Number = onCourt,
      Type = "Out",
      Time = lastMin
    )
    df_result <- rbind(df_result, df_data)
  }
  
  for(onCourt in awayOnCourt){
    df_data <- data.frame(
      Team = away.teamName,
      Number = onCourt,
      Type = "Out",
      Time = lastMin
    )
    df_result <- rbind(df_result, df_data)
  }
  
  return(df_result)
}