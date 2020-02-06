gameId <- "0021900744"

if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(httr)) {
  install.packages("httr")
  library(httr)
}

if(!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

Sys.setlocale(locale = "English")

GetDataViaApi <- function(url){
  result <- data.frame()
  
  requiredHeaders <- httr::add_headers("Referer" = "http://stats.nba.com",
                                       "User-Agent" = "RScript/1.0",
                                       "x-nba-stats-origin" = "stats",
                                       "x-nba-stats-token" = "true")
  
  httpResponse = GET(url, requiredHeaders, accept_json())
  res <- content(httpResponse)
  colNames <- res$resultSets[[1]]$headers
  numRows <- length(res$resultSets[[1]]$rowSet)

  for(i in 1:numRows) {
    arrayRow <- as.character(res$resultSets[[1]]$rowSet[[i]])
    row <- as.data.frame(matrix(arrayRow, nrow = 1),
                         stringsAsFactors = FALSE)
    colnames(row) <- colNames
    result <- rbind(result, row)
  }
  
  return(result)
}

ConvertMinStrToDec <- function(min_str) {
  Convert <- function(item) {
    min <- as.numeric(item[1])
    min <- min + as.numeric(item[2]) / 60
    round(min, 2)
  }
  
  ls <- sapply(stringr::str_split(min_str, ":"), Convert)
  return(ls)
}

GetBoxScoreData <- function(targetPeriod){
  boxscoreUrl <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                        "?StartPeriod=",
                        as.character(targetPeriod),
                        "&StartRange=1",
                        "&EndPeriod=",
                        as.character(targetPeriod),
                        "&EndRange=1",
                        "&RangeType=1",
                        "&GameID=", gameId)
  boxData <- GetDataViaApi(boxscoreUrl)
  boxData$START_POSITION <- factor(boxData$START_POSITION, level = c("G", "F", "C"))
  return(boxData)
}

#########################
# Get Play-by-Play data #
#########################
playByPlayUrl <- paste0("https://stats.nba.com/stats/playbyplayv2",
                        "?StartPeriod=0",
                        "&EndPeriod=0",
                        "&GameID=", gameId)
playData <- GetDataViaApi(playByPlayUrl)
playData$PERIOD <- as.integer(playData$PERIOD)
playData$EVENTNUM <- as.integer(playData$EVENTNUM)

# Pre-proces Play-by-Play data
playData %<>%
  dplyr::arrange(EVENTNUM) %>%
  as.data.frame()

playData$PCTIMEDECIMAL <- ConvertMinStrToDec(playData$PCTIMESTRING)
playData$PERIOD_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                    12 - playData$PCTIMEDECIMAL,
                                    5 - playData$PCTIMEDECIMAL)
playData$GAME_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                  playData$PERIOD_TIME_PAST + ((playData$PERIOD - 1) * 12),
                                  playData$PERIOD_TIME_PAST + 48 + ((playData$PERIOD - 5) * 5))
lastPeriod <- max(playData$PERIOD)

###############################
# Create data for gantt chart #
###############################
ganntData <- data.frame()
for(targetPeriod in 1:lastPeriod){
  print(paste0("Target Period -> ", targetPeriod))
  # Get starters of the period
  boxData <- GetBoxScoreData(targetPeriod)
  periodGanntData <- boxData[!is.na(boxData$START_POSITION), c("TEAM_ID", "PLAYER_ID")]
  periodGanntData$DATA_TYPE <- "In"
  periodGanntData$GAME_TIME_PAST <- 0
  
  # Get all substitions of the period
  periodPlayData <- subset(playData, PERIOD == targetPeriod)
  for(i in 1:nrow(periodPlayData)) {
    row <- periodPlayData[i, ]
    if(row$EVENTMSGTYPE == "8"){
      # Player out
      periodGanntData <- rbind(periodGanntData,
                         data.frame(
                           TEAM_ID = row$PLAYER1_TEAM_ID,
                           PLAYER_ID = row$PLAYER1_ID,
                           DATA_TYPE = "Out",
                           GAME_TIME_PAST = row$GAME_TIME_PAST))
      # Player in
      periodGanntData <- rbind(periodGanntData,
                         data.frame(
                           TEAM_ID = row$PLAYER2_TEAM_ID,
                           PLAYER_ID = row$PLAYER2_ID,
                           DATA_TYPE = "In",
                           GAME_TIME_PAST = row$GAME_TIME_PAST))
    }
  }
  
  # Get all five players on the court at the end of the period
  onCourtData <- periodGanntData %>%
    mutate(COUNTER = 1) %>%
    group_by(TEAM_ID, PLAYER_ID) %>%
    summarise(InCount = sum(COUNTER[DATA_TYPE == "In"]),
              OutCount = sum(COUNTER[DATA_TYPE == "Out"])) %>%
    as.data.frame()
  
  if(nrow(subset(onCourtData, OutCount > InCount)) > 0){
    print(onCourtData)
    stop("Invalid substition data. Out > In.")
  } else if(nrow(subset(onCourtData, (InCount - OutCount) > 1)) > 0){
    print(onCourtData)
    stop("Invalid substition data. In - Out > 1.")
  }
  
  onCourtData <- onCourtData[onCourtData$InCount > onCourtData$OutCount, c("TEAM_ID", "PLAYER_ID")]
  periodGanntData <- rbind(periodGanntData,
                      data.frame(
                        TEAM_ID = onCourtData$TEAM_ID,
                        PLAYER_ID = onCourtData$PLAYER_ID,
                        DATA_TYPE = "Out",
                        GAME_TIME_PAST = row$GAME_TIME_PAST))
  
  # Add the period data to the total
  ganntData <- rbind(ganntData, periodGanntData)
  break
}

onCourtData <- ganntData %>%
  mutate(COUNTER = 1) %>%
  group_by(TEAM_ID, PLAYER_ID) %>%
  summarise(InCount = sum(COUNTER[DATA_TYPE == "In"]),
            OutCount = sum(COUNTER[DATA_TYPE == "Out"])) %>%
  View()
