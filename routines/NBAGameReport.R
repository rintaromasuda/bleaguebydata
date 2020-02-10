gameId <- "0021900744"
positions <- c("G", "F", "C")

if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
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

#############
# Functions #
#############
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

ConvertMinStrToDec <- function(min_str){
  Convert <- function(item) {
    min <- as.numeric(item[1])
    min <- min + as.numeric(item[2]) / 60
    round(min, 2)
  }

  array <- sapply(stringr::str_split(min_str, ":"), Convert)
  return(array)
}

GetShortName <- function(name){
  Convert <- function(items){
    stringr::str_c(items[-1], collapse = " ")
  }
  array <- sapply(stringr::str_split(name," "), Convert)
  return(array)
}

GetBoxscore <- function(){
  result <- data.frame()

  boxscoreUrl <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                        "?StartPeriod=0",
                        "&EndPeriod=0",
                        "&StartRange=0",
                        "&EndRange=0",
                        "&RangeType=0",
                        "&GameID=", gameId)
  result <- GetDataViaApi(boxscoreUrl)
  result$START_POSITION <- factor(result$START_POSITION, level = positions)
  result$MINDECIMAL <- ConvertMinStrToDec(result$MIN)
  result$MINDECIMAL <- ifelse(is.na(result$MINDECIMAL), 0, result$MINDECIMAL)
  result$PLAYER_NAME_SHORT <- GetShortName(result$PLAYER_NAME)

  return(result)
}

GetStarterData <- function(targetPeriod){
  # Since there is no perfect way to get start players of each period, we are making some heuristics here.
  # Getting first 30 sec boxscore of each period, we consider 5 players having largets MIN are the starters.
  result <- data.frame()

  if(targetPeriod <= 4){
    startRange <- (targetPeriod - 1) * 12 * 60 * 10
  }else{
    startRange <- (48 + ((targetPeriod - 5) * 5)) * 10
  }
  endRange <- startRange + 300 # 30 seconds

  boxscoreUrl <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                        "?StartPeriod=0",
                        "&EndPeriod=0",
                        "&StartRange=",
                        as.character(startRange),
                        "&EndRange=0",
                        as.character(endRange),
                        "&RangeType=2",
                        "&GameID=", gameId)
  boxData <- GetDataViaApi(boxscoreUrl)
  boxData$START_POSITION <- factor(boxData$START_POSITION, level = positions)
  boxData$MINDECIMAL <- ConvertMinStrToDec(boxData$MIN)

  if(targetPeriod == 1){
    # In case of first period, START_POSITION works for identifying starters
    result <- boxData[!is.na(boxData$START_POSITION),]
  }else if(targetPeriod > 1){
    result <- boxData %>%
      dplyr::group_by(TEAM_ID) %>%
      dplyr::top_n(n = 5, wt = MINDECIMAL) %>%
      as.data.frame()
  }

  return(result[, c("TEAM_ID", "PLAYER_ID")])
}

########
# Main #
########

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

playData$PCTIME_DECIMAL <- ConvertMinStrToDec(playData$PCTIMESTRING)
playData$PERIOD_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                    12 - playData$PCTIME_DECIMAL,
                                    5 - playData$PCTIME_DECIMAL)
playData$GAME_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                  playData$PERIOD_TIME_PAST + ((playData$PERIOD - 1) * 12),
                                  playData$PERIOD_TIME_PAST + 48 + ((playData$PERIOD - 5) * 5))
playData$SCOREMARGIN_DECIMAL <- ifelse(playData$SCOREMARGIN == "NULL", NA,
                                      ifelse(playData$SCOREMARGIN == "TIE", 0,
                                             as.integer(playData$SCOREMARGIN)))
playData[1,]$SCOREMARGIN_DECIMAL <- 0
playData %<>%
  tidyr::fill(SCOREMARGIN_DECIMAL) %>%
  as.data.frame()
playData %<>%
  group_by(GAME_TIME_PAST) %>%
  mutate(SCOREMARGIN_BYCLOCK = last(SCOREMARGIN_DECIMAL)) %>%
  as.data.frame()
playData$VISITOR_SCOREMARGIN_DECIMAL = -1 * playData$SCOREMARGIN_DECIMAL
playData$VISITOR_SCOREMARGIN_BYCLOCK = -1 * playData$SCOREMARGIN_BYCLOCK

######################
# Create team master #
######################
home <- playData %>%
  filter(HOMEDESCRIPTION != "NULL" & VISITORDESCRIPTION == "NULL") %>%
  filter(PLAYER1_TEAM_ID != "NULL" & PLAYER1_TEAM_NICKNAME != "NULL") %>%
  summarize(TEAM_ID = first(PLAYER1_TEAM_ID),
            TEAM_NAME = first(PLAYER1_TEAM_NICKNAME)) %>%
  mutate(TEAM_TYPE = "Home") %>%
  as.data.frame()

visitor <- playData %>%
  filter(HOMEDESCRIPTION == "NULL" & VISITORDESCRIPTION != "NULL") %>%
  filter(PLAYER1_TEAM_ID != "NULL" & PLAYER1_TEAM_NICKNAME != "NULL") %>%
  summarize(TEAM_ID = first(PLAYER1_TEAM_ID),
            TEAM_NAME = first(PLAYER1_TEAM_NICKNAME)) %>%
  mutate(TEAM_TYPE = "Visitor") %>%
  as.data.frame()

teamData <- rbind(home, visitor)

###########################
# Create gantt chart data #
###########################
ganntData <- data.frame()
for(targetPeriod in 1:max(playData$PERIOD)){
  print(paste0("Target Period -> ", targetPeriod))

  periodPlayData <- subset(playData, PERIOD == targetPeriod)
  firstGameTime <- dplyr::first(periodPlayData$GAME_TIME_PAST)
  lastGameTime <- dplyr::last(periodPlayData$GAME_TIME_PAST)
  firstScoreMargin <- dplyr::first(periodPlayData$SCOREMARGIN_BYCLOCK)
  firstVisitorScoreMargin <- dplyr::first(periodPlayData$VISITOR_SCOREMARGIN_BYCLOCK)
  lastScoreMargin <- dplyr::last(periodPlayData$SCOREMARGIN_BYCLOCK)
  lastVisitorScoreMargin <- dplyr::last(periodPlayData$VISITOR_SCOREMARGIN_BYCLOCK)
  # Get starters of the period
  starterData <- GetStarterData(targetPeriod)
  print(head(starterData, 10))
  periodGanntData <- starterData
  periodGanntData$DATA_TYPE <- "In"
  periodGanntData$GAME_TIME_PAST <- firstGameTime
  periodGanntData$SCOREMARGIN <- firstScoreMargin
  periodGanntData$VISITOR_SCOREMARGIN <- firstVisitorScoreMargin

  # Get all substitions of the period
  for(i in 1:nrow(periodPlayData)) {
    row <- periodPlayData[i, ]
    if(row$EVENTMSGTYPE == "8"){
      # Player out
      periodGanntData <- rbind(periodGanntData,
                         data.frame(
                           TEAM_ID = row$PLAYER1_TEAM_ID,
                           PLAYER_ID = row$PLAYER1_ID,
                           DATA_TYPE = "Out",
                           GAME_TIME_PAST = row$GAME_TIME_PAST,
                           SCOREMARGIN = row$SCOREMARGIN_BYCLOCK,
                           VISITOR_SCOREMARGIN = row$VISITOR_SCOREMARGIN_BYCLOCK))
      # Player in
      periodGanntData <- rbind(periodGanntData,
                         data.frame(
                           TEAM_ID = row$PLAYER2_TEAM_ID,
                           PLAYER_ID = row$PLAYER2_ID,
                           DATA_TYPE = "In",
                           GAME_TIME_PAST = row$GAME_TIME_PAST,
                           SCOREMARGIN = row$SCOREMARGIN_BYCLOCK,
                           VISITOR_SCOREMARGIN = row$VISITOR_SCOREMARGIN_BYCLOCK))
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
                        GAME_TIME_PAST = lastGameTime,
                        SCOREMARGIN = lastScoreMargin,
                        VISITOR_SCOREMARGIN = lastVisitorScoreMargin))

  # Add the period data to the total
  ganntData <- rbind(ganntData, periodGanntData)
}

# Pre-process gannt chart data
ganntData %<>%
  dplyr::arrange(TEAM_ID, PLAYER_ID, GAME_TIME_PAST) %>%
  dplyr::group_by(TEAM_ID, PLAYER_ID, DATA_TYPE) %>%
  dplyr::mutate(ITERATION = row_number())

############################
# Create per-duration data #
############################
inData <- subset(ganntData, DATA_TYPE == "In")
outData <- subset(ganntData, DATA_TYPE == "Out")
durationData <- merge(inData, outData, by = c("TEAM_ID", "PLAYER_ID", "ITERATION"))
durationData$X <- durationData$GAME_TIME_PAST.x + (durationData$GAME_TIME_PAST.y - durationData$GAME_TIME_PAST.x) / 2
durationData$NET <- ifelse(durationData$TEAM_ID == teamData[teamData$TEAM_TYPE == "Home", "TEAM_ID"],
                           durationData$SCOREMARGIN.y - durationData$SCOREMARGIN.x,
                           durationData$VISITOR_SCOREMARGIN.y - durationData$VISITOR_SCOREMARGIN.x)

# Get total boxscore
boxData <- GetBoxscore()

foo <- function(){
  ganntChart <- ggplot2::ggplot() +
    geom_point(data = boxData,
               ggplot2::aes(x = 0,
                           y = reorder(PLAYER_ID, MINDECIMAL)),
               alpha = 0)

  lastIter <- max(ganntData$ITERATION)
  for(iter in 1:lastIter){
    ganntChart <- ganntChart +
      ggplot2::geom_line(data = subset(ganntData, ITERATION == iter),
                         ggplot2::aes(x = GAME_TIME_PAST,
                                      y = PLAYER_ID,
                                     color = TEAM_ID),
                         size = 7)
  }

  ganntChart <- ganntChart +
    geom_text(data = boxData,
              ggplot2::aes(x = 0,
                           y = reorder(PLAYER_ID, MINDECIMAL),
                           label = PLAYER_NAME_SHORT),
              hjust = 1.1)

  ganntChart <- ganntChart +
    geom_text(data = durationData,
              ggplot2::aes(x = X,
                           y = PLAYER_ID,
                           label = NET))

  ganntChart <- ganntChart +
    coord_cartesian(xlim = c(-5, 48)) +
    theme(
      axis.text.y = element_blank(),
      legend.title = element_blank(),
      legend.position="top",
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    facet_wrap(~TEAM_ID, nrow = 2, scales = "free")
  print(ganntChart)
}
foo()
ggsave("NBA.jpg", width = 6, height = 9)
