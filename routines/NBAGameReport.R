########
# Args #
########
arg_GameId <- "2021900480"
arg_League <- "20"
arg_Season <- "2019-20"
arg_SeasonType <- "Regular+Season"

#############
# Variables #
#############
overTimeLength <- ifelse(arg_League == "00", 5, 2)
periodEnds <- c(c(12, 24, 36, 48), ((seq(1, 4) * overTimeLength) + 48))
c_JpnPlayers <- c("1629060", "1629139", "1629819") # Hachimura, Watanabe, Baba
c_Positions <- c("G", "F", "C")
c_PeriodData <- data.frame(
  Period = seq(1, 8),
  Label = c("Q1", "Q2", "Q3", "Q4", "OT1", "OT2", "OT3", "OT4"),
  End = periodEnds
)

############
# Settings #
############
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

if(!require(wesanderson)) {
  install.packages("wesanderson")
  library(wesanderson)
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
                        "&GameID=", arg_GameId)
  result <- GetDataViaApi(boxscoreUrl)
  result$START_POSITION <- factor(result$START_POSITION, level = c_Positions)
  result$MINDECIMAL <- suppressWarnings(ConvertMinStrToDec(result$MIN))
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
    startRange <- (48 + ((targetPeriod - 5) * overTimeLength)) * 60 * 10
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
                        "&GameID=", arg_GameId)
  print(boxscoreUrl)
  boxData <- GetDataViaApi(boxscoreUrl)
  boxData$START_POSITION <- factor(boxData$START_POSITION, level = c_Positions)
  boxData$MINDECIMAL <- ConvertMinStrToDec(boxData$MIN)

  if(targetPeriod == 1){
    # In case of first period, START_POSITION works for identifying starters
    result <- boxData[!is.na(boxData$START_POSITION),]
  }else if(targetPeriod > 1){
    result <- boxData %>%
      group_by(TEAM_ID) %>%
      top_n(n = 5, wt = MINDECIMAL) %>%
      as.data.frame()
  }

  return(result[, c("TEAM_ID", "PLAYER_ID")])
}

GetGameData <- function(teamId){
  gameLogUrl <- paste0("https://stats.nba.com/stats/teamgamelog",
                       "?DateFrom=",
                       "&DateTo=",
                       "&LeagueID=", arg_League,
                       "&Season=", arg_Season,
                       "&SeasonType=", arg_SeasonType,
                       "&TeamID=", teamId)
  data <- GetDataViaApi(gameLogUrl)
  return(subset(data, Game_ID == arg_GameId))
}

print("##################")
print("# Start Main()...#")
print("##################")

#########################
# Get Play-by-Play data #
#########################
playByPlayUrl <- paste0("https://stats.nba.com/stats/playbyplayv2",
                        "?StartPeriod=0",
                        "&EndPeriod=0",
                        "&GameID=", arg_GameId)
playData <- GetDataViaApi(playByPlayUrl)
playData$PERIOD <- as.integer(playData$PERIOD)
playData$EVENTNUM <- as.integer(playData$EVENTNUM)

# Pre-proces Play-by-Play data
playData$PCTIME_DECIMAL <- ConvertMinStrToDec(playData$PCTIMESTRING)
playData$PERIOD_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                    12 - playData$PCTIME_DECIMAL,
                                    overTimeLength - playData$PCTIME_DECIMAL)
playData$GAME_TIME_PAST <- ifelse(playData$PERIOD <= 4,
                                  playData$PERIOD_TIME_PAST + ((playData$PERIOD - 1) * 12),
                                  playData$PERIOD_TIME_PAST + 48 + ((playData$PERIOD - 5) * overTimeLength))
# Order Play-by-Play data by game-clock
playData %<>%
  arrange(GAME_TIME_PAST, PERIOD, EVENTNUM) %>%
  as.data.frame()

playData$SCOREMARGIN_DECIMAL <- ifelse(playData$SCOREMARGIN == "NULL", NA,
                                       ifelse(playData$SCOREMARGIN == "TIE", 0, 
                                              suppressWarnings(as.integer(playData$SCOREMARGIN))))
if(!is.na(playData[1, ]$SCOREMARGIN_DECIMAL)){
  print(playData)
  stop("Invalid Play-by-Play data")
}

playData[1,]$SCOREMARGIN_DECIMAL <- 0 # First one should be 0 so that we can fill the followings
playData %<>%
  tidyr::fill(SCOREMARGIN_DECIMAL) %>%
  as.data.frame()

# We get last score margin PER game clock here. This is necessary to handle the case
# when substitions happen when free-throw is going on
playData %<>%
  group_by(GAME_TIME_PAST) %>%
  mutate(SCOREMARGIN_BYCLOCK = last(SCOREMARGIN_DECIMAL)) %>%
  as.data.frame()
playData$VISITOR_SCOREMARGIN_DECIMAL = -1 * playData$SCOREMARGIN_DECIMAL
playData$VISITOR_SCOREMARGIN_BYCLOCK = -1 * playData$SCOREMARGIN_BYCLOCK

# We get positive delta of score margin for each row so that we can see how many points each row gets
playData %<>%
  arrange(GAME_TIME_PAST, PERIOD, EVENTNUM) %>%
  mutate(SCOREMARGIN_DELTA = ifelse(SCOREMARGIN_DECIMAL > lag(SCOREMARGIN_DECIMAL, 1),
                                    SCOREMARGIN_DECIMAL - lag(SCOREMARGIN_DECIMAL, 1, NA), 0),
         VISITOR_SCOREMARGIN_DELTA = ifelse(VISITOR_SCOREMARGIN_DECIMAL > lag(VISITOR_SCOREMARGIN_DECIMAL, 1),
                                            VISITOR_SCOREMARGIN_DECIMAL - lag(VISITOR_SCOREMARGIN_DECIMAL, 1, NA), 0)) %>%
  as.data.frame()

# Last period
lastPeriod <- max(playData$PERIOD)

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
# Visitor team should be displayed first
teamData$DISPLAY_TEAM_NAME <- factor(teamData$TEAM_NAME,
                                     levels = c(teamData[teamData$TEAM_TYPE == "Visitor", "TEAM_NAME"],
                                                teamData[teamData$TEAM_TYPE == "Home", "TEAM_NAME"]))
#################
# Get Game Info #
#################
homeGameLog <- GetGameData(teamData[teamData$TEAM_TYPE == "Home", "TEAM_ID"])
visitorGameLog <- GetGameData(teamData[teamData$TEAM_TYPE == "Visitor", "TEAM_ID"])

###########################
# Create Gantt Chart Data #
###########################
ganntData <- data.frame()
for(targetPeriod in 1:lastPeriod){
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
  arrange(TEAM_ID, PLAYER_ID, GAME_TIME_PAST) %>%
  group_by(TEAM_ID, PLAYER_ID, DATA_TYPE) %>%
  mutate(ITERATION = row_number())

ganntData <- merge(ganntData, teamData, by = "TEAM_ID")

############################
# Create per-duration data #
############################
inData <- subset(ganntData, DATA_TYPE == "In")
outData <- subset(ganntData, DATA_TYPE == "Out")
durationData <- merge(inData, outData, by = c("TEAM_ID", "DISPLAY_TEAM_NAME", "PLAYER_ID", "ITERATION"))
durationData$X <- durationData$GAME_TIME_PAST.x + (durationData$GAME_TIME_PAST.y - durationData$GAME_TIME_PAST.x) / 2
durationData$NET <- ifelse(durationData$TEAM_ID == teamData[teamData$TEAM_TYPE == "Home", "TEAM_ID"],
                           durationData$SCOREMARGIN.y - durationData$SCOREMARGIN.x,
                           durationData$VISITOR_SCOREMARGIN.y - durationData$VISITOR_SCOREMARGIN.x)
durationData$NET_STR <- ifelse(durationData$NET >= 0,
                               paste0("+", as.character(durationData$NET)),
                               as.character(durationData$NET ))
durationData$GAME_TIME_PAST_IN <- durationData$GAME_TIME_PAST.x
durationData$GAME_TIME_PAST_OUT <- durationData$GAME_TIME_PAST.y
durationData %<>%
  select(TEAM_ID,
         DISPLAY_TEAM_NAME,
         PLAYER_ID,
         ITERATION,
         X,
         NET,
         NET_STR,
         GAME_TIME_PAST_IN,
         GAME_TIME_PAST_OUT) %>%
  as.data.frame()

durationPtsData <- merge(durationData,
                         playData[, c("PLAYER1_ID",
                                      "SCOREMARGIN_DELTA",
                                      "VISITOR_SCOREMARGIN_DELTA",
                                      "GAME_TIME_PAST")],
                         by.x = c("PLAYER_ID"),
                         by.y = c("PLAYER1_ID")) %>%
                   filter(GAME_TIME_PAST_IN <= GAME_TIME_PAST & GAME_TIME_PAST < GAME_TIME_PAST_OUT) %>%
                   group_by(PLAYER_ID, ITERATION) %>%
                   summarize(PTS = sum(SCOREMARGIN_DELTA) + sum(VISITOR_SCOREMARGIN_DELTA)) %>%
                   as.data.frame()
durationData <- merge(durationData, durationPtsData, by = c("PLAYER_ID", "ITERATION"), all.y = TRUE)
durationData$TEXT <- ifelse(!is.na(durationData$PTS) & durationData$PTS > 0,
                            paste0(as.character(durationData$PTS),
                                   "p"),
                            "")

################
# Get Boxscore #
################
boxData <- GetBoxscore()
boxData <- merge(boxData, teamData, by = "TEAM_ID")

############################################################
# Plot gannt chart. This is where everything get together. #
############################################################
plotGanntChart <- function(){

  xTickBreaks <- c(0, c_PeriodData[c_PeriodData$Period <= lastPeriod, "End"])
  plotTitle <- paste0(teamData[teamData$TEAM_TYPE == "Visitor", "DISPLAY_TEAM_NAME"],
                     " vs. ",
                     teamData[teamData$TEAM_TYPE == "Home", "DISPLAY_TEAM_NAME"],
                     " (",
                     homeGameLog$GAME_DATE,
                     ")")
  
  ganntChart <- ggplot()
  
  # This is only for lininig up everyone in y-axis
  ganntChart <- ganntChart+
    geom_point(data = boxData,
               aes(x = 0,
                   y = reorder(PLAYER_ID, MINDECIMAL)),
               alpha = 0)

  # This is to draw all the bars
  lastIter <- max(ganntData$ITERATION)
  for(iter in 1:lastIter){
    ganntChart <- ganntChart +
      ggplot2::geom_line(data = subset(ganntData, ITERATION == iter),
                         aes(x = GAME_TIME_PAST,
                             y = PLAYER_ID,
                             color = DISPLAY_TEAM_NAME),
                         size = 7)
  }

  # This is to draw players' names
  ganntChart <- ganntChart +
    geom_text(data = boxData,
              aes(x = -10,
                  y = reorder(PLAYER_ID, MINDECIMAL),
                  label = PLAYER_NAME_SHORT,
                  fontface = ifelse(PLAYER_ID %in% c_JpnPlayers, "bold", "plain")),
              hjust = 0)

  # This is to draw information on the bars
  ganntChart <- ganntChart +
    geom_text(data = durationData,
              aes(x = X,
                  y = PLAYER_ID,
                  label = TEXT,
                  fontface = ifelse(PLAYER_ID %in% c_JpnPlayers, "bold", "plain")))

  # This is to draw lines in-between periods
  for(period in 0:lastPeriod){
    xIntercept = ifelse(period == 0, 0, c_PeriodData[c_PeriodData$Period == period, "End"])
    ganntChart <- ganntChart +
      geom_vline(xintercept =  xIntercept,
                 linetype="dashed",
                 color = "grey",
                 size=1,
                 alpha = 0.7)
  }
  
  # This is for all the visual adjustments
  ganntChart <- ganntChart +
    labs(x="",
         y="",
         title = plotTitle,
         subtitle = "The numbers show points made in the duration") +
    scale_x_continuous(breaks=xTickBreaks) +
    scale_color_manual(values=wes_palette(name= "GrandBudapest2")) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      legend.title = element_blank(),
      legend.position="top",
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    facet_wrap(~DISPLAY_TEAM_NAME, nrow = 2, scales = "free")

  # Print the chart
  print(ganntChart)
}
plotGanntChart()
ggsave(paste0("NBAGanntChart_",
              gsub(" ", "_", homeGameLog$MATCHUP),
              gsub(" ", "_", homeGameLog$GAME_DATE),
              ".jpg"),
       width = 6,
       height = 9)

################
# Twitter Post #
################
for(player in c_JpnPlayers){
  box <- subset(boxData, PLAYER_ID == player)
  if(nrow(box) > 0){
    matchup <- paste0(teamData[teamData$TEAM_TYPE == "Visitor", "DISPLAY_TEAM_NAME"],
                      " vs. ",
                      teamData[teamData$TEAM_TYPE == "Home", "DISPLAY_TEAM_NAME"],
                      " (",
                      homeGameLog$GAME_DATE,
                      ")")
    cat(matchup)
    cat("\n")

    score <- paste0(visitorGameLog$PTS,
                      " - ",
                     homeGameLog$PTS)
    cat(score)
    cat("\n")
    
    
    cat("\n")
    
    name <- ifelse(player == "1629060", "八村塁",
                   ifelse(player == "1629139", "渡邊雄太",
                          ifelse(player == "1629819", "馬場雄大", box[, "PLAYER_NAME"])))
    cat(name)
    cat("\n")
    cat(paste0(box[, "MIN"], " MIN"))
    cat("\n")
    cat(paste0(box[, "PTS"], " PTS",
               " (FG ", box[, "FGM"], "/", box[, "FGA"], ",",
               " 3P ", box[, "FG3M"], "/", box[, "FG3A"], ",",
               " FT ", box[, "FTM"], "/", box[, "FTA"], ")"))
    cat("\n")
    cat(paste0(box[, "REB"], " REB",
               " (OR|DR ", box[, "OREB"], "|", box[, "DREB"], ")"))
    cat("\n")
    cat(paste0(box[, "AST"], " AST"))
    cat("\n")
    cat(paste0(box[, "STL"], " STL"))
    cat("\n")
    cat(paste0(box[, "BLK"], " BLK"))
    cat("\n")
    cat("± ")
    cat(ifelse(box[, "PLUS_MINUS"] >= 0, "+", ""))
    cat(box[, "PLUS_MINUS"])

    if(arg_League == "00"){
      url <- paste0("https://stats.nba.com/game/",
                    arg_GameId,
                    "/")
    }else{
      url <- paste0("https://stats.gleague.nba.com/game/",
                    arg_GameId,
                    "/")
    }
    cat("\n")
    cat("\n")
    cat(url)
    cat("\n")
    cat("#NBA")
    cat("\n")
  }
}

print("################")
print("# End Main()...#")
print("################")
