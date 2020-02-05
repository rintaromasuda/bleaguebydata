gameId <- "0021900744"

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
  
  requiredHeaders <- add_headers("Referer" = "http://stats.nba.com",
                                 "User-Agent" = "RScript/1.0",
                                 "x-nba-stats-origin" = "stats",
                                 "x-nba-stats-token" = "true")
  
  httpResponse = GET(url, requiredHeaders, accept_json())
  res <- content(httpResponse)
  colNames <- res$resultSets[[1]]$headers
  numRows <- length(res$resultSets[[1]]$rowSet)

  for (i in 1:numRows) {
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

#####################
# Get Boxscore data #
#####################
boxscoreUrl <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2",
                      "?StartPeriod=0",
                      "&StartRange=0",
                      "&EndPeriod=0",
                      "&EndRange=0",
                      "&RangeType=0",
                      "&GameID=", gameId)
boxData <- GetDataViaApi(boxscoreUrl)

#########################
# Get Play-by-Play data #
#########################
playByPlayUrl <- paste0("https://stats.nba.com/stats/playbyplayv2",
                        "?StartPeriod=0",
                        "&EndPeriod=0",
                        "&GameID=", gameId)
playData <- GetDataViaApi(playByPlayUrl)
