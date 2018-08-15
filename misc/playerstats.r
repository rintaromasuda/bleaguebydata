if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

colNames <- c(
  "TEAM",
  "NO",
  "PLAYER",
  "PO",
  "G",
  "GS",
  "MIN_STR",
  "MINPG_STR",
  "PTS",
  "PPG",
  "FGM",
  "FGA",
  "FGP",
  "TPGM",
  "TPGA",
  "TPGP",
  "FTM",
  "FTA",
  "FTP",
  "OR",
  "DR",
  "TR",
  "RPG",
  "AS",
  "APG",
  "TO",
  "ST",
  "BS",
  "BSR",
  "F",
  "FD",
  "EFF"
)

loadPlayerStats <- function(fileName) {
  df <- read.csv(fileName,
                 header = TRUE,
                 sep = ",",
                 quote = "",
                 stringsAsFactors = FALSE,
                 col.names = colNames
                 )
  
  # Clean up texts with % at the end
  for (col in c("FGP", "TPGP", "FTP")) {
    df[,col] <- gsub("%", "", df[,col])
    df[,col] <- as.numeric(df[,col])
    df[,col] <- round((df[,col] / 100), 4)
  }
  
  # Convert "MM:SS" (or "MM:SS:00") to MM.(SS/60)
  
  minStrToMinDec <- function(item) {
    min <- as.numeric(item[1])
    min <- min + as.numeric(item[2]) / 60
    round(min, 2)
  }
  
  df$MIN <- sapply(str_split(df$MIN_STR, ":"), minStrToMinDec)
  df$MINPG <- sapply(str_split(df$MINPG_STR, ":"), minStrToMinDec)

  df$PPM <- round(df$PTS / df$MIN, 2)
  df$EFGP <- round((df$FGM + 0.5 * df$TPGM) / df$FGA, 4)
  df$TSP <- round(df$PTS / (2 * (df$FGA + (0.44 * df$FTA))) , 4)

  df$TOPG <- round(df$TO / df$G, 2)
  df$ORPG <- round(df$OR / df$G, 2)
  df$DRPG <- round(df$DR / df$G, 2)
  
  df$STPG <- round(df$ST / df$G, 2)
  df$BSPG <- round(df$BS / df$G, 2)  
      
  return(df)
}

b1.1st <- loadPlayerStats("./201617_b1.csv")
b2.1st <- loadPlayerStats("./201617_b2.csv")
b1.2nd <- loadPlayerStats("./201718_b1.csv")
b2.2nd <- loadPlayerStats("./201718_b2.csv")

