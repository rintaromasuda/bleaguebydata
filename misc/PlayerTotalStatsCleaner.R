df <- read.csv("PlayerTotalStats_20181106.csv")

df$TEAM <- as.factor(df$TEAM)
df$ID <- as.numeric(df$ID)
df$G <- as.numeric(df$G)
df$PPG <- as.numeric(df$PPG)
df$APG <- as.numeric(df$APG)
df$RPG <- as.numeric(df$RPG)

# MIN
df$MIN_RAW <- df$MIN
df$MINPG_RAW <- df$MINPG

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

library(stringr)
df$MIN <- ifelse(is.na(df$MIN_RAW), 0,
          ifelse(df$MIN_RAW == "", 0,
          sapply(str_split(df$MIN_RAW, ":"), minStrToMinDec)))
df$MINPG <- ifelse(is.na(df$MINPG_RAW), 0,
                 ifelse(df$MINPG_RAW == "", 0,
                        sapply(str_split(df$MINPG_RAW, ":"), minStrToMinDec)))

View(df)
