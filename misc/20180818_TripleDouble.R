setwd("c:/users/rinta/Desktop")
load("player_games_data.RData")

df_cleaned <- result

# DATE
df_cleaned$DATE_RAW <- df_cleaned$DAY
df_cleaned$DATE <- as.Date(df_cleaned$DATE_RAW, "%Y.%m.%d")
summary(df_cleaned$DATE)

# MIN
df_cleaned$MIN_RAW <- df_cleaned$MIN

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

library(stringr)
df_cleaned$MIN <- ifelse(is.na(df_cleaned$MIN_RAW), 0,
                  ifelse(df_cleaned$MIN_RAW == "", 0,
                         sapply(str_split(df_cleaned$MIN_RAW, ":"), minStrToMinDec)))

# PPM (Point Per Minute)
df_cleaned$PPM <- ifelse(df_cleaned$MIN <= 0, 0, df_cleaned$PTS / df_cleaned$MIN)

# GAME_TYPE
df_cleaned$GAME_TYPE <- factor(c("Unknown"), levels = c("Unknown", "EC", "RS", "PS", "AS"))

## 2017-18 B1
df_cleaned[df_cleaned$YEAR == 2017 &
           df_cleaned$LEAGUE == 1 &
           df_cleaned$DATE < as.Date("2017-09-29"),]$GAME_TYPE <- "EC"

df_cleaned[df_cleaned$YEAR == 2017 &
           df_cleaned$LEAGUE == 1 &
           df_cleaned$DATE >= as.Date("2017-09-29") &
           df_cleaned$DATE < as.Date("2018-05-08"),]$GAME_TYPE <- "RS"

df_cleaned[df_cleaned$YEAR == 2017 &
           df_cleaned$LEAGUE == 1 &
           df_cleaned$DATE >= as.Date("2018-05-08"),]$GAME_TYPE <- "PS"

## 2017-18 B2

# ALL STAR for both B1 and B2
df_cleaned[df_cleaned$YEAR == 2017 &
           grepl("B.BLACK", df_cleaned$VS),]$GAME_TYPE <- "AS"

df.td <- df_cleaned[df_cleaned$PTS >= 10 &
                      df_cleaned$AS >= 10 &
                      df_cleaned$TR >= 10 &
                      df_cleaned$YEAR == 2017,]
View(df.td)
