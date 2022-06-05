library(dplyr)
library(rvest)
library(stringr)
library(jsonlite)
library(bleaguer)

source("common.R")

league <- "B1"

df <- subset(GetGameSummary(), League == league & Season == b.current.season & HomeAway == "Home" & Category == "Regular")
keys <- unique(df$ScheduleKey)
length(keys)

df_result <- data.frame()
for (key in keys){
  print(paste("key:", key))

  jsonObj <- getJson(key)
  df_pbyp <- getPlayByPlay(jsonObj)

  df_result <- rbind(df_result, df_pbyp)
}

fileName <- paste0("play_by_play_", league, "_202021.csv")
write.csv(df_result, file = fileName,  row.names = FALSE, fileEncoding = "UTF-8")





