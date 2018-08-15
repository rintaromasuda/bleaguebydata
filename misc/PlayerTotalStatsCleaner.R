setwd("c:/users/rinta/Desktop")
load("player_total_data.RData")

str(result)
df <- result

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

b1.1st <- df[df$LEAGUE == 1 & df$YEAR == 2016 & df$ID > 0,]
b1.2nd <- df[df$LEAGUE == 1 & df$YEAR == 2017 & df$ID > 0,]
mg <- merge(b1.1st, b1.2nd, by = "ID")

mg$G_DELTA <- mg$G.y - mg$G.x
mg$PPG_DELTA <- mg$PPG.y - mg$PPG.x
mg$APG_DELTA <- mg$APG.y - mg$APG.x
mg$RPG_DELTA <- mg$RPG.y - mg$RPG.x
mg$MINPG_DELTA <- mg$MINPG.y - mg$MINPG.x
row.names(mg) <- NULL

library(knitr)
kable(head(mg[order(mg$PPG_DELTA, decreasing = TRUE), c("PLAYER.x", "TEAM.y", "PPG_DELTA", "G_DELTA", "MINPG_DELTA")], 20), row.names = FALSE)

kable(head(mg[order(mg$APG_DELTA, decreasing = TRUE), c("PLAYER.x", "TEAM.y", "APG_DELTA", "G_DELTA", "MINPG_DELTA")], 100), row.names = FALSE)

kable(head(mg[order(mg$RPG_DELTA, decreasing = TRUE), c("PLAYER.x", "TEAM.y", "RPG_DELTA", "G_DELTA", "MINPG_DELTA")], 20), row.names = FALSE)

kable(head(mg[mg$ID == 8596, c("PLAYER.x", "TEAM.y", "PPG_DELTA", "APG_DELTA","RPG_DELTA", "G_DELTA", "MINPG_DELTA")], 20), row.names = FALSE)
8596

View(mg)
