setwd("C:/git/bleaguebydata/misc")

library(dplyr)
library(stringr)
library(ineq)
library(ggplot2)

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

###########
# 2017-18 #
###########

df_201718 <- read.csv("B1_201718_AllGames_BoxScore_SJIS.csv",
                      encoding = "Cp932",
                      sep = ",",
                      header = TRUE)
colnames(df_201718)
dim(df_201718)
df_201718 <- df_201718[, c(-1)]
df_201718 <- df_201718[!duplicated(df_201718),]
dim(df_201718)
df_201718$SCHEDULEKEY <- df_201718$SHEDULEKEY
df_201718$SHEDULEKEY <- NULL

df_201718 %>%
  group_by(TEAM) %>%
  summarise(NumOfGames = n_distinct(SCHEDULEKEY))

df_201718 <- subset(df_201718, PLAYER != "TEAM / COACHES" & PLAYER != "合計")

df_201718 %>%
  group_by(TEAM, SCHEDULEKEY) %>%
  summarise(NumPlayers = n()) %>%
  View()

View(subset(df_201718, SCHEDULEKEY == 1988))

df_201718[c("4544", "4546"),] <- NA
df_201718[c("4554", "4560"),] <- NA
df_201718[c("4557", "4550"),] <- NA
df_201718 <- df_201718[!is.na(df_201718$PLAYER),]

df_201718$MIN_STR <- df_201718$MIN

df_201718$MIN <- sapply(str_split(df_201718$MIN_STR, ":"), minStrToMinDec)
summary(df_201718$MIN)

df_201718_Gini <- df_201718 %>%
  group_by(TEAM, SCHEDULEKEY) %>%
  summarise(GINI = ineq(MIN, Type == "Gini")) %>%
  as.data.frame()

View(df_201718_Gini)

###########
# 2018-19 #
###########

df_201819 <- read.csv("B1_201819_5Games_BoxScore.csv",
                      sep = ",",
                      header = TRUE)
colnames(df_201819)
dim(df_201819)
df_201819 <- df_201819[, c(-1)]
df_201819 <- df_201819[!duplicated(df_201819),]
dim(df_201819)
df_201819$SCHEDULEKEY <- df_201819$SHEDULEKEY
df_201819$SHEDULEKEY <- NULL
View(df_201819)

df_201819 %>%
  group_by(TEAM) %>%
  summarise(NumOfGames = n_distinct(SCHEDULEKEY))


df_201819 <- subset(df_201819, PLAYER != "TEAM / COACHES" & PLAYER != "合計")

df_201819 %>%
  group_by(TEAM, SCHEDULEKEY) %>%
  summarise(NumPlayers = n()) %>%
  View()

df_201819$MIN_STR <- df_201819$MIN

df_201819$MIN <- sapply(str_split(df_201819$MIN_STR, ":"), minStrToMinDec)
summary(df_201819$MIN)
View(df_201819)

df_201819_Gini <- df_201819 %>%
  group_by(TEAM, SCHEDULEKEY) %>%
  summarise(GINI = ineq(MIN, Type == "Gini")) %>%
  as.data.frame()

View(df_201819_Gini)

########
# JOIN #
########
df_201718_Gini$SEASON <- "2017-18"
df_201819_Gini$SEASON <- "2018-19"
df_Gini <- rbind(df_201718_Gini, df_201819_Gini)
df_Gini$SEASON <- as.factor(df_Gini$SEASON)
str(df_Gini)

########
# Plot #
########
ggplot() +
  geom_boxplot(data = subset(df_Gini, SEASON == "2017-18"), aes(x = TEAM, y = GINI),color="darkblue") +
  xlab("") +
  ylab("プレイタイムのジニ係数") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme_light()

ggplot() +
  geom_boxplot(data = subset(df_Gini, SEASON == "2017-18"), aes(x = TEAM, y = GINI),color="darkblue") +
  geom_point(data = subset(df_Gini, SEASON == "2018-19"), aes(x = TEAM, y = GINI,color="darkred"), size = 3) +
  xlab("") +
  ylab("プレイタイムのジニ係数") +
  ggtitle("2017-18 レギュラーシーズン60試合分（赤い点は2018-19シーズンの5試合）") +
  guides(colour=FALSE) +
  theme_light()

ggplot() +
  geom_boxplot(data = df_Gini, aes(x = SEASON, y = GINI),color="darkblue") +
  xlab("") +
  ylab("プレイタイムのジニ係数") +
  ggtitle("2017-18レギュラーシーズン60試合と2018-19の最初の5試合の比較") +
  guides(colour=FALSE) +
  theme_light()

View(df_Gini)
