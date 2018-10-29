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

df_201718_Summary <- read.csv("B1_201718_AllGames_Summary_SJIS.csv",
                              #encoding = "Cp932",
                              sep = ",",
                              header = TRUE)
str(df_201718_Summary)

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

df <- merge(df_201718_Summary, df_201718_Gini, by.x=c("TEAM.A","SHEDULEKEY"), by.y = c("TEAM", "SCHEDULEKEY"))
str(df)

ggplot() +
  geom_point(data = df, aes(x = abs(F.A - F.B), y = GINI))

ggplot() +
  geom_point(data = df, aes(x = FASTBREAKPOINTS.B, y = GINI))
View(df)

ggplot() +
  geom_boxplot(data = df, aes(x = (GAMEIDX %% 2) == 0, y = GINI))

