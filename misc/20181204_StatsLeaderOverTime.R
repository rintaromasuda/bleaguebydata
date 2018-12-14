df_cleaned <- read.csv("player_games_201819_20181204.csv")
  
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

## 2018-19 B1
df_cleaned[df_cleaned$YEAR == 2018 &
           df_cleaned$LEAGUE == 1 &
           df_cleaned$DATE < as.Date("2018-10-04"),]$GAME_TYPE <- "EC"

df_cleaned[df_cleaned$YEAR == 2018 &
             df_cleaned$LEAGUE == 1 &
             df_cleaned$DATE >= as.Date("2018-10-04") &
             df_cleaned$DATE < as.Date("2019-05-08"),]$GAME_TYPE <- "RS"

library(dplyr)
library(ggplot2)
library(lazyeval)

cumplot <- function(ids, stat, title, ylab) {
  df <- df_cleaned %>%
          filter(YEAR == 2018) %>%
          filter(LEAGUE == 1) %>%
          filter(GAME_TYPE == "RS") %>%
          filter(MIN > 0) %>%
          filter(PID %in% ids)

  df <- df %>%
          group_by(PLAYER) %>%
          mutate(SEQ = row_number(DATE))

  df <- df %>%
          group_by(PLAYER) %>%
          arrange(PLAYER,SEQ) %>%
          mutate_(CUM = interp(~ cumsum(x), x = as.name(stat)))

  ggplot(df, aes(SEQ, CUM/SEQ, col = PLAYER)) +
    geom_line(size = 1.2) +
    theme_bw() +
    ylab(ylab) +
    xlab("試合数") +
    ggtitle(title) +
    #theme(legend.position = c(0.9, 0.15)) +
    theme(legend.title=element_blank()) +
    #theme(legend.background = element_rect(fill=alpha('blue', 0.0))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(type = 'div', palette = 'Set3', direction = 1)
  
  ggsave("3pointOverTime.jpeg", width = 8, height = 5)
}

# Rebound
cumplot(c(8490,8591,8649,9040,12585),
        "TR",
        "1試合平均リバウンド数の推移（2017-18シーズン）",
        "1試合平均リバウンド数")

cumplot(c(9040,8490,8593,12595,8503),
        "PTS",
        "1試合平均得点数の推移（2017-18シーズン）",
        "1試合平均得点数")

cumplot(c(8503,8592,9055,8721,8589),
        "PTS",
        "1試合平均得点数の推移（2017-18シーズン）",
        "1試合平均得点数")

cumplot(c(8503,9489,8645,8596,9037),
        "AS",
        "1試合平均アシスト数の推移（2017-18シーズン）",
        "1試合平均アシスト数")

cumplot(c(12585,12640,12587,12595,12582),
        "BS",
        "1試合平均ブロックショット数の推移（2017-18シーズン）",
        "1試合平均ブロックショット数")

cumplot(c(10815, 8463, 8731, 8721, 8645, 9055, 8592, 8483, 8655, 8712),
        "X3FGM",
        "1試合平均スリーポイント成功数の推移（2018-19シーズン 19ゲーム終了時点）",
        "1試合平均スリーポイント成功数")

cumplot(c(8579,12595,10256,9489,8447),
        "ST",
        "1試合平均スティール数の推移（2017-18シーズン）",
        "1試合平均スティール数")

cumplot2 <- function(ids, stat, stat2, title, ylab) {
  df <- df_cleaned %>%
    filter(YEAR == 2018) %>%
    filter(LEAGUE == 1) %>%
    filter(GAME_TYPE == "RS") %>%
    filter(MIN > 0) %>%
    filter(PID %in% ids)
  
  df <- df %>%
    group_by(PLAYER) %>%
    mutate(SEQ = row_number(DATE))
  
  df <- df %>%
    group_by(PLAYER) %>%
    arrange(PLAYER,SEQ) %>%
    mutate_(CUM = interp(~ cumsum(x), x = as.name(stat)),
            CUM2 = interp(~ cumsum(x), x = as.name(stat2)))
  
  ggplot() +
    geom_line(data = df, aes(x = SEQ, y = CUM/CUM2, col = PLAYER), size = .8) +
    ylab(ylab) +
    xlab("試合数") +
    ggtitle(title) +
    ylim(c(.25,.5)) +
    theme_bw()
#    theme(legend.position = c(0.9, 0.2)) +
#    theme(legend.title=element_blank()) +
#    theme(legend.background = element_rect(fill=alpha('blue', 0.0))) +
#    theme(plot.title = element_text(hjust = 0.5)) +
#    scale_color_brewer(type = 'div', palette = 'Set1', direction = 1) +
#   ylim(c(.5,1))
}

cumplot2(c(8583,10815,8592,8463,8731,8729,18436,8571,8712,8499),
        "X3FGM",
        "X3FGA",
        "スリーポイント成功率の推移（2018-19シーズン 19ゲーム終了時点）",
        "スリーポイント成功率")

cumplot2(c(8657,8449,8487,8592,8475),
         "FTM",
         "FTA",
         "フリースロー成功率の推移（2017-18シーズン）",
         "フリースロー成功率")

cumplot3 <- function(ids, stat, title, ylab) {
  df <- df_cleaned %>%
    filter(YEAR == 2017) %>%
    filter(LEAGUE == 1) %>%
    filter(GAME_TYPE == "RS") %>%
    filter(MIN > 0) %>%
    filter(PID %in% ids)
  
  df <- df %>%
    group_by(PLAYER) %>%
    mutate(SEQ = row_number(DATE))
  
  df <- df %>%
    group_by(PLAYER) %>%
    arrange(PLAYER,SEQ) %>%
    mutate_(CUM = interp(~ cumsum(x), x = as.name(stat)))
  
  ggplot(df, aes(SEQ, CUM, col = PLAYER)) +
    geom_line(size = 0.8) +
    theme_bw() +
    ylab(ylab) +
    xlab("試合数") +
    ggtitle(title) +
    theme(legend.position = c(0.9, 0.2)) +
    theme(legend.title=element_blank()) +
    theme(legend.background = element_rect(fill=alpha('blue', 0.0))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer(type = 'div', palette = 'Set1', direction = 1)
}

