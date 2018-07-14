setwd('C:/git/bleaguebydata/analysis')

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

df <- read.csv("Uto_201718.csv",
               header = TRUE,
               sep = ",",
               quote = "",
               stringsAsFactors = FALSE
)

df <- df[order(df$SEQ),]

df$MIN_STR <- df$MIN
df[df$MIN_STR == "",]$MIN_STR <- "00:00:00"

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

df$MIN <- sapply(str_split(df$MIN_STR, ":"), minStrToMinDec)

df$PPM <- ifelse(df$MIN > 0, df$PTS / df$MIN, 0)
df$ASPM <- ifelse(df$MIN > 0, df$AS / df$MIN, 0)
df$TOPM <- ifelse(df$MIN > 0, df$TO / df$MIN, 0)

df$ISRENSEN <- ifelse(df$RENSEN > 0, "あった", "なかった")
df$ISRENSEN_F <- as.factor(df$ISRENSEN)
summary(df)
df.season <- subset(df, df$TYPE == "SEASON")

library(ggplot2)

# Game-by-game
ggplot(df.season, aes(SEQ, PPM)) +
  geom_line() +
  xlab("試合") +
  ylab("出場1分あたりの得点数") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

df.season2 <- subset(df.season, MIN > 0)

# Box PPM
ggplot(df.season2, aes(ISRENSEN_F, PPM, fill=ISRENSEN)) +
  geom_boxplot() +
  xlab("前日に試合があったか？") +
  ylab("出場1分あたりの得点数") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

# Box FG%
ggplot(df.season2, aes(ISRENSEN_F, FGM/FGA, fill=ISRENSEN)) +
  geom_boxplot() +
  xlab("前日に試合があったか？") +
  ylab("フィールドゴール%") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

ggplot(df.season2, aes(ISRENSEN_F, ASPM, fill=ISRENSEN)) +
  geom_boxplot() +
  xlab("前日に試合があったか？") +
  ylab("出場1分あたりのアシスト数") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

ggplot(df.season2, aes(ISRENSEN_F, TOPM, fill=ISRENSEN)) +
  geom_boxplot() +
  xlab("前日に試合があったか？") +
  ylab("出場1分あたりのターンオーバー数") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

ggplot(df.season2, aes(ISRENSEN_F, MIN, fill=ISRENSEN)) +
  geom_boxplot() +
  xlab("前日に試合があったか？") +
  ylab("出場時間（分）") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("宇都直輝（富山） 2017-18シーズン")

