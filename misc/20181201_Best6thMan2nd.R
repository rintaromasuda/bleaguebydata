library(dplyr)
library(ggplot2)
library(knitr)

df <- read.csv("PlayerTotalStats_201819_20181201.csv")

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

####
#
####

df_b1 <- subset(df, LEAGUE == 1)

ggplot() +
  geom_point(data = subset(df_b1, G < 16 | GS > 4 | MINPG < 20), aes(x = G, y = GS, size = MINPG), alpha = 0.3) +
  geom_point(data = subset(df_b1, G >= 16 & GS <= 4 & MINPG >= 20), aes(x = G, y = GS, size = MINPG), color = "red", alpha = 0.3) +
  xlab("出場試合数") +
  ylab("スターターとしての出場試合数") +
  ggtitle("2018-19 B1 19ゲーム終了時点の出場試合数（〇の大きさは試合平均プレイタイム）") +
  theme_bw()

ggsave("SixthMan.jpeg", width = 8, height = 5)

dd <- df_b1 %>%
  filter(G >= 16 & GS <= 4 & MINPG >= 20) %>%
  select(c("TEAM", "PLAYER", "G", "GS", "MINPG_RAW", "PPG", "FG.", "RPG", "APG"))
dd <- dd[order(dd$MINPG_RAW, decreasing = TRUE), ]
kable(dd)
