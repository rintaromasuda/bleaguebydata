df <- read.csv("B1_201819_9setsu_Summary.csv")

library(dplyr)
library(ggplot2)

df %>%
  group_by(TEAM.A) %>%
  summarise(N = n())

df$POS.A <- df$X2POINTSFGA.A +
  df$X3POINTSFGA.A +
  df$TURNOVER.A +
  (df$FREE.THROWSA.A * 0.44) -
  df$OFFENSIVEREBOUNDS.A

df$POS.B <- df$X2POINTSFGA.B +
  df$X3POINTSFGA.B +
  df$TURNOVER.B +
  (df$FREE.THROWSA.B * 0.44) -
  df$OFFENSIVEREBOUNDS.B

df$PPP.A <- df$F.A / df$POS.A
df$PPP.B <- df$F.B / df$POS.B

drawAndSave <- function(gameIndex) {
  title <- paste("2018-19シーズン B1 ", gameIndex, "ゲーム終了時点（過去5試合のみ）", sep = "")
  filename <- paste("B1_", sprintf("%02d", gameIndex), ".jpeg", sep = "")
  
  df_filtered <- df %>%
    filter(GAMEIDX <= gameIndex & GAMEIDX >= 10) %>%
    as.data.frame()

  dd <- df_filtered %>%
    group_by(TEAM.A) %>%
    summarise(OFFEFF = mean(PPP.A),
              DFFEFF = mean(PPP.B),
              WIN = sum(F.A > F.B),
              LOSE = sum(F.A < F.B)) %>%
    as.data.frame()

  ggplot() +
    geom_point(data = dd, aes(x = OFFEFF, y = DFFEFF, size = (WIN/(WIN +LOSE)), color = TEAM.A)) +
    geom_text(data = dd, aes(x = OFFEFF, y = DFFEFF - 0.01, label = TEAM.A), size = 3) +
    guides(color=FALSE, size=guide_legend(title="勝率")) +
    xlab("1ポゼッション当たりの得点") +
    ylab("（相手チームの）1ポゼッション当たりの得点") +
    ggtitle(title) +
    guides(size=FALSE) +
    xlim(c(0.9, 1.2)) +
    ylim(c(0.9, 1.2))

  ggsave(filename = filename, width = 7, height = 5)
}

drawAndSave(10)
drawAndSave(15)

df$RESULT.A <- ifelse(df$F.A > df$F.B, "勝ち", "負け")
df$RESULT.A <- factor(df$RESULT.A, levels = c("負け", "勝ち"))

ggplot() +
  geom_boxplot(data = df, aes(x = TEAM.A, y = (DEFENSIVEREBOUNDS.A / (DEFENSIVEREBOUNDS.A + OFFENSIVEREBOUNDS.B))), color = "darkblue") +
  xlab("") +
  ylab("ディフェンスリバウンド取得率") +
  ggtitle("2018-19シーズン B1 15ゲーム終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggsave(filename = "DRB.png", width = 7, height = 5)

ggplot() +
  geom_boxplot(data = df, aes(x = TEAM.A, y = (OFFENSIVEREBOUNDS.A / (OFFENSIVEREBOUNDS.A + DEFENSIVEREBOUNDS.B))), color = "darkred") +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("2018-19シーズン B1 15ゲーム終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggsave(filename = "ORB.png", width = 7, height = 5)
