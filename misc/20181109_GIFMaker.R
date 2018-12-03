df <- read.csv("B1_201819_11setsu_Summary.csv")

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
  title <- paste("2018-19シーズン B1 ", gameIndex, "ゲーム終了時点", sep = "")
  filename <- paste("B1_", sprintf("%02d", gameIndex), ".jpeg", sep = "")
  
  df_filtered <- df %>%
    filter(GAMEIDX <= gameIndex) %>%
    as.data.frame()

  dd <- df_filtered %>%
    group_by(TEAM.A) %>%
    summarise(OFFEFF = mean(PPP.A),
              DFFEFF = mean(PPP.B),
              WIN = sum(F.A > F.B),
              LOSE = sum(F.A < F.B)) %>%
    as.data.frame()

  ggplot() +
    geom_point(data = dd, aes(x = OFFEFF, y = DFFEFF, color = TEAM.A), alpha = 0.7, size = 4) +
    geom_text(data = dd, aes(x = OFFEFF, y = DFFEFF - 0.005, label = TEAM.A), size = 3) +
    guides(color=FALSE, size=guide_legend(title="勝率")) +
    xlab("1ポゼッション当たりの得点") +
    ylab("（相手チームの）1ポゼッション当たりの得点") +
    ggtitle(title) +
    guides(size=FALSE) +
    scale_y_continuous(limits = c(0.9, 1.2), breaks = seq(0.9, 1.2, by = 0.1)) +
    scale_x_continuous(limits = c(0.9, 1.2), breaks = seq(0.9, 1.2, by = 0.1))
  
  ggsave(filename = filename, width = 7, height = 7)
}

for(i in seq(10,10)) {
  drawAndSave(i)
}
