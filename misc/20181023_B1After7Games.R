# df <- read.csv("B1_201819_4setsu_Summary.csv")
df <- read.csv("B2_201819_5setsu_Summary.csv")

library(dplyr)

df %>%
  group_by(TEAM.A) %>%
  summarise(N = n())

df$RESULT.A <- ifelse(df$F.A > df$F.B, "勝ち", "負け")
df$RESULT.A <- factor(df$RESULT.A, levels = c("負け", "勝ち"))

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

dd <- df %>%
  group_by(TEAM.A) %>%
  summarise(OFFEFF = mean(PPP.A),
            DFFEFF = mean(PPP.B),
            WIN = sum(F.A > F.B),
            LOSE = sum(F.A < F.B)) %>%
  as.data.frame()

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
readr::write_excel_csv(dd, "b2.csv")

ggplot() +
  geom_point(data = dd, aes(x = OFFEFF, y = DFFEFF, size = (WIN/(WIN +LOSE)), color = TEAM.A)) +
  geom_text(data = dd, aes(x = OFFEFF, y = DFFEFF - 0.015, label = TEAM.A), size = 3) +
  guides(color=FALSE, size=guide_legend(title="勝率")) +
  xlab("1ポゼッション当たりの得点") +
  ylab("（相手チームの）1ポゼッション当たりの得点") +
  ggtitle("2018-19シーズン B2 第5節終了時点")

library(ggplot2)

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = F.A, size = abs(F.A - F.B), color = RESULT.A), alpha = 0.7) +
  guides(size=guide_legend(title="得失点差"), color=guide_legend(title="結果")) +
  xlab("") +
  ylab("得点") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = F.B, size = abs(F.A - F.B), color = RESULT.A), alpha = 0.7) +
  guides(color=guide_legend(title="結果"), size=guide_legend(title="得失点差")) +
  xlab("") +
  ylab("失点") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = (DEFENSIVEREBOUNDS.A / (DEFENSIVEREBOUNDS.A + OFFENSIVEREBOUNDS.B)), color = RESULT.A), size = 4, alpha = 0.7) +
  guides(color=guide_legend(title="結果")) +
  xlab("") +
  ylab("ディフェンスリバウンド取得率") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = (OFFENSIVEREBOUNDS.A / (OFFENSIVEREBOUNDS.A + DEFENSIVEREBOUNDS.B)), color = RESULT.A), size = 4, alpha = 0.7) +
  guides(color=guide_legend(title="結果")) +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = ((X2POINTSFGM.A + X3POINTSFGM.A) / (X2POINTSFGA.A + X3POINTSFGA.A)), color = RESULT.A), size = 4, alpha = 0.7) +
  guides(color=guide_legend(title="結果")) +
  xlab("") +
  ylab("フィールドゴール%") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = ((X2POINTSFGM.B + X3POINTSFGM.B) / (X2POINTSFGA.B + X3POINTSFGA.B)), color = RESULT.A), size = 4, alpha = 0.7) +
  guides(color=guide_legend(title="結果")) +
  xlab("") +
  ylab("被フィールドゴール%") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot() +
  geom_point(data = df, aes(x = TEAM.A, y = (X2POINTSFGM.A / X2POINTSFGA.A), color = RESULT.A), size = 4, alpha = 0.7) +
  guides(color=guide_legend(title="結果")) +
  xlab("") +
  ylab("フィールドゴール%（2点シュートのみ）") +
  ggtitle("2018-19シーズン B2 第5節終了時点") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


