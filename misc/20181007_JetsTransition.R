df <- read.csv("all_games_201718.csv")

library(dplyr)

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$POS.B <- df$X2POINTSFGA.B + df$X3POINTSFGA.B - df$OFFENSIVEREBOUNDS.B + (0.44 * df$FREE.THROWSA.B)
df$PPP100.B <- (df$F.B / df$POS.B) * 100

df$X2POINTSFGR_RAW.B <- df$X2POINTSFGR.B
df$X3POINTSFGR_RAW.B <- df$X3POINTSFGR.B
df$FREE.THROWSR_RAW.B <- df$FREE.THROWSR.B

df$FGR.B <- (df$X2POINTSFGM.B + df$X3POINTSFGM.B) / (df$X2POINTSFGA.B + df$X3POINTSFGA.B)
df$X2POINTSFGR.B <- (df$X2POINTSFGM.B) / (df$X2POINTSFGA.B)
df$X3POINTSFGR.B <- (df$X3POINTSFGM.B) / (df$X3POINTSFGA.B)

library(ggplot2)

ggplot(data = NULL, aes(x = TEAM.A, y = FASTBREAKPOINTS.B)) +
  geom_point(data = subset(df, TEAM.A != "千葉" & TEAM.B != "千葉")) +
  geom_point(data = subset(df, TEAM.A != "千葉" & TEAM.B == "千葉"),
             aes(colour = TEAM.B)) +
  xlab("") +
  ylab("ファストブレイクでの失点") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")
