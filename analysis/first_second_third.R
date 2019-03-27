devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(ggplot2)

if(!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}

df.first20 <- GetStanding("2018-19", "B1", fromGame = 1, atEndOfGame = 20, needRank = TRUE)
df.second20 <- GetStanding("2018-19", "B1", fromGame = 21, atEndOfGame = 40, needRank = TRUE)

df <- GetGameSummary()
df.cur <- subset(df, Season == "2018-19" & League == "B1" & Category == "Regular")
df.cur$Duration <- ifelse(df.cur$Game.Index >= 1 & df.cur$Game.Index <= 20, "Game 1-20",
                          ifelse(df.cur$Game.Index >= 21 & df.cur$Game.Index <= 40, "Game 21-40", "Invalid"))

ggplot() +
  geom_boxplot(data = df.cur,
               aes(x = TeamName,
                   y = PTS,
                   fill = Duration)) +
  xlab("") +
  ylab("得点") +
  ggtitle("最初の20ゲームと次の20ゲームの比較（2018-19シーズン）") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    
    legend.title = element_blank(),
    
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PTS.jpeg", width = 8, height = 5) 

ggplot() +
  geom_boxplot(data = df.cur,
               aes(x = TeamName,
                   y = OR / (Opp.DR + OR),
                   fill = Duration)) +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("最初の20ゲームと次の20ゲームの比較（2018-19シーズン）") +
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    
    legend.title = element_blank(),
    
    plot.title = element_text(hjust = 0.5)
  )

ggsave("ORR.jpeg", width = 8, height = 5) 

ggplot() +
  geom_boxplot(data = df.cur,
               aes(x = TeamName,
                   y = F3GA,
                   fill = Duration)) +
  xlab("") +
  ylab("スリーポイント試投数") +
  ggtitle("最初の20ゲームと次の20ゲームの比較（2018-19シーズン）") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    
    legend.title = element_blank(),
    
    plot.title = element_text(hjust = 0.5)
  )

ggsave("F3GA.jpeg", width = 8, height = 5) 

ggplot() +
  geom_boxplot(data = df.cur,
               aes(x = TeamName,
                   y = (F2GM + (F3GM * 1.5)) / (F2GA + F3GA),
                   fill = Duration)) +
  xlab("") +
  ylab("eFG%") +
  ggtitle("最初の20ゲームと次の20ゲームの比較（2018-19シーズン）") +
  scale_y_continuous(label = scales::percent) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    
    legend.title = element_blank(),
    
    plot.title = element_text(hjust = 0.5)
  )

ggsave("eFG.jpeg", width = 8, height = 5)  
