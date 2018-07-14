setwd('C:/git/bleaguebydata/analysis')

library(dplyr)
library(knitr)
library(ggplot2)

niigata.2nd <- read.csv("201718_niigata.csv",
                         header = TRUE,
                         sep = ",",
                         quote = "",
                         stringsAsFactors = FALSE
)

# Column Names
niigata.2nd$F3PGM <- niigata.2nd$X3FGM
niigata.2nd$F3PGA <- niigata.2nd$X3FGA
niigata.2nd$F3PGP <- niigata.2nd$X3FG.
niigata.2nd$F2PGM <- niigata.2nd$X2FGM
niigata.2nd$F2PGA <- niigata.2nd$X2FGA
niigata.2nd$F2PGP <- niigata.2nd$X2FG.
niigata.2nd$FTP   <- niigata.2nd$FT.

# Additional columns
niigata.2nd$RLT_STR <- ifelse(niigata.2nd$OPTS < niigata.2nd$PTS, "WIN", "LOSE")
niigata.2nd$RLT <- as.factor(niigata.2nd$RLT_STR)
niigata.2nd$POS <- niigata.2nd$F3PGA + 
                   niigata.2nd$F2PGA -
                   niigata.2nd$OR +
                   niigata.2nd$TO +
                   (0.44 * niigata.2nd$FTA)

str(niigata.2nd)

ggplot(niigata.2nd, aes(x = RLT, y = POS, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("ポゼッション数") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

ggplot(niigata.2nd, aes(x = RLT, y = PTS / POS, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("得点効率") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

niigata.2nd %>%
  filter(PTS / POS > 1.2 & OPTS > PTS) %>%
  select(c("DAY", "VS", "PTS", "OPTS")) %>%
  kable()

ggplot(niigata.2nd, aes(x = RLT, y = F2PGM / F2PGA, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("2点シュート%") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

ggplot(niigata.2nd, aes(x = RLT, y = F3PGM / F3PGA, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("3点シュート%") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

ggplot(niigata.2nd, aes(x = RLT, y = FTM / FTA, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("フリースロー%") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

ggplot(niigata.2nd, aes(x = RLT, y = F3PGA, fill = RLT)) +
  geom_boxplot() +
  ggtitle("新潟アルビレックスBB 2017-18") +
  xlab("試合結果") +
  ylab("3点シュートを打った回数") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

source('playerstats.r')

b1.2nd[order(b1.2nd$TPGA, decreasing = TRUE),] %>% 
  filter(TEAM == "新潟") %>%
  select(c("PLAYER", "PO", "TPGA", "TPGP")) %>%
  kable()

b1.2nd[order(b1.2nd$TPGA, decreasing = TRUE),] %>% 
  select(c("TEAM", "PLAYER", "PO","TPGA", "TPGP")) %>%
  head(10) %>%
  kable()

ggplot(niigata.2nd, aes(x = F2PGM/POS, y = F3PGM/POS, col = RLT, size = abs(PTS - OPTS))) +
  geom_point(alpha = 0.8) +
#  ggtitle("新潟アルビレックスBB 2017-18") +
#  xlab("フリースロー成功確率") +
#  ylab("スリーポイント成功確率") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

