source('./playerstats.r')

if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Top 10 by PPG
ppg.top10 <- b1.2nd[order(b1.2nd$PPG, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "PPG")) %>%
  filter(G > 30) %>%
  head(10)
           
kable(ppg.top10)

# Top 2 in 2016-17  
first.top2 <- b1.1st %>%
  select(c("TEAM", "PLAYER", "G", "PPG")) %>%
  head(2)

kable(first.top2)

# Top 2 in 2017-18
second.top2 <- b1.2nd %>%
  select(c("PLAYER", "G", "PTS", "PPG", "FGM", "FGP", "TPGM", "TPGP", "FTM", "FTP")) %>%
  head(2)
second.top2$PTSBY2P <- 2 * (second.top2$FGM - second.top2$TPGM)
second.top2$PTSBY3P <- 3 * second.top2$TPGM

kable(second.top2[, c("PLAYER", "PTS", "PTSBY2P", "PTSBY3P", "FTM")])

second.top2.graph <- data.frame(
    PLAYER = c(rep(second.top2[1,1], 3), rep(second.top2[2,1], 3)),
    TYPE = c("PTSBY2P", "PTSBY3P", "FTM"),
    PTS = c(second.top2$PTSBY2P[1],
            second.top2$PTSBY3P[1],
            second.top2$FTM[1],
            second.top2$PTSBY2P[2],
            second.top2$PTSBY3P[2],
            second.top2$FTM[2])
)

ggplot(data = second.top2.graph, aes(PLAYER, PTS, fill = TYPE)) +
  geom_col()

# FTM FTA FTP
kable(b1.2nd[1:2, c("PLAYER", "FTM", "FTA", "FTP", "FD")])

# TOP 5 FD
fd.top5 <- b1.2nd[order(b1.2nd$FD, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "FD")) %>%
  head(5)

kable(fd.top5)

# Team situation
nigata.2nd <- b1.2nd[order(b1.2nd$PPG, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "PPG")) %>%
  filter(TEAM == "VŠƒ")
kable(nigata.2nd)

nigata.1st <- b1.1st[order(b1.1st$PPG, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "PPG")) %>%
  filter(TEAM == "VŠƒ")
kable(nigata.1st)

kawasaki.2nd <- b1.2nd[order(b1.2nd$PPG, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "PPG")) %>%
  filter(TEAM == "ìè")
kable(kawasaki.2nd)
