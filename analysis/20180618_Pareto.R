setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)

df <- b1.2nd[order(b1.2nd$PTS, decreasing = TRUE),]
head(df[,c("PLAYER", "PTS")])

pareto <- function(dd) {
  dd$PLAYER_F <- with(dd,
                      factor(dd$PLAYER,
                             levels=dd[order(-PTS), ]$PLAYER)) 
  
  dd$CUMSUM <- cumsum(dd$PTS)
  sum <- sum(dd$PTS)
  dd$CUMSUMP <- dd$CUMSUM / sum
  
  dd$PTS_N <- (dd$PTS - min(dd$PTS)) / (max(dd$PTS) - min(dd$PTS))

  ggplot(dd, aes(x = PLAYER_F, label = PTS)) +
    geom_point(aes(y = CUMSUMP), col = "orange") +
    geom_bar(stat = "identity", aes(y = PTS_N), fill = "blue", alpha = .6) +
    geom_line(aes(y = CUMSUMP, group = 1, col = "orange")) +
    geom_text(aes(y = PTS_N + 0.05), col = "blue") +
    geom_line(aes(y = 0.8, group = 1, alpha = 1), col = "green", linetype="dotted") +
    geom_line(aes(y = 0.5, group = 1, alpha = 1), col = "red", linetype="dotted") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position='none', panel.grid=element_blank()) +
    xlab("") +
    ylab("") +
    scale_y_continuous(labels = scales::percent)
}

pareto(subset(df, TEAM == "ŽO‰Í"))
pareto(subset(df, TEAM == "ç—t"))
pareto(subset(df, TEAM == "A“Œ‹ž"))
pareto(subset(df, TEAM == "—®‹…"))
pareto(subset(df, TEAM == "ìè"))
pareto(subset(df, TEAM == "‹ž“s"))
pareto(subset(df, TEAM == "“È–Ø"))
pareto(subset(df, TEAM == "–¼ŒÃ‰®D"))
pareto(subset(df, TEAM == "VŠƒ"))
pareto(subset(df, TEAM == "SRa’J"))
pareto(subset(df, TEAM == "–kŠC“¹"))
pareto(subset(df, TEAM == "ŽO‰“"))
pareto(subset(df, TEAM == "Ž ‰ê"))
pareto(subset(df, TEAM == "‘åã"))
pareto(subset(df, TEAM == "‰¡•l"))
pareto(subset(df, TEAM == "¼‹{"))
pareto(subset(df, TEAM == "“‡ª"))
pareto(subset(df, TEAM == "•xŽR"))
pareto(df)
