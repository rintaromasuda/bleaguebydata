setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

str(b1.2nd)

df <- b1.2nd %>%
  select(c("TEAM", "PLAYER", "PO", "FGA", "TPGA", "FTA", "TR", "AS", "ST", "BS"))

df$FGA_Z <- scale(df$FGA)
df$TPGA_Z <- scale(df$TPGA)
df$FTA_Z <- scale(df$FTA)
df$TR_Z <- scale(df$TR)
df$AS_Z <- scale(df$AS)
df$ST_Z <- scale(df$ST)
df$BS_Z <- scale(df$BS)

df2 <- df[, c("FGA_Z", "TPGA_Z", "FTA_Z", "TR_Z", "AS_Z", "ST_Z", "BS_Z")]

set.seed(1234)
clst <- kmeans(df2, 13, iter.max =50, nstart = 5)
df$Cluster <- clst$cluster

View(df[order(df$Cluster), c("TEAM", "PLAYER", "PO", "Cluster", "FGA_Z", "TPGA_Z", "FTA_Z", "TR_Z", "AS_Z", "ST_Z", "BS_Z")])
