setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

df <- b1.2nd %>%
  filter(MIN >= 100)

df$TPGA_PM <- df$TPGA / df$MIN
df$F2GA_PM <- (df$FGA - df$TPGA) / df$MIN
df$F2GA <- (df$FGA - df$TPGA)
df$FTA_PM <- df$FTA / df$MIN
df$TR_PM <- df$TR / df$MIN
df$BS_PM <- df$BS / df$MIN
df$ST_PM <- df$ST / df$MIN
df$AS_PM <- df$AS / df$MIN

df$TPGA_PM_Z <- scale(df$TPGA_PM)[,1]
df$F2GA_Z <- scale(df$F2GA_PM)[,1]
df$FTA_Z <- scale(df$FTA_PM)[,1]
df$TR_PM_Z <- scale(df$TR_PM)[,1]
df$BS_PM_Z <- scale(df$BS_PM)[,1]
df$ST_PM_Z <- scale(df$ST_PM)[,1]
df$AS_PM_Z <- scale(df$AS_PM)[,1]

df$PO_F <- as.factor(df$PO)
df$INIT_CLST <- as.integer(df$PO_F)

set.seed(1234)
clst <- kmeans(
  df[,c("TPGA_PM_Z", "F2GA_Z", "FTA_Z", "TR_PM_Z", "BS_PM_Z", "ST_PM_Z", "AS_PM_Z")],
  13,
  iter.max =50,
  nstart = 5)
df$Cluster <- clst$cluster

rownames(df) <- NULL

df %>%
  group_by(Cluster) %>%
  summarise(NUM = n(),
            TPGA_Z_AVG = round(mean(TPGA_PM_Z), 2),
            F2GA_Z_AVG = round(mean(F2GA_Z), 2),
            FTA_Z_AVG  = round(mean(FTA_Z), 2),
            TR_PM_Z_AVG = round(mean(TR_PM_Z), 2),
            BS_PM_Z_AVG = round(mean(BS_PM_Z), 2),
            ST_PM_Z_AVG = round(mean(ST_PM_Z), 2),
            AS_PM_Z_AVG = round(mean(AS_PM_Z), 2)
  ) %>%
  as.data.frame() %>%
  kable()

df %>%
  select(c("TEAM", "PLAYER", "PO", "MIN", "Cluster", "TPGA_PM_Z", "F2GA_Z", "FTA_Z", "TR_PM_Z", "BS_PM_Z", "ST_PM_Z", "AS_PM_Z")) %>%
  filter(Cluster == 1) %>%
  View()

d <- subset(df[order(df$TPGA_PM, decreasing = TRUE),], Cluster == 1) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#2

df %>%
  select(c("TEAM", "PLAYER", "PO", "MIN", "Cluster", "TPGA_PM_Z", "F2GA_Z", "FTA_Z", "TR_PM_Z", "BS_PM_Z", "ST_PM_Z", "AS_PM_Z")) %>%
  filter(Cluster == 2) %>%
  View()

d <- subset(df[order(df$ST_PM, decreasing = TRUE),], Cluster == 2) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#3

df %>%
  select(c("TEAM", "PLAYER", "PO", "MIN", "Cluster", "TPGA_PM_Z", "F2GA_Z", "FTA_Z", "TR_PM_Z", "BS_PM_Z", "ST_PM_Z", "AS_PM_Z")) %>%
  filter(Cluster == 3) %>%
  View()

d <- subset(df[order(df$F2GA_Z, decreasing = TRUE),], Cluster == 3) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#4

df %>%
  select(c("TEAM", "PLAYER", "PO", "MIN", "Cluster", "TPGA_PM_Z", "F2GA_Z", "FTA_Z", "TR_PM_Z", "BS_PM_Z", "ST_PM_Z", "AS_PM_Z")) %>%
  filter(Cluster == 4) %>%
  View()

d <- subset(df[order(df$FTA_Z, decreasing = TRUE),], Cluster == 4) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#5

d <- subset(df[order(df$ST_PM_Z, decreasing = TRUE),], Cluster == 5) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#6 

d <- subset(df[order(df$AS_PM_Z, decreasing = TRUE),], Cluster == 6) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#7

d <- subset(df[order(df$TPGA_PM_Z, decreasing = TRUE),], Cluster == 7) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#8 

d <- subset(df[order(df$TR_PM_Z, decreasing = TRUE),], Cluster == 8) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#9

d <- subset(df[order(df$TPGA_PM_Z, decreasing = TRUE),], Cluster == 9) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

View(df[df$Cluster == 9,])

#10

d <- subset(df[order(df$TPGA_PM_Z, decreasing = TRUE),], Cluster == 10) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#11

d <- subset(df[order(df$TPGA_PM_Z, decreasing = TRUE),], Cluster == 11) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

View(df[df$Cluster == 11,])

#12

d <- subset(df[order(df$BS_PM_Z, decreasing = TRUE),], Cluster == 12) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)

#13

d <- subset(df[order(df$TR_PM_Z, decreasing = TRUE),], Cluster == 13) %>%
  select(c("TEAM", "PLAYER", "PO", "MIN"))
rownames(d) <- NULL
kable(d)
