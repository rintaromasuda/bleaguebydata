setwd("C:\\git\\bleaguebydata\\misc")
df <- read.csv("all_games_201718.csv")


df1 <- subset(df, BIGGESTLEAD.A > 0 & BIGGESTLEAD.B > 0)
df1$SUMLEAD <- df1$BIGGESTLEAD.A + df1$BIGGESTLEAD.B

View(df1)

df2 <- subset(df1, SUMLEAD == 10)
View(df2)

df3 <- subset(df, F.A > F.B)
View(df3)

df4 <- df
df4$FGA <- df4$X2POINTSFGM.A + df4$X2POINTSFGM.B + df4$X3POINTSFGM.A + df4$X3POINTSFGM.B
View(df4)
