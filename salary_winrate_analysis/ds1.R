library(ggplot2)

df <- read.csv("C:\\git\\bleaguebydata\\salary_winrate_analysis\\B1_1617_Season.csv", header = TRUE, sep = ',')
summary(df)
names(df)

ggplot(df, aes(SALARY, (WIN / G), label = TEAM)) +
  geom_point(size = .5, colour = "RED") +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_x_continuous(limits = c(0, 400000), labels = c("0", "100,000","200,000", "300,000", "400,000")) + 
  geom_text(aes(label=TEAM), hjust = 0.5, vjust = .7, size = 3) +
  labs(x = "トップチーム人件費（千円）", y = "2016-17シーズン勝率")

ggplot(df, aes(SPONSOR, (WIN / G), label = TEAM)) +
  geom_point(size = .5, colour = "RED") +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_x_continuous(limits = c(0, 700000), labels = c("0", "200,000","400,000", "600,000", "800,000")) + 
  geom_text(aes(label=TEAM), hjust = 0.5, vjust = .7, size = 3) +
  labs(x = "スポンサー収入（千円）", y = "2016-17シーズン勝率")

fit1 <- lm((WIN / G) ~ SALARY, data = df)
summary(fit1)

fit2 <- lm((WIN / G) ~ SPONSOR, data = df)
summary(fit2)

fit3 <- lm((WIN / G) ~ SALARY + SPONSOR, data = df)
summary(fit3)

class(df$X3FG.)
df$X3FG.n <- as.numeric(gsub("%","", as.character(df$X3FG.)))
df$X3FG.n <- df$X3FG.n / 100

ggplot(df, aes(SALARY, X3FG.n, label = TEAM)) +
  geom_point(size = .5, colour = "RED") +
  scale_x_continuous(limits = c(0, 400000), labels = c("0", "100,000","200,000", "300,000", "400,000")) + 
  geom_text(aes(label=TEAM), hjust = 0.5, vjust = .7, size = 3) +
  labs(x = "トップチーム人件費（千円）", y = "2016-17シーズン 3ポイントシュート成功率")

class(df$FG.)
df$FG.n <- as.numeric(gsub("%", "", as.character(df$FG.)))
df$FG.n <- df$FG.n / 100

ggplot(df, aes(SALARY, FG.n, label = TEAM)) +
  geom_point(size = .5, colour = "RED") +
  scale_x_continuous(limits = c(0, 400000), labels = c("0", "100,000","200,000", "300,000", "400,000")) + 
  geom_text(aes(label=TEAM), hjust = 0.5, vjust = .7, size = 3) +
  labs(x = "トップチーム人件費（千円）", y = "2016-17シーズン フィールドゴール成功率")

class(df$FT.)
df$FT.n <- as.numeric(gsub("%", "", as.character(df$FT.)))
df$FT.n <- df$FT.n / 100

ggplot(df, aes(SALARY, FT.n, label = TEAM)) +
  geom_point(size = .5, colour = "RED") +
  scale_x_continuous(limits = c(0, 400000), labels = c("0", "100,000","200,000", "300,000", "400,000")) + 
  geom_text(aes(label=TEAM), hjust = 0.5, vjust = .7, size = 3) +
  labs(x = "トップチーム人件費（千円）", y = "2016-17シーズン フリースロー成功率")
