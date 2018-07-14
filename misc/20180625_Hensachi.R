setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)
library(moments)

df <- subset(b1.2nd, MIN >= 100)
df$PPM_Z <- scale(df$PPM)[,1]
df$PPM_HEN <- round(df$PPM_Z * 10 + 50, 1)
df <- df[order(df$PPM_HEN, decreasing = TRUE),]

ggplot(df, aes(PPM)) +
  geom_histogram() +
  ylab("人数") +
  xlab("分当たり平均得点") +
  ggtitle("分当り平均得点の分布（B1 2017-18シーズン）") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df) +
  geom_density(aes(PPM, y = ..density..)) +
  ylab("人数の割合") +
  xlab("分当たり平均得点") +
  ggtitle("分当り平均得点の分布（B1 2017-18シーズン）") +
  theme(plot.title = element_text(hjust = 0.5))


df %>%
  select(c("PPM")) %>%
  summarise(mean = mean(PPM),
            Median = median(PPM),
            SK = skewness(PPM),
            Wid = kurtosis(PPM)) %>%
  as.data.frame() %>%
  kable()
  
mean(df$PPM)
median(df$PPM)
sd(df$PPM)
skewness(df$PPM)
kurtosis(df$PPM)

df %>%
  filter(PPM_HEN >= 80) %>%
  select("TEAM", "PLAYER", "MIN", "PPM", "PPM_HEN") %>%
  kable()

df %>%
  filter(PPM_HEN >= 70 & PPM_HEN < 80) %>%
  select("TEAM", "PLAYER", "MIN", "PPM", "PPM_HEN") %>%
  kable()

df %>%
  filter(PPM_HEN >= 60 & PPM_HEN < 70) %>%
  select("TEAM", "PLAYER", "MIN", "PPM", "PPM_HEN") %>%
  kable()

df %>%
  filter(PPM_HEN >= 50 & PPM_HEN < 60) %>%
  select("TEAM", "PLAYER", "MIN", "PPM", "PPM_HEN") %>%
  kable()

df %>%
  filter(PPM_HEN < 50) %>%
  select("TEAM", "PLAYER", "MIN", "PPM", "PPM_HEN") %>%
  kable()

b1.2nd[order(b1.2nd$MIN, decreasing = TRUE),] %>%
  filter(MIN < 100) %>%
  select("TEAM", "PLAYER", "MIN", "PPM") %>%
  kable()
