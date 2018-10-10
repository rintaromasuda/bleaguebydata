library(dplyr)
library(ggplot2)

setwd("C:\\git\\bleaguebydata\\misc")
df <- read.csv(file = "player_master.csv")

grepl("PG", df$Position)
df$Position <- gsub(" ", "", gsub("ポジション / "," ", df$Position_Raw))
df$Position_Class <- ifelse(grepl("C", df$Position), "C",
                     ifelse(grepl("PF", df$Position), "F",
                     ifelse(grepl("SF", df$Position), "F",
                     ifelse(grepl("SG", df$Position), "G",
                     ifelse(grepl("PG", df$Position), "G",
                     "Unknown")))))

df$Position <- factor(df$Position, levels = c("PG", "PG/SG", "SG",
                                                 "SG/SF", "SF",
                                                 "SF/PF", "PF", "PF/C", "C"))
df$Sub <- df$Height - df$Weight

str(df)
df %>%
  group_by(Team) %>%
  summarise(N = n())

ggplot(df, aes(x = Weight, y = Height, colour = Position)) +
  geom_point() +
  xlab("体重") +
  ylab("身長")

ggplot(df, aes(x = Sub)) +
  geom_histogram(binwidth = 2) +
  xlab("身長 - 体重") +
  ylab("人数")

ggplot(df, aes(x = Sub)) +
  geom_histogram(data = subset(df, League == "B1"), binwidth = 2, aes(fill = League, alpha = 0.7)) +
  geom_histogram(data = subset(df, League == "B2"), binwidth = 2, aes(fill = League, alpha = 0.7)) +
  guides(alpha=FALSE) +
  xlab("身長 - 体重") +
  ylab("人数")

ggplot(df, aes(x = Sub, fill = Position)) +
  geom_density(aes(alpha = 0.5)) +
  guides(alpha=FALSE) +
  xlab("身長 - 体重") +
  ylab("全体の人数に占める割合")

ggplot(subset(df, League=="B1"), aes(x = Height, y = Team, colour = Team, alpha = 0.7 )) +
  geom_point(aes(shape=Position_Class), size=3) +
  guides(alpha=FALSE, colour=FALSE, shape=guide_legend(title="ポジション")) +
  xlab("身長") +
  ylab("")

ggplot(subset(df, League=="B2"), aes(x = Height, y = Team, colour = Team, alpha = 0.7 )) +
  geom_point(aes(shape=Position_Class), size=3) +
  guides(alpha=FALSE, colour=FALSE, shape=guide_legend(title="ポジション")) +
  xlab("身長") +
  ylab("")

library(knitr)

df %>%
  filter(League=="B1") %>%
  group_by(Team) %>%
  summarise(AverageHeight = round(mean(Height), 1),
            MedianHeight = round(median(Height), 1),
            SdHeight = round(sd(Height), 1)) %>%
  kable()

df %>%
  filter(League=="B2") %>%
  group_by(Team) %>%
  summarise(AverageHeight = round(mean(Height), 1),
            MedianHeight = round(median(Height), 1),
            SdHeight = round(sd(Height), 1)) %>%
  kable()

ggplot(df, aes(x = Height, alpha = 0.2)) +
  geom_histogram(data = subset(df, League=="B1"), aes(fill = League), binwidth = 2) +
  geom_histogram(data = subset(df, League=="B2"), aes(fill = League), binwidth = 2) +
  guides(alpha=FALSE) +
  xlab("") +
  ylab("人数")

ggplot(df, aes(x = Height, fill = League, alpha = .2)) +
  geom_density() +
  guides(alpha=FALSE) +
  ylab("")

if (!require(eeptools)) {
  install.packages("eeptools")
  library(eeptools)
}

df$Age <- floor(age_calc(as.Date(df$Birthday), as.Date("2018-10-01"), units = "years"))

ggplot(subset(df, League=="B1"), aes(x = Age, y = Team, colour = Team, alpha = 0.5)) +
  geom_point(aes(), size=3) +
  guides(alpha=FALSE, colour=FALSE) +
  xlab("年齢") +
  ylab("") +
  xlim(c(15,50))

ggplot(subset(df, League=="B2"), aes(x = Age, y = Team, colour = Team, alpha = 0.5)) +
  geom_point(aes(), size=3) +
  guides(alpha=FALSE, colour=FALSE) +
  xlab("年齢") +
  ylab("") +
  xlim(c(15,50))

dd <- df %>%
  filter(League=="B1") %>%
  group_by(Team) %>%
  summarise(AverageHeight = round(mean(Age), 1),
            MedianHeight = round(median(Age), 1),
            SdHeight = round(sd(Age), 1)) %>%
  as.data.frame()

kable(dd[order(dd$Team, decreasing = TRUE),])

dd <- df %>%
  filter(League=="B2") %>%
  group_by(Team) %>%
  summarise(AverageHeight = round(mean(Age), 1),
            MedianHeight = round(median(Age), 1),
            SdHeight = round(sd(Age), 1)) %>%
  as.data.frame()

kable(dd[order(dd$Team, decreasing = TRUE),])

ggplot(df, aes(x = Age, alpha = 0.2)) +
  geom_histogram(data = subset(df, League=="B1"), aes(fill = League), binwidth = 2) +
  geom_histogram(data = subset(df, League=="B2"), aes(fill = League), binwidth = 2) +
  guides(alpha=FALSE) +
  xlab("") +
  ylab("人数")

#####

best10 <- df %>%
  group_by(School) %>%
  summarise(N = n()) %>%
  filter(N > 7) %>%
  as.data.frame()
  
df2 <- merge(df, best10, by="School")

dd <- df2 %>%
  group_by(School, League) %>%
  summarise(Count = n()) %>%
  as.data.frame()

x <- dd %>%
  group_by(School) %>%
  summarise(N = sum(Count)) %>%
  as.data.frame()

kable(x[order(x$N, decreasing = TRUE),])

dd <- subset(df, School == "東海大学")
dd <- dd[order(dd$Birthday),]
dd <- dd[, c("Name", "Team", "Birthday", "Age")]
kable(dd)
