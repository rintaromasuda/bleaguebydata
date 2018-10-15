url <- "https://github.com/rintaromasuda/bleaguebydata/raw/master/blog/2018101301/bleaguers_20181013.csv"
df <- read.csv(url)

dim(df)
str(df)
summary(df)
View(df)
head(df)
tail(df)

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

df %>%
  group_by(Team) %>%
  summarise(NumOfPlayers = n()) %>%
  as.data.frame()

df %>% head() %>% summary()

df %>%
  filter(League == "B1") %>%
  group_by(Team) %>%
  summarise(MinHeight = min(Height),
            MaxHeight = max(Height),
            MeanHeight = mean(Height),
            MedianHeight = median(Height)) %>%
  as.data.frame()

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

ggplot() +
  geom_histogram(data = df, binwidth = 2, aes(x = Height))

ggplot() +
  geom_histogram(data = subset(df, League == "B1"), binwidth = 2, aes(x = Height, fill = League, alpha = 0.5)) +
  geom_histogram(data = subset(df, League == "B2"), binwidth = 2, aes(x = Height, fill = League, alpha = 0.5))
