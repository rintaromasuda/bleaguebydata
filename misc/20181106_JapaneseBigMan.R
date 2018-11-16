library(dplyr)
library(ggplot2)

df <- read.csv("player_master_20181106.csv")
df$Type <- ifelse(df$Nationality == "日本", "日本",
            ifelse(df$Nationality == "帰化", "帰化",
                   "外国籍"))

if (!require(eeptools)) {
  install.packages("eeptools")
  library(eeptools)
}

df$Age <- floor(age_calc(as.Date(df$Birthday), as.Date("2018-11-08"), units = "years"))

df_inside <- df %>%
  filter(grepl("PF", Position_Raw) | grepl("C", Position_Raw)) %>%
  as.data.frame()

ggplot() +
  geom_point(data = subset(df_inside, League == "B1"), aes(x = Weight, y = Height, color = Type), size = 3) +
  ylab("身長 (cm)") +
  xlab("体重 (kg)") +
  ggtitle("PFまたはCで登録のある選手の体格（B1のみ）") +
  guides(color=guide_legend(title=NULL)) +
  ylim(c(180, 220)) +
  xlim(c(80, 140))

#ggsave("B1.png", width = 7, height = 5)

ggplot() +
  geom_point(data = subset(df_inside, League == "B2"), aes(x = Weight, y = Height, color = Type), size = 3) +
  ylab("身長 (cm)") +
  xlab("体重 (kg)") +
  ggtitle("PFまたはCで登録のある選手の体格（B2のみ）") +
  guides(color=guide_legend(title=NULL)) +
  ylim(c(180, 220)) +
  xlim(c(80, 140))

#ggsave("B2.png", width = 7, height = 5)

#df_inside %>%
#  filter(Type == "日本") %>%
#  select("Team", "Name", "Height_Raw", "Weight_Raw") %>%
#  View()

df_inside %>%
  filter(Type == "日本") %>%
  select("Team", "Name", "Height_Raw", "Age") %>%
  View()

# Stats

df_stats <- read.csv("PlayerTotalStats_20181106.csv")

df_stats$TEAM <- as.factor(df_stats$TEAM)
df_stats$ID <- as.numeric(df_stats$ID)
df_stats$G <- as.numeric(as.character(df_stats$G))
df_stats$PPG <- as.numeric(as.character(df_stats$PPG))
df_stats$APG <- as.numeric(as.character(df_stats$APG))
df_stats$RPG <- as.numeric(as.character(df_stats$RPG))
df_stats$PPM <- df_stats$PTS / df_stats$MIN

# MIN
df_stats$MIN_RAW <- df_stats$MIN
df_stats$MINPG_RAW <- df_stats$MINPG

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

library(stringr)
df_stats$MIN <- ifelse(is.na(df_stats$MIN_RAW), 0,
                 ifelse(df_stats$MIN_RAW == "", 0,
                        sapply(str_split(df_stats$MIN_RAW, ":"), minStrToMinDec)))
df_stats$MINPG <- ifelse(is.na(df_stats$MINPG_RAW), 0,
                   ifelse(df_stats$MINPG_RAW == "", 0,
                          sapply(str_split(df_stats$MINPG_RAW, ":"), minStrToMinDec)))


df_merged <- merge(df_inside, df_stats, by.x = "PlayerId", by.y = "ID")

ggplot() +
  geom_histogram(data = subset(df_merged, League == "B1" & Type == "外国籍"), aes(x = MIN, fill = Type), alpha = 0.7, binwidth = 20) +
  geom_histogram(data = subset(df_merged, League == "B1" & Type == "帰化"), aes(x = MIN, fill = Type), alpha = 0.7, binwidth = 20) +
  geom_histogram(data = subset(df_merged, League == "B1" & Type == "日本"), aes(x = MIN, fill = Type), alpha = 0.7, binwidth = 20) +
  guides(color=guide_legend(title=NULL)) 

ggplot() +
  geom_density(data = subset(df_merged, League == "B1" & (Type == "外国籍" | Type == "日本")), aes(x = MIN, fill = Type), alpha = 0.7) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("密度") +
  xlab("総プレイタイム（分）") +
  ggtitle("PFまたはCで登録のある選手の総プレータイムの分布（B1のみ）") +
  guides(color=guide_legend(title=NULL)) +
  xlim(c(0, 500)) +
  ylim(c(0, 0.01))

#ggsave("B1_TotalPlayTime.png", width = 7, height = 5)

ggplot() +
  geom_density(data = subset(df_merged, League == "B2" & (Type == "外国籍" | Type == "日本")), aes(x = MIN, fill = Type), alpha = 0.7) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("密度") +
  xlab("総プレイタイム（分）") +
  ggtitle("PFまたはCで登録のある選手の総プレータイムの分布（B2のみ）") +
  guides(color=guide_legend(title=NULL)) +
  xlim(c(0, 500)) +
  ylim(c(0, 0.01))

#ggsave("B2_TotalPlayTime.png", width = 7, height = 5)

ggplot() +
  geom_density(data = subset(df_merged, League == "B1" & (Type == "外国籍" | Type == "日本")), aes(x = MINPG, fill = Type), alpha = 0.7) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("密度") +
  xlab("試合当たりのプレータイム（分）") +
  ggtitle("PFまたはCで登録のある選手の試合当たりプレータイムの分布（B1のみ）") +
  guides(color=guide_legend(title=NULL)) +
  xlim(c(0, 40)) +
  ylim(c(0, 0.12))

#ggsave("B1_MINPG.png", width = 7, height = 5)

ggplot() +
  geom_density(data = subset(df_merged, League == "B2" & (Type == "外国籍" | Type == "日本")), aes(x = MINPG, fill = Type), alpha = 0.7) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("密度") +
  xlab("試合当たりのプレータイム（分）") +
  ggtitle("PFまたはCで登録のある選手の試合当たりプレータイムの分布（B2のみ）") +
  guides(color=guide_legend(title=NULL)) +
  xlim(c(0, 40)) +
  ylim(c(0, 0.12))

#ggsave("B2_MINPG.png", width = 7, height = 5)

ggplot() +
  geom_point(data = subset(df_merged, Type == "外国籍"), aes(x = Weight, y = Height, color = League), size = 3) +
  ylab("身長 (cm)") +
  xlab("体重 (kg)") +
  ggtitle("PFまたはCで登録のある選手の体格（B1のみ）") +
  guides(color=guide_legend(title=NULL)) +
  ylim(c(180, 220)) +
  xlim(c(80, 140))

library(knitr)

dd <- df_merged %>%
  filter(Type == "日本") %>%
  select(c("Team", "Name", "Height_Raw", "MIN_RAW", "MINPG_RAW", "MINPG"))

dd <- dd[order(dd$MINPG, decreasing = TRUE),c("Team", "Name", "Height_Raw", "MIN_RAW", "MINPG_RAW")]
kable(dd)

ggplot() +
  geom_density(data = subset(df_merged, League == "B1" & (Type == "外国籍" | Type == "日本")), aes(x = Age, fill = Type), alpha = 0.7) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("密度") +
  xlab("試合当たりのプレータイム（分）") +
  ggtitle("PFまたはCで登録のある選手の試合当たりプレータイムの分布（B2のみ）") +
  guides(color=guide_legend(title=NULL)) +
  xlim(c(0, 40)) +
  ylim(c(0, 0.12))
