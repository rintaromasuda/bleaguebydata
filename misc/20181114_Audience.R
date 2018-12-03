library(dplyr)
library(ggplot2)
library(stringr)
library(ggrepel)

options(scipen=999)

cleanup <- function(df) {
  df$ATTENDANCE <- as.integer(gsub("人", "", gsub("人数：", "", df$ATTENDANCE_RAW)))
  df$DATE_RAW <- df$DATE
  df <- subset(df, ISHOME.A)
  return(df)
}

####
# Read data
####

df_b1_2nd <- cleanup(read.csv("B1_201718_All_Attendance.csv"))
df_b1_3rd <- cleanup(read.csv("B1_201819_11setsu_Attendance.csv"))
df_b2_3rd <- cleanup(read.csv("B2_201819_8setsu_Attendance.csv"))

###
# Handle dates
###

df_b1_2nd$DATEID <- df_b1_2nd$DATE_RAW * 100 + 20170000
df_b1_2nd[df_b1_2nd$DATEID < 20170601,]$DATEID <-  df_b1_2nd[df_b1_2nd$DATEID < 20170601,]$DATEID + 10000
df_b1_2nd$DATE <- as.Date(as.character(df_b1_2nd$DATEID), "%Y%m%d")
df_b1_2nd$DAYOFWEEK <- weekdays(df_b1_2nd$DATE, abbreviate = T)

df_b1_3rd$DATEID <- df_b1_3rd$DATE_RAW * 100 + 20180000
df_b1_3rd[df_b1_3rd$DATEID < 20180601,]$DATEID <- df_b1_3rd[df_b1_3rd$DATEID < 20180601,]$DATEID + 10000
df_b1_3rd$DATE <- as.Date(as.character(df_b1_3rd$DATEID), "%Y%m%d")
df_b1_3rd$DAYOFWEEK <- weekdays(df_b1_3rd$DATE, abbreviate = T)

###
# Compare each B1 team to the last season
###

compareSeason <- function(team) {
  title <- paste("2017-18と2018-19のホームゲーム観客動員数の比較（", team, "）", sep = "")
  ggplot() +
    geom_line(data = subset(df_b1_2nd, TEAM.A == team), aes(x = DATE + 365, y = ATTENDANCE, color = "2017-18"), size = 1, linetype = 3) +
    geom_point(data = subset(df_b1_2nd, TEAM.A == team & (DAYOFWEEK == "日" | DAYOFWEEK == "土")), aes(x = DATE + 365, y = ATTENDANCE), color = "red", size = 1.5) +
    geom_point(data = subset(df_b1_2nd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土")), aes(x = DATE + 365, y = ATTENDANCE), color = "red", size = 1.5) +
    geom_label_repel(data = subset(df_b1_2nd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土")),
                    aes(x = DATE + 365, y = ATTENDANCE, label = DAYOFWEEK),
                    nudge_y = 250 - subset(df_b1_2nd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土"))$ATTENDANCE,
                    color ="red",
                    segment.size  = 0.05) +
    
    geom_line(data = subset(df_b1_3rd, TEAM.A == team), aes(x = DATE, y = ATTENDANCE, color = "2018-19"), size = 2, alpha = 0.6) +
    geom_point(data = subset(df_b1_3rd, TEAM.A == team & (DAYOFWEEK == "日" | DAYOFWEEK == "土")), aes(x = DATE, y = ATTENDANCE), color = "blue", size = 2) +
    geom_point(data = subset(df_b1_3rd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土")), aes(x = DATE, y = ATTENDANCE), color = "blue", size = 2) +
    geom_label_repel(data = subset(df_b1_3rd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土")),
                    aes(x = DATE, y = ATTENDANCE, label = DAYOFWEEK),
                    nudge_y = 6500 - subset(df_b1_3rd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土"))$ATTENDANCE,
                    color ="blue",
                    segment.size  = 0.05) +
      
    ylim(c(0, 6500)) +
    ylab("観客動員数（人）") +
    xlab("開催日") +
    ggtitle(title) +
    scale_x_date(date_labels = "%m/%d", limits = c(as.Date("2018-09-28"), as.Date("2019-05-07"))) +
    scale_colour_discrete(name = "シーズン") +
    theme_bw()
  
  filename <- paste(team, "_compare.jpg", sep = "")
  ggsave(filename = filename, width = 7, height = 5)
}

compareSeason("京都")
compareSeason("新潟")
compareSeason("滋賀")
compareSeason("大阪")

compareSeason("千葉")
compareSeason("栃木")
compareSeason("北海道")
compareSeason("琉球")

compareSeason("川崎")
compareSeason("富山")
compareSeason("名古屋D")
compareSeason("SR渋谷")

compareSeason("A東京")
compareSeason("三遠")
compareSeason("三河")
compareSeason("横浜")

###
# Compare average b/w 2018-19
###

df_b1_2nd.grouped <- df_b1_2nd %>%
  filter(TEAM.A != "西宮" & TEAM.A != "島根") %>%
  group_by(DATE, DAYOFWEEK) %>%
  summarise(SUM_ATTENDANCE = sum(ATTENDANCE),
            AVG_ATTENDANCE = mean(ATTENDANCE),
            NUM_GAMES = n())

df_b1_3rd.grouped <- df_b1_3rd %>%
  filter(TEAM.A != "秋田" & TEAM.A != "福岡") %>%
  group_by(DATE, DAYOFWEEK) %>%
  summarise(SUM_ATTENDANCE = sum(ATTENDANCE),
            AVG_ATTENDANCE = mean(ATTENDANCE),
            NUM_GAMES = n())  

title <- "2017-18と2018-19のB1ホームゲーム平均観客動員数の比較\n（昇降格のなかった16チームのみ対象）"
ggplot() +
  geom_line(data = df_b1_2nd.grouped, aes(x = DATE + 365, y = AVG_ATTENDANCE, color = "2017-18"), size = 1, linetype = 3) +
  geom_point(data = subset(df_b1_2nd.grouped, (DAYOFWEEK == "日" | DAYOFWEEK == "土")), aes(x = DATE + 365, y = AVG_ATTENDANCE), color = "red", size = 1.5) +
  geom_point(data = subset(df_b1_2nd.grouped, (DAYOFWEEK != "日" & DAYOFWEEK != "土")), aes(x = DATE + 365, y = AVG_ATTENDANCE), color = "red", size = 1.5) +
  geom_label_repel(data = subset(df_b1_2nd.grouped, (DAYOFWEEK == "水" & DAYOFWEEK != "土")),
                   aes(x = DATE + 365, y = AVG_ATTENDANCE, label = DAYOFWEEK),
                   nudge_y = 250 - subset(df_b1_2nd.grouped, (DAYOFWEEK == "水" & DAYOFWEEK != "土"))$AVG_ATTENDANCE,
                   color ="red",
                   segment.size  = 0.05) +
  
  geom_line(data = df_b1_3rd.grouped, aes(x = DATE, y = AVG_ATTENDANCE, color = "2018-19"), size = 2, alpha = 0.6) +
  geom_point(data = subset(df_b1_3rd.grouped, (DAYOFWEEK == "日" | DAYOFWEEK == "土")), aes(x = DATE, y = AVG_ATTENDANCE), color = "blue", size = 2) +
  geom_point(data = subset(df_b1_3rd.grouped, (DAYOFWEEK != "日" & DAYOFWEEK != "土")), aes(x = DATE, y = AVG_ATTENDANCE), color = "blue", size = 2) +
  geom_label_repel(data = subset(df_b1_3rd.grouped, (DAYOFWEEK == "水" & DAYOFWEEK != "土")),
                   aes(x = DATE, y = AVG_ATTENDANCE, label = DAYOFWEEK),
                   nudge_y = 6500 - subset(df_b1_3rd.grouped, (DAYOFWEEK == "水" & DAYOFWEEK != "土"))$AVG_ATTENDANCE,
                   color ="blue",
                   segment.size  = 0.05) +
  
  ylim(c(0, 6500)) +
  ylab("平均観客動員数（人）") +
  xlab("開催日") +
  ggtitle(title) +
  scale_x_date(date_labels = "%m/%d", limits = c(as.Date("2018-09-28"), as.Date("2019-05-07"))) +
  scale_colour_discrete(name = "シーズン") +
  theme_bw()

filename <- paste("B1_16teams", "_compare.jpg", sep = "")
ggsave(filename = filename, width = 7, height = 5)

###
# Goal Achievement Rate
###

df_b1_2nd.total <- df_b1_2nd %>%
  filter(TEAM.A != "西宮" & TEAM.A != "島根") %>%
  group_by(TEAM.A) %>%
  summarise(MEAN_ATTENDANCE = mean(ATTENDANCE))

df_b1_3rd.total <- df_b1_3rd %>%
  group_by(TEAM.A) %>%
  summarise(MEAN_ATTENDANCE_NOW = mean(ATTENDANCE),
            NUM_GAMES = n()) 

df_merged <- merge(df_b1_2nd.total, df_b1_3rd.total, by = "TEAM.A")

df_merged$UPDOWN <- (df_merged$MEAN_ATTENDANCE_NOW - df_merged$MEAN_ATTENDANCE) / df_merged$MEAN_ATTENDANCE

ggplot() +
  geom_bar(data = subset(df_merged, UPDOWN >= 0),
           aes(x = TEAM.A, y = UPDOWN),
           stat = "identity",
           fill = "darkgreen") +
  geom_text(data = subset(df_merged, UPDOWN >= 0),
           aes(x = TEAM.A, y = UPDOWN + 0.015),
           color = "darkgreen",
           size = 3,
           label = paste(round(subset(df_merged, UPDOWN >= 0)$UPDOWN, 3) * 100, "%", sep = "")) +
  geom_bar(data = subset(df_merged, UPDOWN < 0),
           aes(x = TEAM.A, y = UPDOWN),
           stat = "identity",
           fill = "darkred") +
  geom_text(data = subset(df_merged, UPDOWN < 0),
            aes(x = TEAM.A, y = UPDOWN - 0.015),
            color = "darkred",
            size = 3,
            label = paste(round(subset(df_merged, UPDOWN < 0)$UPDOWN, 3) * 100, "%", sep = "")) +
  xlab("") +
  ylab("") +
  ggtitle("2017-18の平均観客総動員数と比べて2018-19の平均観客総動員数はどのくらいか") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filename <- "AttendanceRateOverLastSeason.jpeg"
ggsave(filename = filename, width = 7, height = 5)

####
# BREX
####

ggplot() +
  geom_boxplot(data = df_b1_3rd,
               aes(x = TEAM.A, y = ATTENDANCE),
               color = "darkred") +
  geom_point(data = subset(df_b1_3rd, TEAM.B == "栃木"),
             aes(x = TEAM.A, y = ATTENDANCE),
             size = 4,
             alpha = 0.5,
             color = "navy") +
  ylab("観客動員数") + 
  xlab("") +
  ggtitle("2018-19シーズン B1 19ゲーム終了時点の観客動員数（青〇はvs栃木ブレックス）") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("TochigiAway_201819.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = df_b1_2nd,
               aes(x = TEAM.A, y = ATTENDANCE),
               color = "darkgreen") +
  geom_point(data = subset(df_b1_2nd, TEAM.B == "栃木"),
             aes(x = TEAM.A, y = ATTENDANCE),
             size = 4,
             alpha = 0.5,
             color = "navy") +
  ylab("観客動員数") + 
  xlab("") +
  ggtitle("2017-18シーズン B1の観客動員数（青〇はvs栃木ブレックス）") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("TochigiAway_201718.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df_b1_2nd, TEAM.B != "栃木"),
               aes(x = TEAM.B, y = ATTENDANCE),
               color = "black") +
  geom_boxplot(data = subset(df_b1_2nd, TEAM.B == "栃木"),
               aes(x = TEAM.B, y = ATTENDANCE),
               color = "navy",
               size = 1) +
  ylab("観客動員数") + 
  xlab("") +
  ggtitle("2017-18シーズン B1 アウェイでの観客動員数") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AwayAttendance_201718.jpeg", width = 8, height = 5)
