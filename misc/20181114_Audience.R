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
df_b1_3rd <- cleanup(read.csv("B1_201819_9setsu_Summary.csv"))
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
                    nudge_y = 5500 - subset(df_b1_3rd, TEAM.A == team & (DAYOFWEEK != "日" & DAYOFWEEK != "土"))$ATTENDANCE,
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

#compareSeason("京都")
#compareSeason("新潟")
#compareSeason("滋賀")
#compareSeason("大阪")

#compareSeason("千葉")
#compareSeason("栃木")
#compareSeason("北海道")
#compareSeason("琉球")

#compareSeason("川崎")
#compareSeason("富山")
#compareSeason("名古屋D")
#compareSeason("SR渋谷")

compareSeason("A東京")
compareSeason("三遠")
compareSeason("三河")
compareSeason("横浜")

###
# Compare average b/w 2018-19
###

df_grouped <- df %>%
  filter(ISHOME.A) %>%
  group_by(DATE) %>%
  summarise(SUM_ATTENDANCE = sum(ATTENDANCE),
            AVG_ATTENDANCE = mean(ATTENDANCE),
            GAMES = n())

df_grouped <- df_grouped[order(df_grouped$DATE, decreasing = FALSE),]
df_grouped$TOTAL <- cumsum(df_grouped$SUM_ATTENDANCE)
df_grouped$DAYOFWEEK <- weekdays(df_grouped$DATE, abbreviate = T)

ggplot() +
  geom_line(data = df_grouped, aes(x = DATE, y = TOTAL)) +
  geom_hline(yintercept=2750000, color  ='green', size = 2) +
  xlim(c(as.Date("2018-9-30"), as.Date("2018-12-31")))

ggplot() +
  geom_line(data = df_grouped, aes(x = DATE, y = AVG_ATTENDANCE)) +
  geom_point(data = subset(df_grouped, DAYOFWEEK == "日" | DAYOFWEEK == "土"), aes(x = DATE, y = AVG_ATTENDANCE), size = 3) +
  geom_point(data = subset(df_grouped, DAYOFWEEK != "日" & DAYOFWEEK != "土"), aes(x = DATE, y = AVG_ATTENDANCE), size = 3, color = "red") +
  geom_text_repel(data = subset(df_grouped, DAYOFWEEK != "日" & DAYOFWEEK != "土"),
                  aes(x = DATE, y = AVG_ATTENDANCE, label = paste(DAYOFWEEK, " (", GAMES, ")", sep = "")),
                  nudge_x = 1.5,
                  nudge_y = 500 - subset(df_grouped, DAYOFWEEK != "日" & DAYOFWEEK != "土")$AVG_ATTENDANCE,
                  color ="red",
                  segment.size  = 0.01,
                  ) +
  scale_x_date(date_labels = "%m/%d") +
  ylim(c(0, 6000)) +
  ylab("平均観客動員数（人）") +
  xlab("開催日") +
  ggtitle("2018-19シーズン B2の平均観客動員数の推移\n（）内はその日のゲーム数") +
  theme_bw()

ggsave(filename = "AudienceB2.jpg", width = 7, height = 5)
