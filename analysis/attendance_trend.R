devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

df <- GetGameSummary()
df <- subset (df, Category == "Regular" & HomeAway == "Home")
df$DayOfWeek <- weekdays(df$Date, abbreviate = T)

showAttendanceTrend <- function(team) {
  longName <-subset(b.teams, NameShort == team & Season == "2018-19")$NameLong
  
  df.team <- subset(df, TeamName == team)
  
  ggplot() +
    geom_line(data = subset(df.team, Season == "2016-17"),
              aes(x = (Date + 365 + 365),
                  y = Attendance,
                  color = "2016-17"),
              size = 1,
              linetype = 1) +
    geom_point(data = subset(df.team, Season == "2016-17"),
               aes(x = (Date + 365 + 365),
                   y = Attendance),
               color = "red") +
    geom_label_repel(data = subset(df.team, Season == "2016-17" & (DayOfWeek != "日" & DayOfWeek != "土")),
                     aes(x = Date + 365 + 365, y = Attendance, label = DayOfWeek),
                     nudge_y = 5500 - subset(df.team, Season == "2016-17" & (DayOfWeek != "日" & DayOfWeek != "土"))$Attendance,
                     color ="red",
                     segment.size  = 0.05) +
    geom_line(data = subset(df.team, Season == "2017-18"),
              aes(x = (Date + 365),
                  y = Attendance,
                  color = "2017-18"),
              size = 1,
              linetype = 1) +
    geom_point(data = subset(df.team, Season == "2017-18"),
               aes(x = (Date + 365),
                   y = Attendance),
               color = "darkgreen") +
    geom_label_repel(data = subset(df.team, Season == "2017-18" & (DayOfWeek != "日" & DayOfWeek != "土")),
                     aes(x = Date + 365, y = Attendance, label = DayOfWeek),
                     nudge_y = 6000 - subset(df.team, Season == "2017-18" & (DayOfWeek != "日" & DayOfWeek != "土"))$Attendance,
                     color ="darkgreen",
                     segment.size  = 0.05) +
    geom_line(data = subset(df.team, Season == "2018-19"),
              aes(x = (Date),
                  y = Attendance,
                  color = "2018-19"),
              alpha = 0.8,
              size = 3,
              linetype = 1) +
    geom_point(data = subset(df.team, Season == "2018-19"),
               aes(x = (Date),
                   y = Attendance),
               color = "blue") +
    geom_label_repel(data = subset(df.team, Season == "2018-19" & (DayOfWeek != "日" & DayOfWeek != "土")),
                     aes(x = Date, y = Attendance, label = DayOfWeek),
                     nudge_y = 100 - subset(df.team, Season == "2018-19" & (DayOfWeek != "日" & DayOfWeek != "土"))$Attendance,
                     color ="blue",
                     segment.size  = 0.05) +
    ylab("観客動員数（人）") +
    xlab("開催日") +
    ggtitle(paste0(longName, "のホームゲーム観客動員数")) +
    scale_x_date(date_labels = "%m/%d", limits = c(as.Date("2018-09-28"), as.Date("2019-05-07"))) +
    scale_y_continuous(limits = c(0, 7500)) +
    scale_colour_discrete(name = "シーズン") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(team, "_AttendanceTrend.jpeg"), width = 8, height = 5)
}

showAttendanceTrend("栃木")

####
# Accumulated attendance
####
df.cum <- df %>%
  group_by(Season, Date) %>%
  arrange(Season, Date) %>%
  summarise(Attendance = sum(Attendance)) %>%
  mutate(CumAttendance = cumsum(Attendance)) %>%
  as.data.frame()

ggplot() +
  geom_line(data = subset(df.cum, Season == "2016-17"),
            aes(x = Date + 365 + 365,
                y = CumAttendance,
                color = Season),
            size = 2) +
  geom_line(data = subset(df.cum, Season == "2017-18"),
            aes(x = Date + 365,
                y = CumAttendance,
                color = Season),
            size = 2) +
  geom_line(data = subset(df.cum, Season == "2018-19"),
            aes(x = Date,
                y = CumAttendance,
                color = Season),
            size = 2) +
  ylab("観客動員数の累計（人）") +
  xlab("開催日") +
  ggtitle("B1、B2すべてのホームゲーム観客動員数の累計") +
  scale_x_date(date_labels = "%m/%d") +
  scale_colour_discrete(name = "シーズン") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("AccumulatedAttendance.jpeg", width = 8, height = 5)
