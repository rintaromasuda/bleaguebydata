devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)  
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)  
}

df <- merge(b.games, b.teams, by.x = c("Season","HomeTeamId"), by.y = c("Season", "TeamId"))

df <- df %>%
  filter(EventId %in% c(2, 7)) %>%
  mutate(Month = format(Date, "%m"),
         Year = format(Date, "%Y"),
         DayOfWeek = weekdays(Date, abbreviate = T))
df$DayOfWeek <- factor(df$DayOfWeek, levels = c("月", "火", "水", "木", "金", "土", "日"))

ggplot() +
  geom_jitter(data = df,
              aes(x = Date, y = Attendance, color = League),
              size = 1) +
  geom_smooth(data = df,
              aes(x = Date, y = Attendance, color = League),
              show.legend = FALSE,
              method = "glm") +
  ylab("") +
  xlab("") +
  ggtitle("過去3シーズンの観客動員数") +
  theme_bw() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("plot1.jpeg", width = 8, height = 5)

ggplot() +
  geom_jitter(data = df,
              aes(x = Month, y = Attendance, color = Season)) +
  ylab("") +
  xlab("") +
  ggtitle("過去3シーズンの月ごとの観客動員数") +
  theme_bw() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("plot2.jpeg", width = 8, height = 5)

ggplot() +
  geom_jitter(data = df,
              aes(x = DayOfWeek, y = Attendance, color = Season)) +
  ylab("") +
  xlab("") +
  ggtitle("過去3シーズンの曜日ごとの観客動員数") +
  theme_bw() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("plot3.jpeg", width = 8, height = 5)
