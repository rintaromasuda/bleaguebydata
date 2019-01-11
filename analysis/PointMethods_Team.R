devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)

df <- GetGameSummary()
df.cur <- subset(df, Season == "2018-19" & Category == "Regular")

df.grouped <- df.cur %>%
  group_by(League, TeamName) %>%
  summarise(PointBy3 = sum(F3GM * 3),
            PointBy2Paint = sum(PtsInPaint),
            PointBy2 = sum(F2GM * 2),
            PointByFT = sum(FTM),
            Total = sum(PTS)) %>%
  as.data.frame()

df.grouped$Total.Check <- (df.grouped$PointBy3 + df.grouped$PointBy2 + df.grouped$PointByFT)
nrow(subset(df.grouped, Total != Total.Check)) # Should be 0

df.unpivot <-
  rbind(
      data.frame(
        League = df.grouped$League,
        TeamName = df.grouped$TeamName,
        Method = "3P",
        Point = df.grouped$PointBy3,
        Total = df.grouped$Total,
        Position = 0.95
      ),
      data.frame(
        League = df.grouped$League,
        TeamName = df.grouped$TeamName,
        Method = "2P (Paint)",
        Point = df.grouped$PointBy2Paint,
        Total = df.grouped$Total,
        Position = 0.50
      ),
      data.frame(
        League = df.grouped$League,
        TeamName = df.grouped$TeamName,
        Method = "2P (Not Paint)",
        Point = df.grouped$PointBy2 - df.grouped$PointBy2Paint,
        Total = df.grouped$Total,
        Position = 0.23
      ),
      data.frame(
        League = df.grouped$League,
        TeamName = df.grouped$TeamName,
        Method = "FT",
        Point = df.grouped$PointByFT,
        Total = df.grouped$Total,
        Position = 0.05
      )
)

df.target <- subset(df.unpivot, League == "B2")

ggplot(data = df.target) +
  geom_bar(
    stat = "identity",
    position = "fill",
    aes(x = TeamName,
        y = Point,
        fill = Method)) +
  geom_text(
    aes(x = TeamName,
        y = Position,
        label = paste(as.character(round(Point / Total, 3) * 100),
                      "%",
                      sep = ""))) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ylab("") +
  xlab("") +
  ggtitle("各チームの得点方法（2018-19シーズン31ゲーム終了時点）") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title=element_blank())

ggsave("TeamPointMethod.jpeg", width = 8, height = 5)
