devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(ggplot2)

df <- GetGameSummary()
df.cur <- subset(df, Season == "2018-19" & Category == "Regular")

df.target <-
  rbind(
    data.frame(
      League = df.cur$League,
      TeamName = df.cur$TeamName,
      Point = df.cur$Q1,
      Opp.Point = df.cur$Opp.Q1,
      Q = "Q1"
    ),
    data.frame(
      League = df.cur$League,
      TeamName = df.cur$TeamName,
      Point = df.cur$Q2,
      Opp.Point = df.cur$Opp.Q2,
      Q = "Q2"
    ),
    data.frame(
      League = df.cur$League,
      TeamName = df.cur$TeamName,
      Point = df.cur$Q3,
      Opp.Point = df.cur$Opp.Q3,
      Q = "Q3"
    ),
    data.frame(
      League = df.cur$League,
      TeamName = df.cur$TeamName,
      Point = df.cur$Q4,
      Opp.Point = df.cur$Opp.Q4,
      Q = "Q4"
    )
    )

# Point
ggplot() +
  geom_boxplot(data = subset(df.target, League == "B1"),
               aes(x = TeamName, y = Point, fill = Q)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの得点") +
  xlab("") +
  ggtitle("各チームのクォーターごと得点")

ggsave("Q_Point_B1.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df.target, League == "B2"),
               aes(x = TeamName, y = Point, fill = Q)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの得点") +
  xlab("") +
  ggtitle("各チームのクォーターごと得点")

ggsave("Q_Point_B2.jpeg", width = 8, height = 5)

# Opponent Point
ggplot() +
  geom_boxplot(data = subset(df.target, League == "B1"),
               aes(x = TeamName, y = Opp.Point, fill = Q)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの失点") +
  xlab("") +
  ggtitle("各チームのクォーターごと失点")

ggsave("Q_OppPoint_B1.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df.target, League == "B2"),
               aes(x = TeamName, y = Opp.Point, fill = Q)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの失点") +
  xlab("") +
  ggtitle("各チームのクォーターごと失点")

ggsave("Q_OppPoint_B2.jpeg", width = 8, height = 5)

# Difference b/w Point and Opponent Point
ggplot() +
  geom_boxplot(data = subset(df.target, League == "B1"),
               aes(x = TeamName, y = Point - Opp.Point, fill = Q)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dotted") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの得失点差") +
  xlab("") +
  ggtitle("各チームのクォーターごと得失点差（2019-20）") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave("Q_DiffPoint_B1.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df.target, League == "B2"),
               aes(x = TeamName, y = Point - Opp.Point, fill = Q)) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dotted") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
  ylab("クォーターごとの得失点差") +
  xlab("") +
  ggtitle("各チームのクォーターごと得失点差（2019-20）") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave("Q_DiffPoint_B2.jpeg", width = 8, height = 5)

###
# First Half vs. Second Half
###
plotHalf <- function(teamId) {
  teamId <- 729
  df <- GetGameSummary()
  df <- subset(df, Season == b.current.season &
                 Category == "Regular" &
                 TeamId == teamId)
  df$FirstPts <- df$Q1 + df$Q2
  df$FirstOppPts <- df$Opp.Q1 + df$Opp.Q2
  df$SecondPts <- df$Q3 + df$Q4
  df$SecondOppPts <- df$Opp.Q3 + df$Opp.Q4
  nrow(subset(df, FirstPts > FirstOppPts))
  nrow(subset(df, FirstPts < FirstOppPts))
  nrow(subset(df, FirstPts == FirstOppPts))
  
  nrow(subset(df, SecondPts > SecondOppPts))
  nrow(subset(df, SecondPts < SecondOppPts))
  nrow(subset(df, SecondPts == SecondOppPts))
  
}
