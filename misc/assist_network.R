library(dplyr)
library(bleaguer)
library(knitr)
library(ggplot2)
library(ggrepel)
        
dt <- readr::read_csv("C:\\Users\\rinta\\Desktop\\play_by_play_20200222.csv")
#dt <- readr::read_csv("C:\\Users\\rinta\\Desktop\\play_by_play_b2_20200226.csv")
df <- data.frame(dt)

str(df)

df %>%
  group_by(ActionCD1) %>%
  summarize(F = first(PlayText))

#1 3P-made
#2 3P-miss
#3 2P-outside-paint-made
#4 2P-inside-paint-made
#5 2P-outside-paint-miss
#6 2P-inside-paint-miss
#7 FT-made
#16 Basket Count

df %<>%
  filter(ActionCD1 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 12)) %>%
  group_by(ScheduleKey, TeamID, Period) %>%
  arrange(ScheduleKey, TeamID, Period, No) %>%
  mutate(PrevActionCD1 = lag(ActionCD1, n = 1, default = NA),
         PrevPlayerID1 = lag(PlayerID1, n = 1, default = NA),
         PrevPlayerNameJ1 = lag(PlayerNameJ1, n = 1, default = NA),
         PrevTeamID = lag(TeamID, n = 1, default = NA),
         PrevTeamNameJ = lag(TeamNameJ, n = 1, default = NA),     
         NextActionCD1 = lead(ActionCD1, n = 1, default = NA),
         NextPlayerID1 = lead(PlayerID1, n = 1, default = NA),
         NextPlayerNameJ1 = lead(PlayerNameJ1, n = 1, default = NA),
         NextTeamID = lead(TeamID, n = 1, default = NA),
         NextTeamNameJ = lead(TeamNameJ, n = 1, default = NA),         
         Next2ActionCD1 = lead(ActionCD1, n = 2, default = NA),
         Next2PlayerID1 = lead(PlayerID1, n = 2, default = NA),
         Next2PlayerNameJ1 = lead(PlayerNameJ1, n = 2, default = NA),
         Next2TeamID = lead(TeamID, n = 2, default = NA),
         Next2TeamNameJ = lead(TeamNameJ, n = 2, default = NA)
         ) %>%
  as.data.frame()

df %>%
  filter(ActionCD1 == 12) %>%
  group_by(PrevActionCD1) %>%
  summarise(N = n())

df %>%
  filter(ActionCD1 == 12) %>%
  group_by(TeamID, TeamNameJ, PlayerNameJ1, PrevPlayerNameJ1) %>%
  summarise(N = n()) %>%
  group_by(TeamNameJ) %>% 
  top_n(5, N) %>%
  arrange(TeamNameJ, desc(N)) %>%
  mutate(RowNumber = row_number()) %>%
  select(RowNumber, TeamNameJ, PlayerNameJ1, PrevPlayerNameJ1, N) %>%
  kable()

out <-
df %>%
  filter(ActionCD1 == 12) %>%
  group_by(TeamID, TeamNameJ, PlayerNameJ1, PrevPlayerNameJ1) %>%
  summarise(N = n()) %>%
  group_by(TeamNameJ) %>% 
  arrange(TeamNameJ, desc(N)) %>%
  mutate(RowNumber = row_number()) %>%
  select(RowNumber, TeamNameJ, PlayerNameJ1, PrevPlayerNameJ1, N) %>%
  as.data.frame()

write.csv(out, "b1_assist_to_from_20200226.csv", fileEncoding = "utf-8", row.names = FALSE)

###

plotData <-
df %>%
  mutate(Cnt = 1) %>%
  mutate(IsAssisted =ifelse(!is.na(NextActionCD1) & NextActionCD1 == 12, TRUE, FALSE)) %>%
  group_by(TeamNameJ, PlayerNameJ1) %>%
  summarise(FG = sum(Cnt[ActionCD1 %in% c(1, 3, 4)]),
            FGWithAST = sum(Cnt[ActionCD1 %in% c(1, 3, 4) & IsAssisted]),
            F3G = sum(Cnt[ActionCD1 %in% c(1)]),
            F3GWithAST = sum(Cnt[ActionCD1 %in% c(1) & IsAssisted])) %>%
  mutate(FGRatio = FGWithAST / FG,
         F3GRatio = F3GWithAST / F3G) %>%
  mutate(F3GNoAST = F3G - F3GWithAST)

plotData$Rank <- dense_rank(-1 * plotData$F3G)

ggplot() +
  geom_point(data = subset(plotData, Rank > 30),
             aes(x = F3GWithAST,
                 y = F3G - F3GWithAST),
             size = 1) +
  geom_text(data = subset(plotData, Rank <= 30),
            aes(x = F3GWithAST,
                y = F3G - F3GWithAST,
                label = PlayerNameJ1),
            size = 3,
            alpha = 0.9) +
  labs(x="スリーポイント成功数（アシストあり）",
       y="スリーポイント成功数（アシストなし）",
       title = "スリーポイント成功数（2019-20シーズン B1 第23節終了時点",
       subtitle = "") +
  scale_x_continuous(breaks=seq(0, 100, 10)) +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  coord_fixed() +
  theme_bw()

ggsave("F3GMplot.jpg", width = 9, height = 6)

plotData %>%
  mutate(F3GNoAST = F3G - F3GWithAST) %>%
  mutate(Rate = F3GNoAST / F3G) %>%
  mutate(PctStr = paste0(as.character(round(Rate * 100, 1)), "%")) %>%
  filter(Rank <= 30) %>%
  arrange(Rank) %>%
  select(TeamNameJ, PlayerNameJ1, PctStr) %>%
  kable()

############################# OBSOLETE #############################

df %>%
  filter(ActionCD1 == 12) %>%
  group_by(PrevTeamName, PrevPlayerNameJ1) %>%
  summarize(N = n(),
            GP = n_distinct(ScheduleKey)) %>%
  mutate(Avg = N / GP) %>%
  select(PrevTeamName, PrevPlayerNameJ1, GP, Avg) %>%
  arrange(desc(Avg)) %>%
  head(10) %>%
  kable()

library(ggnetwork)

master <-
data.frame(
  Name = unique(df$PlayerNameJ1),
  X = runif(length(unique(df$PlayerNameJ1)), 0, 10),
  Y = runif(length(unique(df$PlayerNameJ1)), 0, 10)
)  

data <- merge(assist, master, by.x = "PlayerNameJ1", by.y = "Name")
data <- merge(data, master, by.x = "PrevPlayerNameJ1", by.y = "Name")

data <- subset(data, TeamNameJ == "川崎ブレイブサンダース")

row <- data.frame(
  PrevPlayerNameJ1 = c("ジョーダン・ヒース", "鎌田 裕也"),
  PlayerNameJ1 = c("ジョーダン・ヒース", "鎌田 裕也"),
  TeamNameJ = c("川崎ブレイブサンダース", "川崎ブレイブサンダース"),
  Count = c(0, 0),
  
)

ggplot(data,
       aes(x = X.x, y = Y.x, xend = X.y, yend = Y.y)) +
  geom_edges(aes(size = Count),
             angle = 90,
             position = "dodge",
             curvature = 0.45,
             arrow = arrow(length = unit(0.75, "lines"), type = "closed"),
             color = "grey50") +
  geom_nodes() +
  geom_nodetext_repel(aes(label = PlayerNameJ1)) +
  theme_blank()
d