library(bleaguer)
library(dplyr)
library(ggplot2)
library(readr)
library(knitr)

################
# Loading data #
################
readPlayByPlayCsv <- function(fileName) {
  df <- readr::read_csv(fileName,
                        col_types = list(
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_number(),
                          readr::col_integer(),
                          readr::col_integer(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_number(),
                          readr::col_number(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_character(),
                          readr::col_number(),
                          readr::col_character()
                        ),
                        locale = readr::locale(encoding = "UTF-8"))
  return(df)
}

# Roading data
df_201617_b1 <- readPlayByPlayCsv("..//data//play_by_play_b1_201617.csv")
df_201718_b1 <- readPlayByPlayCsv("..//data//play_by_play_b1_201718.csv")
df_201819_b1 <- readPlayByPlayCsv("..//data//play_by_play_b1_201819.csv")
df_201920_b1 <- readPlayByPlayCsv("..//data//play_by_play_b1_201920.csv")

df_201617_b2 <- readPlayByPlayCsv("..//data//play_by_play_b2_201617.csv")
df_201718_b2 <- readPlayByPlayCsv("..//data//play_by_play_b2_201718.csv")
df_201819_b2 <- readPlayByPlayCsv("..//data//play_by_play_b2_201819.csv")
df_201920_b2 <- readPlayByPlayCsv("..//data//play_by_play_b2_201920.csv")

df_pbyp <- rbind(
  df_201617_b1,
  df_201718_b1,
  df_201819_b1,
  df_201920_b1,
  df_201617_b2,
  df_201718_b2,
  df_201819_b2,
  df_201920_b2
)

##################
# Extending data #
##################
df_pbyp <- within(df_pbyp, {
  Counter <- 1
  HomeAway <- ifelse(is.na(HomeAway), 0, HomeAway)

  # ActionCD1 == "1" # 3P Made
  # ActionCD1 == "3" # FG Made (Outside of Paint)
  # ActionCD1 == "4" # FG Made (Outside of Paint)
  # ActionCD1 == "7" # FT Made
  PtsMade <- ifelse(ActionCD1 == "1", 3,
                    ifelse(ActionCD1 == "3", 2,
                           ifelse(ActionCD1 == "4", 2,
                                  ifelse(ActionCD1 == "7", 1, 0))))
  PtsMadeHome <- ifelse(HomeAway == 1, PtsMade, 0)
  PtsMadeAway <- ifelse(HomeAway == 2, PtsMade, 0)

  RestTimeNum <- bleaguer::ConvertMinStrToDec(RestTime)
})

df_pbyp %<>%
  group_by(ScheduleKey) %>%
  arrange(ScheduleKey, No) %>%
  mutate(PtsHome = cumsum(PtsMadeHome),
         PtsAway = cumsum(PtsMadeAway)) %>%
  mutate(PtsDiff = abs(PtsHome - PtsAway))

df_pbyp %<>%
  group_by(ScheduleKey) %>%
  arrange(ScheduleKey, No) %>%
  mutate(PtsDiffPrev = lag(PtsDiff, 1, default = 0))

df_pbyp <- within(df_pbyp, {
  IsClutchTime <- (Period >= 4 & RestTimeNum <= 5 & PtsDiffPrev <= 5)
  IsSuperClutchTime <- (Period >= 4 & RestTimeNum <= 1 & PtsDiffPrev <= 1)
})

##################
# Filtering data #
##################
targetGames <- subset(b.games, (EventId == 2 | EventId == 7) & Season %in% c("2016-17", "2017-18", "2018-19", "2019-20"))
dim(targetGames)

df_pbyp_target <- subset(df_pbyp, ScheduleKey %in% targetGames$ScheduleKey)
length(unique(df_pbyp_target$ScheduleKey))

##################
# Analyzing data #
##################
nrow(subset(df_pbyp_target, ActionCD1 == "7")) # FT Made
nrow(subset(df_pbyp_target, ActionCD1 == "8")) # FT Fail
nrow(subset(df_pbyp_target, ActionCD1 %in% c("7", "8"))) # FT Attempted

df_plot <-
df_pbyp_target %>%
  filter(Period <= 4) %>%
  group_by(PlayerID1) %>%
  mutate(PlayerNameJ1_Fixed = last(PlayerNameJ1)) %>%
  group_by(PlayerID1, PlayerNameJ1_Fixed) %>%
  mutate(FTM_Total = sum(Counter[ActionCD1 == "7"]),
         FTA_Total = sum(Counter[ActionCD1 %in% c("7", "8")])) %>%
  mutate(FTR_Total = FTM_Total / FTA_Total) %>%
  group_by(PlayerID1, PlayerNameJ1_Fixed, FTM_Total, FTA_Total, FTR_Total, Period) %>%
  summarize(FTM = sum(Counter[ActionCD1 == "7"]),
            FTA = sum(Counter[ActionCD1 %in% c("7", "8")])) %>%
  mutate(FTR = FTM / FTA,
         Counter = ifelse(FTA >= 50, 1, 0)) %>%
  arrange(desc(FTA_Total), Period, PlayerID1) %>%
  mutate(FTM_InTheory = FTA * FTR_Total) %>%
  mutate(FTMDiffFromTheory = FTM - FTM_InTheory) %>%
  mutate(VAR = FTA * FTR_Total * (1 - FTR_Total)) %>%
  mutate(SD = sqrt(VAR)) %>%
  mutate(Z = FTMDiffFromTheory / SD) %>%
  group_by(PlayerID1) %>%
  mutate(IsTarget = sum(Counter) >= 4) %>%
  filter(IsTarget) %>%
  as.data.frame()

length(unique(df_plot$PlayerID1))

ggplot() +
  geom_boxplot(data = df_plot,
               aes(x = as.factor(Period),
                   fill = as.factor(Period),
                   y = Z)) +
  labs(x = "クォーター", y = "Z得点") +
  theme(
    legend.position = "none"
  )

df_plot %>%
  group_by(PlayerID1, PlayerNameJ1_Fixed) %>%
  summarize(FTR_All = max(FTR_Total),
            Q1 = max(FTR[Period == 1]),
            Q2 = max(FTR[Period == 2]),
            Q3 = max(FTR[Period == 3]),
            Q4 = max(FTR[Period == 4]),
            Z1 = max(Z[Period == 1]),
            Z2 = max(Z[Period == 2]),
            Z3 = max(Z[Period == 3]),
            Z4 = max(Z[Period == 4])
            ) %>%
  mutate(ALL = paste0(as.character(round(FTR_All, 3) * 100), "%"),
         Q1R = paste0(as.character(round(Q1, 3) * 100), "%"),
         Q2R = paste0(as.character(round(Q2, 3) * 100), "%"),
         Q3R = paste0(as.character(round(Q3, 3) * 100), "%"),
         Q4R = paste0(as.character(round(Q4, 3) * 100), "%"),
         ) %>%
  ungroup() %>%
  select(
    PlayerNameJ1_Fixed,
    ALL,
    Q1R,
    Q2R,
    Q3R,
    Q4R
  ) %>%
  kable()
  