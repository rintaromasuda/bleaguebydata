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
targetGames <- subset(b.games, EventId == 2 & Season %in% c("2016-17", "2017-18", "2018-19", "2019-20"))
dim(targetGames)

df_pbyp_target <- subset(df_pbyp, ScheduleKey %in% targetGames$ScheduleKey)
length(unique(df_pbyp_target$ScheduleKey))

##################
# Analyzing data #
##################
nrow(subset(df_pbyp_target, ActionCD1 == "7")) # FT Made
nrow(subset(df_pbyp_target, ActionCD1 == "8")) # FT Fail

df_pbyp_target %>%
  summarise(FTM = sum(Counter[ActionCD1 == "7"]),
            FTF = sum(Counter[ActionCD1 == "8"])) %>%
  summarise(FTA = sum(FTM) + sum(FTF),
            FTR = sum(FTM) / (sum(FTM) + sum(FTF)))

df_pbyp_target %>%
  group_by(Period) %>%
  summarise(FTM = sum(Counter[ActionCD1 == "7"]),
            FTF = sum(Counter[ActionCD1 == "8"])) %>%
  mutate(FTA = FTM + FTF) %>%
  mutate(FTR = FTM / FTA) %>%
  select(c("Period", "FTA", "FTM", "FTR")) %>%
  kable()

df_pbyp_target %>%
  group_by(IsClutchTime) %>%
  summarise(FTM = sum(Counter[ActionCD1 == "7"]),
            FTF = sum(Counter[ActionCD1 == "8"])) %>%
  mutate(FTA = FTM + FTF) %>%
  mutate(FTR = FTM / FTA) %>%
  select(c("IsClutchTime", "FTA", "FTM", "FTR")) %>%
  kable()

df_pbyp_target %>%
  group_by(IsSuperClutchTime) %>%
  summarise(FTM = sum(Counter[ActionCD1 == "7"]),
            FTF = sum(Counter[ActionCD1 == "8"])) %>%
  mutate(FTA = FTM + FTF) %>%
  mutate(FTR = FTM / FTA) %>%
  select(c("IsSuperClutchTime", "FTA", "FTM", "FTR")) %>%
  kable()

df_pbyp_target %>%
  filter(IsSuperClutchTime) %>%
  filter(ActionCD1 %in% c("7", "8")) %>%
  group_by(PlayerNameJ1) %>%
  summarise(FTA = n(),
            FTM = sum(Counter[ActionCD1 == "7"])) %>%
  mutate(FTR = FTM / FTA) %>%
  select(c("PlayerNameJ1", "FTA", "FTM", "FTR")) %>%
  arrange(desc(FTA)) %>%
  kable()
