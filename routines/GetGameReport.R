target.key <- 4377
home.teamName <- ""
away.teamName <- ""
game.date <- ""
y.adjust <- 5

Sys.setlocale(locale = 'Japanese')

library(bleaguer)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
}

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browserName = "chrome")
remDr$open()

scrapePlayByPlayData <- function(){
  df_pbyp <- data.frame()

  url.detail <- paste0("https://www.bleague.jp/game_detail/?ScheduleKey=",
                       as.character(target.key),
                       "&TAB=P")
  print(url.detail)

  # Access the page until it gets a success
  try.count <- 1
  try.success <- FALSE
  try.threshold <- 5
  while (try.count <= try.threshold) {
    remDr$navigate(url.detail)
    pageSource <- remDr$getPageSource()
    html.pbyp <- read_html(pageSource[[1]])

    nodes.period <- html.pbyp %>%
      html_nodes("#game__playbyplay__inner > ul.playbyplay_contents.playbyplay_contents_text > li > ul")

    if(length(nodes.period) > 0){
      if(length(html_nodes(nodes.period[1], "li")) > 0){
        try.success <- TRUE
        break
      }
    }
  }

  if (!try.success) {
    stop(paste0("Error getting data from ", url.detail))
  }

  # Actual parse
  home <- html.pbyp %>%
    html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home.win > div.team_name > p.for-sp") %>%
    html_text()
  if (identical(home, character(0))) {
    home <- html.pbyp %>%
      html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home > div.team_name > p.for-sp") %>%
      html_text()
  }
  away <- html.pbyp %>%
    html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away.win > div.team_name > p.for-sp") %>%
    html_text()
  if (identical(away, character(0))) {
    away <- html.pbyp %>%
      html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away > div.team_name > p.for-sp") %>%
      html_text()
  }

  home.teamName <<- home
  away.teamName <<- away

  date <- html.pbyp %>%
    html_nodes("#game__top__inner > div.date_wrap > p:nth-child(2) > span") %>%
    html_text()
  game.date <<- bleaguer::GetFullDateString(date, b.current.season)

  for (period_node in nodes.period) {
    period_num <- as.integer(html_attr(period_node, "data-period"))

    nodes.actions <- html_nodes(period_node, "li")
    for (action_node in nodes.actions) {
      data_no <- as.integer(html_attr(action_node, "data-no"))
      action_cd <- html_attr(action_node, "data-action-cd")
      home_away <- html_attr(action_node, "class")

      nodes.divs <- html_nodes(action_node, "div")
      player_data <- ""
      clock <- ""
      image_url <- ""
      for (div_node in nodes.divs) {
        class_name <- html_attr(div_node, "class")
        if (class_name == "player_data") {
          player_data <- html_text(html_node(div_node, "p"))
        } else if (class_name== "time_point_wrap") {
          clock <- html_text(html_node(div_node, "p"))
        } else if (class_name == "player_img") {
          image_url <- html_attr(div_node, "style")
        }
      }

      df.action <- data.frame(
        ScheduleKey = target.key,
        HomeAway = home_away,
        Period = period_num,
        DataNo = data_no,
        ActionCd = action_cd,
        Clock = clock,
        PlayerData = player_data,
        ImageUrl = image_url
      )

      df_pbyp <- rbind(df_pbyp, df.action)
    }
  }

  return(df_pbyp)
}

processPlayByPlayData <- function(df){
  df$ImageUrl <- as.character(df$ImageUrl)
  df$PlayerId <- as.integer(sapply(strsplit(sapply(strsplit(df$ImageUrl, "/"), function(x) {x[7]}), "_"), function(x) {x[1]}))
  df$RemainingMinPeriod <- bleaguer::ConvertMinStrToDec(df$Clock)
  df$PastMinInPeriod <- ifelse(df$Period <= 4,
                                    (10 - df$RemainingMinPeriod),
                                    (5 - df$RemainingMinPeriod))
  df$PastMinInGame <- ifelse(df$Period <= 4,
                                  (((df$Period - 1) * 10) + df$PastMinInPeriod),
                                  (40 + ((df$Period - 5) * 5) + df$PastMinInPeriod))
  df$PastMinInGameClass <- ceiling(df$PastMinInGame)

  df %<>%
    arrange(Period, DataNo) %>%
    mutate(PtsAdded = ifelse(ActionCd == "1", 3,
                             ifelse(ActionCd == "3" | ActionCd == "4", 2,
                                    ifelse(ActionCd == "7", 1, 0)))) %>%
    mutate(HomePtsAdded = ifelse(grepl("home", HomeAway, ignore.case = TRUE), PtsAdded, 0),
           AwayPtsAdded = ifelse(grepl("away", HomeAway, ignore.case = TRUE), PtsAdded, 0))

  df %<>%
    arrange(Period, DataNo) %>%
    mutate(HomeAccPts = cumsum(HomePtsAdded),
           AwayAccPts = cumsum(AwayPtsAdded))

  df$PtsDiff = df$HomeAccPts - df$AwayAccPts

  df$Team <- ifelse(grepl("home", df$HomeAway, ignore.case = TRUE), home.teamName, away.teamName)
  return(df)
}

df_pbyp <- scrapePlayByPlayData()
df_pbyp <- processPlayByPlayData(df_pbyp)

df_pbyp$Team <- factor(df_pbyp$Team, levels = c(home.teamName, away.teamName))

############
# Q labels #
############
df_label <-
  rbind(
    data.frame(Label = "Q1",  X = 1),
    data.frame(Label = "Q2",  X = 11),
    data.frame(Label = "Q3",  X = 21),
    data.frame(Label = "Q4",  X = 31),
    data.frame(Label = "OT1", X = 41),
    data.frame(Label = "OT2", X = 46),
    data.frame(Label = "OT3", X = 51),
    data.frame(Label = "OT4", X = 56)
  )

###########
# Timeout #
###########
df_timeout <-
  df_pbyp %>%
  filter(!is.na(PastMinInGameClass)) %>%
  filter(ActionCd == "88") %>%
  as.data.frame()

#########
# Point #
#########
df_point_per_min <-
  df_pbyp %>%
  filter(!is.na(PastMinInGameClass)) %>%
  arrange(DataNo) %>%
  group_by(PastMinInGameClass) %>%
  summarize(PtsHome = last(HomeAccPts),
            PtsAway = last(AwayAccPts))

df_point <-
  rbind(
    data.frame(
      Team = home.teamName,
      Min = df_point_per_min$PastMinInGameClass,
      Pts = df_point_per_min$PtsHome,
      Diff = df_point_per_min$PtsHome - df_point_per_min$PtsAway
    ),
    data.frame(
      Team = away.teamName,
      Min = df_point_per_min$PastMinInGameClass,
      Pts = df_point_per_min$PtsAway,
      Diff = df_point_per_min$PtsAway - df_point_per_min$PtsHome
    )
  )
df_point$Direction = ifelse(df_point$Diff > 0, 1,
                            ifelse(df_point$Diff < 0, -1,
                                   ifelse(df_point$Team == home.teamName, 1, -1)))

#########
# Stats #
#########
df_stats <-
  df_pbyp %>%
  filter(!is.na(PastMinInGameClass)) %>%
  group_by(Team, Period) %>%
  summarize(FGM = sum(ActionCd %in% c("1", "3", "4")),
            FGA = sum(ActionCd %in% c("1", "2", "3", "4", "5", "6")),
            F2GM = sum(ActionCd %in% c("3", "4")),
            F2GA = sum(ActionCd %in% c("3", "4", "5", "6")),
            F3GM = sum(ActionCd %in% c("1")),
            F3GA = sum(ActionCd %in% c("1", "2")),
            FTM = sum(ActionCd %in% c("7")),
            FTA = sum(ActionCd %in% c("7", "8"))) %>%
  as.data.frame()
df_stats$X <- ifelse(df_stats$Period < 5,
                     (df_stats$Period - 1) * 10 + 5,
                     (df_stats$Period + 3) * 5 + 2.5)
df_stats$Y <- ifelse(df_stats$Period <= 2, max(df_point$Pts) + y.adjust - 10, 25)
df_stats$Y <- ifelse(df_stats$Team != home.teamName, df_stats$Y - 17, df_stats$Y)

####################
# Pts History Plot #
####################
home.longName <- subset(b.teams, Season == b.current.season & NameShort == home.teamName)[c("NameLong")][[1]]
away.longName <- subset(b.teams, Season == b.current.season & NameShort == away.teamName)[c("NameLong")][[1]]
plot.title <- paste0(home.longName,
                     " vs ",
                     away.longName,
                     " (",
                     game.date,
                     ")")

gp1 <-
  ggplot() +
    geom_vline(xintercept =  0, linetype="dashed", color = "grey", size=1) +
    geom_vline(xintercept = 10, linetype="dashed", color = "grey", size=1) +
    geom_vline(xintercept = 20, linetype="dashed", color = "grey", size=1) +
    geom_vline(xintercept = 30, linetype="dashed", color = "grey", size=1) +
    geom_vline(xintercept = 40, linetype="dashed", color = "grey", size=1)

if(max(df_point$Min) > 40) {
gp1 <-
  gp1 +
    geom_vline(xintercept = 45, linetype="dashed", color = "grey", size=1)
}

gp1 +
  geom_text(data = subset(df_label, X < max(df_point$Min)),
            aes(x = X,
                y = max(df_point$Pts) + y.adjust,
                label = as.character(Label)),
            color = "grey") +
  geom_text(data = subset(df_stats, X < max(df_point$Min)),
            aes(x = X,
                y = Y,
                label = paste0("2P: ",
                               as.character(F2GM),
                               "/",
                               as.character(F2GA),
                               "\n",
                               "3P: ",
                               as.character(F3GM),
                               "/",
                               as.character(F3GA),
                               "\n",
                               "FT: ",
                               as.character(FTM),
                               "/",
                               as.character(FTA)),
                color = Team),
            size = 4) +
  geom_line(data = df_point,
            aes(x = Min,
                y = Pts,
                color = Team),
            size = 1) +
  geom_point(data = df_point,
            aes(x = Min,
                y = Pts,
                color = Team),
            shape = 15,
            size = 3) +
  geom_label(data = subset(df_point, Min %in% c(10, 20, 30, 40, 45, 50, 55, 60, 65)),
                   show.legend = FALSE,
                   aes(x = Min,
                       y = Pts + (Direction * 5),
                       label = as.character(Pts),
                       color = Team),
                   size = 5) +
  geom_point(data = df_timeout,
             aes(x = PastMinInGameClass,
                 y = -2,
                 color = Team),
             shape = 17,
             size = 3) +
  xlab("") +
  ylab("") +
  labs(title = plot.title,
       subtitle = "▲は各チームが取得したタイムアウト") +
  theme_bw() +
  theme(
    legend.position="top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

fileName <- paste0("PtsHistory_",
                   target.key,
                   ".jpg")
ggsave(fileName, width = 6, height = 9)

##############
# Plus Minus #
##############
scrapeBoxScoreData <- function(){
  df_boxscore <- data.frame()

  url.detail <- paste0("https://www.bleague.jp/game_detail/?ScheduleKey=",
                       as.character(target.key),
                       "&TAB=B")
  print(url.detail)

  try.count <- 1
  try.success <- FALSE
  try.threshold <- 60
  expected.table.count <- 13
  while (try.count <= try.threshold) {
    remDr$navigate(url.detail)
    pageSource <- remDr$getPageSource()
    html.boxscore <- read_html(pageSource[[1]])
    tables.boxscore <- html_table(html.boxscore)
    # Check the page and leave if it's good
    if ((length(tables.boxscore) >= expected.table.count) &&
        (nrow(tables.boxscore[[4]]) > 0) &&
        (nrow(tables.boxscore[[5]]) > 0)) {
      try.success <- TRUE
      break
    } else {
      Sys.sleep(0.5)
      print(paste("Retry the page load...(", try.count, ")", sep = ""))
      try.count <- try.count + 1
    }
  }

  if (!try.success) {
    stop(paste0("Error getting data from ", url.detail))
  }

  tables.boxscore <- html.boxscore %>%
    html_nodes(xpath = "//ul[@class=\"boxscore_contents\"]/li[@data-period=\"0\"]//tbody")

  for(tbl in tables.boxscore){
    homeAway <- html_attr(tbl, "class")
    team <- ifelse(grepl("home", homeAway, ignore.case = TRUE), home.teamName, away.teamName)
    for(player in html_nodes(tbl, "tr")){
      playerId <- html_attr(player, "data-player-id")
      cols <- html_nodes(player, "td")
      number <- paste0("#", html_text(cols[1]))
      starterBench <- html_text(cols[2])
      name <- html_text(html_node(cols[3], "a > span.for-pc"))
      nameShort <- html_text(html_node(cols[3], "a > span.for-sp"))

      df_player <- data.frame(
        Team = team,
        PlayerId = playerId,
        Number = number,
        StarterBench = starterBench,
        Name = name,
        NameShort = nameShort
      )

      df_boxscore <- rbind(df_boxscore, df_player)
    }
  }

  return(df_boxscore)
}

df_boxscore <- scrapeBoxScoreData()
df_boxscore$Team <- factor(df_boxscore$Team, levels = c(home.teamName, away.teamName))

#############
# Calculate #
#############
calculatePlusMinus <- function(df_pbyp, df_boxscore){
  df <- df_pbyp %>% arrange(DataNo) %>% as.data.frame()

  homeTeam <- rep(0, nrow(subset(df_boxscore, Team == home.teamName)))
  names(homeTeam) <- df_boxscore[df_boxscore$Team == home.teamName,][["Number"]]
  awayTeam <- rep(0, nrow(subset(df_boxscore, Team == away.teamName)))
  names(awayTeam) <- df_boxscore[df_boxscore$Team == away.teamName,][["Number"]]

  homeOnCourt <- list()
  awayOnCourt <- list()

  for(rowNum in 1:nrow(df)){
    row <- df[rowNum,]

    if(row$ActionCd == "86"){
      player_in <- str_extract(row$PlayerData, "#[0-9]+")
      if(grepl("home", row$HomeAway, ignore.case = TRUE)) {
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else if(row$ActionCd == "89"){
      splitted <- str_split(row$PlayerData, " ")[[1]]
      player_out <- str_extract(splitted[1], "#[0-9]+")
      player_in <- str_extract(splitted[4], "#[0-9]+")

      if(grepl("home", row$HomeAway, ignore.case = TRUE)) {
        homeOnCourt <- homeOnCourt[homeOnCourt != player_out]
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- awayOnCourt[awayOnCourt != player_out]
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else{
      if(row$HomePtsAdded > 0){
        homeTeam <-
          append(homeTeam[names(homeTeam) %in% homeOnCourt] + row$HomePtsAdded,
                 homeTeam[!names(homeTeam) %in% homeOnCourt])
        awayTeam <-
          append(awayTeam[names(awayTeam) %in% awayOnCourt] - row$HomePtsAdded,
                 awayTeam[!names(awayTeam) %in% awayOnCourt])
      }

      if(row$AwayPtsAdded > 0){
        homeTeam <-
          append(homeTeam[names(homeTeam) %in% homeOnCourt] - row$AwayPtsAdded,
                 homeTeam[!names(homeTeam) %in% homeOnCourt])
        awayTeam <-
          append(awayTeam[names(awayTeam) %in% awayOnCourt] + row$AwayPtsAdded,
                 awayTeam[!names(awayTeam) %in% awayOnCourt])
      }
    }
  }

  df_result <- data.frame(
    Team = append(rep(home.teamName, length(homeTeam)), rep(away.teamName, length(awayTeam))),
    Number = append(names(homeTeam), names(awayTeam)),
    PlusMinus = append(homeTeam, awayTeam)
  )
  return(df_result)
}

df_plusminus <- calculatePlusMinus(df_pbyp, df_boxscore)
df_adv_boxscore <- merge(df_boxscore, df_plusminus, by = c("Team", "Number"))

#############
# Visualize #
#############
ggplot() +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size=1) +
  geom_bar(data = df_adv_boxscore,
           aes(x = reorder(PlayerId, PlusMinus),
               y = PlusMinus,
               fill = Team),
           stat = "identity") +
  geom_text(data = df_adv_boxscore,
            aes(x = reorder(PlayerId, desc(PlusMinus)),
                y = 0,
                hjust = ifelse(PlusMinus >= 0, -0.1, 1.1),
                label = paste0(NameShort,
                               " (",
                               ifelse(PlusMinus > 0, "+", ""),
                               as.character(PlusMinus),
                               ")")),
            size = 4) +
  labs(x = "",
       y = "",
       title = plot.title,
       subtitle = "（）内は各選手の出場時間帯での得失点差") +
  coord_flip() +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position="top"
  )

fileName <- paste0("PlusMinus_",
                   target.key,
                   ".jpg")
ggsave(fileName, width = 6, height = 9)

###############
# Gannt Chart #
###############
getTimelineData <- function(df_pbyp, df_boxscore){
  df <- df_pbyp %>% arrange(DataNo) %>% as.data.frame()

  df_result <- data.frame()

  homeOnCourt <- list()
  awayOnCourt <- list()
  lastPeriod <- 1

  for(rowNum in 1:nrow(df)){
    row <- df[rowNum,]

    lastPeriod <- row$Period

    if(row$ActionCd == "86"){
      player_in <- str_extract(row$PlayerData, "#[0-9]+")
      df_data <- data.frame(
        Team = row$Team,
        Number = player_in,
        Type = "In",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)

      if(row$Team == home.teamName) {
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else if(row$ActionCd == "89"){
      splitted <- str_split(row$PlayerData, " ")[[1]]
      player_out <- str_extract(splitted[1], "#[0-9]+")
      player_in <- str_extract(splitted[4], "#[0-9]+")

      df_data <- data.frame(
        Team = row$Team,
        Number = player_out,
        Type = "Out",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)

      df_data <- data.frame(
        Team = row$Team,
        Number = player_in,
        Type = "In",
        Time = row$PastMinInGame
      )
      df_result <- rbind(df_result, df_data)

      if(row$Team == home.teamName) {
        homeOnCourt <- homeOnCourt[homeOnCourt != player_out]
        homeOnCourt <- append(homeOnCourt, player_in)
        if(length(homeOnCourt) > 5){
          stop("Home team more than 5 players!")
        }
      }else{
        awayOnCourt <- awayOnCourt[awayOnCourt != player_out]
        awayOnCourt <- append(awayOnCourt, player_in)
        if(length(awayOnCourt) > 5){
          stop("Away team more than 5 players!")
        }
      }
    }else{
      # Ignore
    }
  }

  lastMin <- 40
  if(lastPeriod > 4){
    lastMin <- lastMin + ((lastPeriod - 4) * 5)
  }

  # Add last OUT for the players on court
  for(onCourt in homeOnCourt){
    df_data <- data.frame(
      Team = home.teamName,
      Number = onCourt,
      Type = "Out",
      Time = lastMin
    )
    df_result <- rbind(df_result, df_data)
  }

  for(onCourt in awayOnCourt){
    df_data <- data.frame(
      Team = away.teamName,
      Number = onCourt,
      Type = "Out",
      Time = lastMin
    )
    df_result <- rbind(df_result, df_data)
  }

  return(df_result)
}

df_timeline <- getTimelineData(df_pbyp, df_boxscore)
df_timeline %<>%
  arrange(Team, Number, Time) %>%
  group_by(Team, Number, Type) %>%
  mutate(RowNum = row_number())
df_timeline <- merge(df_timeline, df_boxscore, by = c("Team", "Number"))

gp_gannt <- ggplot() +
  geom_text(data = df_boxscore,
            aes(x = 0,
                y = NameShort,
                label = ""),
            hjust = 0)

gp_gannt <- gp_gannt +
  geom_vline(xintercept =  0, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 10, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 20, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 30, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 40, linetype="dashed", color = "grey", size=1)

if(max(df_timeline$Time) > 40) {
  gp_gannt <-
    gp_gannt +
    geom_vline(xintercept = 45, linetype="dashed", color = "grey", size=1)
}

lastIter <- max(df_timeline$RowNum)
for(iter in 1:lastIter){
  gp_gannt <- gp_gannt +
    geom_line(data = subset(df_timeline, RowNum == iter),
              aes(x = Time,
                  y = NameShort,
                  color = Team),
              size = 7)
}
gp_gannt <-
  gp_gannt +
  geom_text(data = df_boxscore,
            aes(x = 0,
                y = NameShort,
                label = NameShort),
            hjust = -0.1,
            size = 4) +
  labs(x="",
       y="",
       title = plot.title,
       subtitle = "各選手の出場時間帯") +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.position="top",
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  facet_wrap(~Team, nrow = 2, scales = "free")
print(gp_gannt)
fileName <- paste0("PlayTime_",
                   target.key,
                   ".jpg")
ggsave(fileName, width = 6, height = 9)
