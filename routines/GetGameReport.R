target.key <- 4354
home.teamName <- ""
away.teamName <- ""
game.date <- ""
y.adjust <- 5

Sys.setlocale(locale = 'Japanese')

library(bleaguer)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)

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
    for(player in html_nodes(tbl, "tr")){
      playerId <- html_attr(player, "data-player-id")
      cols <- html_nodes(player, "td")
      number <- paste0("#", html_text(cols[1]))
      starterBench <- html_text(cols[2])
      name <- html_text(html_node(cols[3], "a > span.for-pc"))
      nameShort <- html_text(html_node(cols[3], "a > span.for-sp"))
      
      df_player <- data.frame(
        HomeAway = homeAway,
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
            FTM = sum(ActionCd %in% c("7")),
            FTA = sum(ActionCd %in% c("7", "8"))) %>%
  as.data.frame()
df_stats$X <- ifelse(df_stats$Period < 5,
                     (df_stats$Period - 1) * 10 + 5,
                     (df_stats$Period + 3) * 5 + 2.5)
df_stats$Y <- ifelse(df_stats$Period <= 2, max(df_point$Pts) + y.adjust - 10, 15)
df_stats$Y <- ifelse(df_stats$Team != home.teamName, df_stats$Y - 9, df_stats$Y)

####################
# Pts History Plot #
####################
home.longName <- subset(b.teams, Season == b.current.season & NameShort == home.teamName)[c("NameLong")][[1]]
away.longName <- subset(b.teams, Season == b.current.season & NameShort == away.teamName)[c("NameLong")][[1]]
plot.title <- paste0(b.current.season,
                     "シーズン ",
                     home.longName,
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
                label = paste0("FG: ",
                               as.character(FGM),
                               "/",
                               as.character(FGA),
                               "\n",
                               "FT: ",
                               as.character(FTM),
                               "/",
                               as.character(FTA)),
                color = Team)) +  
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
  xlab("経過時間") +
  ylab("得点") +
  labs(title = plot.title,
       subtitle = "▲はチームが取得したタイムアウト") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

fileName <- paste0("PtsHistory_",
                   target.key,
                   ".jpg")
ggsave(fileName, width = 9, height = 6)

##############
# Plus Minus #
##############
df_boxscore <- scrapeBoxScoreData()
