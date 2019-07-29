Sys.setlocale(locale = 'Japanese')

library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

if (!require(RSelenium)) {
  install.packages("RSelenium")
  library(RSelenium)
}

if (!require(readr)) {
  install.packages("readr")
  library(readr)
}

remDr <- RSelenium::remoteDriver(remoteServerAddr = "bleaguer-selenium1",
                                 port = 4444L,
                                 browserName = "chrome")
remDr$open()

df_result <- data.frame()
scraped.games <- unique(b.games.boxscore$ScheduleKey)
exception.games <- c(4090)
irregular.games <- c()

season <- "2018-19"
df.games <- read_csv("games.csv",
                     cols(
                       ScheduleKey = col_integer(),
                       Season = col_factor(),
                       EventId = col_integer(),
                       Date = col_character(),
                       Arena = col_character(),
                       Attendance = col_integer(),
                       HomeTeamId = col_integer(),
                       AwayTeamId = col_integer()),
                     col_names = TRUE,
                     locale = readr::locale(encoding = "UTF-8"))

for (idx in seq(1:nrow(df.games))) {
  
  df_game <- data.frame()

  key <- df.games[idx,]$ScheduleKey
  if (key %in% df_result$ScheduleKey | key %in% exception.games) {
    print(paste("Already done. Skipping->", key))
    next
  }

  homeTeamId <- df.games[idx,]$HomeTeamId
  awayTeamId <- df.games[idx,]$AwayTeamId

  url.detail <- paste("https://www.bleague.jp/game_detail/?ScheduleKey=",
                      as.character(key),
                      "&TAB=P",
                      sep = ""
                      )
  print(url.detail)

  try.count <- 1
  try.success <- FALSE
  try.threshold <- 60
  # Post-season Game 3s in 2016-17 and 2017-18 have less tables
  #expected.table.count <- ifelse(eventId %in% c(300,400,800), 9, 13)
  while (try.count <= try.threshold) {
    remDr$navigate(url.detail)
    pageSource <- remDr$getPageSource()
    html.pbyp <- read_html(pageSource[[1]])
    #tables.boxscore <- html_table(html.boxscore)
    # Check the page and leave if it's good
    #if ((length(tables.boxscore) >= expected.table.count) &&
    #    (nrow(tables.boxscore[[4]]) > 0) &&
    #    (nrow(tables.boxscore[[5]]) > 0)) {
      try.success <- TRUE
      break
    #} else {
    #  Sys.sleep(0.5)
    #  print(paste("Retry the page load...(", try.count, ")", sep = ""))
    #  try.count <- try.count + 1
    #}
  }

  if (!try.success) {
    print(paste("Insufficient tables->", key))
    irregular.games <- append(irregular.games, key)
    next
  }

  nodes.period <- html.pbyp %>%
    html_nodes("#game__playbyplay__inner > ul.playbyplay_contents.playbyplay_contents_text > li > ul")
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
       
      df_action <- data.frame(
        ScheduleKey = key,
        HomeAway = home_away,
        Period = period_num,
        DataNo = data_no,
        ActionCd = action_cd,
        Clock = clock,
        PlayerData = player_data,
        ImageUrl = image_url
      )
      
      df_game <- rbind(df_game, df_action)
      
    }
    
  }
  
  df_game$TeamId <- ifelse(grepl("home", df_game$HomeAway), homeTeamId, ifelse(grepl("away", df_game$HomeAway), awayTeamId, NA))
  
  df_result <- rbind(df_result, df_game)
}

library(dplyr)

df.target <- GetGameSummary()
df.target <- subset(df.target, Season == "2018-19" & Category == "Regular" & League == "B2")

df.play <- df_result %>%
  group_by(ScheduleKey, TeamId, ActionCd) %>%
  summarise(N = n())

df <- merge(df.target, df.play, Key = c("ScheduleKey, TeamId"))

df %<>%
  group_by(TeamId, ActionCd) %>%
  summarize(N = sum(N))

df$Type <- ifelse(df$ActionCd == "1" | df$ActionCd == "2", "スリー",
                  ifelse(df$ActionCd == "3" | df$ActionCd == "5", "ペリメーター",
                  ifelse(df$ActionCd == "4" | df$ActionCd == "6", "ペイント", NA)))

df %<>%
  filter(!is.na(Type)) %>%
  group_by(TeamId) %>%
  summarize(F3GM = sum(N[ActionCd == "1"]),
            F3GMNot = sum(N[ActionCd == "2"]),
            FPeliM = sum(N[ActionCd == "3"]),
            FPeliMNot = sum(N[ActionCd == "5"]),
            FPaintM = sum(N[ActionCd == "4"]),
            FPaintMNot = sum(N[ActionCd == "6"]))

df$F3GR <- df$F3GM / (df$F3GM + df$F3GMNot)
df$FPeliR <- df$FPeliM / (df$FPeliM + df$FPeliMNot)
df$FPaintR <- df$FPaintM / (df$FPaintM + df$FPaintMNot)

df <- merge(df, subset(b.teams, Season == "2018-19"), by = "TeamId")

df.target <-
  rbind(
    data.frame(
      TeamName = df$NameShort,
      SuccessRate = df$FPaintR,
      Type = "ペイント"
    ),
    data.frame(
      TeamName = df$NameShort,
      SuccessRate = df$FPeliR,
      Type = "ペリメーター"
    ),
    data.frame(
      TeamName = df$NameShort,
      SuccessRate = df$F3GR,
      Type = "スリー"
    )
  )

library(ggplot2)

ggplot() +
  geom_bar(data = df.target,
           aes(x = TeamName,
               y = SuccessRate,
               fill = Type),
           position = "dodge",
           stat = "identity") +
  ggtitle("各エリア別のシュート成功率（2018-19 レギュラーシーズン）") +
  xlab("") +
  ylab("シュート成功率") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.title = element_blank()
  )

ggsave("B2_FGRPerArea.jpeg", width = 8, height = 5)
