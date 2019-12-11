target.key <- 4306
home.team <- "富山"
away.team <- "千葉"

Sys.setlocale(locale = 'Japanese')

library(bleaguer)
library(dplyr)
library(ggplot2)
library(ggrepel)

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







df_pbyp$ImageUrl <- as.character(df_pbyp$ImageUrl)
df_pbyp$PlayerId <- as.integer(sapply(strsplit(sapply(strsplit(df_pbyp$ImageUrl, "/"), function(x) {x[7]}), "_"), function(x) {x[1]}))
df_pbyp$RemainingMinPeriod <- bleaguer::ConvertMinStrToDec(df_pbyp$Clock)
df_pbyp$PastMinInPeriod <- ifelse(df_pbyp$Period <= 4,
                                  (10 - df_pbyp$RemainingMinPeriod),
                                  (5 - df_pbyp$RemainingMinPeriod))
df_pbyp$PastMinInGame <- ifelse(df_pbyp$Period <= 4,
                                (((df_pbyp$Period - 1) * 10) + df_pbyp$PastMinInPeriod),
                                (40 + ((df_pbyp$Period - 5) * 5) + df_pbyp$PastMinInPeriod))
df_pbyp$PastMinInGameClass <- ceiling(df_pbyp$PastMinInGame)


df_pbyp %<>%
  arrange(Period, DataNo) %>%
  mutate(PtsAdded = ifelse(ActionCd == "1", 3,
                           ifelse(ActionCd == "3" | ActionCd == "4", 2,
                                  ifelse(ActionCd == "7", 1, 0)))) %>%
  mutate(HomePtsAdded = ifelse(grepl("home", HomeAway, ignore.case = TRUE), PtsAdded, 0),
         AwayPtsAdded = ifelse(grepl("away", HomeAway, ignore.case = TRUE), PtsAdded, 0))

df_pbyp %<>%
  arrange(Period, DataNo) %>%
  mutate(HomeAccPts = cumsum(HomePtsAdded),
         AwayAccPts = cumsum(AwayPtsAdded))

df_pbyp$PtsDiff = df_pbyp$HomeAccPts - df_pbyp$AwayAccPts

#####
df_timeout <-
  df_pbyp %>%
  filter(!is.na(PastMinInGameClass)) %>%
  filter(ActionCd == "88") %>%
  as.data.frame()

df_timeout$Team <- ifelse(grepl("home", df_timeout$HomeAway, ignore.case = TRUE), home.team, away.team)

dd <-
  df_pbyp %>%
  filter(!is.na(PastMinInGameClass)) %>%
  arrange(DataNo) %>%
  group_by(PastMinInGameClass) %>%
  summarize(PtsHome = last(HomeAccPts),
            PtsAway = last(AwayAccPts))

dd_text <-
  rbind(
    data.frame(
      Label = "Q1",
      X = 1
    ),
    data.frame(
      Label = "Q2",
      X = 11
    ),
    data.frame(
      Label = "Q3",
      X = 21
    ),
    data.frame(
      Label = "Q4",
      X = 31
    ),
    data.frame(
      Label = "OT1",
      X = 41
    ),
    data.frame(
      Label = "OT2",
      X = 46
    ),
    data.frame(
      Label = "OT3",
      X = 51
    ),
    data.frame(
      Label = "OT4",
      X = 56
    )
  )

dd2 <-
  rbind(
    data.frame(
      Team = home.team,
      Min = dd$PastMinInGameClass,
      Pts = dd$PtsHome,
      Diff = dd$PtsHome - dd$PtsAway
    ),
    data.frame(
      Team = away.team,
      Min = dd$PastMinInGameClass,
      Pts = dd$PtsAway,
      Diff = dd$PtsAway - dd$PtsHome
    )
  )
dd2$Direction = ifelse(dd2$Diff > 0, 1, -1)

ggplot() +
  geom_vline(xintercept =  0, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 10, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 20, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 30, linetype="dashed", color = "grey", size=1) +
  geom_vline(xintercept = 40, linetype="dashed", color = "grey", size=1) +
  geom_text(data = subset(dd_text, X < max(dd2$Min)),
            aes(x = X,
                y = max(dd2$Pts),
                label = as.character(Label)),
            color = "grey") +
  geom_line(data = dd2,
            aes(x = Min,
                y = Pts,
                color = Team),
            size = 1) +
  geom_point(data = dd2,
            aes(x = Min,
                y = Pts,
                color = Team),
            shape = 15,
            size = 3) +
  geom_label(data = subset(dd2, Min %in% c(10, 20, 30, 40, 45, 50, 55, 60, 65)),
                   show.legend = FALSE,
                   aes(x = Min,
                       y = Pts + ifelse(Diff != 0, Direction * 5, 
                                        ifelse(Team == home.team, 5, -5)),
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
  labs(title = "富山グラウジーズ vs 千葉ジェッツ (12/08)",
       subtitle = "▲はチームが取得したタイムアウト") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  )

ggsave("PtsHistory_4306.jpg", width = 9, height = 6)


