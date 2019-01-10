devtools::install_github("rintaromasuda/bleaguer", force = TRUE)

library(bleaguer)

if (!require(stringr)) {
  install.packages(stringr)
  library(stringr)
}

if (!require(dplyr)) {
  install.packages(dplyr)
  library(dplyr)
}

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

df.result <- data.frame()

season <- "2018-19"
leagues <- c("B1", "B2")
scheduleKeys <- subset(b.games, Season == season)$ScheduleKey

b1.events <- c(2)
b2.events <- c(7)

for (league in leagues) {
  # Target relevant events only
  if (league == "B1") {
    events <- subset(b.events, EventId %in% b1.events)
  } else {
    events <- subset(b.events, EventId %in% b1.events)
  }
  # Retrieve teams
  teams <- subset(b.teams, Season == season & League == league)  
  
  for (event_row in seq(1:nrow(events))) {
    # Iterate each event
    event.Id <- events[event_row, ]$EventId
    event.Name <- events[event_row, ]$ShortName
    event.Category <- events[event_row, ]$Category
    
    for (team_row in seq(1:nrow(teams))) {
      # Iterate each team
      team.Id <- teams[team_row, ]$TeamId
      team.Name <- teams[team_row, ]$NameShort
      url.team <- paste("https://www.bleague.jp/schedule/?",
                  "tab=",
                  gsub("B", "", league),
                  "&year=",
                  substr(season, 0, 4),
                  "&event=",
                  as.character(event.Id),
                  "&club=",
                  as.character(team.Id),
                   sep = "")
      print(url.team)
      html.team <- read_html(url.team, encoding = "utf-8")
      Sys.sleep(0.5)
      urls.game <- html.team %>%
        html_nodes("#round_list > dd > ul > li > div.gamedata_left > div.data_link > div.state_link.btn.report > a") %>%
        html_attr("href")
      num.games = length(urls.game)
      print(num.games)
      for (url.game in urls.game) {
        startStr <- "ScheduleKey="
        key <- substring(url.game,
                         regexpr(startStr, url.game) + nchar(startStr))
        if (!(key %in% scheduleKeys)) {
          scheduleKeys <- append(scheduleKeys, key)
          html.game <- read_html(url.game, encoding = "utf-8")

          ########
          # Parsing all the necessary information
          ########
          date <- html.game %>%
            html_nodes("#game__top__inner > div.date_wrap > p:nth-child(2) > span") %>%
            html_text()

          arena <- html.game %>%
            html_nodes("#game__top__inner > div.place_wrap > p.StadiumNameJ") %>%
            html_text()

          attendance <- html.game %>%
            html_nodes("#game__top__inner > div.place_wrap > p.Attendance") %>%
            html_text()

          home <- html.game %>%
            html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home.win > div.team_name > p.for-sp") %>%
            html_text()
          if (identical(home, character(0))) {
            home <- html.game %>%
              html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.home > div.team_name > p.for-sp") %>%
              html_text()
          }

          away <- html.game %>%
            html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away.win > div.team_name > p.for-sp") %>%
            html_text()
          if (identical(away, character(0))) {
            away <- html.game %>%
              html_nodes("#game__top__inner > div.result_wrap > div.team_wrap.away > div.team_name > p.for-sp") %>%
              html_text()
          }

          ########
          # Create the result
          ########
          str <- paste(key,
                       season,
                       league,
                       event.Category,
                       event.Name,
                       date,
                       arena,
                       attendance,
                       home,
                       away)
          print(str)

          df.record <- data.frame(
            ScheduleKey = key,
            Season = season,
            League = league,
            EventId = event.Id,
            Date = date,
            Arena = arena,
            Attendance = attendance,
            HomeTeam = home,
            AwayTeam = away
          )

          df.result <- rbind(df.result, df.record)
        }
      }
    }
  }
}

########
# Data cleaning
########
df <- df.result

df$Arena <- gsub("会場：", "", df$Arena)
df$Attendance <- as.integer(gsub("人", "", gsub("人数：", "", df$Attendance)))

df$Date <- bleaguer::GetFullDateString(df$Date, df$Season)

df.merged <- df
teams <- b.teams[, c("TeamId", "Season", "NameShort")]

df.merged <- merge(df.merged, teams, by.x = c("Season","HomeTeam"),by.y = c("Season","NameShort"))
names(df.merged)[names(df.merged) == 'TeamId'] <- 'HomeTeamId'
df.merged <- merge(df.merged, teams, by.x = c("Season","AwayTeam"),by.y = c("Season","NameShort"))
names(df.merged)[names(df.merged) == 'TeamId'] <- 'AwayTeamId'

df.output <- df.merged[, c("ScheduleKey",
                           "Season",
                           "EventId",
                           "Date",
                           "Arena",
                           "Attendance",
                           "HomeTeamId",
                           "AwayTeamId")]

write.csv(df.output, "games.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = FALSE)

df.output %>%
  group_by(EventId, AwayTeamId) %>%
  summarise(N = n()) %>%
  View()

