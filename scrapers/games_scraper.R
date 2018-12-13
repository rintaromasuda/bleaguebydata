devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

for (season in c("2016-17")) {
  for (league in c("B1", "B2")) {
    events <- GetEvents(league)
    for (event_row in seq(1:nrow(events))) {
      event.Id <- events[event_row, ]$EventId
      event.Name <- events[event_row, ]$ShortName
      print(event.Name)
      teams <- subset(b.teams, Season == season & League == league)
      for (team_row in seq(1:nrow(teams))) {
        team.Id <- teams[team_row, ]$TeamId
        team.Name <- teams[team_row, ]$NameShort
        print(team.Name)
      }
    }
  }
}
