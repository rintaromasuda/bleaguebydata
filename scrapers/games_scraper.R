devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
}

for (season in c("2016-17")) {
  for (league in c("B1")) {
    teams <- subset(b.teams, Season == season & League == league)
    for (row in seq(1:nrow(teams))) {
      team.Id <- teams[row, ]$TeamId
      team.Name <- teams[row, ]$NameShort
      print(team.Name)
    }
  }
}
