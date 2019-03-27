devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

createImages <- function(season, league) {
  # First get the number of games of the season/league
  df.games <-
    GetGameSummary() %>%
      filter(Season == season & League == league & Category == "Regular") %>%
      group_by(TeamId) %>%
      summarise(NumGames = n_distinct(ScheduleKey)) %>%
      as.data.frame()
  gameUntil <- min(df.games$NumGames)

  # Function to plot the standing
  plotStanding <- function(df.target, game.index) {
    plotTitle <- paste0(season,
                        "シーズン ",
                        ifelse(game.index > gameUntil, gameUntil, game.index),
                        "ゲーム終了時点")

    if (league == "B1") {
      df.target$PostSeason <- ifelse(df.target$WildcardRank <= 2 | df.target$DivisionRank <= 2, "CS",
                                     ifelse(df.target$Rank >= 15, "残留PF", "PSなし"))
      df.target$PostSeasonF <- factor(df.target$PostSeason,
                                      level = c("CS", "PSなし", "残留PF"))

    } else {
      df.target$PostSeason <- ifelse(df.target$WildcardRank == 1 | df.target$DivisionRank <= 1, "B2PF",
                                     ifelse(df.target$Rank == 18, "入替戦", "PSなし"))
      df.target$PostSeasonF <- factor(df.target$PostSeason,
                                      level = c("B2PF", "PSなし", "入替戦"))
    }


    # Plot
    ggplot() +
      geom_label(data = df.target,
                 aes(x = TeamDivision,
                     y = Win,
                     label = paste0(TeamName, " (", Rank, ")"),
                     fill = PostSeasonF),
                 size = 5,
                 alpha = 0.9) +
      xlab("") +
      ylab("勝ち数") +
      ggtitle(plotTitle) +
      scale_fill_manual(values=c("#33CC66", "#EEEEEE", "#DD1122")) +
      ylim(c(0, 60)) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15),

        panel.grid.major.x = element_blank(),

        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),

        legend.title = element_blank()
      )

    filePath <- paste0("standing_", season, "_", sprintf("%02d", game.index), ".jpeg")
    ggsave(filePath, width = 7, height = 8, dpi = 150)
  }

  # Loop the image creation over games
  lastIndex <- gameUntil + 10 # Since we want to show the last picture longer, we add 10 extra pictures with same image
  for(game.index in 1:lastIndex) {
    df.target <- bleaguer::GetStanding(season, league, atEndOfGame = game.index, needRank = TRUE)
    plotStanding(df.target, game.index)
  }
}

createImages("2018-19", "B1")
system("magick -delay 20 -loop 0 standing_*.jpeg standings.gif")
