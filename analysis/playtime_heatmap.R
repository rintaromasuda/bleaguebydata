devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

season <- "2018-19"
league <- "B1"
divisions <- c("東地区", "中地区", "西地区")

for (division in divisions) {
  df <- GetGameSummary()
  df <- subset(df, Season == season)
  df <- merge(df, b.games.boxscore, by = c("ScheduleKey", "TeamId"))
  df <- merge(df, b.teams[, c("Season", "TeamId", "NameLong")], by = c("Season", "TeamId"))
  df$MIN <- ifelse(df$MIN > 40, 40, df$MIN)
    
  ggplot() +
    geom_tile(data = subset(df, League == league & TeamDivision == division),
              aes(x = Game.Index,
                  y = reorder(Player, as.integer(Position)),
                  fill = MIN)) +
    ylab("") +
    xlab("") +
    scale_fill_continuous(high = "blue", low = "white", guide = FALSE) +
    scale_x_continuous(breaks = seq(5, 31, by = 5)) +
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(hjust = 1, size = 8),
          axis.text.x = element_text(size = 8),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "white")
    ) +
    facet_wrap(~NameLong, ncol = 3, scales = "free")
  
  filename <- paste(paste(season, league, division, sep = "_"), ".jpeg", sep = "")
  ggsave(filename, width = 8, height = 5)
}
