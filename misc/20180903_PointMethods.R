df <- read.csv("PlayerTotalStats_201819_20181125.csv")

df$TEAM <- as.factor(df$TEAM)
df$ID <- as.numeric(df$ID)
df$G <- as.numeric(df$G)
df$PPG <- as.numeric(df$PPG)
df$APG <- as.numeric(df$APG)
df$RPG <- as.numeric(df$RPG)

# MIN
df$MIN_RAW <- df$MIN
df$MINPG_RAW <- df$MINPG

minStrToMinDec <- function(item) {
  min <- as.numeric(item[1])
  min <- min + as.numeric(item[2]) / 60
  round(min, 2)
}

library(stringr)
df$MIN <- ifelse(is.na(df$MIN_RAW), 0,
          ifelse(df$MIN_RAW == "", 0,
          sapply(str_split(df$MIN_RAW, ":"), minStrToMinDec)))
df$MINPG <- ifelse(is.na(df$MINPG_RAW), 0,
                 ifelse(df$MINPG_RAW == "", 0,
                        sapply(str_split(df$MINPG_RAW, ":"), minStrToMinDec)))

df$PTSBYFT <- as.numeric(df$FTM)
df$PTSBY3P <- as.numeric(df$'X3FGM') * 3
df$PTSBY2P <- (as.numeric(df$FGM) - as.numeric(df$'X3FGM')) * 2
df$PTSCHK <- df$PTSBY2P + df$PTSBY3P + df$PTSBYFT

df_b1 <- df[df$LEAGUE == 1 & df$YEAR == 2018,]

dd <- df_b1[, c("TEAM", "PLAYER", "PTS", "PTSBYFT", "PTSBY2P", "PTSBY3P", "PTSCHK")]
dd$PLAYER <- paste(dd$PLAYER," (",as.character(dd$PTS),")", sep = "")
dd <- dd[order(dd$PTSCHK, decreasing = TRUE), c("TEAM", "PLAYER", "PTSBY2P", "PTSBY3P", "PTSBYFT", "PTS")]
dd$RANK <- 1:nrow(dd)
View(dd)

dd2 <- rbind(data.frame(TEAM = dd$TEAM, PLAYER = dd$PLAYER, RANK = dd$RANK, TYPE = "2P", PTS = dd$PTSBY2P, TOTAL = as.numeric(dd$PTS)),
      data.frame(TEAM = dd$TEAM, PLAYER = dd$PLAYER, RANK = dd$RANK, TYPE = "3P", PTS = dd$PTSBY3P, TOTAL = as.numeric(dd$PTS)),
      data.frame(TEAM = dd$TEAM, PLAYER = dd$PLAYER, RANK = dd$RANK, TYPE = "FT", PTS = dd$PTSBYFT, TOTAL = as.numeric(dd$PTS)))

dd2 <- dd2[order(dd2$RANK),]
View(dd2)

require(ggplot2)

zipping <- function(x,y,z) {
  result <- c()
  l <- length(x)
  for(i in 1:l) {
    result <- c(result, x[i], y[i], z[i])
  }
  return(result)
}

foo <- function(rank) {
  len <- length(rank)
  ggplot(subset(dd2, RANK %in% rank),
         aes(x = factor(PLAYER, levels = rev(unique(PLAYER))),
             y = PTS,
             fill = factor(TYPE, levels = c("FT", "3P", "2P")))) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(x = factor(PLAYER, levels = rev(unique(PLAYER))),
                  y = zipping(rep(0.05, len),
                              ((PTS / TOTAL)[c(TRUE,FALSE,FALSE)] + (PTS / 2 / TOTAL)[c(FALSE,TRUE,FALSE)]),
                              rep(0.95, len)),
                  label = paste(as.character(round((PTS / TOTAL), 3) * 100), "%", sep = "")),
              size = 3.2) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    ylab("") +
    xlab("") +
    labs(fill='') +
    ggtitle("各選手の得点方法（2018-19シーズン 第11節終了時点）") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  #filename <- paste("rank_from",
#                    as.character(rank_start),
#                    "_to",
#                    as.character(rank_end),
#                    ".jpeg",
#                    sep = "")
  #ggsave(filename, width = 8, height = 5)
}

foo(c(23,48,49))
ggsave("point_method.jpeg", width = 8, height = 5)
