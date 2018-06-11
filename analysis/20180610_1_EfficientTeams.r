setwd('C:/git/bleaguebydata/analysis')

clubs.2nd.b1 <- read.csv("201718_b1_clubs.csv",
                         header = TRUE,
                         sep = ",",
                         quote = "",
                         stringsAsFactors = FALSE
)

clubs.2nd.b1$POS <- round(clubs.2nd.b1$FGA - clubs.2nd.b1$OR + clubs.2nd.b1$TO + (clubs.2nd.b1$FTA * 0.44), 0)
clubs.2nd.b1$PPP <- round(clubs.2nd.b1$PTS / clubs.2nd.b1$POS, 2)
clubs.2nd.b1$PPP100 <- clubs.2nd.b1$PPP * 100

clubs.2nd.b1[order(clubs.2nd.b1$PPP, decreasing = TRUE),] %>%
  select(c("TEAM", "PTS", "POS", "PPP", "PPP100")) %>%
  kable()

clubs.2nd.b1$WNR <- c(0.8, 0.767, 0.683, .467, .433, .733, .517, .567, .4, .3, .7, .567, .4, .4, .417, .2, .467, .183)

plot(clubs.2nd.b1$PPP100, clubs.2nd.b1$WNR)

View(clubs.2nd.b1[,c("TEAM", "PPP100", "WNR")])

clubs.2nd.b1[order(clubs.2nd.b1$PPP, decreasing = TRUE),] %>%
  select(c("TEAM", "FGA", "OR", "TO", "FTA")) %>%
  kable()
