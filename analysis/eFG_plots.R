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

# Data Preparation
df <- GetGameSummary()

# Visualize
plotEFG <- function(league = "B1") {
  df.target <- df %>%
    filter(Category == "Regular" & League == league)
  
  plotTitle <- paste0("直近３レギュラーシーズンのeFG%（", league, "）")

  ggplot() +
    geom_boxplot(data = df.target,
                 aes(x = TeamName,
                     y = EFG,
                     fill = Season)) +
    xlab("") +
    ylab("eFG%") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(plotTitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.title = element_blank()
    )
}

plotEFG(league = "B1")
ggsave("EFG_B1.jpeg", width = 8, height = 5)

plotEFG(league = "B2")
ggsave("EFG_B2.jpeg", width = 8, height = 5)

plotEFGAll <- function() {
  df.target <- df %>%
    filter(Category == "Regular")
  
  plotTitle <- paste0("直近３レギュラーシーズンのeFG%（B1 & B2）")
  
  ggplot() +
    geom_boxplot(data = df.target,
                 aes(x = Season,
                     y = EFG,
                     fill = Season)) +
    xlab("") +
    ylab("eFG%") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(plotTitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.title = element_blank()
    )
}

plotEFGAll()
ggsave("EFG_All.jpeg", width = 8, height = 5)
