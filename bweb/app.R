#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)
library(jsonlite)
library(bleaguer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    jsonObj <- getJson(6337)
    
    df_pbyp <- getPlayByPlay(jsonObj)
    df_box <- getBoxScore(jsonObj)
    df_timeline <- getTimeline(df_pbyp)

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$myPlot <- renderPlot({
        # Prepares data for a gannt chart
        df_gannt <- df_timeline %>%
            arrange(TeamId, PlayerId, PastMinInGame) %>%
            group_by(TeamId, PlayerId, Type) %>%
            mutate(RowNum = row_number())
        
        df_player <- df_box %>%
            filter(Category == 1 & PeriodCategory == 18) %>% # Player and final boxscore only
            as.data.frame()
        
        gannt <- ggplot()
        
        # Fake geom_point to set up Y-axis in order
        gannt <- gannt +
            geom_point(data = df_player,
                      aes(x = 0,
                          y = PlayerId),
                      alpha = 0)
        
        # Lines between quarters
        for(min in seq(0, 40, by = 10)){
            gannt <- gannt +
                geom_vline(xintercept =  min, linetype="dashed", color = "grey", size=1)
        }
        
        # Lines for OTs
        for(min in c(40, 45, 50)){
            if(max(df_gannt$PastMinInGame) > min) {
                gannt <-
                    gannt +
                    geom_vline(xintercept = min + 5, linetype="dashed", color = "grey", size=1)
            }
        }
        
        # The actual gannt chart part
        for(iter in 1:max(df_gannt$RowNum)){
            gannt <- gannt +
                geom_line(data = subset(df_gannt, RowNum == iter),
                          aes(x = PastMinInGame,
                              y = PlayerId,
                              color = TeamId),
                          size = 7)
        }
        
        # Player names
        gannt <- gannt +
            geom_text(data = df_player,
                      aes(x = 0,
                          y = PlayerId,
                          label = PlayerNameJ),
                      hjust = -0.1,
                      size = 4)
        
        # Cosmetics
        gannt <- gannt +
            labs(x="",
                 y="",
                 title = "",
                 subtitle = "各選手の出場時間帯") +
            theme_bw() +
            theme(
                axis.text.y = element_blank(),
                legend.title = element_blank(),
                legend.position="top",
                strip.background = element_blank(),
                strip.text.x = element_blank()
            ) +
            facet_wrap(~TeamId, nrow = 2, scales = "free")
        
        print(gannt)
    })
}

# Run the application
source("common.R")
shinyApp(ui = ui, server = server)
