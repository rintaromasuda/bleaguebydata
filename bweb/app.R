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
    View(df_pbyp)
    df_timeline <- getTimeline(df_pbyp, df_box)
    
    View(df_timeline)
    
    df <- data.frame(
        A = seq(1, 1000)
    )

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$myPlot <- renderPlot({
        
        ggplot() +
            geom_boxplot(data = df,
                         aes(y = A))
    })
}

# Run the application
source("common.R")
shinyApp(ui = ui, server = server)
