#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(readxl)
library(dplyr)
library(csvread)
library(cowplot)
library(e1071)
library(modeldata)
library(xgboost)
library(caret)
library(mltools)
library(data.table)
library(caret)
library(tictoc)
library(rsconnect)

AllGames <- read_excel("AllGames.xls")
SortedAllGames <- arrange(AllGames, date)
ChronoAllGames <- filter(SortedAllGames, winner != 0)
TeamNames <- read.csv("TeamNames.csv")


get_string <- function() {
  s <- "c(8,9,10)"
  return(s)
}

my_new_title <- "Global Title"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    textOutput("MyTitle"),

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
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    my_string <- get_string()
    
    output$MyTitle <- renderText({
      my_new_title
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
