#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Run PredictorTasks.R then run this file

library(shiny)
library(shinythemes)

#UI
ui <- fluidPage(
  titlePanel("Win Predictor"),
  selectInput("TeamOne", "Select a Team", choices = OrderedTeamNames$team_name),
  selectInput("TeamTwo", "Select opponent", choices = OrderedTeamNames$team_name),
  actionButton("PredictButton", "Predict game result"),
  #selectInput("MatchID", "Select a match ID", choices = GetMatches(ChronoAllGames, TeamOne, TeamTwo)),
  textOutput("prediction")
)


#server
server <- function(input, output, session) {
  PredictionButton <- eventReactive(input$PredictButton, {
    print("Prediction will be printed here")
  })
  output$prediction <- renderText({
    PredictionButton()
  })
}

#runs app
shinyApp(ui = ui, server = server)

