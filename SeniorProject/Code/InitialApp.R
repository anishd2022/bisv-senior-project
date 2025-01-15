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

#sourcing files
# source("SuggesterTasks.R")
# source("PredictorTasks.R")


#UI
ui <- fluidPage(
  #Asks the user which predictive algorithm to use:
  #selectInput("Model", "Select a predictive algorithm", choices = c("Support Vector Machine", "Logistic Regression")),  
  
  #sets the theme
  theme = shinytheme("cerulean"),
  
  #Title of "Win Predictor and Suggester"
  titlePanel("Win Predictor and Suggester"),
  
  sidebarLayout(
    sidebarPanel(
      #header that says "parameter tuning":
      h4("Parameter tuning"),
      
      #asks user to select a classification type:
      selectInput("type", "Select a classification type", choices = c("C-classification", "nu-classification")),
      
      #asks user to select a kernel:
      selectInput("kernel", "Select a kernel", choices = c("linear", "polynomial", "sigmoid")),
      
      #asks user to select a cost:
      sliderInput("cost", label = "Select a cost", min = 1, max = 100, value = 1),
      
      #outputs the model accuracy
      textOutput("Accuracy"),
      
      #header that introduces game selection widgets
      h4("Game Selection"),
      
      #asks user to input a date range
      dateRangeInput(
        inputId = "daterange",
        label = "Select the date range for your games",
        start = min(ChronoAllGames$date),
        end = max(ChronoAllGames$date),
        min = min(ChronoAllGames$date),
        max = max(ChronoAllGames$date),
        format = "yyyy/mm/dd",
        separator = "to"
      ),
      #user selects a fixture
      selectInput("Fixture", "Select a fixture", choices = NULL),
    ),
    mainPanel(
      #Header of predictor:
      h3("Predictor"),
      #Outputs the prediction
      span(verbatimTextOutput("Prediction"), style = "color:darkblue"),
      
      #header of "Suggester"
      h3("Suggester"), 
      
      #Outputs the suggestion
      span(verbatimTextOutput("Suggestion"), style = "color:darkblue"),
    ),
  ),
  
  #Title that introduces game creation
  titlePanel("Create your own game"),
  
  fluidRow(
    column(6,
           #Asks user to select first team
           selectInput("TeamOne", "Select a team", choices = c(sort(TeamNames$team_name))),
           
           #asks user to select playing 11 for team one
           selectizeInput("TeamOnePlayers", "Select your playing 11", choices = NULL, multiple = TRUE, options = list(maxItems = 11)),
    ),
    column(6,
           #Asks user to select second team
           selectInput("TeamTwo", "Select an opponent", choices = c(sort(TeamNames$team_name))),
           
           #asks user to select playing 11 for team two
           selectizeInput("TeamTwoPlayers", "Select opponent's playing 11", choices = NULL, multiple = TRUE, options = list(maxItems = 11)),
    ),
  ),
  
  #asks the user to select the ground:
  selectInput("Ground", "Select a game location", choices = sort(unique(ChronoAllGames$name))),
  
  #button that calculates winner once clicked
  actionButton("btn", "Predict Winner"),
  
  #outputs the winner of the create game
  verbatimTextOutput("CreateGamePrediction"),
  
  #button that suggests both teams once clicked
  br(),
  actionButton("SuggestButton", "Suggest for both Teams"),
  
  #outputs the suggestions for the create game
  verbatimTextOutput("CreateGameSuggestion"),
  br()
)





#server
server <- function(input, output, session) {
  #updating the Fixure selections to only show fixtures within the date range
  observe({
    AppropriateFixtures <- FixtureList[min(which(ChronoAllGames$date >= input$daterange[1])):max(which(ChronoAllGames$date <= input$daterange[2]))]      
    updateSelectInput(session, "Fixture", "Select a fixture", choices = unique(AppropriateFixtures))
  })
  
  #outputs the accuracy of the predictor based on inputted parameter values
  observe({
    Model <- GetSVMModel(logisticstraindata, input$type, input$kernel, input$cost)
    ModelAccuracy <- GetSVMTestAccuracy(Model, logisticstestdata)
    output$Accuracy <- renderText({
      paste("The prediction accuracy of this model is", round(100*ModelAccuracy, digits = 4), "%")
    })
    
    #gives the text output of the prediction
    output$Prediction <- renderPrint({
      PredictGame (ChronoAllGames$match_id[which(FixtureList == input$Fixture)], ChronoAllGames, logisticsdata, Model, TeamNames)
    })
  })
  
  #gives the text output of the suggestion
  output$Suggestion <- renderPrint({
    SuggestGame (ChronoAllGames$match_id[which(FixtureList == input$Fixture)], ChronoAllGames, SuggesterLogisticsData, SuggesterSVMModel, TeamNames)
  })
  
  #gives the playing 11 of team one and team two once the team has been selected
  observe({
    TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamOne)]
    TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamTwo)]
    TeamOneRegisteredPlayerNameandID <- filter(PlayerFullNames, team_id %in% TeamOneID)
    TeamOneRegisteredPlayers <- subset(TeamOneRegisteredPlayerNameandID, select = c(Full_Name))
    updateSelectInput(session, "TeamOnePlayers", "Select your playing 11", choices = TeamOneRegisteredPlayers)
  })
  observe({
    TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamTwo)]
    TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamOne)]
    TeamTwoRegisteredPlayerNameandID <- filter(PlayerFullNames, team_id %in% TeamTwoID)
    TeamTwoRegisteredPlayers <- subset(TeamTwoRegisteredPlayerNameandID, select = c(Full_Name))
    updateSelectInput(session, "TeamTwoPlayers", "Select opponent's playing 11", choices = TeamTwoRegisteredPlayers)
  })
  
  #code that reacts to button press and gives the create game prediction
  GamePrediction <- eventReactive(input$btn, {
    TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamOne)]
    TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamTwo)]
    FeatureRow <- GetFeatureRowforCreateGame(input$TeamOnePlayers, input$TeamTwoPlayers,
                                             PlayerStats, ChronoAllGames, AllGames,
                                             GameStats, TeamOneID, TeamTwoID, logisticsdata)
    GetCreateGamePrediction(SVMModel, TeamOneID, TeamTwoID, FeatureRow, TeamNames)
  })
  
  output$CreateGamePrediction <- renderPrint({
    paste(GamePrediction(), "are predicted to win this game")
  })
  
  #code that reacts to button press and gives the suggestions for both teams
  GameSuggestion <- eventReactive(input$SuggestButton, {
    GROUNDID <- ChronoAllGames$ground_id[min(which(ChronoAllGames$name == input$Ground))]
    TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamOne)]
    TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamTwo)]
    GetCreateGameSuggestion(ChronoAllGames, SuggesterSVMModel, GROUNDID, TeamNames, TeamOneID, TeamTwoID)
  })
  
  output$CreateGameSuggestion <- renderPrint({
    GameSuggestion()
  })
  
}













print("Shiny app started")
#runs app
shinyApp(ui = ui, server = server)








