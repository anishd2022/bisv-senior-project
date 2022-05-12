

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
  #textOutput("CreateGamePrediction")
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
  
  #doing calculations to eventually output the game prediction for the created game
  # observe({
  #   TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamOne)]
  #   TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == input$TeamTwo)]
  #   RelativeTeamStrength <- GetRelativeTeamStrengthForCreateGame(input$TeamOnePlayers, input$TeamTwoPlayers, PlayerStats)
  #   FeatureRow <- GetFeatureRowforCreateGame(input$TeamOnePlayers, input$TeamTwoPlayers,
  #                                            PlayerStats, ChronoAllGames, AllGames,
  #                                            GameStats, TeamOneID, TeamTwoID)
  #   calculate <- eventReactive(input$btn{
  #     GetCreateGamePrediction(SVMModel, TeamOneID, TeamTwoID, FeatureRow, TeamNames)
  #   })
  #   output$CreateGamePrediction <- renderText({
  #     calculate()
  #   })
  # })
}








#runs app
shinyApp(ui = ui, server = server)











####Extra Notes / code:
#
#
#
#
#

