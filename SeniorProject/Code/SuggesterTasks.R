
#1. We want to find the number of times the winning team has won batting first and bowling first at all games at a given ground (done) (in percentage format) 
#2. Out of all the games that team A won, what percentage of games were won batting first
#3. Between Team A and Team B, out of all the games that team A won, what percentage of those wins were batting first?
#4. Between Team A and Team B on Ground X, out of all the games that team A won, what percentage of those wins were batting first? 

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
library(shiny)
library(shinythemes)
library(caret)
library(tictoc)
library(rsconnect)

###################Start suggester functions
#Task 1 for Suggester
Task1 <- function (AllGames, GROUND) {
  AllGamesforGroundX <- filter(AllGames, ground_id == GROUND)
  gameswonbattingfirst <- 0
  gameswonbowlingfirst <- 0
  for (game in 1:nrow(AllGamesforGroundX)) {
    if (AllGamesforGroundX$batting_first[game] == AllGamesforGroundX$winner[game]) {
      gameswonbattingfirst <- gameswonbattingfirst + 1
    } else {
      gameswonbowlingfirst <- gameswonbowlingfirst + 1
    }
  }
  One_BattingFirstWinProb <- gameswonbattingfirst / nrow(AllGamesforGroundX)
  return(One_BattingFirstWinProb)
}



#Task 2 for suggester
Task2 <- function (AllGames, TeamONE) {
  AllGamesforTeam1 <- filter(AllGames, winner == TeamONE)
  if (nrow(AllGamesforTeam1) == 0) {
    Two_BattingFirstWinProbTeamOne <- 0.5
  } else {
    gameswonbattingfirst <- 0
    gameswonbowlingfirst <- 0
    for (game in 1:nrow(AllGamesforTeam1)) {
      if (AllGamesforTeam1$batting_first[game] == AllGamesforTeam1$winner[game]) {
        gameswonbattingfirst <- gameswonbattingfirst + 1
      } else {
        gameswonbowlingfirst <- gameswonbowlingfirst + 1
      }
    }
    Two_BattingFirstWinProbTeamOne <- gameswonbattingfirst / nrow(AllGamesforTeam1)
  }
  return(Two_BattingFirstWinProbTeamOne)
}



#Task 3 for suggester
Task3 <- function (AllGames, TeamONE, TeamTWO) {
  AllGamesBetweenTeamATeamB <- filter(AllGames, (team_one == TeamONE | team_one == TeamTWO) & (team_two == TeamONE | team_two == TeamTWO)) 
  TeamAWinsAgainstB <- filter(AllGamesBetweenTeamATeamB, winner == TeamONE)
  TeamBWinsAgainstA <- filter(AllGamesBetweenTeamATeamB, winner == TeamTWO)
  if (nrow(TeamAWinsAgainstB) == 0) {
    Three_TeamAWinBattingFirstProb <- 0.5
  } else {
    gameswonbattingfirst <- 0
    for (game in 1:nrow(TeamAWinsAgainstB)) {
      if (TeamAWinsAgainstB$batting_first[game] == TeamAWinsAgainstB$winner[game]) {
        gameswonbattingfirst <- gameswonbattingfirst + 1
      }
    }
    Three_TeamAWinBattingFirstProb <- gameswonbattingfirst / nrow(TeamAWinsAgainstB)
  }
  return(Three_TeamAWinBattingFirstProb) 
  #print(Three_TeamAWinBattingFirstProb)
  #print(Three_TeamBWinBattingFirstProb)
}



#Task #4 for suggester
Task4 <- function (AllGames, TeamONE, TeamTWO, GROUND) {
  AllGamesTeamATeamBGroundX <- filter(AllGames, (team_one == TeamONE | team_one == TeamTWO) & (team_two == TeamONE | team_two == TeamTWO) & ground_id == GROUND)
  TeamAWinsAgainstBonGroundX <- filter(AllGamesTeamATeamBGroundX, winner == TeamONE)
  TeamBWinsAgainstAonGroundX <- filter(AllGamesTeamATeamBGroundX, winner == TeamTWO)
  if (nrow(TeamAWinsAgainstBonGroundX) == 0) {
    Four_TeamABattAingFirstProb <- 0.5
    Four_TeamBBattingFirstProb <- 0.5
  } else {
    gameswonbattingfirst <- 0
    for (game in 1:nrow(TeamAWinsAgainstBonGroundX)) {
      if (TeamAWinsAgainstBonGroundX$batting_first[game] == TeamAWinsAgainstBonGroundX$winner[game]) {
        gameswonbattingfirst <- gameswonbattingfirst + 1
      }
      Four_TeamABattingFirstProb <- gameswonbattingfirst / nrow(TeamAWinsAgainstBonGroundX)
      return(Four_TeamABattingFirstProb)
    }
  }
}


#Returns the feature data for the suggester
GetSuggesterFeatureData <- function (ChronoAllGames) {
  #creating a feature table for suggester data:
  FeatureTable <- data.frame(useless_column = c(1:nrow(ChronoAllGames)))
  
  #adding Task1 Column:
  GroundTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    GroundTendency <- Task1(ChronoAllGames, ChronoAllGames$ground_id[game])
    GroundTendencyList <- append(GroundTendencyList, GroundTendency)
  }
  FeatureTable <- cbind (FeatureTable, GroundTendency = GroundTendencyList)
  print("task 1 done")
  
  #adding Task2 Column:
  TeamTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    TeamTendency <- Task2(ChronoAllGames, ChronoAllGames$team_one[game])
    TeamTendencyList <- append(TeamTendencyList, TeamTendency)
  }
  FeatureTable <- cbind (FeatureTable, TeamTendency = TeamTendencyList)
  print("Task 2 done")
  
  #adding Task3 Column:
  AvsBTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    AvsBTendency <- Task3(ChronoAllGames, ChronoAllGames$team_one[game], ChronoAllGames$team_two[game])
    AvsBTendencyList <- append(AvsBTendencyList, AvsBTendency)
  }
  FeatureTable <- cbind (FeatureTable, AvsBTendency = AvsBTendencyList)
  print("Task 3 done")
  
  #adding Task4 Column:
  AvsBonXTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    AvsBonXTendency <- Task4(ChronoAllGames, ChronoAllGames$team_one[game], ChronoAllGames$team_two[game], ChronoAllGames$ground_id[game])
    AvsBonXTendencyList <- append(AvsBonXTendencyList, AvsBonXTendency)
  }
  FeatureTable <- cbind (FeatureTable, AvsBonXTendency = AvsBonXTendencyList)
  print("Task 4 done")
  
  #adding 4 way classification:
  #1 = toss won, batting first, game won
  #2 = toss won, bowling first, game won
  #3 = toss won, batting first, game lost
  #4 = toss won, bowling first, game lost 
  TossGameWonVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    if ((ChronoAllGames$toss_won[game] == ChronoAllGames$batting_first[game]) & (ChronoAllGames$batting_first[game] == ChronoAllGames$winner[game])) {
      TossGameWon <- 1
    }
    if ((ChronoAllGames$toss_won[game] != ChronoAllGames$batting_first[game]) & (ChronoAllGames$batting_first[game] != ChronoAllGames$winner[game])) {
      TossGameWon <- 2
    }
    if ((ChronoAllGames$toss_won[game] == ChronoAllGames$batting_first[game]) & (ChronoAllGames$batting_first[game] != ChronoAllGames$winner[game])) {
      TossGameWon <- 3
    }
    if ((ChronoAllGames$toss_won[game] != ChronoAllGames$batting_first[game]) & (ChronoAllGames$batting_first[game] == ChronoAllGames$winner[game])) {
      TossGameWon <- 4
    }
    TossGameWonVector <- append(TossGameWonVector, TossGameWon)
  }
  FeatureTable <- cbind (FeatureTable, TossGameWon = TossGameWonVector)
  
  #removing the useless column:
  FeatureTable <- subset (FeatureTable, select = -c(useless_column))
  
  #returns the features data:
  return(FeatureTable)
}

#Creating a support vector machine model for the suggester:
GetSuggesterSVMModel <- function (SuggesterLogisticsData) {
  SVM_SuggesterModel <- svm(TossGameWon ~., 
                            data = SuggesterLogisticsData,
                            type = 'C-classification',
                            kernel = 'polynomial',
                            scale = FALSE,
                            cost = 100)
  summary(SVM_SuggesterModel)
  return(SVM_SuggesterModel)
}

#Returns the accuracy of the SVM model for the suggester:
GetSuggesterSVMAccuracy <- function (SuggesterSVMModel, SuggesterLogisticsData) {
  Features <- subset (SuggesterLogisticsData, select = -c(TossGameWon))
  SVM_Predictions <- predict(SuggesterSVMModel, Features)
  print(SVM_Predictions)
  success <- 0
  failure <- 0
  for (game in 1:nrow(SuggesterLogisticsData)) {
    if (SVM_Predictions[game] == SuggesterLogisticsData$TossGameWon[game]) {
      success <- success + 1
    } else {
      failure <- failure + 1
    }
  }
  SVM_Accuracy <- success / (success + failure)
  return (SVM_Accuracy)
}

#Suggesting for a specific game
SuggestGame <- function (MatchID, ChronoAllGames, logisticsdata, Model, TeamNames) {
  TeamOne <- ChronoAllGames$team_one[which(ChronoAllGames$match_id == MatchID)]
  TeamOneName <- TeamNames$team_name[which(TeamNames$team_id == TeamOne)]
  TeamTwo <- ChronoAllGames$team_two[which(ChronoAllGames$match_id == MatchID)]
  TeamTwoName <- TeamNames$team_name[which(TeamNames$team_id == TeamTwo)]
  TossWinningTeam <- ChronoAllGames$toss_won[which(ChronoAllGames$match_id == MatchID)]
  BattingFirstTeam <- ChronoAllGames$batting_first[which(ChronoAllGames$match_id == MatchID)]
  TossWinningTeamName <- TeamNames$team_name[which(TeamNames$team_id == TossWinningTeam)]
  if (TossWinningTeam == BattingFirstTeam) {
    TossDecision <- "bat"
  } else {
    TossDecision <- "bowl"
  }
  logisticsdataforgame <- logisticsdata[which(ChronoAllGames$match_id == MatchID), ]
  Features <- subset (logisticsdataforgame, select = -c(TossGameWon))
  Prediction <- predict(Model, Features)
  #print(paste("This game is between", TeamOneName, "and", TeamTwoName))
  print(paste(TossWinningTeamName, "won the toss and chose to", TossDecision))
  if (Prediction == 1) {
    print(paste("My suggester suggests that", TossWinningTeamName, "should bat first"))
  }
  if (Prediction == 2) {
    print(paste("My suggester suggests that", TossWinningTeamName, "should bowl first"))
  }
  if (Prediction == 3) {
    print(paste("My suggester suggests that", TossWinningTeamName, "should bowl first"))
  }
  if (Prediction == 4) {
    print(paste("My suggester suggests that", TossWinningTeamName, "should bat first"))
  }
}

#Getting a feature row for my create game suggestion
GetFeatureRowforSuggestion <- function (ChronoAllGames, GroundID, TeamOneID, TeamTwoID) {
  FeatureRow <- data.frame(GroundTendency = 0,
                           TeamTendency = 0,
                           AvsBTendency = 0,
                           AvsBonXTendency = 0)
  
  #adding the Ground Tendency to the feature row:
  GroundTendency <- Task1(ChronoAllGames, GroundID)
  
  #adding the Team Tendency to the feature row:
  TeamTendency <- Task2(ChronoAllGames, TeamOneID)
  
  #adding the A vs B tendency to the feature row:
  AvsBTendency <- Task3(ChronoAllGames, TeamOneID, TeamTwoID)
  
  #adding the A vs B on X tendency to the feature row:
  AvsBonXTendency <- Task4(ChronoAllGames, TeamOneID, TeamTwoID, GroundID)
  
  #Adding all features to the feature row: (First row is in relation to "Team One")
  FeatureRow <- add_row(FeatureRow,
                        GroundTendency = GroundTendency,
                        TeamTendency = TeamTendency,
                        AvsBTendency = AvsBTendency,
                        AvsBonXTendency = AvsBonXTendency)
  FeatureRow <- FeatureRow[-1, ]
  #adding a second row that will be in relation to "Team Two"
  FeatureRow <- add_row(FeatureRow,
                        GroundTendency = GroundTendency,
                        TeamTendency = Task2(ChronoAllGames, TeamTwoID),
                        AvsBTendency = 1 - AvsBTendency,
                        AvsBonXTendency = 1 - AvsBonXTendency)
  return(FeatureRow)
}

#Getting a suggestion for the Create Game:
GetCreateGameSuggstion <- function (ChronoAllGames, Model, GroundID, TeamNames, TeamOneID, TeamTwoID) {
  TeamOneName <- TeamNames$team_name[which(TeamNames$team_id == TeamOneID)]
  TeamTwoName <- TeamNames$team_name[which(TeamNames$team_id == TeamTwoID)]
  Features <- GetFeatureRowforSuggestion(ChronoAllGames, GroundID, TeamOneID, TeamTwoID)
  Predictions <- predict(Model, Features)
  TeamOnePrediction <- Predictions[1]
  TeamTwoPrediction <- Predictions[2]
  
  if (TeamOnePrediction == 1 | TeamOnePrediction == 4) {
    print(paste("If", TeamOneName, "wins the toss, they should bat first"))
  }
  if (TeamOnePrediction == 2 | TeamOnePrediction == 3) {
    print(paste("If", TeamOneName, "wins the toss, they should bowl first"))
  }
  
  if (TeamTwoPrediction == 1 | TeamTwoPrediction == 4) {
    print(paste("If", TeamTwoName, "wins the toss, they should bat first"))
  }
  if (TeamTwoPrediction == 2 | TeamTwoPrediction == 3) {
    print(paste("If", TeamTwoName, "wins the toss, they should bowl first"))
  }
}
####################end suggester functions



