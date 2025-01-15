

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

###################start predictor functions
generate_win_vector <- function(logisticsdata, ChronoAllGames) {
  WinLossVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    if (ChronoAllGames$winner[game] == ChronoAllGames$team_one[game]) {
      WinLossVector <- append(WinLossVector, 1)
    } else {
      WinLossVector <- append(WinLossVector, 0)
    }
  }
  ld <- cbind (logisticsdata, team_one_win = as.factor(WinLossVector))
  return (ld)
}

#does Task 1 for the predictor
PredTask1 <- function (AllGames, TeamA, MatchID) {
  PastGames <- AllGames[1:(which(AllGames$match_id == MatchID) - 1), ]
  #WinRate1 <- PredTask1 (PastGames, ChronoAllGames$team_one[nrow(PastGames) + 1])
  
  AllGamesForTeamA <- filter(AllGames, (team_one == TeamA | team_two == TeamA))
  win <- 0
  loss <- 0
  if (nrow(AllGamesForTeamA) == 0) {
    WinRate <- 0.5
  } else {
    for (game in 1:nrow(AllGamesForTeamA)) {
      if (AllGamesForTeamA$winner[game] == TeamA) {
        win <- win + 1
      } else {
        loss <- loss + 1
      }
    }
    WinRate <- win / (win + loss)
  }
  return(WinRate)
}

#does Task 2 for the predictor
PredTask2 <- function (AllGames, TeamA, TeamB) {
  AllGamesAandB <- filter(AllGames, (team_one == TeamA & team_two == TeamB) | (team_one == TeamB & team_two == TeamA))
  win <- 0
  loss <- 0
  if (nrow(AllGamesAandB) == 0 | is.na(nrow(AllGamesAandB)) | is.infinite(nrow(AllGamesAandB))) {
    WinRate <- 0.5
  } else {
    for (game in 1:nrow(AllGamesAandB)) {
      if (AllGamesAandB$winner[game] == TeamA) {
        win <- win + 1
      } else {
        loss <- loss + 1
      }
    }
    WinRate <- win / (win + loss)
  }
  return(WinRate)
}

#does Task 3 for the predictor
GetTeamStrengthA <- function (ChronoPlayerStats, MatchID) {
  #Get all players from both teams playing this match
  MatchPlayersTable <- filter(ChronoPlayerStats, match_id == MatchID & team_id != 0)
  #Get all records in which the above players played any games including current match
  PlayerEntries <- filter(ChronoPlayerStats, (player_id %in% MatchPlayersTable$player_id))
  if (is.finite(min(which(PlayerEntries$match_id == MatchID)))) {
    PastPlayerEntries <- PlayerEntries[1:min(which(PlayerEntries$match_id == MatchID) - 1), ]
    # if (PastPlayerEntries == Inf) {
    #   print("it's infinite!!!")
    #   return(67.4155)
    # }
    TeamA <- MatchPlayersTable$team_id[1]
    TotalMVPPoints <- 0
    TeamPlayerCount <- 0
    PlayerList <- c()
    for (player in 1:nrow(PastPlayerEntries)) {
      if (PastPlayerEntries$player_id[player] %in% MatchPlayersTable$player_id[which(MatchPlayersTable$team_id == TeamA)]) {
        TotalMVPPoints <- TotalMVPPoints + PastPlayerEntries$bowling_points[player] +
          PastPlayerEntries$batting_points[player] +
          PastPlayerEntries$fielding_points[player]
        TeamPlayerCount <- TeamPlayerCount + 1
        PlayerList <- append(PlayerList, PastPlayerEntries$player_id[player])
      } 
      if (length(PlayerList) == 0) {
        return(67.4155)
      }
    }
    TeamStrength <- TotalMVPPoints / TeamPlayerCount
    return(TeamStrength)
  } else {
    return(67.4155)
  }
}
GetTeamStrengthB <- function (ChronoPlayerStats, MatchID) {
  MatchPlayersTable <- filter(ChronoPlayerStats, match_id == MatchID & team_id != 0)
  ReversedMatchPlayersTable <- arrange(MatchPlayersTable, desc(team_id))
  PlayerEntries <- filter(ChronoPlayerStats, player_id %in% MatchPlayersTable$player_id)
  if (is.finite(min(which(PlayerEntries$match_id == MatchID)))) {
    PastPlayerEntries <- PlayerEntries[1:min(which(PlayerEntries$match_id == MatchID) - 1), ]
    # if (PastPlayerEntries == Inf) {
    #   print("it's infinite!!!")
    #   return(67.4155)
    # }
    TeamB <- ReversedMatchPlayersTable$team_id[1]
    TotalMVPPoints <- 0
    TeamPlayerCount <- 0
    PlayerList <- c()
    for (player in 1:nrow(PastPlayerEntries)) {
      if (PastPlayerEntries$player_id[player] %in% ReversedMatchPlayersTable$player_id[which(ReversedMatchPlayersTable$team_id == TeamB)]) {
        TotalMVPPoints <- TotalMVPPoints + PastPlayerEntries$bowling_points[player] +
          PastPlayerEntries$batting_points[player] +
          PastPlayerEntries$fielding_points[player]
        TeamPlayerCount <- TeamPlayerCount + 1
        PlayerList <- append(PlayerList, PastPlayerEntries$player_id[player])
      } 
      if (length(PlayerList) == 0) {
        return(67.4155)
      }
    }
    TeamStrength <- TotalMVPPoints / TeamPlayerCount
    return(TeamStrength)
  } else {
    return(67.4155)
  }
}


#does Task 4 for the predictor
PredTask4 <- function (AllGames, TeamA, GroundX) {
  AllGamesForAonX <- filter(AllGames, (team_one == TeamA | team_two == TeamA) & (ground_id == GroundX))
  win <- 0
  loss <- 0
  if (nrow(AllGamesForAonX) == 0) {
    WinRate <- 0.5
  } else {
    for (game in 1:nrow(AllGamesForAonX)) {
      if (AllGamesForAonX$winner[game] == TeamA) {
        win <- win + 1
      } else {
        loss <- loss + 1
      }
    }
    WinRate <- win / (win + loss)
  }
  return(WinRate)
}

#does Task 5 for the predictor
PredTask5 <- function (ChronoAllGames) {
  TossWonVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    if (ChronoAllGames$team_one[game] == ChronoAllGames$toss_won[game]) {
      TossWon <- 1
    } else {
      TossWon <- 0
    } 
    TossWonVector <- append(TossWonVector, TossWon)
  }
  return(TossWonVector)
}


#does Task 7 for the predictor 
GetRunRateA <- function (ChronoGameStats, ChronoAllGames, MatchID) {
  PastGameStats <- ChronoGameStats[0:(which(ChronoGameStats$match_id == MatchID)-1), ]
  PastGames <- filter (ChronoAllGames, match_id %in% PastGameStats$match_id)
  TeamA <- ChronoAllGames$team_one[which(ChronoAllGames$match_id == MatchID)]
  TeamAGames <- filter (PastGames, team_one == TeamA | team_two == TeamA)
  TeamAGameStats <- filter (PastGameStats, match_id %in% TeamAGames$match_id)
  if (nrow(TeamAGames) == 0) {
    return (1)
  }
  TotalRuns <- 0
  TotalBalls <- 0
  for (game in 1:nrow(TeamAGames)) {
    if (TeamAGames$team_one[game] == TeamA) {
      TotalRuns <- TotalRuns + TeamAGameStats$t1_total[game]
      TotalBalls <- TotalBalls + TeamAGameStats$t1_balls[game]
    } else {
      TotalRuns <- TotalRuns + TeamAGameStats$t2_total[game]
      TotalBalls <- TotalBalls + TeamAGameStats$t2_balls[game]
    }
  }
  RunRateA <- TotalRuns / TotalBalls
  return (RunRateA)
}
GetRunRateB <- function (ChronoGameStats, ChronoAllGames, MatchID) {
  PastGameStats <- ChronoGameStats[0:(which(ChronoGameStats$match_id == MatchID)-1), ]
  PastGames <- filter (ChronoAllGames, match_id %in% PastGameStats$match_id)
  TeamB <- ChronoAllGames$team_two[which(ChronoAllGames$match_id == MatchID)]
  TeamBGames <- filter (PastGames, team_one == TeamB | team_two == TeamB)
  TeamBGameStats <- filter (PastGameStats, match_id %in% TeamBGames$match_id)
  if (nrow(TeamBGames) == 0) {
    return (1)
  }
  TotalRuns <- 0
  TotalBalls <- 0
  for (game in 1:nrow(TeamBGames)) {
    if (TeamBGames$team_one[game] == TeamB) {
      TotalRuns <- TotalRuns + TeamBGameStats$t1_total[game]
      TotalBalls <- TotalBalls + TeamBGameStats$t1_balls[game]
    } else {
      TotalRuns <- TotalRuns + TeamBGameStats$t2_total[game]
      TotalBalls <- TotalBalls + TeamBGameStats$t2_balls[game]
    }
  }
  RunRateB <- TotalRuns / TotalBalls
  return (RunRateB)
}


#does Task 8 for the predictor
GetWicketRateA <- function (ChronoGameStats, ChronoAllGames, MatchID) {
  PastGameStats <- ChronoGameStats[0:(which(ChronoGameStats$match_id == MatchID)-1), ]
  PastGames <- filter (ChronoAllGames, match_id %in% PastGameStats$match_id)
  TeamA <- ChronoAllGames$team_one[which(ChronoAllGames$match_id == MatchID)]
  TeamAGames <- filter (PastGames, team_one == TeamA | team_two == TeamA)
  TeamAGameStats <- filter (PastGameStats, match_id %in% TeamAGames$match_id)
  if (nrow(TeamAGames) == 0) {
    return (5)
  }
  TotalWickets <- 0
  for (game in 1:nrow(TeamAGames)) {
    if (TeamAGames$team_one[game] == TeamA) {
      TotalWickets <- TotalWickets + TeamAGameStats$t1_wickets[game]
    } else {
      TotalWickets <- TotalWickets + TeamAGameStats$t2_wickets[game]
    }
  }
  WicketRateA <- TotalWickets / nrow(TeamAGames)
  return (WicketRateA)
}
GetWicketRateB <- function (ChronoGameStats, ChronoAllGames, MatchID) {
  PastGameStats <- ChronoGameStats[0:(which(ChronoGameStats$match_id == MatchID)-1), ]
  PastGames <- filter (ChronoAllGames, match_id %in% PastGameStats$match_id)
  TeamB <- ChronoAllGames$team_two[which(ChronoAllGames$match_id == MatchID)]
  TeamBGames <- filter (PastGames, team_one == TeamB | team_two == TeamB)
  TeamBGameStats <- filter (PastGameStats, match_id %in% TeamBGames$match_id)
  if (nrow(TeamBGames) == 0) {
    return (5)
  }
  TotalWickets <- 0
  for (game in 1:nrow(TeamBGames)) {
    if (TeamBGames$team_one[game] == TeamB) {
      TotalWickets <- TotalWickets + TeamBGameStats$t1_wickets[game]
    } else {
      TotalWickets <- TotalWickets + TeamBGameStats$t2_wickets[game]
    }
  }
  WicketRateB <- TotalWickets / nrow(TeamBGames)
  return (WicketRateB)
}


#gives a final prediction based on the Match ID
PredictionTask1 <- function (ChronoAllGames, MatchID) {
  #  AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
  #  ChronoAllGames <- arrange(AllGames, date)
  
  #  PlayerStats <- read.csv("/Users/anishdeshpande/projects/SeniorProject/PlayerStats.csv")
  #  ChronoPlayerStats <- arrange(PlayerStats, match_date)
  
  PastGames <- ChronoAllGames[1:(which(ChronoAllGames$match_id == MatchID) - 1), ]
  
  
  WinRate1 <- PredTask1 (PastGames, ChronoAllGames$team_one[nrow(PastGames) + 1])
  return (WinRate1)
}

#goes over all the games (so far only considering first task)
AllPredictionsTask1 <- function (ChronoAllGames) {
  WinRateVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    print(game)
    WinRate1 <- PredTask1 (ChronoAllGames, ChronoAllGames$team_one[game], ChronoAllGames$match_id[game])
    WinRateVector <- append (WinRateVector, WinRate1)
  }
  return (WinRateVector)
}


#taking it a step further to include Task 2:
AllPredictionsTask2 <- function (ChronoAllGames) {
  WinRateAvsBVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    print(game)
    PastGames <- ChronoAllGames[1:(game - 1), ]
    WinRate <- PredTask2 (PastGames, ChronoAllGames$team_one[game], ChronoAllGames$team_two[game])
    WinRateAvsBVector <- append (WinRateAvsBVector, WinRate)
  }
  return (WinRateAvsBVector)
}


#Now including task 6:
AllPredictionsTask6 <- function (ChronoAllGames, ChronoPlayerStats) {
  RelativeTeamStrengthVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    print(game)
    PastGames <- ChronoAllGames[1:(game - 1), ]
    TeamStrengthA <- GetTeamStrengthA(ChronoPlayerStats, ChronoAllGames$match_id[game])
    TeamStrengthB <- GetTeamStrengthB(ChronoPlayerStats, ChronoAllGames$match_id[game])
    RelativeTeamStrength <- TeamStrengthA / (TeamStrengthA + TeamStrengthB)
    RelativeTeamStrengthVector <- append (RelativeTeamStrengthVector, RelativeTeamStrength)
  }
  return (RelativeTeamStrengthVector)
}


#Now including task 7: 
AllPredictionsTask7 <- function (ChronoGameStats, ChronoAllGames) {
  NRRVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    print(game)
    RunRateA <- GetRunRateA (ChronoGameStats, ChronoAllGames, ChronoAllGames$match_id[game])
    RunRateB <- GetRunRateB (ChronoGameStats, ChronoAllGames, ChronoAllGames$match_id[game])
    NRR <- RunRateA - RunRateB
    NRRVector <- append (NRRVector, NRR)
  }
  return (NRRVector)
}


#Now including task 8:
AllPredictionsTask8 <- function (ChronoGameStats, ChronoAllGames) {
  RelativeWicketRateVector <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    print(game)
    WicketRateA <- GetWicketRateA (ChronoGameStats, ChronoAllGames, ChronoAllGames$match_id[game])
    WicketRateB <- GetWicketRateB (ChronoGameStats, ChronoAllGames, ChronoAllGames$match_id[game])
    RelativeWicketRate <- WicketRateA - WicketRateB
    RelativeWicketRateVector <- append (RelativeWicketRateVector, RelativeWicketRate)
  }
  return (RelativeWicketRateVector)
}

#Predicting the outcome of a specific game:
PredictGame <- function (MatchID, ChronoAllGames, logisticsdata, Model, TeamNames) {
  TeamOne <- ChronoAllGames$team_one[which(ChronoAllGames$match_id == MatchID)]
  TeamOneName <- TeamNames$team_name[which(TeamNames$team_id == TeamOne)]
  TeamTwo <- ChronoAllGames$team_two[which(ChronoAllGames$match_id == MatchID)]
  TeamTwoName <- TeamNames$team_name[which(TeamNames$team_id == TeamTwo)]
  logisticsdataforgame <- logisticsdata[which(ChronoAllGames$match_id == MatchID), ]
  Features <- subset (logisticsdataforgame, select = -c(team_one_win))
  Prediction <- predict(Model, Features)
  print(paste("This game is between", TeamOneName, "and", TeamTwoName))
  if (Prediction == 1) {
    print(paste(TeamOneName, "is predicted to win this game"))
    if (logisticsdataforgame$team_one_win == 1) {
      print(paste(TeamOneName, "ended up winning the game. My prediction is correct"))
    }
    if (logisticsdataforgame$team_one_win == 0) {
      print(paste(TeamOneName, "did NOT end up winning the game. My prediction is incorrect"))
    }
  } else {
    print(paste(TeamTwoName, "is predicted to win this game"))
    if (logisticsdataforgame$team_one_win == 1) {
      print(paste(TeamTwoName, "did NOT end up winning the game. My prediction is incorrect"))
    }
    if (logisticsdataforgame$team_one_win == 0) {
      print(paste(TeamTwoName, "ended up winning the game. My prediction is correct"))
    }
  }
}

#Creates a feature table for all data:
GetLogisticsData <- function (AllGames, PlayerStats, GameStats) {
  #now splitting into training and testing datasets: (70 percent training and 30 percent testing)
  TrainAllGames <- AllGames[1:round(0.7*nrow(AllGames)), ]
  TestAllGames <- AllGames[(round(0.7*nrow(AllGames))+1):nrow(AllGames), ]
  
  SortedAllGames <- arrange(AllGames, date)
  ChronoAllGames <- filter(SortedAllGames, winner != 0)
  
  ChronoPlayerStats <- arrange(PlayerStats, match_date)
  
  ChronoGameStats <- arrange(GameStats, date)
  
  #all win loss data will be calculated with respect to team_one
  logisticsdata <- data.frame(useless_column = vector(mode = "numeric", length = nrow(ChronoAllGames)))
  
  logisticsdata <- generate_win_vector(logisticsdata, ChronoAllGames)
  
  WinRateVector <- AllPredictionsTask1 (ChronoAllGames)
  logisticsdata <- cbind (logisticsdata, WinRate = WinRateVector)
  
  WinRateAvsBVector <- AllPredictionsTask2 (ChronoAllGames)
  logisticsdata <- cbind (logisticsdata, WinRateAgstB = WinRateAvsBVector)
  
  RelativeTeamStrengthVector <- AllPredictionsTask6 (ChronoAllGames, ChronoPlayerStats)
  RelativeTeamStrengthVector[is.na(RelativeTeamStrengthVector)] <- 0
  logisticsdata <- cbind (logisticsdata, RelativeTeamStrength = RelativeTeamStrengthVector)
  
  NRRVector <- AllPredictionsTask7 (ChronoGameStats, ChronoAllGames)
  NRRVector[is.na(NRRVector)] <- 0
  logisticsdata <- cbind (logisticsdata, RelativeRunRate = NRRVector)
  
  RelativeWicketRateVector <- AllPredictionsTask8 (ChronoGameStats, ChronoAllGames)
  RelativeWicketRateVector[is.na(RelativeWicketRateVector)] <- 0
  logisticsdata <- cbind (logisticsdata, RelativeWicketRate = RelativeWicketRateVector)
  
  
  logisticsdata <- subset (logisticsdata, select = -c(useless_column))
  
  print("task 8 complete")
  return(logisticsdata)
}

#Creating a logistic regression model:
GetLogisticModel <- function (logisticsdata) {
  Features <- subset (logisticsdata, select = -c(team_one_win))
  WinRateLogisticModel <- glm (team_one_win ~ ., data = logisticsdata, family = "binomial")
  summary (WinRateLogisticModel)
  return(WinRateLogisticModel)
}

#Creating a support vector machine model:
GetSVMModel <- function (logisticsdata, type, kernel, cost) {
  SVM_PredictorModel <- svm(team_one_win ~., 
                            data = logisticsdata,
                            type = type,
                            kernel = kernel,
                            scale = FALSE,
                            cost = cost)
  summary(SVM_PredictorModel)
  return(SVM_PredictorModel)
}

#Function that tests the accuracy of the SVM model on the testing data
GetSVMTestAccuracy <- function (SVMModel, TestData) {
  Features <- subset (TestData, select = -c(team_one_win))
  SVM_Predictions <- predict(SVMModel, Features)
  print(SVM_Predictions)
  success <- 0
  failure <- 0
  for (game in 1:nrow(TestData)) {
    if (SVM_Predictions[game] == TestData$team_one_win[game]) {
      success <- success + 1
    } else {
      failure <- failure + 1
    }
  }
  SVM_Accuracy <- success / (success + failure)
  return (SVM_Accuracy)
}

#Returns a list of all past Match IDs for two teams that played against each other
GetMatches <- function (ChronoAllGames, TeamOne, TeamTwo) {
  MatchIDList <- c()
  TeamOneID <- TeamNames$team_id[which(TeamNames$team_name == TeamOne)]
  TeamTwoID <- TeamNames$team_id[which(TeamNames$team_name == TeamOne)]
  for (game in 1:nrow(ChronoAllGames)) {
    if ((ChronoAllGames$team_one[game] == TeamOneID) & (ChronoAllGames$team_two[game] == TeamTwoID)) {
      MatchIDList <- append(MatchIDList, ChronoAllGames$match_id[game])
    }
    if ((ChronoAllGames$team_one[game] == TeamTwoID) & (ChronoAllGames$team_two[game] == TeamOneID)) {
      MatchIDList <- append(MatchIDList, ChronoAllGames$match_id[game])
    }
  }
  return(MatchIDList)
}

#Returns a list of all the players playing on Team A in a specific match
GetPlaying11TeamA <- function (PlayerStats, PlayerFullNames, MatchID) {
  Playing22 <- filter(PlayerStats, match_id == MatchID) %>% select(player_id, team_id)
  Playing22NameObservations <- filter(PlayerFullNames, player_id %in% Playing22$player_id)   
  Playing22Names <- unique(Playing22NameObservations$Full_Name)     
  #now splitting names based on team
  TeamA <- Playing22$team_id[1]
  TeamB <- Playing22$team_id[nrow(Playing22)]
  Playing11TeamA <- c()
  Playing11TeamB <- c()
  for (player in 1:length(Playing22Names)) {
    if (Playing22$team_id[player] == TeamA) {
      Playing11TeamA <- append(Playing11TeamA, Playing22Names[player])
    }
  }
  return(Playing11TeamA)
}

#Returns a list of all the players playing on Team B in a specific match
GetPlaying11TeamB <- function (PlayerStats, PlayerFullNames, MatchID) {
  Playing22 <- filter(PlayerStats, match_id == MatchID) %>% select(player_id, team_id)
  Playing22NameObservations <- filter(PlayerFullNames, player_id %in% Playing22$player_id)   
  Playing22Names <- unique(Playing22NameObservations$Full_Name)     
  #now splitting names based on team
  TeamA <- Playing22$team_id[1]
  TeamB <- Playing22$team_id[nrow(Playing22)]
  Playing11TeamA <- c()
  Playing11TeamB <- c()
  for (player in 1:length(Playing22Names)) {
    if (Playing22$team_id[player] == TeamB) {
      Playing11TeamB <- append(Playing11TeamB, Playing22Names[player])
    }
  }
  return(Playing11TeamB)
}

#PROBLEM HERE!!!!!!!!!!!!!!!!!!!!!!!
#Function that tests the accuracy of the Logistic model on the testing data
GetLogisticTestAccuracy <- function (LogisticModel, logisticstestdata) {
  Features <- subset (logisticstestdata, select = -c(team_one_win))
  WinProbabilityList <- predict(LogisticModel, Features)
  print(WinProbabilityList)
  predicted.data <- data.frame (probability_of_win = WinProbabilityList,
                                team_one_win = logisticstestdata$team_one_win)
  predicted.data <- predicted.data [order(predicted.data$probability_of_win, decreasing = FALSE), ]
  predicted.data$rank <- 1:nrow(predicted.data)
  #print(predicted.data)
  success <- 0
  failure <- 0
  for (game in 1:nrow(predicted.data)) {
    if ((predicted.data$probability_of_win[game] < 0.5) & (predicted.data$team_one_win[game] == 0)) {
      success <- success + 1
    } else if ((predicted.data$probability_of_win[game] >= 0.5) & (predicted.data$team_one_win[game] == 1)) {
      success <- success + 1
    } else {
      failure <- failure + 1
    }
  }
  LogisticPredictorAccuracy <- success / (success + failure)
  return (LogisticPredictorAccuracy)
}

#Gets a list of fixtures for all games:
GetFixtureList <- function (ChronoAllGames) {
  FixtureList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    GameDate <- ChronoAllGames$date[game]
    TeamOneID <- ChronoAllGames$team_one[game]
    TeamTwoID <- ChronoAllGames$team_two[game]
    TeamOneName <- TeamNames$team_name[which(TeamOneID == TeamNames$team_id)]
    TeamTwoName <- TeamNames$team_name[which(TeamTwoID == TeamNames$team_id)]
    Fixture <- paste(TeamOneName, "vs", TeamTwoName, "on", GameDate)
    FixtureList <- append(FixtureList, Fixture)
  }
  return(FixtureList)
}

#Creates a feature row for the created game
GetFeatureRowforCreateGame <- function (TeamOnePlayers, TeamTwoPlayers, PlayerStats, ChronoAllGames, AllGames, GameStats, TeamOneID, TeamTwoID, logisticsdata) {
  FeatureRow <- data.frame(WinRate = 0,
                           WinRateAgstB = 0,
                           RelativeTeamStrength = 0,
                           RelativeRunRate = 0,
                           RelativeWicketRate = 0)
  
  #adding the win rate to the feature row
  GameIndexes <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    if (TeamOneID == ChronoAllGames$team_one[game] | TeamOneID == ChronoAllGames$team_two[game]) {
      GameIndexes <- append(GameIndexes, game)
    }
  }
  MostRecentGameIndex <- GameIndexes[length(GameIndexes)]
  TeamOneWinRate <- logisticsdata$WinRate[MostRecentGameIndex]
  
  #adding the win rate against team B to the feature row
  AllGamesAandB <- filter(ChronoAllGames, (team_one == TeamOneID & team_two == TeamTwoID) | (team_one == TeamTwoID & team_two == TeamOneID))
  win <- 0
  loss <- 0
  if (nrow(AllGamesAandB) == 0 | is.na(nrow(AllGamesAandB)) | is.infinite(nrow(AllGamesAandB))) {
    WinRateAgstB <- 0.5
  } else {
    for (game in 1:nrow(AllGamesAandB)) {
      if (AllGamesAandB$winner[game] == TeamOneID) {
        win <- win + 1
      } else {
        loss <- loss + 1
      }
    }
    WinRateAgstB <- win / (win + loss)
  }
  
  #adding the relative team strength to the feature row:
  TeamOnePlayerInfo <- filter(PlayerFullNames, Full_Name %in% TeamOnePlayers)
  TeamOnePlayerIDs <- c()
  for (player in 1:nrow(TeamOnePlayerInfo)) {
    PlayerID <- PlayerFullNames$player_id[which(PlayerFullNames$Full_Name %in% TeamOnePlayers )] 
    TeamOnePlayerIDs <- append(TeamOnePlayerIDs, PlayerID)
  }
  TeamOnePlayerIDs <- unique(TeamOnePlayerIDs)
  
  TeamTwoPlayerInfo <- filter(PlayerFullNames, Full_Name %in% TeamTwoPlayers)
  TeamTwoPlayerIDs <- c()
  for (player in 1:nrow(TeamTwoPlayerInfo)) {
    PlayerID <- PlayerFullNames$player_id[which(PlayerFullNames$Full_Name %in% TeamTwoPlayers )] 
    TeamTwoPlayerIDs <- append(TeamTwoPlayerIDs, PlayerID)
  }
  TeamTwoPlayerIDs <- unique(TeamTwoPlayerIDs)
  
  InterestedPlayerStatsTeamOne <- filter(PlayerStats, player_id %in% TeamOnePlayerIDs)
  InterestedPlayerStatsTeamTwo <- filter(PlayerStats, player_id %in% TeamTwoPlayerIDs)
  
  InterestedPlayerStatsTeamOne <- mutate(InterestedPlayerStatsTeamOne, MVPPoints = bowling_points + batting_points + fielding_points)
  InterestedPlayerStatsTeamTwo <- mutate(InterestedPlayerStatsTeamTwo, MVPPoints = bowling_points + batting_points + fielding_points)
  
  TotalMVPPointsTeamOne <- sum(InterestedPlayerStatsTeamOne$MVPPoints)
  TotalMVPPointsTeamTwo <- sum(InterestedPlayerStatsTeamTwo$MVPPoints)
  
  TeamOneRelativeTeamStrength <- TotalMVPPointsTeamOne / (TotalMVPPointsTeamOne + TotalMVPPointsTeamTwo)
  
  #adding the relative run rate to the feature row
  GoodRowYesNo <- c()
  for (game in 1:nrow(GameStats)) {
    if (GameStats$match_id[game] %in% ChronoAllGames$match_id) {
      GoodRowYesNo <- append(GoodRowYesNo, 1)
    } else {
      GoodRowYesNo <- append(GoodRowYesNo, 0)
    }
  }
  GameStats <- mutate(GameStats, GoodRow = GoodRowYesNo) 
  GameStats <- filter(GameStats, GoodRow == 1)
  GameStats <- arrange(GameStats, date)
  GameStats <- mutate(GameStats, team_one = ChronoAllGames$team_one)
  GameStats <- mutate(GameStats, team_two = ChronoAllGames$team_two)
  
  TeamOneGameStats <- filter(GameStats, team_one == TeamOneID | team_two == TeamOneID)
  TeamTwoGameStats <- filter(GameStats, team_one == TeamTwoID | team_two == TeamTwoID)
  
  TeamOneRuns <- 0
  TeamOneBalls <- 0
  TeamOneWickets <- 0
  for (game in 1:nrow(TeamOneGameStats)) {
    if (TeamOneGameStats$team_one[game] == TeamOneID) {
      TeamOneRuns <- TeamOneRuns + TeamOneGameStats$t1_total[game]
      TeamOneBalls <- TeamOneBalls + TeamOneGameStats$t1_balls[game]
      TeamOneWickets <- TeamOneWickets + TeamOneGameStats$t1_wickets[game]
    } else {
      TeamOneRuns <- TeamOneRuns + TeamOneGameStats$t2_total[game]
      TeamOneBalls <- TeamOneBalls + TeamOneGameStats$t2_balls[game]
      TeamOneWickets <- TeamOneWickets + TeamOneGameStats$t2_balls[game]
    }
  }
  TeamOneRunRate <- TeamOneRuns / TeamOneBalls
  TeamOneWicketRate <- TeamOneWickets / nrow(TeamOneGameStats)
  
  TeamTwoRuns <- 0
  TeamTwoBalls <- 0
  TeamTwoWickets <- 0
  for (game in 1:nrow(TeamTwoGameStats)) {
    if (TeamTwoGameStats$team_two[game] == TeamTwoID) {
      TeamTwoRuns <- TeamTwoRuns + TeamTwoGameStats$t2_total[game]
      TeamTwoBalls <- TeamTwoBalls + TeamTwoGameStats$t2_balls[game]
      TeamTwoWickets <- TeamTwoWickets + TeamTwoGameStats$t2_wickets[game]
    } else {
      TeamTwoRuns <- TeamTwoRuns + TeamTwoGameStats$t1_total[game]
      TeamTwoBalls <- TeamTwoBalls + TeamTwoGameStats$t1_balls[game]
      TeamTwoWickets <- TeamTwoWickets + TeamTwoGameStats$t1_balls[game]
    }
  }
  TeamTwoRunRate <- TeamTwoRuns / TeamTwoBalls
  TeamTwoWicketRate <- TeamTwoWickets / nrow(TeamTwoGameStats)
  
  RelativeRunRate <- TeamOneRunRate / (TeamOneRunRate + TeamTwoRunRate)
  
  #adding the relative wicket rate to the feature row
  RelativeWicketRate <- TeamOneWicketRate / (TeamOneWicketRate + TeamTwoWicketRate)
  
  #adding all items to the feature row
  FeatureRow <- add_row(FeatureRow, 
                        WinRate = TeamOneWinRate, 
                        WinRateAgstB = WinRateAgstB,
                        RelativeTeamStrength = TeamOneRelativeTeamStrength,
                        RelativeRunRate = RelativeRunRate,
                        RelativeWicketRate = RelativeWicketRate)
  FeatureRow <- FeatureRow[-1, ]
  
  return(FeatureRow)
}

#Predicts the create game result:
GetCreateGamePrediction <- function (Model, TeamOneID, TeamTwoID, FeatureRow, TeamNames) {
  TeamOneName <- TeamNames$team_name[which(TeamNames$team_id == TeamOneID)]
  TeamTwoName <- TeamNames$team_name[which(TeamNames$team_id == TeamTwoID)]
  Prediction <- predict(Model, FeatureRow)
  if (Prediction == 1) {
    print(TeamOneName)
    #print(paste(TeamOneName, "is predicted to win this game"))
  } else {
    print(TeamTwoName)
    #print(paste(TeamTwoName, "is predicted to win this game"))
  }
}
###################end predictor functions



#main data frame which stores all games, date played, teams playing, team that won the toss, team that batted first, and team that won the game
AllGames <- read_excel("AllGames.xls")
SortedAllGames <- arrange(AllGames, date)
ChronoAllGames <- filter(SortedAllGames, winner != 0)
TeamNames <- read.csv("TeamNames.csv")
ChronoAllGames$date <- as.Date(ChronoAllGames$date)
PlayerStats <- read.csv("PlayerStats.csv")
GameStats <- read.csv("GameStatistics.csv")
OrderedTeamNames <- arrange(TeamNames, team_name)
PlayerNames <- read.csv("TeamPlayerNameTable.csv")
PlayerFullNames <- mutate(PlayerNames, Full_Name = paste(PlayerNames$f_name, PlayerNames$l_name))






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
  textOutput("CreateGamePrediction")
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




#############Suggester
#Getting the feature data for the suggester
SuggesterLogisticsData <- GetSuggesterFeatureData(ChronoAllGames)

#Getting the SVM model for the suggester
SuggesterSVMModel <- GetSuggesterSVMModel(SuggesterLogisticsData)

#Getting the accuracy of the SVM model for the suggester
SuggesterSVMAccuracy <- GetSuggesterSVMAccuracy(SuggesterSVMModel, SuggesterLogisticsData)

#Making a suggestion for a specific Match ID:
GameSuggestion <- SuggestGame(3256, ChronoAllGames, SuggesterLogisticsData, SuggesterSVMModel, TeamNames)

#The feature row for the suggestion:
SuggestionFeatureRow <- GetFeatureRowforSuggestion(ChronoAllGames, 1, 902, 877)

#The suggestion for both teams for the create game:
CreateGameSuggestion <- GetCreateGameSuggstion(ChronoAllGames, SuggesterSVMModel, 1, TeamNames, 902, 877)
#updating the Fixure selections to only show fixtures within the date range
observe({
  AppropriateFixtures <- FixtureList[min(which(ChronoAllGames$date >= input$daterange[1])):max(which(ChronoAllGames$date <= input$daterange[2]))]
  updateSelectInput(session, "Fixture", "Select a fixture", choices = unique(AppropriateFixtures))
})

###########Predictor
#feature data for all games
logisticsdata <- GetLogisticsData(AllGames, PlayerStats, GameStats)

#now splitting into training and testing set:
TrainAllGames <- AllGames[1:round(0.7*nrow(AllGames)), ]
TestAllGames <- AllGames[(round(0.7*nrow(AllGames))+1):nrow(AllGames), ]
logisticstraindata <- logisticsdata[1:nrow(TrainAllGames), ]
logisticstestdata <- logisticsdata[(nrow(TrainAllGames) + 1):nrow(logisticsdata), ]

#getting my logistic model based off of the training data
LogisticModel <- GetLogisticModel(logisticstraindata)

#getting my SVM model based off of the training data
SVMModel <- GetSVMModel(logisticstraindata, 'C-classification', 'linear', 1)

#Gives the accuracy of the Logistic Model by running it on testing data
LogisticTestAccuracy <- GetLogisticTestAccuracy(LogisticModel, logisticstestdata)

#Gives the accuracy of the SVM model by running it on testing data
SVMTestAccuracy <- GetSVMTestAccuracy(SVMModel, logisticstestdata)

#predicts which team will win the game (Uses SVM model since it is more accurate)
GamePrediction <- PredictGame (3256, ChronoAllGames, logisticsdata, SVMModel, TeamNames)

#A list of fixtures from all games:
FixtureList <- GetFixtureList (ChronoAllGames)

#Gives a feature row for the create game
FeatureRow <- GetFeatureRowforCreateGame(c("Rahul Jariwala", "Anish Deshpande"), c("Neeraj Goel", "Hammad Azam"), PlayerStats, ChronoAllGames, AllGames, GameStats, 902, 877, logisticsdata)

#Predicts the winner of the create game:
CreateGamePrediction <- GetCreateGamePrediction(SVMModel, 902, 877, FeatureRow, TeamNames)
######################

#runs app
shinyApp(ui = ui, server = server)
























