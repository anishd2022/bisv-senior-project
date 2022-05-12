library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyverse)

#main data frame which stores all games, date played, teams playing, team that won the toss, team that batted first, and team that won the game
AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
SortedAllGames <- arrange(AllGames, date)
ChronoAllGames <- filter(SortedChronoAllGames, winner != 0)


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
  
  #adding Task2 Column:
  TeamTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    TeamTendency <- Task2(ChronoAllGames, ChronoAllGames$team_one[game])
    TeamTendencyList <- append(TeamTendencyList, TeamTendency)
  }
  FeatureTable <- cbind (FeatureTable, TeamTendency = TeamTendencyList)
  
  #adding Task3 Column:
  AvsBTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    AvsBTendency <- Task3(ChronoAllGames, ChronoAllGames$team_one[game], ChronoAllGames$team_two[game])
    AvsBTendencyList <- append(AvsBTendencyList, AvsBTendency)
  }
  FeatureTable <- cbind (FeatureTable, AvsBTendency = AvsBTendencyList)
  
  #adding Task4 Column:
  AvsBonXTendencyList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    AvsBonXTendency <- Task4(ChronoAllGames, ChronoAllGames$team_one[game], ChronoAllGames$team_two[game], ChronoAllGames$ground_id[game])
    AvsBonXTendencyList <- append(AvsBonXTendencyList, AvsBonXTendency)
  }
  FeatureTable <- cbind (FeatureTable, AvsBonXTendency = AvsBonXTendencyList)
  
  #adding binary result (Toss Won + Game Won = 1       Toss Won + Game Lost = 0)
  TossGameWonList <- c()
  for (game in 1:nrow(ChronoAllGames)) {
    if (ChronoAllGames$toss_won[game] == ChronoAllGames$winner[game]) {
      TossGameWon <- 1
    } else {
      TossGameWon <- 0
    }
    TossGameWonList <- append(TossGameWonList, TossGameWon)
  }
  FeatureTable <- cbind (FeatureTable, TossGameWon = TossGameWonList)
  
  #removing the useless column:
  FeatureTable <- subset (FeatureTable, select = -c(useless_column))
  
  #returns the features data:
  return(FeatureTable)
}

#Creating a logistic model for the suggester:
GetSuggesterLogisticModel <- function (SuggesterLogisticsData) {
  Features <- subset (SuggesterLogisticsData, select = -c(TossGameWon))
  SuggesterLogisticModel <- glm (TossGameWon ~., data = SuggesterLogisticsData, family = "binomial")
  summary(SuggesterLogisticModel)
  return(SuggesterLogisticModel)
}

#Creating a support vector machine model for the suggester:
GetSuggesterSVMModel <- function (SuggesterLogisticsData) {
  SVM_SuggesterModel <- svm(TossGameWon ~., 
                            data = SuggesterLogisticsData,
                            type = 'C-classification',
                            kernel = 'linear',
                            scale = FALSE,
                            cost = 1)
  summary(SVM_SuggesterModel)
  return(SVM_SuggesterModel)
}

#Returns the accuracy of the logistic model for the suggester:
GetSuggesterLogisticAccuracy <- function (SuggesterLogisticModel, SuggesterLogisticsData) {
  TossGameWonVector <- SuggesterLogisticsData$TossGameWon 
  Predicted_Values <- SuggesterLogisticModel$fitted.values
  success <- 0
  fail <- 0
  for (game in 1:nrow(ChronoAllGames)) {
    if ((Predicted_Values[game] < 0.5) & (TossGameWonVector[game] == 0)) {
      success <- success + 1
    }
    if ((Predicted_Values[game] >= 0.5) & (TossGameWonVector[game] == 1)) {
      success <- success + 1
    }
    if ((Predicted_Values[game] < 0.5) & (TossGameWonVector[game] == 1)) {
      fail <- fail + 1
    }
    if ((Predicted_Values[game] >= 0.5) & (TossGameWonVector[game] == 0)) {
      fail <- fail + 1
    }
  }
  accuracy <- success / (success + fail)
  return (accuracy)
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

###############################################################################
#Program starts here
#Getting the feature data for the suggester
SuggesterLogisticsData <- GetSuggesterFeatureData(ChronoAllGames)

#Getting the logistic model for the suggester
SuggesterLogisticModel <- GetSuggesterLogisticModel (SuggesterLogisticsData)

#Getting the SVM model for the suggester
SuggesterSVMModel <- GetSuggesterSVMModel(SuggesterLogisticsData)

#Getting the accuracy of the logistic model for the suggester
SuggesterLogisticAccuracy <- GetSuggesterLogisticAccuracy(SuggesterLogisticModel, SuggesterLogisticsData)

#Getting the accuracy of the SVM model for the suggester
SuggesterSVMAccuracy <- GetSuggesterSVMAccuracy(SuggesterSVMModel, SuggesterLogisticsData)













##############################################################################
# #SQL STUFF
# select *
#   from grounds
# 
# select *
#   from fixtures
# 
# select *
#   from matches
# 
# select *
#   from team 
# 
# 
# #shows teams playing and venue and toss winner and game winner for each game 
# select matches.match_id, matches.toss_won, batting_first, winner, 
# fixtures.ground_id, grounds.name, matches.overs, matches.t1_total,
# matches.t2_total, matches.t1_balls, matches.t2_balls, matches.t1_wickets, matches.t2_wickets, fixtures.`date`  
# from matches
# inner join fixtures
# on matches.match_id = fixtures.match_id
# inner join grounds
# on grounds.ground_id = fixtures.ground_id
# 
# 
# 
# #games won batting first on each ground
# select count(matches.batting_first) as won_batting_first, 
# fixtures.ground_id, grounds.name
# from matches
# inner join fixtures
# on matches.match_id = fixtures.match_id
# inner join grounds
# on grounds.ground_id = fixtures.ground_id
# where  matches.batting_first = matches.winner
# group by fixtures.ground_id
# 
# 
# #games won bowling first on each ground
# select count(matches.batting_first) as won_bowling_first, 
# fixtures.ground_id, grounds.name
# from matches
# inner join fixtures
# on matches.match_id = fixtures.match_id
# inner join grounds
# on grounds.ground_id = fixtures.ground_id
# where  matches.batting_first != matches.winner
# group by fixtures.ground_id
# 
# 
# #suggester table
# select fixtures.ground_id, grounds.name,
# matches.match_id, matches.team_one, matches.team_two, matches.toss_won, 
# matches.batting_first, matches.winner, fixtures.`date`  
# from fixtures 
# inner join grounds 
# on fixtures.ground_id = grounds.ground_id 
# inner join matches 
# on matches.match_id = fixtures.match_id 
# inner join team
# on team.team_id = matches.team_one 
# 
# 
# #attempt to get players playing per match and MVP points
# select player_id, team_id, match_id, match_date, bowling_points, batting_points, fielding_points
# from player_statistics_summary 
# 
# 
# #use this to see what all players are registered to each team 
# select *
#   from team_player
# 
# #use this to see what team names are associated with a certain team ID
# select team_id, team_name
# from team
# 
# #use this to see player IDs corresponding with their name and other player information
# select *
#   from event_registered_users 
# 
# describe mcc.player 
# 
# 
# #shows all players named anish along with their player IDs
# select f_name, l_name, player_id
# from mcc.player 
# where f_name = "Anish"
# 
# 
# #shows all players for club 1191 along with their full names and player IDs
# select team_player.team_id, team_player.player_id, mcc.player.f_name, mcc.player.l_name  
# from team_player
# inner join mcc.player 
# on mcc.player.player_id = team_player.player_id











