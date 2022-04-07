#Task 1: For all previous games of team A, what fraction of games were won?
#Task 2: For all previous games between team A and team B, what fraction of games were won by team A?
#Task 3: Average MVP Point Team Strength for Team A (NOT SIGNIFICANT)
#Task 4: For all previous games of team A on ground X, what fraction of games were won? (NOT SIGNIFICANT)
#Task 5: Did team A win the toss? (NOT SIGNIFICANT)
#Task 6: Relative Team Strength for Team A
#Task 7: Relative Run Rate (Team's A's Average Run Rate minus Team B's Avg Run Rate)
#Task 8: Relative Wicket Rate (Avg Wickets per game team A minus Avg Wickets per game team B)
#To do: Understand Git and UI building in R and Player ID to Player name relationship
#use R shiny package to build my app
#NCCA Club ID = 1191
#MiLC Club ID = 18036

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

AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
SortedAllGames <- arrange(AllGames, date)
ChronoAllGames <- filter(SortedAllGames, winner != 0)
PlayerStats <- read.csv("/Users/anishdeshpande/projects/SeniorProject/PlayerStats.csv")
GameStats <- read.csv("/Users/anishdeshpande/projects/SeniorProject/GameStatistics.csv")
TeamNames <- read.csv("/Users/anishdeshpande/projects/SeniorProject/TeamNames.csv")
OrderedTeamNames <- arrange(TeamNames, team_name)


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
GetSVMModel <- function (logisticsdata) {
  SVM_PredictorModel <- svm(team_one_win ~., 
                            data = logisticsdata,
                            type = 'C-classification',
                            kernel = 'linear',
                            scale = FALSE,
                            cost = 1)
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





##################################################################################
#My program starts here:

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
SVMModel <- GetSVMModel(logisticstraindata)

#Gives the accuracy of the Logistic Model by running it on testing data
LogisticTestAccuracy <- GetLogisticTestAccuracy(LogisticModel, logisticstestdata)

#Gives the accuracy of the SVM model by running it on testing data
SVMTestAccuracy <- GetSVMTestAccuracy(SVMModel, logisticstestdata)

#predicts which team will win the game
GamePrediction <- PredictGame (3256, ChronoAllGames, logisticsdata, SVMModel, TeamNames)

############################################################################################# 








   
   
   
