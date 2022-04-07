library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyverse)

#main dataframe which stores all games, date played, teams playing, team that won the toss, team that batted first, and team that won the game
AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
ChronoAllGames <- arrange(AllGames, date)

suggester_accuracy_table <- data.frame(B_1 = c(1, 0, 0, 1, 1, 1),
                                       B_2 = c(1, 0, 0, 1, 0, 0.5),
                                       B_3 = c(1, 0, 0, 0, 1, 0.5),
                                       B_4 = c(1, 0, 0.25, 1, 1, 1),
                                       Accuracy = c(0.6263089, 0.4454343, 0.4454343, 0.7187798,
                                                    0.7497858, 0.7625772),
                                       stringsAsFactors = FALSE) 
sorted_suggester_table <- arrange(suggester_accuracy_table, desc(Accuracy))

#B_1 <- 0
#B_2 <- 0
#B_3 <- 0
#B_4 <- 0
#weight_vector <- c(B_1, B_2, B_3, B_4)

#function that tells you if each team should bat or bowl upon winning the toss given Team1, Team2, and Ground
TossDecision <- function (TeamONE, TeamTWO, GROUND, TossWinner) {
  AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
  B_1 <- 1
  B_2 <- 0.5
  B_3 <- 0.5
  B_4 <- 1

  #does task #1 for the suggester
  One_BattingFirstWinProb <- Task1(AllGames, GROUND)
  
  #does task #2 for the suggester
  Two_BattingFirstWinProbTeamOne <- Task2(AllGames, TeamONE)
  Two_BattingFirstWinProbTeamTwo <- Task2(AllGames, TeamTWO)

  #does task #3 for the suggester
  Three_TeamAWinBattingFirstProb <- Task3(AllGames, TeamONE, TeamTWO)
  Three_TeamBWinBattingFirstProb <- Task3(AllGames, TeamTWO, TeamONE)
  
  #does task #4 for the suggester
  Four_TeamABattingFirstProb <- Task4(AllGames, TeamONE, TeamTWO, GROUND)
  Four_TeamBBattingFirstProb <- Task4(AllGames, TeamTWO, TeamONE, GROUND)

  Team_1_Tendency <- (B_1 * (One_BattingFirstWinProb) +
    B_2 * (Two_BattingFirstWinProbTeamOne) +
    B_3 * (Three_TeamAWinBattingFirstProb) +
      B_4 * (Four_TeamABattingFirstProb)) / 4 
  
  Team_2_Tendency <- (B_1 * (One_BattingFirstWinProb) +
                        B_2 * (Two_BattingFirstWinProbTeamTwo) +
                        B_3 * (Three_TeamBWinBattingFirstProb) + 
                        B_4 * (Four_TeamBBattingFirstProb)) / 4
  
  #print(Team_1_Tendency)
  #print(Team_2_Tendency)
  
  if (TossWinner == TeamONE) {
    if (Team_1_Tendency >= 0.5) {
      #print("Team 1 should bat first")
      return(1)
    } else {
      #print("Team 1 should bowl first")
      return(2)
    }
  }
  
  if (TossWinner == TeamTWO) {
    if (Team_2_Tendency >= 0.5) {
      #print("Team 2 should bat first")
      return(1)
    } else {
      #print("Team 2 should bowl first")
      return(2)
    }
  }
}



DynamicTossDecision <- function (game) {
  AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
  ChronoAllGames <- arrange(AllGames, date)
  DynamicChronoAllGames <- ChronoAllGames[1:(game - 1), ]
  NthGameSuggestion <- TossDecision(ChronoAllGames$team_one[game], ChronoAllGames$team_two[game], ChronoAllGames$ground_id[game], ChronoAllGames$toss_won[game])
  return(NthGameSuggestion)
}


SuggesterAccuracyFinder <- function () {
  suggester_accuracy_table <- data.frame(B_1 = c(),
                                         B_2 = c(),
                                         B_3 = c(),
                                         B_4 = c(),
                                         Accuracy = c(),
                                         stringsAsFactors = FALSE)
  AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
  ChronoAllGames <- arrange(AllGames, date)
            ApplicableYesNo <- c() #zeroes = not applicable, ones = applicable
          for (game in 1:nrow(ChronoAllGames)) {
            NthGameDecision <- DynamicTossDecision(game)
            if (NthGameDecision == 1) {
              if (ChronoAllGames$toss_won[game] == ChronoAllGames$batting_first[game]) {
                ApplicableYesNo <- append(ApplicableYesNo, 1) 
              } else {
                ApplicableYesNo <- append(ApplicableYesNo, 0)
              }
            }
            if (NthGameDecision == 2) {
              if (ChronoAllGames$toss_won[game] != ChronoAllGames$batting_first[game]) {
                ApplicableYesNo <- append(ApplicableYesNo, 1)
              } else {
                ApplicableYesNo <- append(ApplicableYesNo, 0)
              }
            }
          }
          FullChronoAllGames <- cbind(ChronoAllGames, new_col = ApplicableYesNo)
          ApplicableChronoGames <- filter(FullChronoAllGames, ApplicableYesNo == 1)
          ApplicableSuccessVector <- c()
          GameVector <- c()
          ApplicableSuccess <- 0
          ApplicableFail <- 0
          Game <- 0
          for (game in 1:nrow(ApplicableChronoGames)) {
            Game <- Game + 1
            if (ApplicableChronoGames$toss_won[game] == ApplicableChronoGames$winner[game]) {
              ApplicableSuccess <- ApplicableSuccess + 1
            } else {
              ApplicableFail <- ApplicableFail + 1
            }
            GameVector <- append(GameVector, Game)
            ApplicableSuccessVector <- append(ApplicableSuccessVector, ApplicableSuccess / (ApplicableSuccess + ApplicableFail))
          }
          ApplicableSuccessRate <- ApplicableSuccess / (ApplicableSuccess + ApplicableFail)
          SuggesterAccuracy <- ApplicableSuccessRate
          #plot(GameVector, ApplicableSuccessVector,
          #xlab = "Games", ylab = "Suggester success rate")
          
          #SuccessThroughGames <- cbind(GameVector, new_col = ApplicableSuccessVector)
          #SuccessTrend <- ggplot(SuccessThroughGames, 
          #aes(x = SuccessThroughGames$Game, y = SuccessThroughGames$ApplicableSuccessVector)) + 
          #geom_point() + labs(x = "Games", y = "Suggester Success rate")
          
          print(SuggesterAccuracy)
          #suggester_accuracy_table$B_1 <- append(suggester_accuracy_table$B_1, weight_vector[1])
          #suggester_accuracy_table$B_2 <- append(suggester_accuracy_table$B_2, weight_vector[2])
          #suggester_accuracy_table$B_3 <- append(suggester_accuracy_table$B_3, weight_vector[3])
          #suggester_accuracy_table$B_4 <- append(suggester_accuracy_table$B_4, weight_vector[4])
          #suggester_accuracy_table$Accuracy <- append(suggester_accuracy_table$Accuracy, SuggesterAccuracy)
          return(SuggesterAccuracy)
}










