library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(csvread)
library(cowplot)
library(e1071)
library(modeldata)
library(xgboost)
library(Matrix)

#main dataframe which stores all games, date played, teams playing, team that won the toss, team that batted first, and team that won the game
AllGames <- read_excel("/Users/anishdeshpande/projects/SeniorProject/AllGames.xls")
SortedAllGames <- arrange(AllGames, date)
ChronoAllGames <- filter(SortedAllGames, winner != 0)

PlayerStats <- read.csv("/Users/anishdeshpande/projects/SeniorProject/PlayerStats.csv")
ChronoPlayerStats <- arrange(PlayerStats, match_date)

GameStats <- read.csv("/Users/anishdeshpande/projects/SeniorProject/GameStatistics.csv")
ChronoGameStats <- arrange(GameStats, date)

# #Attempting to create an XGboost model:
# #first run "predictor tasks" script and then run this script...
# Features <- logisticsdata %>% select(-team_one_win)
# params <- list (set.seed = 1502,
#                 eval_metric = "auc",
#                 objective = "binary:logistic")
# #running XGboost:
# XGBOOSTmodel <- xgboost(data = as.matrix(Features),
#                         label = WinLossVector,
#                         params = params,
#                         nrounds = 25,
#                         verbose = 1)
# #shap values
# xgb.plot.shap(data = as.matrix(Features),
#               model = XGBOOSTmodel,
#               top_n = 5)

# #xgboost model:
# #partition data into training and testing set
# set.seed(1234)
# ind <- sample (2, nrow(logisticsdata), replace = T, prob = c(0.70, 0.30))
# train <- logisticsdata[ind == 1, ]
# test <- logisticsdata[ind == 2, ]
# 
# #Create matrix:
# trainm <- as.matrix(train)
# train_label <- train[ ,"team_one_win"]
# train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

#Watch this video for SVM(Support Vector Machine)--> https://www.youtube.com/watch?v=ueKqDlMxueE
#Watch this video for XGboost --> https://www.youtube.com/watch?v=gKyUucJwD8U
#Also this video to understand how XGboost works--> https://www.youtube.com/watch?v=3CC4N4z3GJc&t=0s

