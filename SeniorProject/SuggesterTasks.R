
#1. We want to find the number of times the winning team has won batting first and bowling first at all games at a given ground (done) (in percentage format) 
#2. Out of all the games that team A won, what percentage of games were won batting first
#3. Between Team A and Team B, out of all the games that team A won, what percentage of those wins were batting first?
#4. Between Team A and Team B on Ground X, out of all the games that team A won, what percentage of those wins were batting first? 



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


