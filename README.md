# bisv-senior-project

Suggester and Predictor for cricket games scored on CricClubs.com

- Data was extracted from the CricClubs database using SQL, and then further analysis was done in R
- The final result is an RShiny App which takes in inputs of two teams, game location, and players, and outputs a prediction of which team will win, as well as a suggestion as to what each team should do if they win the toss (either bat first or bowl first). 

## Predictor:

- Looks at past game data such as team win percentages, home field advantage, player strengths, and head to head record to predict the winner of the game
- A support vector machine (SVM) model was used, and the RShiny app allows the user to tune the kernel and cost parameters
- I also experimented with Logistic Regression and Gradient Boosting methods
- Prediction accuracy reached 74% 

## Suggester:

- groups teams into "lost toss and lost game", "won toss and lost game", "won toss and won game", and "lost toss and won game" for each game, and then uses a classification algorithm to suggest teams to either bat or bowl first if they win the toss, given game location, team, and opponent.

## Files:

- running **app.R** will open up an Rshiny web app where the user can select various inputs to get predictions and suggestions on a game
- **SeniorProjectFinalPresentation.pdf** gives an overview of my project and my process
- All player statistics and game stats are in the data folder
