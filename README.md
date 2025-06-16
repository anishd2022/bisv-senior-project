# 🎯 BISV Senior Project — Cricket Game Predictor & Toss Decision Suggester

An interactive RShiny application that predicts the outcome of cricket games played on [CricClubs.com](https://cricclubs.com) and provides strategic toss decision suggestions based on historical data.

---

## 🧠 Overview

This project combines data analysis, machine learning, and sports strategy to help teams make informed decisions before a game.

- **Data Source**: Extracted directly from the CricClubs database using SQL.
- **Analysis**: Performed in R, culminating in an RShiny application.
- **Functionality**: Given two teams, a location, and selected players, the app:
  - Predicts the **likely winner** of the game.
  - Suggests whether the toss-winning team should **bat or bowl first**.

---

## 🔮 Predictor Module

Uses machine learning models to forecast the winner based on:

- Team win percentages  
- Home field advantage  
- Player statistics  
- Head-to-head performance

**Modeling Approach**:
- Primary model: **Support Vector Machine (SVM)**
  - User-adjustable kernel and cost parameters through the app
- Also experimented with: **Logistic Regression** and **Gradient Boosting**
- Achieved a **74% prediction accuracy**

---

## 🧩 Suggester Module

Recommends toss decisions using historical game outcomes:

- Categorizes past games into:
  - *Lost toss, lost game*
  - *Won toss, lost game*
  - *Won toss, won game*
  - *Lost toss, won game*

Given the selected team, opponent, and location, a classification algorithm suggests whether to bat or bowl first if the team wins the toss.

---

## 📁 File Structure

- `SeniorProject/Code/app.R` — Launches the interactive RShiny app on local machine 
- `SeniorProject/SeniorProjectFinalPresentation.pdf` — Summary of methodology and results  
- `SeniorProject/data/` — Contains player and match statistics used for modeling

---

## ▶️ How to Run

### 🔧 Step 1: Set up the R Environment

This project uses [`renv`](https://rstudio.github.io/renv/) to manage dependencies. To recreate the required package environment:

1. Open the project in **RStudio** (or set the working directory to the project folder in R).
2. In the R console, run:
   ```r
   renv::restore()
   ```
This will install the exact versions of all required packages listed in the `renv.lock` file.

If you don't have `renv` installed, first run:
  ```r
  install.packages("renv")
  ```

### 🚀 Step 2: Launch the App

Once the environment is restored:

- Open `app.R` and click **Run App** in RStudio **or** run the following in R console:
  ```r
  shiny::runApp()
  ```

Check out a video demo of the app in action here:  
📽️ **[Senior Project App Demo](https://anishdeshpande.com/Images_and_Attachments/SeniorProjectAppDemo.mp4)**

---

## 📌 Acknowledgments

This project was developed as part of the BISV Senior Project under guidance from external advisor Mr. Prakash Giri, and internal faculty advisor Mr. Cruz.
