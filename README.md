# How Accurate Are Prediction Markets?

> This project examines the accuracy of prediction markets as compared with polls in the context of the 2016 and 2020 Democratic primary elections.

### Background

In recent years, prediction markets like PredictIt.org have become popular ways of betting on the outcomes of political events. These markets allow you to buy a contract related to a specific possible outcome, and if that outcome actually happens, your contract can be redeemed for a dollar. For example, you could buy a contract for __ cents that you think ______ will win an election, and if they actually do, the website will pay you a dollar. As more people want to buy contracts for a specific outcome that they see as more likely, the higher the price gets, making the market price a valuable prediction tool.

Some studies have suggested that these markets, which often have hundreds of thousands of contracts sold per day, may be better predictors of political outcomes than actual polls. The purpose of this project is to examine the predictive capacity of prediction markets as compared with polls in the context of the 2016 and 2020 Democratic primary elections.

### Data Sources

The polling data used in this project is from [FiveThirtyEight's estimated polling average](https://github.com/fivethirtyeight/data/tree/master/polls) for each candidate. The prediction market data is from [PredictIt.org](predictit.org), and the daily closing market price will be used as the default value. Election results are from the [New York Times](https://www.nytimes.com/news-event/2020-election) and various state websites. Candidates were included in the data set only if they had sufficient market, polling, and election results data for a specific state. States were only included if there was more than one remaining candidate with enough data. As a result, only 59 primaries and caucuses from the two election years are included in the data.

### Features

#### Shiny App

The final result of this project is a Shiny app which can be found [here](https://bchapuis.shinyapps.io/prediction_markets/). The code for this app is included in the shiny_app folder.

#### Final Data

The final data set used to create the plots and tables shown in the Shiny app can be found in the final-election-data folder as "dem_primary.csv". This CSV file contains polling averages, prediction market prices, and election results for major candidates in 59 primary elections and caucuses in 2016 and 2020. Some states and candidates have data going back months before the election, while others only have a few days.

#### Data Processing

All the original data used to create this master file can be found in the processing folder above, where it is divided into two subfolders based on year. The code used to compile this code into the final "dem_primary.csv" data set is also included in the processing folder as an RMD file. This file contains the functions used to scrape, clean, and compile the data from the different sources used.

#### Analysis

All of the data analysis included in the Shiny app can also be found in the analysis folder. The RMD file in this folder shows all the code that was used to analyze the data and create the plots and tables that are shown in the Shiny app. 

### Authors
- **Brendan Chapuis** - [LinkedIn](https://www.linkedin.com/in/brendan-chapuis-341703160/)
