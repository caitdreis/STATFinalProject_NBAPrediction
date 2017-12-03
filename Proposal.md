## Proposal

# Introduction 
The National Basketball Association (NBA) grossed almost five billion dollars in total revenue in the 2013-2014 season.1 A component of the steady increase in revenue has been the advanced tracking of players and ball possession during games through a technological service called SportVu. SportVu was first introduced in a small subset of NBA teams and rapidly expanded to cover all 30 teams during the 2013-2014 season.2 The SportVu data includes detailed information about the exact location of the ball on the court assessed 25 times per second resulting in over a million entries to the database per game.2 This advanced tracking has been cataloged and made available for public investigation.

# Approach & Objectives 
The proposed project includes the following research questions:

What are the factors that are most predictive of a successful shot? Statistical approach: significance testing of predictor variables, model testing across linear methods.

In the assessment of player salary based on shot prediction, can we identify players that are over or underpaid for their performance? Statistical approach: significance testing of predictor variables that are strengthen a model for predicting salary based on performance.

# Model Construction 
With an analytic focus on linear methodology, model testing will focus on simple linear regression as well as multilinear regression, least squares regression, polynomial and lasso methods. Assessment of the plotted residuals and mean square error will help to define the model of best fit. To avoiding overfitting the data to too many data points, we will attempt to use a more flexible type of linear model. We intend our model to be interpretable placing a strong value on some restriction in the model.

# Datasets 
See table below for information regarding the proposed datasets. The four major datasets representing shot and game information for 2013 and 2014 seasons will be merged and cleaned as needed. The merged dataset will include over 200,000 observations. Cleaning the data will include removing any missing values (either through imputation or deletion) and validating the merge with additional data. During the assembly of our datasets, we intend to explore available data to supplement information including revenue data tied to specific games or fan support for a given player or team based on, for example, game winning shots. Furthermore, as we continue our research if we find relevant factors to our predictions we plan to expand our dataset.

All_chart_2013.csv,all_chart_2014.csv https://github.com/hwchase17/sportvu Details about each shot (example: position on the court, type of shot, team member)

All_shots_2013.csv, all_shots_2014.csv https://github.com/hwchase17/sportvu Details about the game (example: location, date, time clock information, name of defender)

NBA Player Salaries http://www.espn.com/nba/salaries/_/year/2014/seasontype/1 NBA player salaries for 2013-2014 seasons

Potential Supplemental Data for Additional Exploration NBA Revenue https://www.statista.com/statistics/193467/total-league-revenue-of-the-nba-since-2005/ 2001-2016 seasons

NBA players, tattoo https://github.com/fivethirtyeight/data/blob/master/nba-tattoos/nba-tattoos-data.csv Whether or not they have tatoos ( 0 - No, 1 - Yes)

General NBA data http://stats.nba.com/ Web scrape this using R or python

# Level of Difficulty 
The difficulty in using this compilation of datasets includes significant merging and alignment of various datasets to create a robust CSV file representing the necessary parameters. Because our team intends to expand beyond the play statistics, we intend to scrape the official NBA page to get additional data as needed.

# Analysis Workflow and Expected Results 
After appropriate cleaning of the data, exploratory analysis will be done to understand important variables within the dataset. Plotting of predictor variables against our response variable will be important in understanding relationships in the data. We intend to create and test many different models to optimize each research question separately.

# Conclusion 
Through this analysis, we intend to learn more about the factors that contribute to making accurate predictions regarding a successful shot in the NBA. Possession of the ball and ability to successfully make a point-earning shot at the basket is extremely lucrative for NBA teams. Understanding how these factors could, in the future, be controlled, monitored and altered in real-time could drastically increase viewership and revenue for the NBA.

# References 
[1]. Statista. Total NBA league revenue from 2001/02 to 2015/16 (in billion U.S. dollars). Retrieved from https://www.statista.com/statistics/193467/total-league- revenue-of-the-nba-since-2005/. October 15, 2017.

[2]. NBAStuffer. SportVu Data. Retrieved from https://www.nbastuffer.com/analytics101 /sportvu-data/. October 15, 2017.
