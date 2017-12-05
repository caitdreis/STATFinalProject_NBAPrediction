# STAT 6021: Final Project
# Caitlin Dreisbach: cnd2y
# Isabelle Liu: xl9qw
# Sai Prasanth: lbs7aa
# Boh Young Suh: bs6ea

#################################### Packages ###################################

library(chron)

############################### Data Preprocessing ##############################

# Read in data
chart13 <- read.csv("all_chart_2013.csv")
chart14 <- read.csv("all_chart_2014.csv")
shot13 <- read.csv("all_shots_2013.csv")
shot14 <- read.csv("all_shots_2014.csv")
salary13 <- read.csv("NBA Player Salaries 2013.csv")
salary14 <- read.csv("NBA Player Salaries 2014.csv")

# Merge the datasets from the same season together by player Id, game ID, and period
merge13 <- merge(chart13, shot13[, -1], by = c("PLAYER_ID", "GAME_ID", "PERIOD"))
# As the defense should be around the same time as the shot made,
# rows with the two time far apart from each other should be removed
# Also, the result of a shot should be the same
index <- c()
for (i in 1:nrow(merge13)){
  time1 <- merge13$MINUTES_REMAINING[i] * 60 + merge13$SECONDS_REMAINING[i]
  time2 <- c(as.matrix(read.table(text = toString(merge13$GAME_CLOCK[i]), sep = ":")) %*% c(60, 1))
  event_type <- tolower(toString(read.table(text = toString(merge13$EVENT_TYPE[i]), sep = " ")[[1]]))
  if (time2 < time1 || time2 > (time1 + 5) || merge13$SHOT_RESULT[i] != event_type){
    index <- c(index, i)
  }
}
merge13 <- merge13[-index,]
# Add a new column for player salary
merge13$PLAYER_SALARY <- 0
# Convert dollar values in salary dataset to numeric
salary13$SALARY <- as.numeric(gsub('[$,]', '', factor(salary13$SALARY)))
# For every player in the merged dataset, find his corresponding salary
for (i in 1:nrow(merge13)){
  for (j in 1:nrow(salary13)){
    name <- strsplit(toString(salary13$NAME[j]), ",")[[1]][1]
    if (identical(toString(merge13$PLAYER_NAME[i]), name)){
      merge13$PLAYER_SALARY[i] = salary13$SALARY[j]
    }
  }
}

# Merge the datasets from the same season together by player Id, game ID, and period
merge14 <- merge(chart14, shot14[, -1], by = c("PLAYER_ID", "GAME_ID", "PERIOD"))
# As the defense should be around the same time as the shot made,
# rows with the two time far apart from each other should be removed
# Also, the result of a shot should be the same
index <- c()
for (i in 1:nrow(merge14)){
  time1 <- merge14$MINUTES_REMAINING[i] * 60 + merge14$SECONDS_REMAINING[i]
  time2 <- c(as.matrix(read.table(text = toString(merge14$GAME_CLOCK[i]), sep = ":")) %*% c(60, 1))
  event_type <- tolower(toString(read.table(text = toString(merge14$EVENT_TYPE[i]), sep = " ")[[1]]))
  if (time2 < time1 || time2 > (time1 + 5) || merge14$SHOT_RESULT[i] != event_type){
    index <- c(index, i)
  }
}
merge14 <- merge14[-index,]
# Add a new column for player salary
merge14$PLAYER_SALARY <- 0
# Convert dollar values in salary dataset to numeric
salary14$SALARY <- as.numeric(gsub('[$,]', '', factor(salary14$SALARY)))
# For every player in the merged dataset, find his corresponding salary
for (i in 1:nrow(merge14)){
  for (j in 1:nrow(salary14)){
    name <- strsplit(toString(salary14$NAME[j]), ",")[[1]][1]
    if (identical(toString(merge14$PLAYER_NAME[i]), name)){
      merge14$PLAYER_SALARY[i] = salary14$SALARY[j]
    }
  }
}

# Join two datasets of season 2013 and 14 vertically
nba <- rbind(merge13, merge14)

# Remove all rows with missing values
nba <- nba[complete.cases(nba),]

# Characterize or Factoriaze categorical variables
nba$CLOSEST_DEFENDER <- as.character(nba$CLOSEST_DEFENDER)
nba$GAME_CLOCK <- as.character(nba$GAME_CLOCK) # categorical variable, not sure what to do
nba$FGM <- as.factor(nba$FGM) # originally not categorical but has only 3 values
nba$LOCATION <- as.factor(nba$LOCATION)
nba$MATCHUP <- as.character(nba$MATCHUP)
nba$PERIOD <- as.factor(nba$PERIOD) # originally not categorical but has only 7 values
nba$PTS <- as.factor(nba$PTS) # originally not categorical but has only 4 values
nba$PTS_TYPE <- as.factor(nba$PTS_TYPE) # originally not categorical but has only 2 values
nba$SHOT_RESULT <- as.factor(nba$SHOT_RESULT)
nba$W <- as.factor(nba$W)
nba$GRID_TYPE <- as.factor(nba$GRID_TYPE)
nba$PLAYER_NAME <- as.character(nba$PLAYER_NAME)
nba$EVENT_TYPE <- as.factor(nba$EVENT_TYPE)
nba$ACTION_TYPE <- as.factor(nba$ACTION_TYPE)
nba$SHOT_TYPE <- as.factor(nba$SHOT_TYPE)
nba$SHOT_ZONE_BASIC <- as.factor(nba$SHOT_ZONE_BASIC)
nba$SHOT_ZONE_AREA <- as.factor(nba$SHOT_ZONE_AREA)
nba$SHOT_ZONE_RANGE <- as.factor(nba$SHOT_ZONE_RANGE)
nba$SHOT_ATTEMPTED_FLAG <- as.factor(nba$SHOT_ATTEMPTED_FLAG) # originally not categorical but has only 1 value
nba$SHOT_MADE_FLAG <- as.factor(nba$SHOT_MADE_FLAG) # originally not categorical but has only 2 values

# Convert defender names from "last, first" to "first last"
nba$CLOSEST_DEFENDER <- sub("(\\w+),\\s(\\w+)","\\2 \\1", nba$CLOSEST_DEFENDER)

# Write to a csv file for easier use later
write.table(nba, file="nba.csv", row.names=F)

############################### Data exploration ################################

# Shot Results
shotfreq <- table(nba$SHOT_RESULT)
barplot(shotfreq, main = "Shot Results", xlab = "Shot Result", ylab = "No. of Shots", col = "darkblue")

# Mean Distance of both shot and defense
mean(nba$SHOT_DISTANCE) # 12.13123
mean(nba$CLOSE_DEF_DIST) # 4.139689
shot_def <- matrix(c(12.13123, 4.139689), ncol = 2, byrow = TRUE)
colnames(shot_def) <- c("Shot", "Close_Defense")
rownames(shot_def) <- c("Mean_Distance")
shot_def <- as.table(shot_def)
barplot(shot_def, ylab = "Mean Distance", col = "darkblue", beside=TRUE)

# Shot Counts vs. Period & Shot Result
prd_rslt <- table(nba$SHOT_RESULT, nba$PERIOD)
barplot(prd_rslt, main = "Shot Counts by Period and Result",
        xlab = "Period", ylab = "No. of Shots", col = c("darkblue", "red"),
        legend = rownames(prd_rslt), beside=TRUE)

# Shot Counts vs. Shot Types & Shot Results
type_rslt <- table(nba$SHOT_RESULT, nba$SHOT_TYPE)
barplot(type_rslt, main = "Shot Counts by Type and Result",
        xlab = "Shot Type", ylab = "No. of Shots", col = c("darkblue", "red"),
        legend = rownames(type_rslt), beside=TRUE)

# Shot Counts vs. Shot Zone Area & Shot Results
area_rslt <- table(nba$SHOT_RESULT, nba$SHOT_ZONE_AREA)
barplot(area_rslt, main = "Shot Counts by Zone Area and Result",
        xlab = "Shot Zone Area", ylab = "No. of Shots", col = c("darkblue", "red"),
        legend = rownames(area_rslt), beside=TRUE)

# Shot Counts vs. Shot Distance & Shot Results
dist_rslt <- table(nba$SHOT_RESULT, nba$SHOT_DISTANCE)
barplot(dist_rslt, main = "Shot Counts by Distance and Result",
        xlab = "Shot Distance", ylab = "No. of Shots", col = c("darkblue", "red"),
        legend = rownames(dist_rslt), beside=TRUE)


################################# Shot Analysis #################################

# Load dataset
nba <- read.csv("nba.csv")

# Drop columns that are not relevant to predicting shots made
nba$PLAYER_ID <- NULL 
nba$GAME_ID <- NULL 
nba$GRID_TYPE <- NULL
nba$GAME_EVENT_ID <- NULL
nba$TEAM_ID <- NULL 
nba$TEAM_NAME <- NULL 
nba$MINUTES_REMAINING <- NULL
nba$SECONDS_REMAINING <- NULL
nba$EVENT_TYPE <- NULL  
nba$SHOT_TYPE <- NULL  
nba$SHOT_ZONE_BASIC <- NULL
nba$SHOT_ZONE_RANGE <- NULL
nba$LOC_X <- NULL
nba$LOC_Y <- NULL
nba$CLOSEST_DEFENDER_PLAYER_ID <- NULL
nba$FGM <- NULL
nba$FINAL_MARGIN <- NULL
nba$GAME_CLOCK <- NULL 
nba$LOCATION <- NULL
nba$MATCHUP <- NULL
nba$SHOT_RESULT <- NULL
nba$W <- NULL
nba$PLAYER_SALARY <- NULL
nba$SHOT_DIST <- NULL

# Remove negative and 0 touch time values
nba <- subset(nba, nba$TOUCH_TIME > 0)

# Calculate block percentage for every closest defender
nba1 <- aggregate(nba$SHOT_MADE_FLAG, list(nba$CLOSEST_DEFENDER), sum)
nba1[,c(3,4)] <- aggregate(nba$SHOT_ATTEMPTED_FLAG, list(nba$CLOSEST_DEFENDER), sum)
nba1$Group.1.1 <- NULL

# Assign column name
colnames(nba1) <- c("CLOSEST_DEFENDER", "Total_Shot_Made", "Total_Shot_Attempted")

# Calculate block rate for each defender
nba1$Block_Rate <- 1- (nba1$Total_Shot_Made/nba1$Total_Shot_Attempted)

# Merge with original dataset
nba <- merge(nba, nba1, by = "CLOSEST_DEFENDER", all = T)

# No need for these two variables
nba$Total_Shot_Made <- NULL
nba$Total_Shot_Attempted <- NULL
nba$SHOT_ATTEMPTED_FLAG <- NULL

# Create total points scored for each player to use instead of each player name 
nba3 <- aggregate(nba$PTS, list(nba$PLAYER_NAME), sum)
colnames(nba3) <- c("PLAYER_NAME", "TOTAL_PTS_SCORED")

# Merge with original dataset
nba <- merge(nba, nba3, by = "PLAYER_NAME", all = T)

# Create total points conceded for each defender player
nba4 <- aggregate(nba$PTS, list(nba$CLOSEST_DEFENDER), sum)
colnames(nba4) <- c("CLOSEST_DEFENDER", "TOTAL_PTS_CONCEDED")

nba <- merge(nba, nba4, by = "CLOSEST_DEFENDER", all = T)

# Total_PTS scored will replace player_name to quantify each players ability to make a successful shot.

# Now remove player name and defender name
nba$PLAYER_NAME <- NULL
nba$CLOSEST_DEFENDER <- NULL
# pts variable is not needed anymore
nba$PTS <- NULL

# Characterize or Factorize categorical variables
nba$PERIOD <- as.factor(nba$PERIOD)
nba$ACTION_TYPE <- as.factor(nba$ACTION_TYPE)
nba$SHOT_ZONE_AREA <- as.factor(nba$SHOT_ZONE_AREA)
nba$SHOT_DISTANCE <- as.numeric(nba$SHOT_DISTANCE)
nba$SHOT_MADE_FLAG <- as.factor(nba$SHOT_MADE_FLAG)
nba$DRIBBLES <- as.numeric(nba$DRIBBLES)
nba$PTS_TYPE <- as.factor(nba$PTS_TYPE)

# Categorize player by total_points made
nba$player_rank <- 0
for (i in 1:nrow(nba)){
  if (nba$TOTAL_PTS_SCORED[i] < 600){
    nba$player_rank[i] <- 1
  } else if ((601 <= nba$TOTAL_PTS_SCORED[i]) && (nba$TOTAL_PTS_SCORED[i] < 1200)){
    nba$player_rank[i] <- 2
  } else if ((1201 <= nba$TOTAL_PTS_SCORED[i]) && (nba$TOTAL_PTS_SCORED[i] < 1800)){
    nba$player_rank[i] <- 3
  } else if ((1801 <= nba$TOTAL_PTS_SCORED[i]) && (nba$TOTAL_PTS_SCORED[i] < 2400)){
    nba$player_rank[i] <- 4
  } else{
    nba$player_rank[i] <- 5
  }
}

# Categorize defenders by total_points conceded
nba$defender_rank <- 9
for (i in 1:nrow(nba)){
  if (nba$TOTAL_PTS_CONCEDED[i] < 500){
    nba$defender_rank[i] <- 1
  } else if ((501 <= nba$TOTAL_PTS_CONCEDED[i]) && (nba$TOTAL_PTS_CONCEDED[i] < 1000)){
    nba$defender_rank[i] <- 2
  } else if ((1001 <= nba$TOTAL_PTS_CONCEDED[i]) && (nba$TOTAL_PTS_CONCEDED[i] < 1500)){
    nba$defender_rank[i] <- 3
  } else if ((1501 <= nba$TOTAL_PTS_CONCEDED[i]) && (nba$TOTAL_PTS_CONCEDED[i] < 2000)){
    nba$defender_rank[i] <- 4
  } else{
    nba$defender_rank[i] <- 5
  }
}

glm <- glm(SHOT_MADE_FLAG ~ .-player_rank - defender_rank, data = nba, family = binomial)
summary(glm) # AIC: 459658

# Now we can remove total_points and total_points_conceded
nba$TOTAL_PTS_SCORED <- NULL
nba$TOTAL_PTS_CONCEDED <- NULL

# Factorize new variables
nba$player_rank <- as.factor(nba$player_rank)
nba$defender_rank <- as.factor(nba$defender_rank)

# Run logistic regression model
glm <- glm(SHOT_MADE_FLAG ~ ., data = nba, family = binomial)
summary(glm) # AIC: 459642

# Multiply defender rank and block rate for a solid measure
nba$defence_lvl <- 0
nba$defender_rank <- as.numeric(nba$defender_rank)
for (i in 1:nrow(nba)){
  nba$defence_lvl[i] <- nba$Block_Rate[i] * nba$defender_rank[i]
}

# Run regression model once again
glm <- glm(SHOT_MADE_FLAG ~ ., data = nba, family = binomial)
# However, this does not improve the model and defense lvl is not significant. Thus, remove and stick to our previous model.

# Remove shot number variable that is not significant
nba$defence_lvl <- NULL
nba$defender_rank <- as.factor(nba$defender_rank)

glm <- glm(SHOT_MADE_FLAG ~ . - SHOT_NUMBER, data = nba, family = binomial)
summary(glm) # AIC: 459643

# Finding the best model
nba.null <- glm(SHOT_MADE_FLAG ~1, data=nba, family = binomial)
nba.full <- glm(SHOT_MADE_FLAG ~.-SHOT_NUMBER, data=nba, family = binomial)

nba.bs <- step(nba.full, scope=list(lower=nba.null, upper=nba.full), direction="both", trace = FALSE)

summary(nba.bs)
anova(nba.bs)
# Not a big difference, all variables are useful to explain what attributes to a successful shot.

### results ###

# In terms of periods, period 5 was critical in that every shot made in this period was more likely to be missed compared to other period with -0.143 coefficient.
# This ties to the fact that period 5 is overtime and normally players can get exhausted.
# For action type, jump shot normally tend to lower your chance of a successful shot. Interestingly we also noticed that tip shot was relatively hard to make it in with -3 coefficient. 
# Many tip shots have tough body contact in the center so we can assume why it might be hard to make when it relatively looks easy to make a basket when just observed.
# In our analysis shot zone area did not showed a big difference in shot prediction
# shot clock did not really matter as well. As we see in this chart that many shots were made successful right after possession of the ball.
# The most important variable was what the player rank was and the defender rank along with his block rate.


################################ Player's Salary ################################

nba <- read.csv("nba.csv")

nba$PLAYER_NAME <- as.factor(nba$PLAYER_NAME)

unique(nba$PLAYER_NAME) # 572 players

unique(nba$PLAYER_ID) # can remove this column
nba$PLAYER_ID <- NULL

unique(nba$GAME_ID) # can remove this column
nba$GAME_ID <- NULL

unique(nba$PERIOD) # if this more than 4, that means over time
# we need the period column

unique(nba$GRID_TYPE) # can remove this column
nba$GRID_TYPE <- NULL

unique(nba$GAME_EVENT_ID) # can remove this column
nba$GAME_EVENT_ID <- NULL

unique(nba$TEAM_ID) # can remove this column
nba$TEAM_ID <- NULL

unique(nba$TEAM_NAME)
# The charlotte bobcats changed their name to charlotte hornets
nba$TEAM_NAME[nba$TEAM_NAME == "Charlotte Bobcats"]  <- "Charlotte Hornets"
unique(nba$TEAM_NAME)
nba$TEAM_NAME <- as.factor(nba$TEAM_NAME)  

unique(nba$EVENT_TYPE)  # we have points columns in the end. We don't need many such variables
nba$EVENT_TYPE <- NULL  

unique(nba$SHOT_TYPE) 
nba$SHOT_TYPE <- NULL  

unique(nba$SHOT_ZONE_BASIC)
nba$SHOT_ZONE_BASIC <- NULL

unique(nba$SHOT_ZONE_RANGE)
nba$SHOT_ZONE_RANGE <- NULL

unique(nba$SHOT_DIST)
nba$SHOT_DIST <- NULL # we intend to work with SHOT_DISTANCE (both the columns are nearly the same)

nba$LOC_X <- NULL
nba$LOC_Y <- NULL

nba$SHOT_ATTEMPTED_FLAG <- NULL # obviously every shot was attempted, no matter successful or not

nba$SHOT_MADE_FLAG <- NULL

nba$CLOSEST_DEFENDER_PLAYER_ID <- NULL # we have the names

nba$FGM <- NULL

nba$GAME_CLOCK <- NULL # we already have minutes and seconds

nba$LOCATION <- NULL # we have better variables

unique(nba$SHOT_RESULT) # we already removed event ID

# I see that some of the touch time's are negative. That's not possible. I am going to delete them
nba <- subset(nba, nba$TOUCH_TIME >= 0)

nba$W # let me keep this
nba$FINAL_MARGIN # let me keep this as well

# The team name has 30 levels. We need to do something about this. There are about 500 players and one of columns has 30 levels
# Our team decided to use a winning percentage for each team as one of the predictors

nba$win <- ifelse(nba$W == "W", 1, 0)

Team_winning_perc <- aggregate(nba$win*100, list(nba$TEAM_NAME), mean)
# I am going to use team's winning percentage as one of the predictors

unique(nba$PLAYER_SALARY) # A lot of players salaries changed between 2013 and 2014 actually

missing_salary_df <- (subset(nba, nba$PLAYER_SALARY == 0 ))
unique(nba$PLAYER_NAME) 
# total of 572 players in the data set
missing_salary_df$PLAYER_NAME <- as.factor(missing_salary_df$PLAYER_NAME)
unique(missing_salary_df$PLAYER_NAME) 
# We notice that we don't have salary for 168 players
# We have some salaries for 2014 but not 2013

# Interesting that we don't have salary for 30% of players but their names are present in just 10% of the rows. 
# This confirms our suspicion that we only have salary for the top players

nba <- subset(nba, !(nba$PLAYER_SALARY == 0))
unique(as.factor(nba$PLAYER_NAME))

# Let me create a composite time column
nba$time <- nba$MINUTES_REMAINING + (nba$SECONDS_REMAINING)/60

# I want 3 min 15 sec to be 3.25
nba$MINUTES_REMAINING <- NULL
nba$SECONDS_REMAINING <- NULL

# Actually, let me add period into time info as well
# I won't be creating separate over time features. This is because a little over 10 games went into the data set. Not enough data to study the effect of over time points on salary
nba_regular <- subset(nba, nba$PERIOD < 5)
nba_overtime <- subset(nba, nba$PERIOD > 5)
nba_regular$TIME_OF_GAME <-  nba_regular$PERIOD*12 - nba_regular$time
nba_overtime$TIME_OF_GAME <- 48 + nba_overtime$PERIOD*5 - nba_overtime$time
nba <- rbind(nba_regular, nba_overtime)
nba$time <- NULL
nba$PERIOD <- NULL

# I want to create a separate variable that tracks close games later
# creating a final data set for salary regression
# I don't care how players score. Get me the points
nba$PLAYER_NAME <- as.factor(nba$PLAYER_NAME)
xcv <-  nba[row.names(unique(nba[,c("PLAYER_NAME", "TEAM_NAME")])),]
sdf <- xcv[!duplicated(xcv$PLAYER_NAME), ]
NAME_TEAM_SAL <- sdf[,c(1,2,17)] 

# I am ultimately going to join this table with the player aggregate of all other predictors I am going to calculate next
nba$TEAM_NAME <- NULL #i don't need this anymore

# I am going to delete all these how the player shot variables. Get me the points. That's all that matters
nba$TEAM_NAME <- NULL
nba$ACTION_TYPE <- NULL
nba$SHOT_ZONE_AREA <- NULL
nba$SHOT_DISTANCE <- NULL
nba$CLOSEST_DEFENDER # I need to calculate something like a block rate. How effective they are in blocking

# I am going to add these rows to NAME_TEAM_SAL
wer <- aggregate(nba$PTS, list(nba$CLOSEST_DEFENDER), sum) # which defenders conceded most points
nrow(wer)

# I just realized there are defenders that have never taken a shot. We joined based on PLAYER_NAME but that column is recorded only if he at least shot once
wer[, c(3,4)] <- aggregate(nba$PTS_TYPE, list(nba$CLOSEST_DEFENDER), sum)

# I am sure we can assume the closest defender to be in the process of defending. It is rare in basketball for a shooter to shoot freely without any disturbance from defender
wer$PERCENTAGE_BLOCKED <- wer[, 2]*100/wer[, 4]
DEFENDERS <- subset(wer, unique(as.factor(wer$Group.1)) %in% unique(as.factor(NAME_TEAM_SAL$PLAYER_NAME)))
DEFENDERS$Group.1.1 <- NULL
colnames(DEFENDERS) <- c("PLAYER_NAME", "POINTS_CONCEDED", "POINTS_ATTEMPTED_AGAINST", "PERCENTAGE_BLOCKED")
NAME_TEAM_SAL <-  merge(x = NAME_TEAM_SAL, y = DEFENDERS, by = "PLAYER_NAME", all.x = TRUE)

# We see that there are shooters who have never absolutely defended in the entire data set
NAME_TEAM_SAL[is.na(NAME_TEAM_SAL)] <- 0
NAME_TEAM_SAL$dribbles <- aggregate(nba$DRIBBLES, list(nba$PLAYER_NAME), mean)[,2]
NAME_TEAM_SAL$total_points <- aggregate(nba$PTS, list(nba$PLAYER_NAME), sum)[,2]
NAME_TEAM_SAL$points_attempted <- aggregate(nba$PTS_TYPE, list(nba$PLAYER_NAME), sum)[,2]
NAME_TEAM_SAL$accuracy <- NAME_TEAM_SAL[, 8]*100/NAME_TEAM_SAL[, 9]
NAME_TEAM_SAL$points_attempted <- NULL
NAME_TEAM_SAL$time <- aggregate(nba$TIME_OF_GAME, list(nba$PLAYER_NAME), mean)[,2]
NAME_TEAM_SAL$POINTS_ATTEMPTED_AGAINST <- NULL # I already have percentage blocked
colnames(Team_winning_perc) <- c("TEAM_NAME", "Winning_percentage")
NAME_TEAM_SAL <- merge(x = NAME_TEAM_SAL, y = Team_winning_perc, by = "TEAM_NAME", all.x = TRUE)

# I am going to remove Team name now
NAME_TEAM_SAL$TEAM_NAME <- NULL
colSums(is.na(NAME_TEAM_SAL)) # no NA's
final_data <- NAME_TEAM_SAL

# final preparation
final_data$PLAYER_NAME <- as.factor(final_data$PLAYER_NAME)
final_data$salary <- final_data$PLAYER_SALARY # I just like my response variable to be last columns
final_data$PLAYER_SALARY <- NULL
colnames(final_data) <- c("name", "total_points_conceded", "percentage_blocked", "dribbles", "total_points_scored", "shooting_accuracy", "avg_time_shot", "team_winning_percentage", "salary")

# Finally we have our data set. I am trying to explain salary with 
# Of course, there's the player name column which I am going to leave out before regression

# 1. Total points scored
# 2. Shooting accuracy
# 3. Team's winning percentage
# 4. Average number of dribbles
# 5. Total points he conceded as a defender (other team's players were able to score when he was defending)
# 6. Percentage of points he successfully blocked

# Modeling
cor(final_data[, c(2:9)])

# We see a correlation of 0.83 between total points scored and total points conceded. We checked the joins
# What this means is that the players who score the most are often the ones who don't know how to defend. 
# So, opponents score lot of points when they are the closest defender
model1 <- lm(salary ~ .-name, data = final_data)
summary(model1)
# The most shocking result is this: for every minute you delay your shooting in the game, your annual salary drops by $150K!
# This confirms our suspicion that early shots matter more. 
# This is because we want players to score early and psychologically hurt other the team. 
# It doesn't matter if the players score towards the end (after the end match result is settled anyway)

# Backwards to look at the best model
b1.null <- lm(salary ~1, data=final_data)
b1.full <- lm(salary ~.-name , data=final_data)
b1.bs <- step(b1.full, scope=list(lower=b1.null, upper=b1.full), direction="backward")
# Looking at the results and the drops in AIC, it is clear that only 2 things matter: How many points did you score and when did you score them

# Best model stuff
install.packages("leaps")
library(leaps)
b1b.leaps <- regsubsets(salary ~. -name, data=final_data, nbest=10)

best.sum <- as.data.frame(summary(b1b.leaps)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

best.sum$mse <- (summary(b1b.leaps)$rss)/(nrow(final_data) - 2)
best.sum$adjr2 <- summary(b1b.leaps)$adjr2
best.sum$cp <- summary(b1b.leaps)$cp

# Best model according to MSE:
best.sum[order(best.sum$mse),]

# Once again, the MSE drops slightly if I include few other variables beyond points_scored and time, but the drop is really minimal
# Best model according to Adj. R-squared:
best.sum[order(best.sum$adjr2),]

# Even by R square, the best model is literally the one with only points scored and time
# Best model according to Cp:
best.sum[order(best.sum$cp),]
# Same in case of Cp as well. As a matter of fact, Cp drastially goes up if you add anything beyond points scored and time

# We have an absolute consensus: total points scored and avg. time when scored
model2 <- lm(salary ~ avg_time_shot + total_points_scored, data = final_data)
summary(model2)
#  Coefficients:
#                  Estimate Std.      Error t value Pr(>|t|)    
#  (Intercept)         4934065.9  1177604.8   4.190 3.33e-05 ***
#  avg_time_shot       -140391.0    40944.9  -3.429 0.000659 ***
#  total_points_scored    3752.7      305.5  12.282  < 2e-16 ***

# Data has spoken

# Residual plots
qqnorm(rstudent(model2))
qqline(rstudent(model2))

# There is a point beyond 6 standard deviation
# What we see in the plot is that there are bunch of guys like Kobe Bryant who are right up top. 
# Obviously Kobe Bryant's salary cannot be just explained by how many points he scored in 2013
# Some of these players are almost like brands, great crowd pullers. Points alone don't explain their outsized salaries
# On the other hand, we see that there are 6 players in the data set who make less than $45K. 
# there are quite a few players who make $45K - $75k range
# We have decided to exclude these 6 players 

# Before we plot perform anymore residual analysis, we think it is appropriate at this point to remove certain rows
final_data <- final_data[order(final_data$salary),]
final_data_cut <- final_data[8:nrow(final_data)-1, ]
model3 <- lm(salary ~. -name, data = final_data_cut)
summary(model3)  

# Once again, it's total points and time  
qqnorm(rstudent(model3))
qqline(rstudent(model3))

# Still can't sufficiently explain the top player salaries. there is only thing that can be done now
library(MASS)
boxcox(model3)

# What the box cox seems to be telling us is that we should transform the salary. So the relationship is not exactly linear
# This again makes sense to us. There are not many players who are right up top. Since a team can field only 5 players, teams will often be ready to pay outsized salaries for top performers. 
# One $15 million player is more effective than 5 $3 million players. Only 5 players on pitch! 
# In life, execellence beyond a point is exponential
final_data_cut$salary_modified <- (final_data_cut$salary)^0.3
model4 <- lm(salary_modified ~. -name - salary, data = final_data_cut)
model4 <- lm(salary_modified ~ total_points_scored + avg_time_shot, data = final_data_cut)
summary(model4) # R square of 0.45

# Once again, its the same old total points scored and avg. time of shot
# We lost interpretability now, but just to repeat our previous findings for a non transformed model,
# Every point scored improves annual salary by $3.7K
# Delaying shooting by a minute reduces player salary by $140K

# Residual plots
qqnorm(rstudent(model4))
qqline(rstudent(model4))

# Look much better than earlier
Predicted_values = predict(model4, newdata = final_data_cut)
standardized = rstandard(model4)
plot(Predicted_values, standardized)

# Pretty good
plot(final_data_cut$total_points_scored, standardized)
plot(final_data_cut$avg_time_shot, standardized) 
# We see some concentration of residuals but they are equally distributed around 0, no pattern and most of them are between -2 and 2

# Some more analysis
cor(final_data_cut$salary, final_data_cut$team_winning_percentage)

# It's official. Which team you play doesn't matter much
final_data_cut$contribution <- final_data_cut$total_points_scored - final_data_cut$total_points_conceded

# How many points did you contribute to your team
model5 <- lm(final_data_cut$salary_modified ~. -name -salary -total_points_scored, data = final_data_cut)
summary(model5) # We don't really get any higher R square

# The reason is obvious
# Total points conceded is highly correlated with total points scored anyway
# who are the most underpaid and overpaid players? Lets see
# Please note, that the top players cannot be explained by points alone
# At the same time, salaries on the low end cannot be explained by points alone either. This is because teams need to back ups in case main players get injured. It's like insurance. Their salary cannot be explained by points alone either

final_data <- final_data[order(final_data$salary),]
predictions <- as.data.frame((predict(model4, newdata = final_data))^3.33333)
summary_data <- cbind(final_data, predictions)
max(predictions$`(predict(model4, newdata = final_data))^3.33333`)
min(predictions$`(predict(model4, newdata = final_data))^3.33333`)  
summary_data$diff <- summary_data$`(predict(model4, newdata = final_data))^3.33333` - summary_data$salary
summary_data[order(summary_data$diff, decreasing = TRUE),]$name[1:10]

# Most underpaid
#[1] Klay Thompson    Damian Lillard   Chandler Parsons Nikola Vucevic   Kenneth Faried   Andre Drummond  
#[7] Markieff Morris  Stephen Curry    Reggie Jackson   Avery Bradley

# Most overpaid
summary_data[order(summary_data$diff),]$name[1:10]
#[1] Kobe Bryant       Amar'e Stoudemire Dwight Howard     Derrick Rose      Joe Johnson       Deron Williams   
# [7] Dirk Nowitzki     Carmelo Anthony   Dwyane Wade       Chris Bosh 

# But what if i did the underpiad - overpaid analysis only for player that had salaries between $200,000K and $15 Million? The reliable range
summary_data_reliable <- subset(summary_data, summary_data$salary > 200000 & summary_data$salary < 15000000)
summary_data_reliable[order(summary_data_reliable$diff, decreasing = TRUE),]$name[1:10]

# Most underpaid players
#[1] Klay Thompson    Damian Lillard   Chandler Parsons Nikola Vucevic   Kenneth Faried   Andre Drummond  
#[7] Markieff Morris  Stephen Curry    Reggie Jackson   Avery Bradley 

summary_data_reliable[order(summary_data_reliable$diff),]$name[1:10]
# Most overpaid players
#[1] Kevin Garnett    Tyson Chandler   Andrew Bogut     Andre Iguodala   JaVale McGee     Josh Smith      
#[7] Kris Humphries   Roy Hibbert      Rajon Rondo      Danilo Gallinari

### Key Conclusions ###

# Only 2 variables explain salary: Total points scored and average time of shot (R square of 0.43)
# Approximately, every point scored improves annual salary by $3.7K. Delaying shooting by a minute reduces player annaul salary by $140K
# Good shooters don't know how to defend. Hence, opposition often tends to score points when the closest defender is a good attacker
# However, ability to defend doesn't matter. Total points conceded or blocking accuracy don't affect player salary
# Interestingly, shooting accuracy doesn't matter either. Only total points scored
# How good a team plays doesn't affect players salary
