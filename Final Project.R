# STAT 6021: Final Project
# Caitlin Dreisbach: cnd2y
# Isabelle Liu: xl9qw
# Sai Prasanth: lbs7aa
# Boh Young Suh: bs6ea

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

# Read in joined dataset for 2013
joined_shot13 <- read.csv("joined_shots_2013.csv")
# Add a new column for player salary
joined_shot13$PLAYER_SALARY <- 0
# Convert dollar values in salary dataset to numeric
salary13$SALARY <- as.numeric(gsub('[$,]', '', factor(salary13$SALARY)))
# For every player in the joined dataset, find his corresponding salary
for (i in 1:nrow(joined_shot13)){
  for (j in 1:nrow(salary13)){
    name <- strsplit(toString(salary13$NAME[j]), ",")[[1]][1]
    if (identical(toString(joined_shot13$PLAYER_NAME[i]), name)){
      joined_shot13$PLAYER_SALARY[i] = salary13$SALARY[j]
    }
  }
}

# Read in joined dataset for 2014
joined_shot14 <- read.csv("joined_shots_2014.csv")
# Add a new column for player salary
joined_shot14$PLAYER_SALARY <- 0
# Convert dollar values in salary dataset to numeric
salary14$SALARY <- as.numeric(gsub('[$,]', '', factor(salary14$SALARY)))
# For every player in the joined dataset, find his corresponding salary
for (i in 1:nrow(joined_shot14)){
  for (j in 1:nrow(salary14)){
    name <- strsplit(toString(salary14$NAME[j]), ",")[[1]][1]
    if (identical(toString(joined_shot14$PLAYER_NAME[i]), name)){
      joined_shot14$PLAYER_SALARY[i] = salary14$SALARY[j]
    }
  }
}

# Join two datasets of season 2013 and 14 vertically
nba <- rbind(joined_shot13, joined_shot14)
# Remove duplicated columns
nba <- nba[, -c(1, 10, 14, 26)]
# Remove all rows with missing values
nba <- nba[complete.cases(nba),] # Final dataset with no NAs.

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

############################# Further Data Cleaning #############################

unique(nba$PLAYER_ID) # we have player name, can remove this column
nba$PLAYER_ID <- NULL

unique(nba$GAME_ID) # can remove this column
nba$GAME_ID <- NULL

unique(nba$PERIOD) # if this more than 4, that means over time
# we need the period column

unique(nba$GRID_TYPE) # can remove this column
nba$GRID_TYPE <- NULL

unique(nba$GAME_EVENT_ID) # can remove this column
nba$GAME_EVENT_ID <- NULL

unique(nba$TEAM_ID) # we have team name, can remove this column
nba$TEAM_ID <- NULL

unique(nba$TEAM_NAME)
# oh, the charlotte bobcats changed their name to charlotte hornets
nba$TEAM_NAME[nba$TEAM_NAME == "Charlotte Bobcats"] <- "Charlotte Hornets"
unique(nba$TEAM_NAME)
nba$TEAM_NAME <- factor(nba$TEAM_NAME)

unique(nba$EVENT_TYPE) # we have points in the end. We don't need many similar variables
nba$EVENT_TYPE <- NULL

unique(nba$SHOT_TYPE) # the same as PTS_TYPE
nba$SHOT_TYPE <- NULL  

unique(nba$SHOT_ZONE_BASIC) # similar to shot zone area
nba$SHOT_ZONE_BASIC <- NULL

unique(nba$SHOT_ZONE_RANGE) # similar to shot zone area
nba$SHOT_ZONE_RANGE <- NULL

unique(nba$SHOT_DIST) # the same as SHOT_DISTANCE
nba$SHOT_DIST <- NULL

nba$LOC_X <- NULL
nba$LOC_Y <- NULL # more specific information about shot distance, but does not matter as much

nba$SHOT_ATTEMPTED_FLAG <- NULL # obviously every shot was attempted, no matter made or missed

nba$SHOT_RESULT <- NULL # the same as shot made flag

nba$CLOSEST_DEFENDER_PLAYER_ID <- NULL # we have the name

nba$FGM <- NULL

nba$FINAL_MARGIN <- NULL # we are looking at row level

nba$GAME_CLOCK <- NULL # we already have minutes and seconds

nba$LOCATION <- NULL # we have better variables

# some of the touch time's are negative. That's not possible, so remove them
nba <- subset(nba, nba$TOUCH_TIME >= 0)

nba$W <- NULL


####################Player's Salary#################

