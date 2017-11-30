# STAT 6021: Final Project
# Caitlin Dreisbach: cnd2y
# Isabelle Liu: xl9qw
# Sai Prasanth: lbs7aa
# Boh Young Suh: bs6ea

library(chron)

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

### data cleaning ---------------

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

# oh, the charlotte bobcats changed their name to charlotte hornets

nba$TEAM_NAME[nba$TEAM_NAME == "Charlotte Bobcats"]  <- "Charlotte Hornets"

unique(nba$TEAM_NAME)

nba$TEAM_NAME <- as.factor(nba$TEAM_NAME)  
 
# oh, the charlotte bobcats changed their name to charlotte hornets


unique(nba$EVENT_TYPE)  # we have points columns in the end. We don't need many such variables
nba$EVENT_TYPE <- NULL  

unique(nba$SHOT_TYPE)  
nba$SHOT_TYPE <- NULL  

unique(nba$SHOT_ZONE_BASIC)
nba$SHOT_ZONE_BASIC <- NULL

unique(nba$SHOT_ZONE_RANGE)
nba$SHOT_ZONE_RANGE <- NULL

unique(nba$SHOT_DISTANCE)
nba$SHOT_DISTANCE <- NULL


nba$LOC_X <- NULL
nba$LOC_Y <- NULL

nba$SHOT_ATTEMPTED_FLAG <- NULL #obviously every successfull shot was attempted

nba$SHOT_MADE_FLAG <- NULL

nba$CLOSEST_DEFENDER_PLAYER_ID <- NULL # we have the name

nba$FGM <- NULL

nba$FINAL_MARGIN <- NULL # we are looking at row level

nba$GAME_CLOCK <- NULL # we already have minutes and seconds

nba$LOCATION <- NULL # we have better variables

unique(nba$SHOT_RESULT) # we already removed event ID

# I see that some of the touch time's are negative. That's not possible. I am going to delete them

nba <- subset(nba, nba$TOUCH_TIME >= 0)


nba$W <- NULL


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
