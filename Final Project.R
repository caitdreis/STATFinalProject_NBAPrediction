# STAT 6021: Final Project
# Caitlin Dreisbach: cnd2y
# Isabelle Liu: xl9qw
# Sai Prasanth: lbs7aa
# Boh Young Suh: bs6ea

library(chron)

# Read in joined dataset for 2013
joined_shot13 <- read.csv("joined_shots_2013.csv")
# Add a column for player salary
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
# Add a column for player salary
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
nba <- nba[, -c(10, 14, 26)]
# Remove all rows with missing values
nba <- nba[complete.cases(nba),] # Final dataset with no NAs.

# Factoriaze categorical variables
nba$FGM <- as.factor(nba$FGM)
nba$LOCATION <- as.factor(nba$LOCATION)
nba$PERIOD <- as.factor(nba$PERIOD)
nba$PTS <- as.factor(nba$PTS)
nba$PTS_TYPE <- as.factor(nba$PTS_TYPE)
nba$SHOT_RESULT <- as.factor(nba$SHOT_RESULT)
nba$W <- as.factor(nba$W)
nba$GRID_TYPE <- as.factor(nba$GRID_TYPE)
nba$EVENT_TYPE <- as.factor(nba$EVENT_TYPE)
nba$ACTION_TYPE <- as.factor(nba$ACTION_TYPE)
nba$SHOT_TYPE <- as.factor(nba$SHOT_TYPE)
nba$SHOT_ZONE_BASIC <- as.factor(nba$SHOT_ZONE_BASIC)
nba$SHOT_ZONE_AREA <- as.factor(nba$SHOT_ZONE_AREA)
nba$SHOT_ZONE_RANGE <- as.factor(nba$SHOT_ZONE_RANGE)
nba$SHOT_ATTEMPTED_FLAG <- as.factor(nba$SHOT_ATTEMPTED_FLAG)
nba$SHOT_MADE_FLAG <- as.factor(nba$SHOT_MADE_FLAG)
