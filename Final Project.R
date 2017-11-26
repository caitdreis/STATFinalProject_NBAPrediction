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