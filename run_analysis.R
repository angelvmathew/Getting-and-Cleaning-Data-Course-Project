# Getting and Cleaning Data Project

library(dplyr)
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# 1. Merge training and test sets to create one data set

#Reading features and activity data
features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Reading training data
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

#Reading test data
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

#Combine subjects, activity labels, and features into training and test sets
train <- cbind(subject_train, y_train, X_train)
test  <- cbind(subject_test, y_test, X_test)

# Combine test and train sets into full data set
allData <- rbind(test, train)

# 2. Extract only measurements on mean and standard deviation

allNames <- c("subject", "activity", as.character(features$V2))
meanStdColumns <- grep("subject|activity|[Mm]ean|std", allNames, value = FALSE)
reducedSet <- allData[ ,meanStdColumns]

# 3. Use descriptive activities names for activity measurements

names(activities) <- c("activityNumber", "activityName")
reducedSet$V1.1 <- activities$activityName[reducedSet$V1.1]

# 4. Appropriately Label the Dataset with Descriptive Variable Names

reducedNames <- allNames[meanStdColumns]    # Names after subsetting
reducedNames <- gsub("mean", "Mean", reducedNames)
reducedNames <- gsub("std", "Std", reducedNames)
reducedNames <- gsub("gravity", "Gravity", reducedNames)
reducedNames <- gsub("[[:punct:]]", "", reducedNames)
reducedNames <- gsub("^t", "time", reducedNames)
reducedNames <- gsub("^f", "frequency", reducedNames)
reducedNames <- gsub("^anglet", "angleTime", reducedNames)
names(reducedSet) <- reducedNames   # Apply new names to dataframe

# 5. Create tidy data set with average of each variable, by activity, by subject

# Create tidy data set
tidydataset <- reducedSet %>% group_by(activity, subject) %>% 
  summarise_all(funs(mean))

# Write tidy data to ouput file
write.table(tidydataset, file = "tidydataset.txt", row.names = FALSE)

