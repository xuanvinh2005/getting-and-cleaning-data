library(dplyr)


# Preparation

# download zip file containing data if it hasn't already been downloaded
zipLink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# don't need to redownload
if (!file.exists(zipFile)) {
  download.file(zipLink, zipFile, mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingX <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingY <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testX <- read.table(file.path(dataPath, "test", "X_test.txt"))
testY <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

print("Step 1: Merges the training and the test sets to create one data set.")

humanAct <- rbind(
  cbind(trainingSubjects, trainingX, trainingY),
  cbind(testSubjects, testX, testY)
)

str(humanAct)

# assign column names
colnames(humanAct) <- c("subject", features[, 2], "activity")


print("Step 2 - Extract only the measurements on the mean and standard deviation")

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanAct))

# ... and keep data in these columns only
humanAct <- humanAct[, columnsToKeep]


print("Step 3 - Use descriptive activity names to name the activities in the data set")

# replace activity values with named factor levels
humanAct$activity <- factor(humanAct$activity, 
  levels = activities[, 1], labels = activities[, 2])


print("Step 4 - Appropriately label the data set with descriptive variable names")

# get column names
humanActCols <- colnames(humanAct)

# remove special characters
humanActCols <- gsub("[\\(\\)-]", "", humanActCols)

# expand abbreviations and clean up names
humanActCols <- gsub("^f", "frequencyDomain", humanActCols)
humanActCols <- gsub("^t", "timeDomain", humanActCols)
humanActCols <- gsub("Acc", "Accelerometer", humanActCols)
humanActCols <- gsub("Gyro", "Gyroscope", humanActCols)
humanActCols <- gsub("Mag", "Magnitude", humanActCols)
humanActCols <- gsub("Freq", "Frequency", humanActCols)
humanActCols <- gsub("mean", "Mean", humanActCols)
humanActCols <- gsub("std", "StandardDeviation", humanActCols)

# correct typo
humanActCols <- gsub("BodyBody", "Body", humanActCols)

# use new labels as column names
colnames(humanAct) <- humanActCols


print("Step 5 - Create a second, independent tidy set with the average of each
      variable for each activity and each subject")

# group by subject and activity and summarise using mean
humanActMeans <- humanAct %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))


# output to file "my_tidy_data.txt"
write.table(humanActMeans, "my_tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
