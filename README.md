# tidy-data
Course project to demonstrate how to transform raw to a tidy data.

The following is the scrypt code run_analysis.R to show the step by step procedures from getting the raw data and transforming it to a tidy data.

#Merge the training and test sets to create one dataset.
############################################################################################################### 

#Create testSet
dataTest <- read.csv("./data/UCI HAR Dataset/test/X_test.txt", sep = "", header = F)
activityTest <- read.csv("./data/UCI HAR Dataset/test/y_test.txt", sep = "", header = F)
subjectTest <- read.csv("./data/UCI HAR Dataset/test/subject_test.txt", sep = "", header = F)
testSet <- cbind(subjectTest, activityTest, dataTest)

#Create new column for identifier whether row is a test or training data.
testSet$identifier <- "test"

#Create trainingSet
dataTrain <- read.csv("./data/UCI HAR Dataset/train/X_train.txt", sep = "", header = F)
activityTrain <- read.csv("./data/UCI HAR Dataset/train/y_train.txt", sep = "", header = F)
subjectTrain <- read.csv("./data/UCI HAR Dataset/train/subject_train.txt", sep = "", header = F)
trainingSet <- cbind(subjectTrain, activityTrain, dataTrain)

#Create new column for identifier whether row is a test or training data.
trainingSet$identifier <- "training"

#Merge testSet and trainingSet.

dataset <- rbind(testSet, trainingSet)
featuresName <- read.csv("./data/UCI HAR Dataset/features.txt", sep = "", header = F)
colnames(dataset) <- c("Volunteer" , "Activity", as.character(featuresName$V2), "identifier")

# Convert subject, activity and identifier to factors.

dataset$Volunteer <- as.factor(dataset$Volunteer)
dataset$Activity <- as.factor(dataset$Activity)
dataset$identifier <- as.factor(dataset$identifier)

#Extract only the measurements on the mean and standard deviation for each measurement.
################################################################################################################
Means <- grep("mean()", colnames(dataset))
StDevs <- grep("std()", colnames(dataset))
revisedColumns <- c(Means, StDevs)
revisedColumns2 <- sort(revisedColumns) 
newDataset <- dataset[, c(1,2,revisedColumns2)]
newDataset2 <- newDataset[, !grepl("Freq", colnames(newDataset))] #get rid of the meanFreq columns

#Use descriptive activity names to name the activities in the data set.
################################################################################################################
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
factorActivities <- factor(activities$V2, levels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
newDataset2[,2] <- activities[newDataset2[,2], 2]

#Create a second, independent tidy data set with the average of each variable for each activity and each subject
################################################################################################################
library(reshape2)
dataMelt <- melt(newDataset2, id = c("Volunteer", "Activity"))
tidyData <- dcast(dataMelt, Volunteer + Activity ~ variable, mean)

write.table(tidyData, "./data/UCI HAR Dataset/SamsungData.txt", sep = "", row.name = F)

