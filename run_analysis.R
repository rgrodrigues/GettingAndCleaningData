# Clean up workspace
rm(list=ls())

#set working directory
setwd("D:/Dropbox/_Inv/WorkSpace/R/Coursera/03 - Getting and Cleaning Data")

#install packages
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)

###The project###

##  You should create one R script called run_analysis.R that does the following. 

##  1.  Merges the training and the test sets to create one data set.
##  2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
##  3.	Uses descriptive activity names to name the activities in the data set
##  4.	Appropriately labels the data set with descriptive variable names. 
##  5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##### Preparing the 5 steps

## Download the zip file        

## Download and unzip the file.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./data")){dir.create("./data")}
download.file(url, "./data/UCI-HAR-dataset.zip")
unzip("./data/UCI-HAR-dataset.zip")

##  reading the files
features <- read.table("./data/UCI HAR Dataset/features.txt") ## List of all features.
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt") ## Training set.
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt") ## Training labels.
xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt") ## Test set.
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt") ## Test labels.

subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
        ## Each row identifies the subject who performed the activity for each 
        ## window sample. Its range is from 1 to 30.

activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt") ## Links the class labels with their activity name.

##changing variable names 
colnames(xTrain) <- features$V2; colnames(xTest)  <- features$V2
        ##on train and test datasets according to features.txt
colnames(activityLabels) = c('activityId','activityType'); ##Change variable names
colnames(subjectTrain) = "subjectId"; colnames(subjectTest) = "subjectId"
colnames(yTrain) = "activityId"; colnames(yTest) = "activityId"


##### STEPS

##  STEP 1.  Merges the training and the test sets to create one data set.
train <- cbind(subjectTrain,yTrain,xTrain) #joins subject, activity and measurements
test <- cbind(subjectTest,yTest,xTest) #same for test data
data <- rbind(train, test) #merges train and test

##  STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.
data2 <- data[,grepl("Id|std|mean", colnames(data))]

##  STEP 3. Uses descriptive activity names to name the activities in the data set
data2$activityId <- activityLabels[data2$activityId, 2]

##  STEP 4. Appropriately labels the data set with descriptive variable names.
vars <- colnames(data2)
for (i in 1:length(vars))
{
        vars[i] = gsub("\\()","",vars[i])
        vars[i] = gsub("-Std$","StdDev",vars[i], ignore.case=TRUE)
        vars[i] = gsub("-std-$","StdDev",vars[i])
        vars[i] = gsub("-mean","Mean",vars[i])
        vars[i] = gsub("^(t)","time",vars[i])
        vars[i] = gsub("^(f)","freq",vars[i])
        vars[i] = gsub("([Gg]ravity)","Gravity",vars[i])
        vars[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",vars[i])
        vars[i] = gsub("[Gg]yro","Gyro",vars[i])
        vars[i] = gsub("AccMag","AccMagnitude",vars[i])
        vars[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",vars[i])
        vars[i] = gsub("JerkMag","JerkMagnitude",vars[i])
        vars[i] = gsub("GyroMag","GyroMagnitude",vars[i])
};
colnames(data2) = vars;

##  STEP 5. From the data set in step 4, creates a second, independent tidy data
     ##set with the average of each variable for each activity and each subject.

data3 <- aggregate(data2[,3:(ncol(data2))], by=list(activityId=data2$activityId,subjectId = data2$subjectId), mean)
write.csv(data3, file="./data/results.csv", row.names=FALSE)
