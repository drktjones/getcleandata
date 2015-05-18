# The purpose of this project is to demonstrate my ability 
# to collect, work with, and clean a data set. The goal is 
# to prepare tidy data that can be used for later analysis.
# A full description is available at the site where the data 
# was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# Created in R 3.20.0, Developed by Kenneth Jones, 5/18/2015

## 1. Get libraries for use and set working directory or create as needed. 

library(data.table)
library(dplyr)
library(plyr)
if (!file.exists("datasciencecoursera/UCI HAR Dataset/")) {
        dir.create("datasciencecoursera/UCI HAR Dataset/")
}
setwd("datasciencecoursera/UCI HAR Dataset/")

## 2. Read in files for test datasets using read.table.
subjects_test <- read.table("test/subject_test.txt")
y_test <- read.table("test/y_test.txt")
x_test <- read.table("test/X_test.txt")
subjects_train <- read.table("train/subject_train.txt")
y_train <- read.table("train/y_train.txt")
x_train <- read.table("train/X_train.txt")
activity_labels <-read.table("activity_labels.txt")
features <- read.table("features.txt")

## 3. Merge the training and the test sets to create one data
## data set and add variable names.
## - Subject is the subject's id
## - Activity is the activity that is being assessed
mergedata <- rbind(cbind(subjects_test, y_test, x_test),
                   cbind(subjects_train, y_train, x_train))
names(mergedata)[1:2] <- c("subject", "activity")
names(mergedata)[3:563] <- as.list(as.character(features[,2]))

## 4. Extracts only the measurements on the mean and standard 
## deviation for each measurement.
mergedata <- mergedata %>% 
        mutate(observation=rownames(mergedata)) %>% 
        select(grep("subject|activity|mean\\(\\)|std\\(\\)", 
                   tolower(colnames(mergedata))))

## 5. Uses descriptive activity names to name the activities in 
## the data set and drop duplicate variables from merge.
mergedata <- merge(mergedata, activity_labels, by.x="activity", 
                by.y="V1", all=TRUE) %>% 
                mutate(activity=V2)
mergedata <- mergedata[1:68]

## 6. Labels the data set with descriptive variable names.
### - Consult original documentation for variable names. For 
###   these purposes, the following transformations apply:
###   T - Time
###   F - Frequency
###   Acc - Accelerometer
###   Gyro - Gyroscope
###   Mag - Magnitude
###   BodyBody -Body
names(mergedata) <- gsub("^t", "time", names(mergedata))
names(mergedata) <- gsub("^f", "frequency", names(mergedata))
names(mergedata) <- gsub("Acc", "Accelerometer", names(mergedata))
names(mergedata) <- gsub("Gyro", "Gyroscope", names(mergedata))
names(mergedata) <- gsub("Mag", "Magnitude", names(mergedata))
names(mergedata) <- gsub("BodyBody", "Body", names(mergedata))

## 7. Creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.
tidydat <- aggregate(. ~subject + activity, mergedata, mean)
tidydat <- tidydat[order(tidydat$subject, tidydat$activity),]
write.table(tidydat, file="finaldataset.txt", row.names=FALSE)



                
