# Assignment: Getting and Cleaning Data Course Project
###
# It is assumed that the package "dplyr" has been already installed using the command "install.packages('dplyr')" 
###
# Define a function named "run_analysis.R"
#library(dplyr)
run_analysis <- function() {
  ################################################
  #Downloading, unzipping, and saving the data into "data" file
  ################################################
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/UCI%20HAR%20Dataset.zip",method="curl")
  unzip(zipfile="./data/UCI%20HAR%20Dataset.zip",exdir="./data")
  #
  ##################################################
  ## part 1. Merging the training and the test sets to create one data set
  ##################################################
  #
  features<-read.table("./data/UCI HAR Dataset/features.txt",header=FALSE)
  DatasetSubjectTest<-read.table("./data/UCI HAR Dataset/test/subject_test.txt",header=FALSE)
  DatasetXTest<-read.table("./data/UCI HAR Dataset/test/X_test.txt",header=FALSE)
  DatasetYTest<-read.table("./data/UCI HAR Dataset/test/y_test.txt")
  DatasetSubjectTrain<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  DatasetXTrain<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
  DatasetYTrain<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
  #
  DatasetSubject<-rbind(DatasetSubjectTrain,DatasetSubjectTest)
  DatasetX<-rbind(DatasetXTrain, DatasetXTest)
  DatasetY<-rbind(DatasetYTrain, DatasetYTest)
  #
  names(features)
  names(DatasetX)<-features$V2
  names(DatasetY)<-c("Activity_ID")
  names(DatasetSubject)<-c("Subject_ID")
  dim(DatasetX)
  dim(DatasetY)
  dim(DatasetSubject)
  GalaxyData<-cbind(DatasetX,c(DatasetY, DatasetSubject))
  dim(GalaxyData)
  #
  ##############################################################################
  #Part 2 Extracts only the measurements on the mean and standard deviation for each measurement
  ##############################################################################
  #
  ExtractedNames<-features$V2[grep("mean\\(|std\\(",features$V2)]
  ExtractedNames<-c(as.character(ExtractedNames),"Activity_ID","Subject_ID")
  requiredData<-subset(GalaxyData, select= ExtractedNames)
  dim(requiredData)
  #
  #######################################################################
  #Part 3 Uses descriptive activity names to name the activities in the data set
  ########################################################################
  #
  ## reading data from "./data/UCI HAR Dataset/activity_labels.txt"
  activityLabels<-read.table("./data/UCI HAR Dataset/activity_labels.txt",header=FALSE)
  names(activityLabels) <- c("Activity_ID", "Activity")
  part3Data <- merge(requiredData, activityLabels,by='Activity_ID',all.x=TRUE)
  # To view variables, use "names(part3Data)"
  #
  ########################################################################
  #Part 4 Appropriately labels the data set with descriptive variable names
  ########################################################################
  #
  part4Data<-part3Data
  names(part4Data)<-gsub("^t","Time", names(part4Data))
  names(part4Data)<-gsub("^f","Frequence", names(part4Data))
  names(part4Data)<-gsub("Acc","Acceleration", names(part4Data))
  names(part4Data)<-gsub("Mag","Magnitude", names(part4Data))
  names(part4Data)<-gsub("Gyro","Gyroscope", names(part4Data))
  names(part4Data)<-gsub("BodyBody","Body", names(part4Data))
  names(part4Data) <- gsub("\\()", "", names(part4Data))
  names(part4Data) <- gsub("-mean", "Mean", names(part4Data))
  names(part4Data) <- gsub("-std", "StdDev", names(part4Data))
  write.table(part4Data, file="tidy_data.txt")
  td<-read.table(file="tidy_data.txt")
  dim(td)
  #
  ########################################################################
  #Part 5 From the data set in step 4, creates a second, independent tidy data set with the 
  # average of each variable for each activity and each subject
  ########################################################################
  #
  td<-read.table("tidy_data.txt")
  TidyData <- with(part4Data, aggregate(part4Data[,1:66], by=list(activity= part4Data$Activity,subject= part4Data$Subject_ID), FUN=mean))
 write.table(TidyData, file="tidy_data2.txt")
 read.table(file="tidy_data2.txt")
}	