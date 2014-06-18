# Getting and Cleaning Data Project 1
# ========================================================
#     
#     One of the most exciting areas in all of data science right now is 
#     wearable computing - see for example [this article](http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/) . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#     
#     http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#     
#     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# First we need to download the data file:

dataUrl<-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
localFile<-'Dataset.zip'
if (!file.exists(localFile)) {
    # Download file
    download.file (dataUrl, localFile, method="curl",quiet=TRUE)
}

# get a list of files in the downloaded ZIP file
zipFiles<-unzip(localFile,list=TRUE)

# Now let's read the "features.txt" file.
# This file contains the list of features present in the dataset. It has two
# columns: id, feature
# Example data:
# 1 tBodyAcc-mean()-X
# 2 tBodyAcc-mean()-Y
# 3 tBodyAcc-mean()-Z
# ...
# Get the full file name of the "features.txt" file by searching for 
# "features.txt" in the list of files included in the ZIP
f<-unz(localFile,zipFiles$Name[grep("features.txt",zipFiles$Name)])
features<-read.table(f,col.names=c("id", "feature"))

# simplyfy feature names 
# we want to remove "()" or "," to get "nicer" feature names
# opening bracket needs to be escaped in regexp
features$feature<-gsub("\\()|,", "", features$feature)

# Also read the "activity_labels.txt" file.
# This file has the descriptive names for the various sporting activities
# comumns: id, activity
# Example data: 
# 1 WALKING
# 2 WALKING_UPSTAIRS
# ... 
f<-unz(localFile,zipFiles$Name[grep("activity_labels.txt",zipFiles$Name)])
activityLabels<-read.table(f,col.names=c("id", "activity"))

# The dataset contains data files in the "test" and "train" folder
# The individual data files are:
#   subject_(test|train).txt
#   X_(test|train).txt
#   X_(test|train).txt
# The below helper creates the actual data file based on prefix and test or 
# train tag and returns the data contained in that data file
readSingleFile <- function (testOrTrain = 'test', prefix) {
    f<-unz(localFile,zipFiles$Name[grep(paste('/', prefix,
                                              testOrTrain,
                                              '.txt',
                                              sep=''),
                                        zipFiles$Name)])
    read.table(f) 
}

# This helper reads all data files of the train or test data set an returns the
# combined result
readData <- function(testOrTrain = 'test') {
    # read subject IDs from subject_(train|test).txt
    # This file contains a single column with subject IDs of the respective 
    # measurement
    subjectId<-readSingleFile(testOrTrain, 'subject_')
    colnames(subjectId)<-'subjectId'
    
    # read X_(train|test}.txt (data)
    # This file has one measurement per line with one column per feature. 
    # Features already have been read from features.txt
    x<-readSingleFile(testOrTrain, 'X_')
    
    # name the columns using the feature names
    # The number of columns in x is identical to the number of features
    colnames(x)<-as.list(features$feature)
    
    # read y_(train|test).txt
    # this file has a single column with the activity IDs of the respective
    # measurement. This references the activities already read from 
    # activity_labels.txt 
    y<-readSingleFile(testOrTrain, 'y_')
    colnames(y)<-'activityId'
    
    # Merge activitiy table and data by activity ID to get activity labels
    # also add the subjectId as the 1st column
    data<-data.frame (subjectId$subjectId, merged<-merge(y,activityLabels,
                                                         by.x='activityId', 
                                                         by.y='id')$activity)
    colnames(data)<-c('subjectId','activity')
    
    # get names of all mean and standard deviation measurement features
    relevantData<-features$feature[grep('-(mean|std)($|-)',features$feature)]
    
    # now add all columns from x with these variable to the right of the data 
    # frame
    data<-cbind(data,x[,relevantData])
    return (data)
}

# combine test and train data into one dataset
testData<-rbind(readData('test'),readData('train'))
write.table (testData,"combined_data.txt", row.names=FALSE)

# aggregate using mean and group by subject and activity
# --> average of each variable per subject and activity type
aggData<-aggregate(testData[,-1:-2],by=list(testData$subjectId,testData$activity),FUN=mean)
colnames(aggData)[1:2]<-c('subjectId','activity')
write.table (aggData,"aggregated_data.txt", row.names=FALSE)


