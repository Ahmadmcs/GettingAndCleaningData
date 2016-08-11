# Assumption: user of this code has already set the working directory 

## Loanding required libraries 
library(data.table)
library(reshape2)

## Downloading Dataset if it is not in the working directory 
# Download and unzip the dataset:

filename <- "getdata_dataset.zip"
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename)
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}


# Load activity labels
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# Assert that labels are typed as characters 
activityLabels[,2] <- as.character(activityLabels[,2])


## Load features 
features <- read.table("UCI HAR Dataset/features.txt")

# Assert that labels are typed as characters
features[,2] <- as.character(features[,2])

## Filtering the dataset
# Extract only the data on mean and standard deviation 
# that are called "measurements" in the question
featuresFiltered <- grep(".*mean.*|.*std.*", features[,2])

#Cleaning features names to be more expressive
featuresFiltered.names <- features[featuresFiltered,2]
featuresFiltered.names = gsub('-mean', 'Mean', featuresFiltered.names)
featuresFiltered.names = gsub('-std', 'Std', featuresFiltered.names)
featuresFiltered.names <- gsub('[-()]', '', featuresFiltered.names)

## Read the datasets
# 1. Read Traning datasets
XTrain <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresFiltered]
YTrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
TrainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

# bind all training columns into one dataset
TrainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
AllTrainData <- cbind(TrainSubjects, YTrain, XTrain)

# 2. Read Test datasets
XTest <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresFiltered]
YTest <- read.table("UCI HAR Dataset/test/Y_test.txt")
TestSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
AllTestData <- cbind(TestSubjects, YTest, XTest)

## Merge Training and Test Datasets into one dataset
allData <- rbind(AllTrainData, AllTestData)

## Cleaning and factorizing the dataset
# set the columns names
colnames(allData) <- c("subject", "activity", featuresFiltered.names)

# transforming activities & subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

## Massaging data
allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

## Final step: write the data file
# Asserting that file does not exist
DatasetFileName <- "tidy.txt"
if(file.exists(DatasetFileName))
{
    DatasetFileName <- gsub(' ', '', 
                            gsub(':', '.', 
                            paste("tiny-", Sys.time() , ".txt")))
}
#write the dataset file
write.table(allData.mean, DatasetFileName, row.names = FALSE, quote = FALSE)

print(paste("Dataset is saved to", getwd(), "/", DatasetFileName))





