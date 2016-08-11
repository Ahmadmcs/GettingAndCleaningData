## Getting and Cleaning Data

this code fulfills the requirements in Getting and Cleaning Data course project from UJH. 

##1. Variables: 
filename: charcter variable that holds the value of the dataset downloeded file name

activityLabels: DataFrame that contains the information in UCI HAR Dataset/activity_labels.txt

features: DataFrame that contains the feautres names from UCI HAR Dataset/features.txt

featuresFiltered: DataFrame that contains measurements features only.

XTrain: DataFrame that contains measurements data from UCI HAR Dataset/train/X_train.txt

YTrain: DataFrame that contains activities data from UCI HAR Dataset/train/Y_train.txt

TrainSubjects: DataFrame that contains the subject training data from UCI HAR Dataset/train/subject_train.txt

AllTrainData: DataFrame that contains all the columns in XTrain, YTrain and SubjectTraining data

XTest: DataFrame that contains measurements data from UCI HAR Dataset/test/X_test.txt

YTest:  DataFrame that contains activities data from UCI HAR Dataset/test/Y_test.txt

TestSubjects: DataFrame that contains the subject test data from UCI HAR Dataset/test/subject_test.txt

AllTestData: DataFrame that contains all the columns in XTest, YTest and SubjectTest data

allData: DataFrame that contains all rows in AllTrainData and AllTestData

DatasetFileName: The file name where the tidy Dataset will be saved.


##2. Transformation
#2.1 Objective: Generate a DataFrame that contains only measurement columns from all traninig and dataset with proper columns names and factors. 
#2.2 Transformations:
2.2.1 Merge Training and Test Datasets into one dataset
2.2.2 set the activities and subjects columns names
2.2.3 transform activities & subjects into factors
2.2.4 Massage the data

