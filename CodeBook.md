#Getting and Cleaning Data Course Project

##Description

This is the relevant information needed to understand the run_analysis.R code found in this repo.

##Source Data

The data used in this repo can be dowloaded from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Additional information can be obtained from:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Analysis is performed on the Human Activity Recognition Using Smartphones Dataset

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

###For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##1) Merges the training and the test sets to create one data set.

Download the zip file and extract to a desired location, renaming folder as desired, for easier location.
Set your working directory to where you saved the raw data.
Import the following tables into R.
	features.txt, activity_labels.txt, subject_train.txt, subject_test.txt, 
	x_train.txt, x_test.txt, y_train.txt, y_test.txt
	
Assign column names to all tables imported.
merge training data and test data into their own datasets by column
merge training data and test data into one dataset by rows

##2)Extracts only the measurements on the mean and standard deviation for each measurement. 
create a logical vector that only returns true values for ID std dev and mean columns
subset out only these that at TRUE

##3)Uses descriptive activity names to name the activities in the data set
merge the dataset with the ActivityTypes table

##4) Appropriately labels the data set with descriptive variable names. 
Create a vector containing column names which are more descriptive
set the column names of your dataset to the contents of this vector

##5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Create a new dataset that summarizes the data by the averages of each activity type 
save the file using write.table()