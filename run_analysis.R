#1. Merge training and test set to create one dataset

#Set Working Directory
setwd("C:/Users/Kayla Medina/Documents/RProgramming/CleaningDataAssignment/UCI HAR Dataset/")

#Read training files into R
features<-read.table("./features.txt", header=FALSE)
activitylables<-read.table("./activity_labels.txt", header=FALSE)
subjecttrain<-read.table("./train/subject_train.txt", header=FALSE)
xtrain<-read.table("./train/X_train.txt", header=FALSE)
ytrain<-read.table("./train/y_train.txt", header=FALSE)

#Assign column names to imported data
colnames(activitylables)= c("activityID","activityType")
colnames(subjecttrain)= "subjectID"
colnames(xtrain)=features[,2]
colnames(ytrain)="activityID"

#Create final training table
trainingdata<-cbind(ytrain,subjecttrain,xtrain)

#Read test data into R
subjecttest<-read.table("./test/subject_test.txt", header=FALSE)
xtest<-read.table("./test/X_test.txt", header=FALSE)
ytest<-read.table("./test/y_test.txt", header=FALSE)

#Assign column names to test data which was imported
colnames(subjecttest)= "subjectID"
colnames(xtest)=features[,2]
colnames(ytest)="activityID"

#Create final test table
testdata<-cbind(ytest,subjecttest,xtest)

# Combine training and test data
combinedata<-rbind(trainingdata,testdata)

#create vectors for column names 
columnnames<-colnames(combinedata)

#2. Extract only the measurements for STD Dev and Mean for each measurement

# create logical vector that seperates out the mean and std dev and ID
logicalVector <- (grepl("activity..",columnnames)
                 | grepl("subject..",columnnames) 
                 | grepl("-mean..",columnnames) 
                 & !grepl("-meanFreq..",columnnames) 
                 & !grepl("mean..-",columnnames) 
                 | grepl("-std..",columnnames) 
                 & !grepl("-std()..-",columnnames))

#subset out ID, STD Dev and Mean using logical vector
combinedata<-combinedata[logicalVector==TRUE]

#3 Use descriptive activities names to name the activities in the dataset

#merge combinedata with activity labels vector
combinedata<-merge(combinedata,activitylables,by="activityID",all.x=TRUE)

#Update Column names
columnnames<-colnames(combinedata)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
columnnames<-c("activityID","subjectID","TimebodyAccMagnitudeMean","TimeBodyAccMagnitudeSTD",
               "TimeGravityAccMagnitudeMean","TimeGravityAccMagnitudeSTD",
               "TimeBodyAccJerkMagnitudeMean","TimeBodyAccJerkMagnitudeSTD",
               "TimeBodyGyroMagnitudeMean","TimeBodyGyroMagnitudeSTD",
               "TimeBodyGyroJerkMagnitudeMean","TimeBodyGyroJerkMagnitudeSTD",
               "FreqBodyAccMagnitudeMean","FreqBodyAccMagnitudeSTD",
               "FreqBodyAccJerkMagnitudeMean","FreqBodyAccMagnitudeSTD",
               "FreqBodyGyroMagnitudeMean","FreqBodyGyroMagnitudeSTD",
               "FreqBodyGyroJerkMagnitudeMean","FreqBodyGyroJerkMagnitudeSTD",
               "activityType")

#Update column names of combinedata
colnames(combinedata)<-columnnames

# 5. Create a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

#create new dataset without the activity type column
CombineDataNoActivityType <- combinedata[,names(combinedata) != 'activityType']

# summarize the dataset with average of each variable
tidyData    <- aggregate(CombineDataNoActivityType[,names(CombineDataNoActivityType) != c('activityID','subjectID')],
                        by=list(activityID=CombineDataNoActivityType$activityID,subjectID = CombineDataNoActivityType$subjectID),
                        mean)

#Reattach the Activity Type
tidyData    <- merge(tidyData,activitylables,by='activityID',all.x=TRUE)

# Save the tidy data set
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');