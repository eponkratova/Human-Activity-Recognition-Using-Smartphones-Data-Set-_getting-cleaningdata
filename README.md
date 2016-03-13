#Step 1. Read tables.
subjectTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/X_test.txt", header = FALSE)
featureNames <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/features.txt", header = FALSE)
activityLabels <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Step 2. Merges the training and the test sets.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
merged <- cbind(features,activity,subject)
head(merged)

#Step 3. Extracts only the measurements on the mean and standard deviation for each measurement. 
meanStandard <- grep(".*mean.*|.*std.*", names(merged))
newColumns <- c(meanStandard, 562, 563)
str(newColumns)
extracted <- merged[,newColumns]
dim(extracted)

#Step 4. Uses descriptive activity names to name the activities in the data set
extractedActivity <- as.character(extracted$Activity)
for (i in 1:6){
        extracted$Activity[extracted$Activity == i] <- as.character(activityLabels[i,2])
}
extractedActivity <- as.factor(extracted$Activity)

#Step 5. Appropriately labels the data set with descriptive variable names. 
names(extracted)<-gsub("Acc", "Accelerometer", names(extracted))
names(extracted)<-gsub("Gyro", "Gyroscope", names(extracted))
names(extracted)<-gsub("BodyBody", "Body", names(extracted))
names(extracted)<-gsub("Mag", "Magnitude", names(extracted))
names(extracted)<-gsub("^t", "Time", names(extracted))
names(extracted)<-gsub("^f", "Frequency", names(extracted))
names(extracted)<-gsub("tBody", "TimeBody", names(extracted))
names(extracted)<-gsub("-mean()", "Mean", names(extracted))
names(extracted)<-gsub("-std()", "STD", names(extracted))
names(extracted)<-gsub("-freq()", "Frequency", names(extracted))
names(extracted)<-gsub("angle", "Angle", names(extracted))
names(extracted)<-gsub("gravity", "Gravity", names(extracted))
names(extracted)

#Step6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extractedSubject <- as.factor(extracted$Subject)
extracted <- data.table(extracted)
tidy <- aggregate(. ~Subject + Activity, extracted, mean)
tidy <- tidy[order(tidy$Subject,tidy$Activity),]
write.table(tidy, file = "Tidy.txt", row.names = FALSE)
