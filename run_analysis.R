ls()
#install packages to read  .txt
install.packages("data.table")
library(data.table)
#read the datasets
subjectTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/subject_train.txt")
activityTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/y_train.txt")
featuresTrain <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/train/X_train.txt")
subjectTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/subject_test.txt")
activityTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/y_test.txt")
featuresTest <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/test/X_test.txt")
featureNames <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/features.txt")
activityLabels <- read.table("C:/Users/eponkratov003/Documents/PwC_2012-2016/course_mine/R programming_2016/gettingAndCleaningData/course project/UCI HAR Dataset/activity_labels.txt")
#Merges the training and the test sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
merged <- cbind(features,activity,subject)
head(merged)
#Extracts only the measurements on the mean and standard deviation for each measurement
meanStandard <- grep(".*mean.*|.*std.*", names(merged))
newColumns <- c(meanStandard, 562, 563)
str(newColumns)
extracted <- merged[,newColumns]
dim(extracted)
#Uses descriptive activity names to name the activities in the data set
extractedActivity <- as.character(extracted$Activity)
for (i in 1:6){extracted$Activity[extracted$Activity == i] <- as.character(activityLabels[i,2])}
extractedActivity <- as.factor(extracted$Activity)
#Appropriately labels the data set with descriptive variable names. 
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
extractedSubject <- as.factor(extracted$Subject)
extracted <- data.table(extracted)
tidy <- aggregate(. ~Subject + Activity, extracted, mean)
tidy <- tidy[order(tidy$Subject,tidy$Activity),]
write.table(tidy, file = "Tidy.txt", row.names = FALSE)
