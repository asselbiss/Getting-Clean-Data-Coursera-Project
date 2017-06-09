################################################################################
#
# Getting Clean Data Coursera Project
#
# Project Requirements:
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each
#    measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
#

################################################################################
### STEP 1 Merges the training and the test sets to create one data set.###
# Import test data
setwd("~/Desktop/Data Cleaning/UCI HAR Dataset/test")
test_data <- read.table("X_test.txt")
test_labels <- read.table("y_test.txt")
test_subjects <- read.table("subject_test.txt")

# Import training data
setwd("~/Desktop/Data Cleaning/UCI HAR Dataset/train")
train_data <- read.table("X_train.txt")
train_labels <- read.table("y_train.txt")
train_subjects <- read.table("subject_train.txt")

# Join test and training data
combine_data <- rbind(test_data, train_data) #This fulfills Step 1
combine_labels <- rbind(test_labels, train_labels)
combine_subjects <- rbind(test_subjects, train_subjects)

################################################################################
### STEP 2 Extracts only the measurements on the mean and standard deviation 
###        for each measurement.
setwd("~/Desktop/Data Cleaning/UCI HAR Dataset")
features <- read.table("features.txt")
meanstdcols <- grep("(.*)mean[^F]|std(.*)",features[,2])
dataMeanStd <- combine_data[,meanstdcols] #This fulfills Step 2

################################################################################
### STEP 3 Uses descriptive activity names to name the activities in the data set
activitynames <- read.table("activity_labels.txt")
activies <- right_join(activitynames, combine_labels, by = "V1") #Partially satisfies Step 4
activies <- select(activies, activity = V2) #Fulfills Step 3

################################################################################
### STEP 4 Appropriately labels the data set with descriptive variable names.
dataLabels <- as.vector(features[meanstdcols, 2])
dataLabels <- sub("mean\\(\\)","Mean",dataLabels)
dataLabels <- sub("std\\(\\)","SD",dataLabels)
names(dataMeanStd) <- dataLabels
combine_subjects$V1 <- as.factor(combine_subjects$V1)
names(combine_subjects) <- "subject"
combineall <- bind_cols(combine_subjects, activies, dataMeanStd)

################################################################################
### STEP 5 From the data set in step 4, creates a second, independent tidy data 
###        set with the average of each variable for each activity and each 
###        subject.
meansummary <- combineall %>% arrange(subject, activity) %>% group_by(subject, activity) %>% summarize_each(funs(mean))
summaryLabels <- c("Subject", "Activity", paste("Mean", dataLabels, sep = "_"))
names(meansummary) <- summaryLabels

# Write final tidy summary to a text file for submission
write.table(meansummary, file = "finalTidySummary.txt", row.names = FALSE)
