library(tidyr)
library(dplyr)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./data/Dataset.zip")

# 1.Merges the training and the test sets to create one data set.

testsets <- read.csv("./data/UCI HAR Dataset/test/X_test.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
testsets_y  <- read.csv("./data/UCI HAR Dataset/test/y_test.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
testsets_subject <- read.csv("./data/UCI HAR Dataset/test/subject_test.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
trainsets <- read.csv("./data/UCI HAR Dataset/train/X_train.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
trainsets_y  <- read.csv("./data/UCI HAR Dataset/train/y_train.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
trainsets_subject <- read.csv("./data/UCI HAR Dataset/train/subject_train.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)

activity_labels <- read.csv("./data/UCI HAR Dataset/activity_labels.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)
features <- read.csv("./data/UCI HAR Dataset/features.txt", sep = "" , header = F , na.strings ="", stringsAsFactors= F)

full_testsets <- data.frame( testsets_subject, testsets_y, testsets)

full_trainsets <- data.frame( trainsets_subject, trainsets_y, trainsets)

full_sets <- rbind(full_testsets, full_trainsets)


## build column name for full_sets with features (561 obs -> 561 columns)
full_sets.colname <- as.vector(features[,2])
full_sets.colname <- c("subject_id", "activity_labels", full_sets.colname)

full_sets.valid_name <- make.names(names = full_sets.colname, unique=TRUE, allow_ = TRUE)
colnames(full_sets) <- full_sets.valid_name 

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.Uses descriptive activity names to name the activities in the data set


sub_sets <- select(full_sets, subject_id, activity_labels, contains(".mean."), contains(".std."))

# 4.Appropriately labels the data set with descriptive variable names.
sub_sets$activity_labels <- activity_labels[ sub_sets$activity_labels, 'V2']


# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_set <- sub_sets %>% 
  gather(Var, Value, -subject_id, -activity_labels) %>%
  group_by(subject_id, activity_labels, Var) %>% 
  summarize_each(funs(mean))


write.table(tidy_set, row.name=FALSE, file="./data/tidy_set.txt")