library(dplyr)

# 1. Merges the training and the test sets to create one data set.

dt <- bind_rows(
    read.table("UCI HAR Dataset/train/X_train.txt"),
    read.table("UCI HAR Dataset/test/X_test.txt")
)

# Note: the label and subject vectors will be added in the steps further below.

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
featureIndices <- which(grepl("mean|std", features[,2]))
dt <- dt[,featureIndices]

# 3. Uses descriptive activity names to name the activities in the data set

activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")
activities <- bind_rows(
    read.table("UCI HAR Dataset/train/y_train.txt"),
    read.table("UCI HAR Dataset/test/y_test.txt")
)
activities <- sapply(activities, function(x) activityNames[x,2])[,1]

# 4. Appropriately labels the data set with descriptive variable names.

# Rename feature vectors
featureNames <- features[featureIndices,2]
names(dt) <- featureNames

# Insert activity vector
dt <- mutate(dt, activity = activities)

# Insert subject vector
subjects <- bind_rows(
    read.table("UCI HAR Dataset/train/subject_train.txt"),
    read.table("UCI HAR Dataset/test/subject_test.txt")
)
dt <- mutate(dt, subject = subjects[,1])

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

grouped <- group_by(dt, activity, subject)
means <- summarise_each(grouped, funs(mean))
