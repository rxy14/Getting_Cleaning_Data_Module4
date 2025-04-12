#### Scripts for cleaning and processing the input dataset "Human Activity Recognition Using Smartphones"

## Load library
library(dplyr)

## Load input dataset
# Download input datasets from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Moving the files to the working directory
# sep = "" treats any whitespace—whether it's a single space, multiple spaces, or tabs—as a delimiter between columns.
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep = "")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = "")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep = "")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "")

## Creating data.frames for "test" and "train" groups before merging
df_test <- data.frame(subject_test, y_test, X_test)
df_train <- data.frame(subject_train, y_train, X_train)

features <- read.table("./UCI HAR Dataset/features.txt", sep = "", row.names = 1)
colnames(df_test) <- c("subject", "activity", features[,1])
colnames(df_train) <- c("subject", "activity", features[,1])

## 1 Merges the training and the test sets to create one data set.
df_all <- rbind(df_test, df_train)

## 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# select the columns with "mean()" or "std()".     # "meanFreq()" is NOT the one that we should include
grep("mean\\(\\)|std\\(\\)", colnames(df_all))
table(grepl("mean\\(\\)|std\\(\\)", colnames(df_all)))
features_mean_std <- grep("mean\\(\\)|std\\(\\)", colnames(df_all), value = TRUE)
df_all_sub <- df_all %>% select(subject, activity, all_of(features_mean_std)) 
# all_of(): If any columns in features_mean_std are missing from the data frame, it will raise an error.


## 3 Uses descriptive activity names to name the activities in the data set
df_all_sub2 <- df_all_sub %>% 
  mutate(activity = recode(activity, "1" = "Walking", "2" = "Walking_upstairs", 
                           "3" = "Walking_downstairs", "4" = "Sitting", "5" = "Standing", "6" = "Laying"))

## 4 Appropriately labels the data set with descriptive variable names.
# Remove "()"
colnames(df_all_sub2) <- gsub("\\(\\)", "", colnames(df_all_sub2))
# Replace "-" with "_"
colnames(df_all_sub2) <- gsub("-", "_", colnames(df_all_sub2))
# Replace the initial "t" with "time"
colnames(df_all_sub2) <- gsub("^t", "time", colnames(df_all_sub2))
# Replace the initial "f" with "freqeuncy"
colnames(df_all_sub2) <- gsub("^f", "frequency", colnames(df_all_sub2))

## 5 From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

# across() of dplyr apply a function (like mean(), sum()) across multiple columns of a data frame
# where() of dplyr is used inside across() to specify which columns to apply a function to
# where() could select columns of a certain data type (is.numeric, is.character, is.logical, etc.).
df_all_sub2_mean <- df_all_sub2 %>%
  group_by(subject, activity) %>%
  summarize_all(mean)        

# Save "df_all_sub2_mean" as "tidy_data.txt"   
write.table(df_all_sub2_mean, "tidy_data.txt", row.names = FALSE, col.names = TRUE)
# Check the saved txt file
tidy_data <- read.table("tidy_data.txt", header = TRUE) 
View(tidy_data)
