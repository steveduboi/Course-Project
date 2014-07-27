
######## script agenda ########

#  1) create test dataset ('TEST_DATASET') from various text files authered by UCI Machine Learning Repository
#  2) creation of a similar train dataset('TRAIN_DATASET').
#  3) merging of train & test sets into much larger dataset ('MERGED_DATA_SET').
#  4) extracting NON-mean() and NON-standard deviation from MERGED_DATA_SET';  renamed to 'EXTRACTED_SET'.
#  5) creating smaller dataset 'GROUP_MEAN', which groups subject/persons and activity and calculates average of variable observations for each group form.


#######################


#  (1) CREATION OF TEST DATA SET

####    reading test data of measured variables
            Xtest <- read.table("X_test.txt")

####    reading in activity done per observation window in test set
            ytest_labels <- read.table("y_test.txt") 

####    reading in person/subject per observation window of test set            
            persons_id <- read.table("subject_test.txt") 

####    reading in labels of measured variables for each feature vector (i.e., measurements/data columns for both test and train sets ) 
            features <- read.table("features.txt") 
            features2 <- features[,2]  ## (measurements - variables)

####    assigning column names to subject/(persons_id), activity(ytest_labels), measurement(Xtest))
            colnames(Xtest) <- features2   
            colnames(ytest_labels) <- "Activity"
            colnames(persons_id) <- "Person_ID"

####    using 'cbind' function to form TEST DATASET by combining columns from above
combine1a <- cbind(persons_id, ytest_labels)
combine1b <- cbind(combine1a, Xtest)  ## test data set

TEST_DATASET <- combine1b
TEST_DATASET_subset <- combine1b[1:20, 1:5] 


##################################


#  (2) CREATION OF TRAIN DATA SET

####    reading train data of measured variables
            Xtrain <- read.table("X_train.txt")

####    reading in activity done per observation window in train set
            ytrain_labels <- read.table("y_train.txt") 

####    reading in person/subject per observation window of train test set
            persons_id2 <- read.table("subject_train.txt") 

####    reading in labels of measured variables for each feature vector (i.e., test and train set data columns) 
            colnames(Xtrain) <- features2
            colnames(ytrain_labels) <- "Activity"
            colnames(persons_id2) <- "Person_ID"

####    using 'cbind' function to form TRAIN DATASET by combining columns from above
            combine3a <- cbind(persons_id2, ytrain_labels)
            combine3b <- cbind(combine3a, Xtrain)  ##  train data sets

            TRAIN_DATASET <- combine3b


##################################


#  (3) MERGED COMPLETE TRAIN & TEST DATA SETS (w/all measurements included)

####    merging train & test sets using 'rbind' function
            
            MERGED_DATA_SET <- rbind(TRAIN_DATASET, TEST_DATASET)

            # sample subset: MERGED_DATA_SET_subset <- combine3b[1:10, 1:5]


##################################


#  (4) EXTRACTING NON-MEAN & STANDARD DEVIATION MEASUREMENTS/VARIABLES FROM MERGED_DATA_SET
#  NEW DATASET FORMED >>  'MERGED_DATA_SET_no.2' (only mean & std vars)

####     manually creating list of mean & standard measurements/variables to retain
          
            retained_vars <- c("Person_ID", "Activity", "tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z","tBodyAcc-std()-X",    "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-std()", "tBodyAccMag-mad()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyAccJerkMag-mean()", "fBodyGyroMag-mean()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")

####    forming new merged dataset ('MERGED_DATA_SET_no2') with non-mean/std variables removed--using %in% operator, colnames function, and indexing
            
            MERGED_DATA_SET_no2 <- MERGED_DATA_SET[, (colnames(MERGED_DATA_SET) %in% retained_vars)]

####        replacing activity code numbers with actual activity names(e.g. replacing "1" with "WALKING")

      MERGED_DATA_SET_no2$Activity <- gsub("1", "WALKING", MERGED_DATA_SET_no2$Activity)
      MERGED_DATA_SET_no2$Activity <- gsub("2", "WALKING_UPSTAIRS", MERGED_DATA_SET_no2$Activity)
      MERGED_DATA_SET_no2$Activity <- gsub("3", "WALKING_DOWNSTAIRS", MERGED_DATA_SET_no2$Activity)
      MERGED_DATA_SET_no2$Activity <- gsub("4", "SITTING", MERGED_DATA_SET_no2$Activity)
      MERGED_DATA_SET_no2$Activity <- gsub("5", "STANDING", MERGED_DATA_SET_no2$Activity)
      MERGED_DATA_SET_no2$Activity <- gsub("6", "LAYING", MERGED_DATA_SET_no2$Activity)                              
      
print("### MERGED_DATA_SET_no.2 - only mean & std vars")
MERGED_DATA_SET_no2 

##################################


#   (5) GROUPING SUBJECTS (1:30 PERSONS) WITH ACTIVITY PERFORMED (6 DIFFERENT ACTIVITIES)
# GROUPED MEAN: CALCULATION OF EACH GROUP MEAN FOR EACH MEASUREMENT VARIABLE IN FROM 'EXTRACTED_SET'.
# Clarifying example of 'group': a group, for example, is the combined observations of person no.1 performing activity no.(walking).
# Variable Measurements from the group are averaged into a total mean for the group of the variable measurements.
# groupings performed using aggregate function.

      agg <- aggregate(MERGED_DATA_SET_no2[, 3:66], by=list(Person._ID=MERGED_DATA_SET_no2$Person_ID, Activity=MERGED_DATA_SET_no2$Activity), FUN=mean, na.rm=TRUE)

      ####    replacing again activity code numbers with actual activity names(e.g. replacing "1" with "WALKING")
                  agg$Activity <- gsub("1", "WALKING", agg$Activity)
                  agg$Activity <- gsub("2", "WALKING_UPSTAIRS", agg$Activity)
                  agg$Activity <- gsub("3", "WALKING_DOWNSTAIRS", agg$Activity)
                  agg$Activity <- gsub("4", "SITTING", agg$Activity)
                  agg$Activity <- gsub("5", "STANDING", agg$Activity)
                  agg$Activity <- gsub("6", "LAYING", agg$Activity)

            GROUPED_MEAN <- agg

print("###  STEP 5 FINAL RESULT;  Small Dataset with Calculated Total Means for each person per activity per variable measurement ") 

GROUPED_MEAN


sink("run_analysis_datasets_output5.txt", append = FALSE, split = FALSE)

