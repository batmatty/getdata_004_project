##
# Course Project - run_analysis.R
#

# Load the required packages

library(data.table)

# Constants

FILE_URL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
DATA_DIR = "./data"
DATA_ZIP_FILE = "./data.zip"


# Function that gets the datafile and extracts it to the current directory

getData <- function(){
    # Check if the data directory exists, if not create it
    dir.create(file.path(DATA_DIR))
    # Change to the data directory
    setwd(file.path(DATA_DIR))
    # Download the data file if it does not exists
    if (!file.exists(DATA_ZIP_FILE)){
        download.file(FILE_URL, DATA_ZIP_FILE, method="curl") 
        # Unzip the file
        unzip(DATA_ZIP_FILE, overwrite=TRUE)
    }    
    # Return to the top level starting directory
    setwd(file.path("./.."))
}

# Function that imports all the data into a data table

importData <- function(){
    # Get the list of files
    files <- list.files(path=DATA_DIR,recursive=TRUE, full.names=TRUE)
    
    # Read in the subject files
    subject_test <- fread(files[15]) # read in subject_test.txt
    subject_train <- fread(files[27]) # read in subject_train.txt
    subject <- rbind(subject_test, subject_train) # combing them
    
    # Read in the activity files
    y_test <- fread(files[17]) # read in y_test.txt
    y_train <- fread(files[29]) # read in y_train.txt
    y <- rbind(y_test, y_train) # combine them
    
    # Read in the other file
    X_test <- read.table(files[16]) # read in X_test.txt
    X_train <- read.table(files[28]) # read in X_train.txt
    X <- rbind(X_test, X_train) # combine them
    
    # Read in the header file
    features <- fread(files[4]) # read in features.txt for the headers
    features$V1 <- NULL # drop the first column
    features <- rbind(c("subject","activity"), features) # add the first 2 col names
    features <- gsub("\\(|\\)", "", features[,V2]) # clean up the headers
    features <- gsub(",|-| - ", "_", features) # clean up the headers
    
    # Create the new Data Table    
    data <- data.table(subject = subject, activity = y, X) # create the data.table
    setnames(data, colnames(data), features) # set the column names
    
    # Drop the columns we don't want
    meanstd <- grepl("_mean|_std", colnames(data)) # Find occurances of mean and std
    meanstd[1] <- TRUE # Make sure we don't drop the first column
    meanstd[2] <- TRUE # Make sure we don't drop the second column
    data <- data[,meanstd, with=FALSE] # Subset data for just the mean and std columns
        
    # Substitute in the activities
    data$activity[data$activity == 1]<- "WALKING"
    data$activity[data$activity == 2]<- "WALKING_UPSTAIRS"
    data$activity[data$activity == 3]<- "WALKING_DOWNSTAIRS"
    data$activity[data$activity == 4]<- "SITTING"
    data$activity[data$activity == 5]<- "STANDING"
    data$activity[data$activity == 6]<- "LAYING"

    # Function that creates the clean dataset from the dataset passed in to the function
    
    aggdata <-aggregate(data, by=list(data$subject, data$activity), FUN=mean) # calc the means
    aggdata$subject <- NULL # drop unwanted columns
    aggdata$activity <- NULL # drop unwanted columns
    setnames(aggdata, c("Group.1", "Group.2"), c("subject", "activity")) # change the headings
    aggdata # return the clean data set 
    
    # Exports the clean dataset to a csv file
       
    write.csv(aggdata, file="cleanDataSet.csv")
    
    # Return the clean data table
    aggdata
}