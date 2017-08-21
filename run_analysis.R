#set the download link and download the data file
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/data.zip")

#unzip the data
unzip(zipfile = "./data/data.zip", exdir = ".")

#read data files into memory
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
features <- readLines("./UCI HAR Dataset/features.txt")
activitylabels <- readLines("./UCI HAR Dataset/activity_labels.txt")

# 1. Merge the training and the test sets to create one data set
traindata <- cbind(subject_train,y_train,x_train) #create table from all train data
testdata <- cbind(subject_test,y_test,x_test) #create table from all test data
# dim(traindata)
# dim(testdata)
library(gdata)
whole <- combine(traindata,testdata) #merge traindata and testdata
whole <- whole[,c(564,1:563)] #move the "source" variable to the first column

#add names for the 3 unnamed columns in the newly created merged dataset
variablenames <- append(features, values = c("source", "subjectID", "activity"), after = 0)

#name the columns with the initial set of variable names
colnames(whole) <- variablenames

# 2. Extract only the measurements on the mean and standard deviation for each measurement, store in a new dataset
relevantdata <- whole[,grep("mean|std|source|subjectID|activity",variablenames)]

# 3. Use descriptive activity names to name the activities in the data set
activitylabels <- gsub("_","",tolower(activitylabels)) #make activity labels to lower case and remove underscore
activityNames <- sub("[0-9] ","",activitylabels) #extract descriptive activity names by removing numbers and spaces from initial labels
for (x in 1:length(activityNames)) {relevantdata$activity[relevantdata$activity==x] <- activityNames[x]} #replacing integer-coded activities with descriptive names

# 4. Appropriately label the data set with descriptive variable names
library(magrittr)
colnames(relevantdata) <- relevantdata %>% names %>% sub(pattern = "[0-9]+ ",replacement = "") %>% sub(pattern = "\\(\\)",replacement = "") %>%
  sub(pattern = "^t",replacement = "time_") %>% sub(pattern = "^f",replacement = "frequency_") %>%
  sub(pattern = "Gyro",replacement = "Velocity") %>% sub(pattern = "Acc",replacement = "Acceleration") %>%
  sub(pattern = "mean",replacement = "Mean") %>% sub(pattern = "std",replacement = "STD") %>%
  sub(pattern = "Mag",replacement = "Magnitude") %>% sub(pattern = "-",replacement = "") %>%
  sub(pattern = "-",replacement = "_") %>% sub(pattern = "(X|Y|Z)$",replacement = "\\1axis") #transform the names of the column to become readable and descriptive (remove variable numbers, spaces, parentheses, expand descriptive elements, improve readibility)

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
meltdata <- melt(relevantdata,id=c("subjectID","activity"),measure.vars = as.character(names(relevantdata[,4:82]))) #making the measure values vertical, preparation to summarizing
meandata <- dcast(meltdata,activity+subjectID~variable,mean) #produces a tidy table with the average of each variable for each activity and each subject