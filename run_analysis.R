# First read all the feature names.
features <- read.table("./samsung/UCI HAR Dataset//features.txt",sep= " ", header=F)
# Check the type and number of features.
class(features[,2])
length(features[,2])
colnames <- features[,2]
#

# Read training dataset and set the column names to the 2nd column of features table.
#
trainData <- read.table("./samsung/UCI HAR Dataset/train/X_train.txt")
colnames(trainData) <- features[,2]
head(trainData)
#
testData <- read.table("./samsung/UCI HAR Dataset/test/X_test.txt")
colnames(testData) <- features[,2]
#
# merge the two tables (training and test data)
mergeData <- rbind(trainData,testData)
#
#
# Checking how many feature names have mean() and std() in them.
#
dim(subset(features,grepl(".*mean\\(\\).*",features[,2])))
dim(subset(features,grepl(".*std\\(\\).*",features[,2])))
# Combining the above two in one.
dim(subset(features,grepl(".*mean\\(\\).*",features[,2]) | grepl(".*std\\(\\).*",features[,2])))
class(subset(features,grepl(".*mean\\(\\).*",features[,2]) | grepl(".*std\\(\\).*",features[,2]))[,1])
#
#
# Get ID of all the features with mean() and std() in them
colId <- subset(features,grepl(".*mean\\(\\).*",features[,2]) | grepl(".*std\\(\\).*",features[,2]))[,1]
colId
# Get all the features with mean() and std() in their names.
colNames <- subset(features,grepl(".*mean\\(\\).*",features[,2]) | grepl(".*std\\(\\).*",features[,2]))[,2]
#
# subset the data on column ids extracted above
subData <- mergeData[,colId]
# visually checking if the above table have only those features with mean() and std() in their names by printing the column names.
names(subData)
# Checking if the subset of the data has missing values.
summary(subData)
if (sum(complete.cases(subData)) == nrow(subData)){
  print("No NA's in the data")
} else {
  print("clean the data")
}
# There are no missing values, so the data is clean.
#
#
# Now read the activity label and subject label tables for both training and test data sets.
trainActivityData <- read.table("./samsung/UCI HAR Dataset/train/y_train.txt")
testActivityData <- read.table("./samsung/UCI HAR Dataset/test/y_test.txt")
# Just checking if the values are 1:6
unique(trainActivityData)
unique(testActivityData)
#
trainSubjectData <- read.table("./samsung/UCI HAR Dataset/train/subject_train.txt")
unique(trainSubjectData)
testSubjectData <- read.table("./samsung/UCI HAR Dataset/test/subject_test.txt")
unique(testSubjectData)
#
# merge the rows of training and test data for activity and subject.
mergeActivityData <- rbind(trainActivityData,testActivityData)
mergeSubjectData <- rbind(trainSubjectData,testSubjectData)
#
#
# Bind the columns of Activity and Subject data and give the column names
actSubData <- cbind(mergeActivityData,mergeSubjectData) 
colnames(actSubData) <- c("Activity","Subject")
#
# Final data: Bind the columns of above table with the table containing mean and std information.
#
completeData <- cbind(subData,actSubData)
#
#
## change the column names and give more descriptive names.
# First remove spurious characters such as "-" and "()"
colnames(completeData)
colnames(completeData) <- gsub("-","",colnames(completeData))
colnames(completeData) <- gsub("\\(\\)","",colnames(completeData))
# Expand t and f to provide better meaning to the names.
colnames(completeData) <- gsub("^t","time",colnames(completeData))
colnames(completeData) <- gsub("^f","frequency",colnames(completeData))
colnames(completeData)
#write.csv(completeData,file="tidyData.csv",row.names=F)
write.table(completeData,file="tidyData.txt",row.names=F)
#tmp <- read.table("./tidyData.txt",header=T)
#
#
# Create the second table with the average of each variable by subject and activity.
# aveSubActData <- aggregate(completeData[,1:66],by=list(completeData$Activity,completeData$Subject),mean)
# The above removes the column name for activity and subject.So, I used the following.
aveSubActData <- aggregate(completeData[,1:66],by=completeData[,67:68],mean)