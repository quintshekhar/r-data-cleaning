# Preliminaries
# Load Packages

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
library(data.table)
path<-getwd()
path

# Get the data
# Download the file. Put it in the Data folder.

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))

# Unzip the file to Dataset folder .
# The archive put the files in a folder named UCI HAR Dataset. 
# Set this folder as the input path. List the files here.

pathIn <- file.path(path,"/Dataset/UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

# Read the files

# Read the subject files.

dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

# Read the activity files. For some reason, these are called label files in the README.txt documentation.

dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

# Read the data files. fread seems to be giving me some trouble reading files. 
# Using a helper function, read the file with read.table instead, 
# then convert the resulting data frame to a data table. Return the data table

fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

# a) Merge the training and the test sets

# 1. Concatenate the data tables
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# 2. Merge columns.
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# 3. Set key
setkey(dt, subject, activityNum)

# b) Extract only the mean and standard deviation

# 1. Read the features.txt file. This tells which variables in dt are measurements for the mean and standard deviation.
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# 2. Subset only measurements for the mean and standard deviation
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# 3. Convert the column numbers to a vector of variable names matching columns in dt
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# 4. Subset these variables using variable names.
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

# c) Use descriptive activity names
# Read activity_labels.txt file. This will be used to add descriptive names to the activities.

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

# d) Label with descriptive activity names

# 1. Merge activity labels
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)

# 2. Add activityName as a key.
setkey(dt, subject, activityNum, activityName)

# 3. Melt the data table to reshape it from a short and wide format to a tall and narrow format.
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

# 4. Merge activity name.
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)

# 5. Create a new variable, activity that is equivalent to activityName as a factor class. 
#   Create a new variable, feature that is equivalent to featureName as a factor class.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# 6. Seperate features from featureName using the helper function grepthis
grepthis <- function(regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

# 7. Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.
r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
                           "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2


# e) Create a tidy data set 

# Create a data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
head(dtTidy)
write.table(dtTidy,file = "tidyData.csv",sep = ",",row.names = FALSE)
