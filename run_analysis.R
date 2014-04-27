library(stringr)
library(plyr)
test_dir <- "./UCI HAR Dataset/test/"
train_dir <- "./UCI HAR Dataset/train/"
labels_file <- "./UCI HAR Dataset/activity_labels.txt"
features_file <- "./UCI HAR Dataset/features.txt"

all_features <- read.table(features_file)
features <- grep("mean()|std()", all_features$V2)
features_names <- grep("mean()|std()", all_features$V2, value=TRUE)

activity_labels <- read.table(labels_file, col.names = c("key", "label"), stringsAsFactors=FALSE)

# This function: 
# reads data, 
# extracts only the measurements on the mean and standard deviation for each measurement,
# uses descriptive activity names to name the activities in the data set
read_data <- function(type, dir) {
  X <- read.table( paste0(dir, "X_", type, ".txt"))
  colnames(X) <- all_features[[2]]
  X <- X[,features]
  
  Y <- read.table( paste0(dir, "Y_", type, ".txt") )
  activity <- factor(Y[[1]], labels = activity_labels[[2]])
  
  subject <- read.table( paste0(dir, "subject_", type, ".txt"), col.names = "subject" )
  data <- cbind(subject, activity=activity, X)
  return (data)
}

# Read test and train data sets
test_data <- read_data("test", test_dir)
train_data <- read_data("train", train_dir)

# Merge test and train datasets
data <- rbind(test_data, train_data)

# Create a second, independent tidy data set with the average of each variable 
# for each activity and each subject
averages <- ddply(data, c("subject", "activity"), 
                    .fun = function(x){return(apply(x[, -c(1,2)], 2, mean))}
                  )
write.table(averages, "./averages.txt", row.names = FALSE)