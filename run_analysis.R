library(plyr)

You should create one R script called run_analysis.R that does the following. 

Creates a second, independent tidy data set with the average of each variable for
each activity and each subject. 



# load activity labels and features
actLabs <- read.table(("activity_labels.txt"), sep="",stringsAsFactors=FALSE)
features <- read.table(("features.txt"), sep="",stringsAsFactors=FALSE)

# load training data and test data
x_train <- read.table(("X_train.txt"), sep="",col.names = features$V2)
y_train <- read.table(("y_train.txt"), sep="",col.names = "activity")
subject_train <- read.table(("subject_train.txt"), sep="",col.names="ID")

x_test <- read.table(("X_test.txt"), sep="",col.names = features$V2)
y_test <- read.table((,"y_test.txt"), sep="",col.names = "activity")
subject_test <- read.table(("subject_test.txt"), sep="",col.names="ID")

# cbind because order is the same in all three data sets
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)

# merge datasets
full <- rbind(train, test)
full <- arrange(full, ID) # reorder with ID
full$activity <- factor(full$activity, levels=actLabs$V1, labels=actLabs$V2)
full$ID <- as.factor(full$ID)
summary(full[,c("ID", "activity")])

# Extract standard deviation and mean
tidy1 <- full[,c(1,2,
                 grep("std", names(full)),
                 grep("mean", names(full)))]
summary(tidy1)
dim(tidy1)
write.table(tidy1, "tidy_dataset1.txt", sep=";", row.names=FALSE)


# second tidy data set with ID and activity means
tidy2 <- ddply(tidy1, .(ID, activity), .fun=function(x){
  colMeans(x[,-c(1:2)])
})
colnames(tidy2)[-c(1:2)] <- paste0(colnames(tidy2)[-c(1:2)], "_mean")

summary(tidy2)
dim(tidy2)
write.table(tidy2, "tidy_dataset2.txt", sep=";", row.names=FALSE)
