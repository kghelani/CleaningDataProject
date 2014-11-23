library(dplyr)

#Read the activity labels and name the columns
activity <- read.table("activity_labels.txt")
colnames(activity) <- c("activity_id", "activity")

#Read the features and name the columns
features <- tbl_df(read.table("features.txt"))
colnames(features) <- c("feature_id", "feature")

#Setup the files to read from the two directories
test_train <- c("test", "train")

#Make a list of data files to read from the test directory as well as the train directory
xfiles <- paste (test_train, "/X_", test_train, ".txt", sep="")
#Read the files and put it in the table xset
xset <- do.call("rbind", lapply(xfiles, FUN <- function (x) {read.table(x)}))

#Make a list of activity files to read from the test directory as well as the train directory
yfiles <- paste (test_train, "/y_", test_train, ".txt", sep="")
aset   <- do.call("rbind", lapply(yfiles, FUN <- function (x) {read.table(x)}))

#Make a list of subject files from training and test dataset
subfiles <- paste (test_train, "/subject_", test_train, ".txt", sep="")
subjects <- do.call("rbind", lapply(subfiles, FUN <- function (x) {read.table(x)}))

#Clean up the names of the features
xnames <- gsub("\\(|\\)|,|-", "", features$feature, ignore.case=TRUE)
colnames(xset) <- xnames

#Get all the mean and standard deviation columns.
#I specifically ignored the meanFreq column as it is weighted average
cms <- grep("mean[^F]|std", colnames(xset), ignore.case=TRUE)
xset2 <- xset [, cms]
colnames(subjects) <- c("subject")
colnames(aset) <- c("activity_id")

dbmerged <- tbl_df(cbind(xset2, aset, subjects))

#Get all the variables for which the mean needs to be calculated
#The columns are idientified by having mean or std in their names
cn <- grep("mean|std", colnames(dbmerged) )

#Add Activity Labels and mean for each variable in each row
dbmerged2 <- mutate(dbmerged, activityMean = rowMeans(dbmerged[,cn]), activitylabel = as.character(activity[dbmerged$activity_id,"activity"]))

#Summarize by subject and activity the Mean of all activities - i.e mean of all variables
aggdb <- arrange(aggregate(activityMean~subject+activitylabel, data=dbmerged2, mean),subject, activitylabel)
write.table(aggdb, file="TidyDataSet.txt", row.names=FALSE)

#Sample Data
# subject      activitylabel activityMean
# 1        1             LAYING   -0.6475142
# 2        1            SITTING   -0.6870542
# 3        1           STANDING   -0.7146459
# 4        1            WALKING   -0.1960025
# 5        1 WALKING_DOWNSTAIRS   -0.1597627
# 6        1   WALKING_UPSTAIRS   -0.3089224
# 7        2             LAYING   -0.7059496
# 8        2            SITTING   -0.6999888
# 9        2           STANDING   -0.7034673
# 10       2            WALKING   -0.3160627
# 11       2 WALKING_DOWNSTAIRS   -0.1131634
# 12       2   WALKING_UPSTAIRS   -0.2661040
# 13       3             LAYING   -0.6967239
# 14       3            SITTING   -0.6713327
# 15       3           STANDING   -0.6768824
# 16       3            WALKING   -0.3071634
# 17       3 WALKING_DOWNSTAIRS   -0.2346462
# 18       3   WALKING_UPSTAIRS   -0.3487826

#Show it in cross tabular form
rx <- xtabs(activityMean~subject+activitylabel, aggdb)
rx
# 
# activitylabel
# subject       LAYING      SITTING     STANDING      WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
# 1  -0.647514216 -0.687054248 -0.714645902 -0.196002515       -0.159762697     -0.308922413
# 2  -0.705949621 -0.699988799 -0.703467304 -0.316062690       -0.113163420     -0.266103957
# 3  -0.696723911 -0.671332669 -0.676882403 -0.307163411       -0.234646231     -0.348782613
# 4  -0.692118101 -0.680925785 -0.669364811 -0.379079879       -0.282272396     -0.330589995
# 5  -0.702458114 -0.681196653 -0.662333019 -0.283922893       -0.157663886     -0.243153342
# 6  -0.679891023 -0.680244769 -0.687461166 -0.231064655       -0.021902981     -0.192166315
# 7  -0.687922118 -0.675978972 -0.689463184 -0.186823899       -0.173527590     -0.353535827
# 8  -0.686409976 -0.689734668 -0.698539499 -0.163131383       -0.020438210     -0.150972134
# 9  -0.692518805 -0.668725246 -0.682874561 -0.280218633       -0.169093105     -0.341700283
# 10 -0.718941547 -0.699915208 -0.685194896 -0.205235573       -0.120764395     -0.213790364
# 11 -0.701733102 -0.692349957 -0.709761477 -0.364348025       -0.222680350     -0.360321619
# 12 -0.691896517 -0.690818321 -0.684150949 -0.254204739       -0.184693603     -0.343206751
# 13 -0.693767369 -0.697665683 -0.705919273 -0.210629439       -0.179167651     -0.322113682
# 14 -0.670502191 -0.693652466 -0.698522237 -0.280707529       -0.006958223     -0.114441263
# 15 -0.696266195 -0.696175463 -0.696950905 -0.298658661       -0.123691470     -0.291778735
# 16 -0.706804389 -0.697518650 -0.707451674 -0.369655285       -0.219615402     -0.368696495
# 17 -0.693701579 -0.709769396 -0.716884952 -0.299829213       -0.238479430     -0.298708751
# 18 -0.708037609 -0.705401709 -0.707474295 -0.382003669       -0.365020265     -0.409564680
# 19 -0.705709621 -0.689824683 -0.698655833 -0.084301171        0.117381358     -0.200841188
# 20 -0.701072087 -0.688908284 -0.669005536 -0.105698955       -0.052363261     -0.207641153
# 21 -0.691827451 -0.705747266 -0.704862476 -0.235145531       -0.148164490     -0.293544753
# 22 -0.682176713 -0.682420673 -0.690576824 -0.161858747       -0.158006860     -0.194934229
# 23 -0.701566301 -0.689068177 -0.699263275 -0.111431701       -0.028889625     -0.257785430
# 24 -0.700948383 -0.699120406 -0.697889745 -0.345425421       -0.239448623     -0.351499380
# 25 -0.627325384 -0.699415387 -0.701774673 -0.400788239       -0.334714738     -0.417174229
# 26 -0.707691053 -0.694173442 -0.703057367 -0.307898889       -0.177606524     -0.323287108
# 27 -0.709611473 -0.700116769 -0.706689716 -0.305482504       -0.188593361     -0.356505359
# 28 -0.716147728 -0.694115806 -0.677277584 -0.304814660       -0.148865861     -0.315700796
# 29 -0.708907810 -0.700564318 -0.709134815 -0.251645815       -0.181520210     -0.228176239
# 30 -0.696256270 -0.693896923 -0.678279135 -0.259481100       -0.214419990     -0.324394989






