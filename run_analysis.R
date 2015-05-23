# run_analysis.R should do the following:
#   
#   1)  Merges the training and the test sets to create one data set.
# 
#   2)  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 
#   3)  Uses descriptive activity names to name the activities in the data set
# 
#   4)  Appropriately labels the data set with descriptive variable names. 
#   
#   5)  From the data set in step 4, creates a second, independent tidy data set with the average
#       of each variable for each activity and each subject.

#     Please upload the tidy data set created in step 5 of the instructions. 

#     Please upload your data set as a txt file created with write.table() using row.name=FALSE 
#     (do not cut and paste a dataset directly into the text box, as this may cause errors 
#     saving your submission).

# Load packages
# Lendo Pacotes
packages <- c("data.table", "reshape2", "dplyr")
# Nice idea
# Otima ideia
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# Set Directory
# Configurando Diretorio
path <- getwd()

# Set Data Directory
# Diretorio de Dados
DataPath <- file.path(path, "UCI HAR Dataset")

# Read test - train data
# Lendo arquivos Test and Train 
dTrain <- fread(file.path(DataPath, "train", "subject_train.txt"))
dTest  <- fread(file.path(DataPath, "test" , "subject_test.txt" ))

# Read  'Activity'
# Lendo 'Activity Data'
dTrainAct <- fread(file.path(DataPath, "train", "Y_train.txt"))
dTestAct  <- fread(file.path(DataPath, "test" , "Y_test.txt" ))


# Read X-train & X-test
# Lendo X-Train e Test
dtTrainMed <- data.table(read.table(file.path(DataPath, "train", "X_train.txt")))
dtTestMed  <- data.table(read.table(file.path(DataPath, "test" , "X_test.txt")))

# Row merge the Training - Test 
# Unindo treinamento e teste
dtSubjects <- rbind(dTrain, dTest)
setnames(dtSubjects, "V1", "subject")

# Row merge the Training and Test
# Unindo todos
dtActivities <- rbind(dTrainAct, dTestAct)
setnames(dtActivities, "V1", "activityNumber")

# Merge the Training and Test
# Novamente...
dtMeasures <- rbind(dtTrainMed, dtTestMed)

# Column merge the subjects to activities
# Nomes interessantes
dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dtSActWM <- cbind(dtSubjectActivities, dtMeasures)

# Order all of the combined data by, subject and activity

setkey(dtSActWM, subject, activityNumber)

## Read in the 'features.txt' 
## This file matches up to the columns in the data.table, dtSubjectActivitiesWithMeasures
## with the features/measures.
dtAllFeatures <- fread(file.path(DataPath, "features.txt"))
setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)]
# Create a column to 'index/cross reference' into the 'measure' headers
# in dtSubjectActivitiesWithMeasures
dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)]

# Build up the columns to select from the data.table,
# dtSubjectActivitiesWithMeasures
columnsToSelect <- c(key(dtSActWM), dtMeanStdMeasures$measureCode)
# Just take the rows with the columns of interest ( std() and mean() )
dtSubActMMs <- subset(dtSActWM, 
                                                select = columnsToSelect)

# Read in the activity names and give them more meaningful names
dtActivityNames <- fread(file.path(DataPath, "activity_labels.txt"))
setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName"))

# Merge the 'meaningful activity names' with the 
# dtSubjectActiitiesWithMeasuresMeanStd
dtSubActMMs <- merge(dtSubActMMs, 
                                               dtActivityNames, by = "activityNumber", 
                                               all.x = TRUE)

# Sort the data.table, dtSubActMMs
setkey(dtSubActMMs, subject, activityNumber, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
dtSubActMMs <- data.table(melt(dtSubActMMs, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureCode", 
                                                         value.name="measureValue"))

# Merge measure codes
dtSubActMMs <- merge(dtSubActMMs, 
                                               dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)], 
                                               by="measureCode", all.x=TRUE)

# Convert activityName and measureName to factors
dtSubActMMs$activityName <- 
  factor(dtSubActMMs$activityName)
dtSubActMMs$measureName <- 
  factor(dtSubActMMs$measureName)

# Reshape the data to get the averages 
measureAvgerages <- dcast(dtSubActMMs, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")