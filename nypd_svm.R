#################################################
#  Company    : Stevens 
#  Project    : CS 513 B- Prediction of Vehicle Collision Severity using NYPD Data 2017 
#  Purpose    : Final Project
#  Topic      : SVM
#  Date       : 12/7/2018





rm(list=ls())
#################################################
#### Reading the CSV File ####
NYPD_SVM_Project <- read.csv("C:/Users/HP/Desktop/513 Project/NYPD2017.csv")

#### Removing the rows with NA in Location, Latitude, Longitude & Zip Code ####
NYPD_SVM <-na.omit(NYPD_SVM_Project)

#### Adding rows with killed or injured to find the total Killed and Injured ####
NYPD_SVM$KILLED <- NYPD_SVM$NUMBER.OF.PERSONS.KILLED + NYPD_SVM$NUMBER.OF.PEDESTRIANS.KILLED + NYPD_SVM$NUMBER.OF.CYCLIST.KILLED + NYPD_SVM$NUMBER.OF.MOTORIST.KILLED
NYPD_SVM$INJURED <- NYPD_SVM$NUMBER.OF.PERSONS.INJURED + NYPD_SVM$NUMBER.OF.PEDESTRIANS.INJURED + NYPD_SVM$NUMBER.OF.CYCLIST.INJURED + NYPD_SVM$NUMBER.OF.MOTORIST.INJURED

#### As we are predicting injured or not, we are changing the total injured to binary ####
NYPD_SVM$INJURED <- replace(NYPD_SVM$INJURED, NYPD_SVM$INJURED > 0, 1)
NYPD_SVM$INJURED <- replace(NYPD_SVM$INJURED, NYPD_SVM$INJURED == 0, 0)

#### Taking month from date for categorization ####
NYPD_SVM$DATE <- as.Date(NYPD_SVM$DATE,"%d-%m-%Y")
NYPD_SVM$MONTH <- strftime(NYPD_SVM$DATE,"%m")

#### categorizing the time into 6 divisions ####
NYPD_SVM$TIME <- gsub(":.*","",NYPD_SVM$TIME)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 4 & NYPD_SVM$TIME < 8, 2)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 8 & NYPD_SVM$TIME < 12, 3)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 12 & NYPD_SVM$TIME < 16, 4)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 16 & NYPD_SVM$TIME < 20, 5)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 20 & NYPD_SVM$TIME < 24, 6)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME == 8, 3)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME == 9, 3)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME == 10, 3)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME == 11, 3)
NYPD_SVM$TIME <- replace(NYPD_SVM$TIME, NYPD_SVM$TIME >= 0 & NYPD_SVM$TIME < 4, 1)

#### Replacing the "Blanks" with "unknown" for Vehicle Type Code ####
NYPD_SVM$VEHICLE.TYPE.CODE.2 <- sub("^$","UNKNOWN",NYPD_SVM$VEHICLE.TYPE.CODE.2)
NYPD_SVM$VEHICLE.TYPE.CODE.1 <- sub("^$","UNKNOWN",NYPD_SVM$VEHICLE.TYPE.CODE.1)
NYPD_SVM$VEHICLE.TYPE.CODE.3 <- sub("^$","UNKNOWN",NYPD_SVM$VEHICLE.TYPE.CODE.3)
NYPD_SVM$VEHICLE.TYPE.CODE.4 <- sub("^$","UNKNOWN",NYPD_SVM$VEHICLE.TYPE.CODE.4)
NYPD_SVM$VEHICLE.TYPE.CODE.5 <- sub("^$","UNKNOWN",NYPD_SVM$VEHICLE.TYPE.CODE.5)

#### Replacing the "Blanks" with "Unspecified" for CONTRIBUTING FACTORS ####
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.1 <- sub("^$","Unspecified",NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.1)
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.2 <- sub("^$","Unspecified",NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.2)
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.3 <- sub("^$","Unspecified",NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.3)
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.4 <- sub("^$","Unspecified",NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.4)
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.5 <- sub("^$","Unspecified",NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.5)

#### Categorizing the DataSet into Numericals ####
NYPD_SVM$VEHICLE.TYPE.CODE.2 <- as.integer(as.factor(NYPD_SVM$VEHICLE.TYPE.CODE.2))
NYPD_SVM$VEHICLE.TYPE.CODE.1 <- as.integer(as.factor(NYPD_SVM$VEHICLE.TYPE.CODE.1))
NYPD_SVM$VEHICLE.TYPE.CODE.3 <- as.integer(as.factor(NYPD_SVM$VEHICLE.TYPE.CODE.3))
NYPD_SVM$VEHICLE.TYPE.CODE.4 <- as.integer(as.factor(NYPD_SVM$VEHICLE.TYPE.CODE.4))
NYPD_SVM$VEHICLE.TYPE.CODE.5 <- as.integer(as.factor(NYPD_SVM$VEHICLE.TYPE.CODE.5))
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.1 <- as.integer(as.factor(NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.1))
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.2 <- as.integer(as.factor(NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.2))
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.4 <- as.integer(as.factor(NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.4))
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.5 <- as.integer(as.factor(NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.5))
NYPD_SVM$LOCATION <- as.integer(NYPD_SVM$LOCATION)
NYPD_SVM$ZIP.CODE <- as.integer(NYPD_SVM$ZIP.CODE)
NYPD_SVM$ON.STREET.NAME <- as.integer(NYPD_SVM$ON.STREET.NAME)
NYPD_SVM$CROSS.STREET.NAME <- as.integer(NYPD_SVM$CROSS.STREET.NAME)
NYPD_SVM$OFF.STREET.NAME <- as.integer(NYPD_SVM$OFF.STREET.NAME)
NYPD_SVM$BOROUGH <- as.integer(NYPD_SVM$BOROUGH)
NYPD_SVM$MONTH <- as.integer(NYPD_SVM$MONTH)
NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.3 <- as.integer(as.factor(NYPD_SVM$CONTRIBUTING.FACTOR.VEHICLE.3))
NYPD_SVM$INJURED <- as.integer(NYPD_SVM$INJURED)

NYPD_SVM$TIME <- as.integer(NYPD_SVM$TIME)
NYPD_SVM$KILLED <- as.integer(NYPD_SVM$KILLED)
NYPD_SVM$LATITUDE <- as.integer(NYPD_SVM$LATITUDE)
NYPD_SVM$LONGITUDE <- as.integer(NYPD_SVM$LONGITUDE)


#### Taking every 5th row into the test dataset, and remaining into the training dataset ####
index <- seq (1,nrow(NYPD_SVM),by=5)
test<-NYPD_SVM[index,]
training<-NYPD_SVM[-index,]
str(training)

#### Taking the first 12 features of the random forest for computation ####
training <- training[,c(-1,-2,-5,-6,-11,-12,-13,-14,-15,-16,-17,-18,-22,-23,-24,-27,-28,-29,-30)]
test <- test[,c(-1,-2,-5,-6,-11,-12,-13,-14,-15,-16,-17,-18,-22,-23,-24,-27,-28,-29,-30)]

library(e1071)

#### SVM ####
svm.model <- svm(factor(INJURED)~ ., data = training)

#### Prediction ####
svm.pred <- predict(svm.model,test)
summary(svm.pred)

#### Confusion Matrix ####
table(actual=test[,12], svm.pred )
SVM_wrong<- (test$INJURED!=svm.pred)
summary(SVM_wrong)

#### Error Rate ####
rate<-sum(SVM_wrong)/length(SVM_wrong)
