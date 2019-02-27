#################################################
#  Company    : Stevens 
#  Project    : CS 513 B- Prediction of Vehicle Collision Severity using NYPD Data 2017 
#  Purpose    : Final Project
#  Topic      : C5.0   
#  Date       : 12/7/2018


rm(list=ls())
#################################################
###########NYPD COLLISION PREDICTION#####


########Loading the data######
NYC <- read.csv("C://Users/Murali Gudipati/Downloads/NYPD2017.csv")
NYC<-na.omit(NYC)
#########binning the time############
NYC$TIME<-gsub(":.*","",NYC$TIME)
NYC$TIME<-as.numeric(NYC$TIME)
typeof(NYC$TIME)
str(NYC$TIME)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 0 & NYC$TIME <= 3, 1)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 4 & NYC$TIME <= 7, 2)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 12 & NYC$TIME <=15, 4)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 16 & NYC$TIME <= 19, 5)
NYC$TIME<- replace(NYC$TIME, NYC$TIME >= 20 & NYC$TIME <=23, 6)

NYC$TIME<- replace(NYC$TIME, NYC$TIME == 8, 3)

NYC$TIME<- replace(NYC$TIME, NYC$TIME == 9, 3)
NYC$TIME<- replace(NYC$TIME, NYC$TIME == 10, 3)
NYC$TIME<- replace(NYC$TIME, NYC$TIME == 11, 3)
###########Converting date into month############
NYC$DATE<-as.Date(NYC[,1],"%m/%d/%Y")
NYC["MONTH"]<-NA
NYC$MONTH<-format(NYC$DATE,"%m")
str(NYC$MONTH)
summary(NYC$KILLED)
NYC$MONTH<-as.integer(NYC$MONTH)
########adding total killed and total injured##########
NYC$KILLED <- NYC$NUMBER.OF.PERSONS.KILLED + NYC$NUMBER.OF.PEDESTRIANS.KILLED + NYC$NUMBER.OF.CYCLIST.KILLED + NYC$NUMBER.OF.MOTORIST.KILLED
NYC$INJURED <- NYC$NUMBER.OF.PERSONS.INJURED + NYC$NUMBER.OF.PEDESTRIANS.INJURED + NYC$NUMBER.OF.CYCLIST.INJURED + NYC$NUMBER.OF.MOTORIST.INJURED
############Adding total injured###############
NYC$INJUREDORNOT<- replace(NYC$INJUREDORNOT, NYC$INJURED >0, 1)
NYC$INJUREDORNOT<- replace(NYC$INJUREDORNOT, NYC$INJURED ==0, 0)
#################FILLING THE BLANKS#########
NYC$VEHICLE.TYPE.CODE.1[NYC$VEHICLE.TYPE.CODE.1==""]<-"UNKNOWN"
NYC$VEHICLE.TYPE.CODE.2[NYC$VEHICLE.TYPE.CODE.2==""]<-"UNKNOWN"
NYC$CONTRIBUTING.FACTOR.VEHICLE.1[NYC$CONTRIBUTING.FACTOR.VEHICLE.1==""]<-"Unspecified"
NYC$CONTRIBUTING.FACTOR.VEHICLE.2[NYC$CONTRIBUTING.FACTOR.VEHICLE.2==""]<-"Unspecified"

############# Getting features from Random forest###########
NY3<-NYC[,c(4,19,20,25,26,32,34)]
idx <- seq (1,nrow(NY3),by=5)
test<-NY3[idx,]
training<-NY3[-idx,]
####################
library('C50')

### tree creation ###
C5.0_TREE<-C5.0(factor(INJUREDORNOT) ~.,data= training)
C5.0_TREE


summary(C5.0_TREE)


plot(C5.0_TREE)

### prediction ###
C5.0_Predict<-predict(C5.0_TREE,test,type="class")
C5.0_Predict


table(test[,7],C5.0_Predict)


### error rate estimation ###

wrong_C5.0<- (test[,7]!=C5.0_Predict)
rate_C5.0<-sum(wrong_C5.0)/length(wrong_C5.0)
rate_C5.0












