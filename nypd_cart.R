#################################################
#  Company    : Stevens 
#  Project    : CS 513 B- Prediction of Vehicle Collision Severity using NYPD Data 2017 
#  Purpose    : Final Project
#  Topic      : CART  
#  Date       : 12/7/2018


rm(list=ls())
#######NYPD COLLISION PREDICTION#####

########Loading the data######
NYC <- read.csv("C:/Users/kitan/Downloads/NYPD2017.csv")
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
############# Getting features from Random forest###########
NY3<-NYC[,c(4,19,20,25,26,32,34)]
idx <- seq (1,nrow(NY3),by=5)
test<-NY3[idx,]
training<-NY3[-idx,]
##########CART##################
mytree<-rpart( INJUREDORNOT~.,data=training)
mytree
###########plot the tree#####
plot(mytree)
text(mytree)
prp(mytree)
#########prediction######
Prediction <- predict(mytree, test[,])
round_Pre<-round(Prediction)
table(actual=test$INJUREDORNOT,round_Pre)
###########error rate######
wrong<- (test$INJUREDORNOT!=round_Pre)
error_rate<-sum(wrong)/length(wrong)
error_rate 
pdf("tree.pdf")
fancyRpartPlot(mytree)