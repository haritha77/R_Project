#################################################
#  Company    : Stevens 
#  Project    : CS 513 B- Prediction of Vehicle Collision Severity using NYPD Data 2017 
#  Purpose    : Final Project
#  Topic      : KNN & ANN
#  Date       : 12/7/2018


rm(list=ls())
#################################################


##Loading the data
NY<-read.csv("C://Users/Murali Gudipati/Downloads/NYPD2017.csv")
View(NY)
NY<-data.frame(NY)

## converting date into month
NY$DATE<-as.Date(NY[,1],"%m/%d/%Y")
NY["MONTH"]<-NA
NY$MONTH<-format(NY$DATE,"%m")

## TOTAL INJURED=NUMBER.OF.PERSONS.INJURED+NUMBER.OF.PEDESTRIANS.INJURED+NUMBER.OF.CYCLIST.INJURED+ NUMBER.OF.MOTORIST.INJURED
NY["TOTAL_INJURED"]<-NA
NY$TOTAL_INJURED<-NY$NUMBER.OF.PERSONS.INJURED+NY$NUMBER.OF.PEDESTRIANS.INJURED+NY$NUMBER.OF.CYCLIST.KILLED+NY$NUMBER.OF.MOTORIST.INJURED

##TOTAL KILLED= NUMBER.OF.PERSONS.KILLED+NUMBER.OF.PEDESTRIANS.KILLED+NUMBER.OF.CYCLIST.KILLED +NUMBER.OF.MOTORIST.KILLED 
NY["TOTAL_KILLED"] <- NA
NY$TOTAL_KILLED<-NY$NUMBER.OF.PERSONS.KILLED+NY$NUMBER.OF.PEDESTRIANS.KILLED+NY$NUMBER.OF.CYCLIST.KILLED+NY$NUMBER.OF.MOTORIST.KILLED

##Spliting the time into T1-T6
NY$TIME<-gsub("\\::*","",NY$TIME)
NY$TIME<- as.numeric(NY$TIME)
for(row in 1:nrow(NY))
{
  
  if((NY$TIME[row]>=800) && (NY$TIME[row]<= 1159))
  {
    NY$TIME[row]<-3
  }
  else if((NY$TIME[row]>=400) && (NY$TIME[row]<=759))
  {
    NY$TIME[row]<-2
    
  }
  else if((NY$TIME[row]>=1200) && (NY$TIME[row]<=1559))
  {
    NY$TIME[row]<-4
    
  }
  else if((NY$TIME[row]>=1600) && (NY$TIME[row]<=1959))
  {
    NY$TIME[row]<-5
  }
  else if((NY$TIME[row]>=2000) && (NY$TIME[row]<=2359))
  {
    NY$TIME[row]<-6
  }
  else
  { 
    NY$TIME[row]<-1
  }
}

## Changing injuries into binary (0 or 1)
for(row in 1:nrow(NY))
{
  if(NY$TOTAL_INJURED[row]==0)
    NY$TOTAL_INJURED[row]<-0
  else
    NY$TOTAL_INJURED[row]<-1
}

## for knn adding columns depcting categorisation of LOCATION 
NY["LO_QUEENS"]<-NA
NY["LO_MANHATTAN"]<-NA
NY["LO_BRONX"]<-NA
NY["LO_BROOKLYN"]<-NA
NY["LO_STATEN"]<-NA
for(row in 1:nrow(NY))
{
  
  if(NY$BOROUGH[row]=="QUEENS")
  {
    NY$LO_QUEENS[row]<-1
  }
  
  else if(NY$BOROUGH[row]=="MANHATTAN")
  {
    NY$LO_MANHATTAN[row]<-1
    
  }
  
  else if(NY$BOROUGH[row]=="BRONX")
  {
    NY$LO_BRONX[row]<-1
  }
  else if(NY$BOROUGH[row]=="BROOKLYN")
  {
    NY$LO_BROOKLYN[row]<-1
  }
  else if(NY$BOROUGH[row]=="STATEN ISLAND")
  {
    NY$LO_STATEN[row]<-1
  }
}

NY$LO_QUEENS[is.na(NY$LO_QUEENS)]<-0
NY$LO_MANHATTAN[is.na(NY$LO_MANHATTAN)]<-0
NY$LO_BRONX[is.na(NY$LO_BRONX)]<-0
NY$LO_BROOKLYN[is.na(NY$LO_BROOKLYN)]<-0
NY$LO_STATEN[is.na(NY$LO_STATEN)]<-0

NY<-na.omit((NY))


## EDA
count1<-0
count2<-0
count3<-0
count4<-0
count5<-0

for(ro in 1:nrow(NY))
{
  if(NY$TOTAL_INJURED[ro]==1 && NY$BOROUGH[ro]=="BRONX")
    count1<-count1+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$BOROUGH[ro]=="QUEENS")
    count2<-count2+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$BOROUGH[ro]=="BROOKLYN")
    count3<-count3+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$BOROUGH[ro]=="MANHATTAN")
    count4<-count4+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$BOROUGH[ro]=="STATEN ISLAND")
    count5<-count5+1
}

# Simple Pie Chart of borough vs Total_injured
slices <- c(count1, count2,count3, count4,count5)
lbls <- c("BRONX", "QUEENS", "BROOKLYN", "MANHATTAN", "STATEN ISLAND")
pie(slices, labels = lbls, main="Pie Chart of brough")
countt1<-0
countt2<-0
countt3<-0
countt4<-0
countt5<-0
countt6<-0

for(ro in 1:nrow(NY))
{
  
  if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==1)
    countt1<-countt1+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==2)
    countt2<-countt2+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==3)
    countt3<-countt3+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==4)
    countt4<-countt4+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==5)
    countt5<-countt5+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$TIME[ro]==6)
    countt6<-countt6+1
}

# Simple Pie Chart Time vs Total injured
slices <- c(countt1, countt2,countt3, countt4,countt5,countt6)
lbls <- c("T1", "T2", "T3", "T4", "T5","T6")
pie(slices, labels = lbls, main="Pie Chart of Time")

countm1<-0
countm2<-0
countm3<-0
countm4<-0
countm5<-0
countm6<-0
countm7<-0
countm8<-0
countm9<-0
countm10<-0
countm11<-0
countm12<-0

for(ro in 1:nrow(NY))
{
  
  if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="01")
    countm1<-countm1+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="02")
    countm2<-countm2+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="03")
    countm3<-countm3+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="04")
    countm4<-countm4+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="05")
    countm5<-countm5+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="06")
    countm6<-countm6+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="07")
    countm7<-countm7+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="08")
    countm8<-countm8+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]=="09")
    countm9<-countm9+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]==10)
    countm10<-countm10+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]==11)
    countm11<-countm11+1
  else if(NY$TOTAL_INJURED[ro]==1 && NY$MONTH[ro]==12)
    countm12<-countm12+1
  
}

# Simple Pie Chart of month vs injuries
slices <- c(countm1, countm2,countm3, countm4,countm5,countm6,countm7,countm8,countm9,countm10,countm11,countm12)
lbls <- c(1,2,3,4,5,6,7,8,9,10,11,12)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of month")
pie(slices, labels = lbls, main="Pie Chart of month")
hist(slices)
barplot(slices)

View(NY)
install.packages("data.table")
install.packages("mltools")
library("data.table")
library("mltools")

## converting blank data into unknown and unspecified
NY$VEHICLE.TYPE.CODE.1[NY$VEHICLE.TYPE.CODE.1==""]<-"UNKNOWN"
NY$VEHICLE.TYPE.CODE.2[NY$VEHICLE.TYPE.CODE.2==""]<-"UNKNOWN"
NY$CONTRIBUTING.FACTOR.VEHICLE.1[NY$CONTRIBUTING.FACTOR.VEHICLE.1]<-"Unspecified"
NY$CONTRIBUTING.FACTOR.VEHICLE.2[NY$CONTRIBUTING.FACTOR.VEHICLE.2]<-"Unspecified"
View(NY)

##Taking the top few categories from Random forest for knn
NY1<-NY[,c(4,19,20,25,26)]
View(NY)
?cbind()

## one hot encoding
NY1<- one_hot(as.data.table(NY1))
NY2<-cbind(NY1,NY[,c(32,34:38)])
str(NY2)
NY2 <- as.data.frame(apply(NY2, 2, function(x) (x - min(x))/(max(x)-min(x))))

##knn

library("kknn")
idx <- seq (1,nrow(NY2),by=4)
test<-NY2[idx,]
training<-NY2[-idx,]


##k=1
predict_k2<-kknn(formula=TOTAL_INJURED~.,training,test,k=1,kernel="rectangular")
fit<- fitted(predict_k2)
fit<-round(fit)
factor(fit)
factor(test$TOTAL_INJURED)
x0<-table(test$TOTAL_INJURED,fit)
x0
avg0<-sum(diag(x0))/sum(x0)
100-(avg0*100)

 
## k=3
predict_k2<-kknn(formula=TOTAL_INJURED~.,training,test,k=3,kernel="rectangular")
fit<- fitted(predict_k2)
fit<-round(fit)
factor(fit)
factor(test$TOTAL_INJURED)
x0<-table(test$TOTAL_INJURED,fit)
x0
##error rate
wrong<-(test$TOTAL_INJURED!=fit)
rate<-sum(wrong)/length(wrong)
rate

## k=5
predict_k2<-kknn(formula=TOTAL_INJURED~.,training,test,k=5,kernel="rectangular")
fit<- fitted(predict_k2)
fit<-round(fit)
factor(fit)
factor(test$TOTAL_INJURED)
x0<-table(test$TOTAL_INJURED,fit)
x0
##error rate
avg0<-sum(diag(x0))/sum(x0)
100-(avg0*100)

##k=10
predict_k2<-kknn(formula=TOTAL_INJURED~.,training,test,k=10,kernel="rectangular")
fit<- fitted(predict_k2)
fit<-round(fit)
factor(fit)
factor(test$TOTAL_INJURED)
x0<-table(test$TOTAL_INJURED,fit)
x0
avg0<-sum(diag(x0))/sum(x0)
100-(avg0*100)

##k=15
predict_k2<-kknn(formula=TOTAL_INJURED~.,training,test,k=15,kernel="rectangular")
fit<- fitted(predict_k2)
fit<-round(fit)
factor(fit)
factor(test$TOTAL_INJURED)
x0<-table(test$TOTAL_INJURED,fit)
x0
avg0<-sum(diag(x0))/sum(x0)
100-(avg0*100)

##ANN Data cleaning
NY7<-NY[,c(4,7,8,9,19,20,25,26,32)]
NY8<-NY7
NY8$ZIP.CODE<-as.numeric(NY7$ZIP.CODE)
NY8$LOCATION<-as.numeric(NY7$LOCATION)
NY8$ON.STREET.NAME<-as.numeric(NY7$ON.STREET.NAME)
NY8$CROSS.STREET.NAME<-as.numeric(NY7$CROSS.STREET.NAME)
NY8$CONTRIBUTING.FACTOR.VEHICLE.1<-as.numeric(NY7$CONTRIBUTING.FACTOR.VEHICLE.1)
NY8$CONTRIBUTING.FACTOR.VEHICLE.2<-as.numeric(NY7$CONTRIBUTING.FACTOR.VEHICLE.2)
NY8$VEHICLE.TYPE.CODE.1<-as.numeric(NY7$VEHICLE.TYPE.CODE.1)
NY8$VEHICLE.TYPE.CODE.2<-as.numeric(NY7$VEHICLE.TYPE.CODE.2)
NY8$TOTAL_INJURED<-as.numeric(NY7$TOTAL_INJURED)
View(NY8)

##ANN
library("neuralnet")
?neuralnet()
##Normalisation
NY9 <- as.data.frame(apply(NY8, 2, function(x) (x - min(x))/(max(x)-min(x))))
View(NY9)
index <- seq (1,nrow(NY9),by=5)
test<-NY9[index,]
training<-NY9[-index,]
View(NY9)

##hidden layer =1
net_bc2  <- neuralnet(TOTAL_INJURED~ZIP.CODE+LOCATION+ON.STREET.NAME+CROSS.STREET.NAME+
                        CONTRIBUTING.FACTOR.VEHICLE.1+CONTRIBUTING.FACTOR.VEHICLE.2+VEHICLE.TYPE.CODE.1+VEHICLE.TYPE.CODE.2
                      ,training, hidden=1, threshold=0.01)
##plotting
plot(net_bc2)
View(test)
net_bc2_results <-compute(net_bc2, test[,-9])
ANN=as.numeric(net_bc2_results$net.result)
factor(ANN)
mean(ANN)
median(ANN)
ANN_round<-round(ANN)
table(Actual=test$TOTAL_INJURED,ANN_round)
wrong<- (test$TOTAL_INJURED!=ANN_round)
rate<-sum(wrong)/length(wrong)
rate

##hidden layer =5
net_bc2  <- neuralnet(TOTAL_INJURED~ZIP.CODE+LOCATION+ON.STREET.NAME+CROSS.STREET.NAME+
                        CONTRIBUTING.FACTOR.VEHICLE.1+CONTRIBUTING.FACTOR.VEHICLE.2+VEHICLE.TYPE.CODE.1+VEHICLE.TYPE.CODE.2
                      ,training, hidden=5, threshold=0.01)
##plotting
plot(net_bc2)
View(test)
net_bc2_results <-compute(net_bc2, test[,-9])
ANN=as.numeric(net_bc2_results$net.result)
factor(ANN)
mean(ANN)
median(ANN)
ANN_round<-round(ANN)
table(Actual=test$TOTAL_INJURED,ANN_round)
wrong<- (test$TOTAL_INJURED!=ANN_round)
rate<-sum(wrong)/length(wrong)
rate




