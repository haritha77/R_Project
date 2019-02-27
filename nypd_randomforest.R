#################################################
#  Company    : Stevens 
#  Project    : CS 513 B- Prediction of Vehicle Collision Severity using NYPD Data 2017 
#  Purpose    : Final Project
#  Topic      : RandomForest
#  Date       : 12/7/2018


rm(list=ls())
#################################################

###Import the files###
dd3<-read.csv("C:/Users/admin/Downloads/NYPD2017.csv" ,header=TRUE , na.strings=" ")
colnames(dd3)
str(dd3)

###adding new columns for EDA###
dd3$total.no.of.killed=dd3$NUMBER.OF.PERSONS.KILLED+dd3$NUMBER.OF.PEDESTRIANS.KILLED+dd3$NUMBER.OF.CYCLIST.KILLED+dd3$NUMBER.OF.MOTORIST.KILLED
dd3$total.no.of.injured=dd3$NUMBER.OF.PERSONS.INJURED+dd3$NUMBER.OF.PEDESTRIANS.INJURED+dd3$NUMBER.OF.CYCLIST.INJURED+dd3$NUMBER.OF.MOTORIST.INJURED
factor(dd3$total.no.of.injured)
dd3$total.no.of.injured[ dd3$total.no.of.injured>0]=1     
dd3$total.no.of.injured[ dd3$total.no.of.injured==0]=0


colnames(dd3)
View(dd3)  

###importing package for Time and Date splitting ###
install.packages('lubridate')
library(lubridate)


### Month Column ###
bb<-as.factor(dd3$DATE)
ans<-mdy(bb)
dd3$months<-month(ans)

###Time Division split ###
b<-as.factor(dd3$TIME)

# parese date
a <- hms(as.character(b))

# get hours
hour(a)

# get minutes
minute(a)
dd3$timezone[hour(a)==0  |hour(a)==01 |hour(a)==02 |hour(a)==03]=1
dd3$timezone[hour(a)==04 |hour(a)==05 |hour(a)==06 |hour(a)==07]=2
dd3$timezone[hour(a)==08 |hour(a)==09 |hour(a)==10 |hour(a)==11]=3
dd3$timezone[hour(a)==12 |hour(a)==13 |hour(a)==14 |hour(a)==15]=4
dd3$timezone[hour(a)==16 |hour(a)==17 |hour(a)==18 |hour(a)==19]=5
dd3$timezone[hour(a)==20 |hour(a)==21 |hour(a)==22 |hour(a)==23]=6

colnames(dd3)  
View(dd3)


###removing all NA values ###
dd33<-na.omit(dd3)
apply(is.na(dd33),2,any)

### 
install.packages('caTools')
require(caTools)

### categorizing variables ###
dd33$timezone<-as.integer(dd33$timezone)
dd33$CONTRIBUTING.FACTOR.VEHICLE.1=as.integer(dd33$CONTRIBUTING.FACTOR.VEHICLE.1)
dd33$CONTRIBUTING.FACTOR.VEHICLE.2=as.integer(dd33$CONTRIBUTING.FACTOR.VEHICLE.2)
dd33$CONTRIBUTING.FACTOR.VEHICLE.3=as.integer(dd33$CONTRIBUTING.FACTOR.VEHICLE.3)
dd33$CONTRIBUTING.FACTOR.VEHICLE.4=as.integer(dd33$CONTRIBUTING.FACTOR.VEHICLE.4)
dd33$CONTRIBUTING.FACTOR.VEHICLE.5=as.integer(dd33$CONTRIBUTING.FACTOR.VEHICLE.5)
dd33$OFF.STREET.NAME=as.integer(dd33$OFF.STREET.NAME)
dd33$ON.STREET.NAME=as.integer(dd33$ON.STREET.NAME)
dd33$CROSS.STREET.NAME=as.integer(dd33$CROSS.STREET.NAME)
dd33$LOCATION=as.integer(dd33$LOCATION)
dd33$DATE=as.integer(dd33$DATE)
dd33$TIME=as.integer(dd33$TIME)
dd33$BOROUGH=as.integer(dd33$BOROUGH)
dd33$LONGITUDE=as.integer(dd33$LONGITUDE)
dd33$LATITUDE=as.integer(dd33$LATITUDE)
dd33$months=as.integer(dd33$months)
str(dd33)
dd33$VEHICLE.TYPE.CODE.1=as.integer(dd33$VEHICLE.TYPE.CODE.1)
dd33$VEHICLE.TYPE.CODE.2=as.integer(dd33$VEHICLE.TYPE.CODE.2)
dd33$VEHICLE.TYPE.CODE.3=as.integer(dd33$VEHICLE.TYPE.CODE.3)
dd33$VEHICLE.TYPE.CODE.4=as.integer(dd33$VEHICLE.TYPE.CODE.4)
dd33$VEHICLE.TYPE.CODE.5=as.integer(dd33$VEHICLE.TYPE.CODE.5)
#dd33$deathoralive=as.integer(dd33$deathoralive)

### Data Division ###
index=seq (1,nrow(dd33),by=5)
test<-dd33[index,]
training<-dd33[-index,]

### Importing RandomForest package ###
install.packages('randomForest')
library(randomForest)
colnames(training)

str(dd33)
### eliminating unwanted fields ###
training<-training[,c(-1,-2,-11,-12,-13,-17,-15,-14,-16,-18)]
test<-test[,c(-1,-2,-11,-12,-13,-17,-15,-14,-16,-18)]

### Random Forest ###
fit<-randomForest(factor(total.no.of.injured)~.,training,importance=TRUE,ntree=60)
importance(fit)
View(dd33)
varImpPlot(fit)

barplot(dd33$BOROUGH,dd33$total.no.of.injured)
##### Performance Measure #####

### Prediction ###
Prediction<-predict(fit,test[,-21])

###Error Rate ###

wrong_CART1<- (test[,21]!=Prediction)
rate_CART1<-sum(wrong_CART1)/length(wrong_CART1)
rate_CART1

### Performance evaluation ###
perf_eval<-table(test[,21],Prediction)
perf_eval

### correctly predicted ###
diag<-diag(perf_eval)
diag
### accracy ###
n<-sum(perf_eval)
acc<-sum(diag)/n
acc
### Row & col sum ###
row_sum<-apply(perf_eval,1,sum)
col_sum<-apply(perf_eval,2,sum)
precision<-diag/col_sum
precision
### ###
recall<-diag/row_sum
recall
### ###
f1<-2*precision*recall/(precision+recall)
f1
### ###
data.frame(precision,recall,f1)



### naivebayes ###
install.packages('e1071')
install.packages('naivebayes')
library(class)
library(e1071)
library(naiveBayes)

nb<-naiveBayes(factor(total.no.of.injured)~.,training)
nb 
np<-predict(nb,test[,-21])
wrong_CART<- (test[,21]!=np)

rate_CART<-sum(wrong_CART)/length(wrong_CART)
rate_CART


###EDA PLOTS ###
install.packages('ggplot2')
library(ggplot2)
library(ggplot)

xx<-ggplot(dd3, aes(dd3$months, fill = total.no.of.injured)) + geom_bar()+labs(title = "Stacked Bar Chart", x = "Months", y = "TOTAL NO OF INJURED")
yy<-ggplot(dd3, aes(dd3$BOROUGH, fill = total.no.of.injured)) + geom_bar()+labs(title = "Stacked Bar Chart", x = "BOROUGH", y = "TOTAL NO OF INJURED")
zz<-ggplot(dd3, aes(dd3$timezone, fill = total.no.of.injured)) + geom_bar()+labs(title = "Stacked Bar Chart", x = "TIMEZONE", y = "TOTAL NO OF INJURED")




