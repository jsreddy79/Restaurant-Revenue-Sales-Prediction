#######################################################################################
rm(list = ls())
setwd("G:/Project/test.csv")
#setwd("G:/Project/train.csv")
#######################################################################################

## Data Preprocessing

#######################################################################################
install.packages("car")
library(car)
library(MASS)
library(caret)
## reading file 
####################################################################################
### Understanding the train data ###
train_data<-read.csv("train.csv",header = T,sep = ",")
dim(train_data)
names(train_data)
summary(train_data)
str(train_data)
sum(is.na(train_data)) ## '0' No missing values

####################################################################################
### Understanding the test data ###

test_data<- read.csv("test.csv", header = T, sep = ",")
dim(test_data)
names(test_data)
summary(test_data)
str(test_data)
sum(is.na(train_data)) ## '0' No missing values
####################################################################################
## Plotting Type data from Test and Train data ###

par(mfrow=c(1,2))
plot(train_data$Type)
plot(test_data$Type)
####################################################################################

## Removing revenue,Id and City from Train data #####

revenue <- train_data$revenue 


train_data <- train_data[,-c(1,3,43)]

## Removing Id and City from Test data #####

test_data <- test_data[,-c(1,3)]
lgrevenue <- log10(revenue)
###################################################################################
## Combine train and test dataset for analysis without target variable "revenue"
##to ensure there will not be any class imbalance

newdata <- rbind(train_data,test_data)
names(newdata)
str(newdata)
dim(newdata)
colnames(newdata)
grep("P35",colnames(newdata))

###### calculating number of years restaurant is open#########################
today=Sys.Date()
newdata=cbind(newdata,today)
class(today)
head(newdata$Open.Date)
newdata$Open.Date=as.Date(newdata$Open.Date, format = "%m/%d/%Y")


library(lubridate)

newdata$open_year=as.numeric(year(newdata$Open.Date))
newdata$current_year=as.numeric(year(today))
class(newdata$open_year)

newdata$diffyears=(newdata$current_year)-(newdata$open_year)
head(newdata$diffyears)
str(newdata)
names(newdata)
newdata$today
newdata$open_year

#newdata=newdata[,-c(1,2,3,45,44,43)]
str(newdata$Type)
dim(newdata)
names(newdata)

par(mfrow=c(1,2))
grep("today",colnames(newdata))
newdata=newdata[,-c(1,41,42,43)]


##################################################################################
#Renaming DT to FC and MB to IL
plot(newdata$Type)
newdata$Type<- recode(newdata$Type,"'DT'='FC'")
newdata$Type<- recode(newdata$Type,"'MB'='IL'")

plot(newdata$Type)

str(newdata$Type)

levels(newdata$Type)
# do NA for MB ,DT and do conversion using knn..
## whole data 


sum(is.na(newdata))
##Seperate Numeric and Categorical attributes
dataNum=newdata[,c(4,5,6,15,28,29,30,31,40)]
dataCat=newdata[,c(1,2,3,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,32,33,34,35,36,37,38,39)]
dataCat=data.frame(apply(dataCat,2,function(x){as.factor(x)}))
str(dataCat)
#str(dataNumStd)
str(newdata$Type)
#sum(is.na(dataNumStd))

#Performing Normalization for Numeric data
library(vegan)
dataNumStd <- decostand(dataNum,"standardize")
summary(dataNumStd)



## Create dummies for the Categorical variables
library(dummies)
function(x){
  dataCatDummy=data.frame(dummy(dataCat$x))
  dataCat=subset(dataCat,select=-c(x))
  dataCat=data.frame(dataCat,dataCatDummy)}
dataCatDummy=dummy.data.frame(dataCat) ## Create dummies for Categorical variables
###################################################################################

## Combine standardized Numerical and Categorical data frames

dataFinal=data.frame(dataNumStd,dataCatDummy)
str(dataFinal) ## 257 Variables
dim(dataFinal)
nrow(dataFinal)
names(dataFinal)
##################################################################################
## Seperate train and test datasets
Train <- dataFinal[1:137, ]
Test <- dataFinal[138:nrow(dataFinal), ]
class(lgrevenue)

str(Train)
str(Test)
# Train = data.frame(Train,lgrevenue) ## Bind the revenue column after pca 

#str(Train)
Train[1,]
nrow(Train)
sum(is.na(Train))


names(Train)
dim(Train)
dim(Test)
#names(Train)[names(Train) == 'lgrevenue'] <- 'revenue'
nrow(Train)
nrow(Test)

## using pca

str(Train)
names(Train)
train_merged<-Train[,-c(22,43,55,93,102,114,118,123,124,150,181,186,215,222,227,231,241,246)] ## all are 0 removing
#grep("P36.8",colnames(Train))
dim(train_merged)
#View(Train)
names(train_merged)
pca_model<-prcomp(train_merged,scale. = T)
summary(pca_model)
str(pca_model)
final_train<-data.frame(pca_model$x)
names(final_train)
dim(final_train)
final_train_data<-final_train[,1:60]
dim(final_train_data)
names(final_train_data)
str(final_train)
final_test<-predict(pca_model,Test)
str(final_test)
class(final_test)
final_test<-data.frame(final_test)
summary(final_test)
names(final_test)
summary(final_test)

final_test<-final_test[,1:60]
str(final_test)

str(final_train_data)


#Train$revenue=log(Train$revenue)  ## Converting revenue to log as the value looks large

#### Split the data into Train and Test sets###do in another model
#smp_size <- floor(0.7 * nrow(Train))
#set.seed(123)
#tr_data <- sample(seq_len(nrow(Train)), size = smp_size)
#train <- Train[tr_data, ]
#test <- Train[-tr_data, ]

######################## Random forest############################# 

final_train_data<-cbind(revenue,final_train_data)
names(final_train_data)
library(randomForest)
rf_model <- randomForest(final_train_data$revenue ~ ., data= final_train_data, keep.forest=TRUE, ntree=30)
dim(final_train_data)
dim(final_test)
##   prediction to final model 
fitted(rf_model)
library(DMwR)
#error on train data
regr.eval(final_train_data$revenue, predict(rf_model,final_train_data[,-1])) # 0
prediction_train<-predict(rf_model,final_train_data[,-1])
prediction<- predict(rf_model,final_test)
prediction<-as.data.frame(prediction)
head(prediction)

######Submission file#####################

id=0:99999
id<-as.data.frame(id)
sample_submission<-cbind(id,prediction)
head(sample_submission)
write.csv(sample_submission,"sampleSubmission_final_pca_rf.csv",row.names = F)


###############################################
##knnn


#rf_knn

model_knn=train(revenue~ .,
                data=final_train_data,
                preProcess = c("center","scale"), tuneLength = 20,
                method="knn",
                trControl=trainControl(method="repeatedcv",number=10,repeats = 5),
                prox=TRUE,allowParallel=TRUE)


#error on train data
regr.eval(final_train_data$revenue, fitted(model_knn)) 

prediction_train<-predict(model_knn,final_train_data[,-1])
prediction<-predict(model_knn,final_test)
prediction_test_knn<-as.data.frame(prediction)

###### Submission file########################

Id=0:99999
Id=as.data.frame(Id)
sampleSubmission=cbind(Id,prediction_test_knn)
head(sampleSubmission)
write.csv(sampleSubmission,"sampleSubmissionfinal_pca_knn.csv",row.names = F)

###############################################################

