#######################################################################################
## Set the current Working Directory

rm(list = ls())
setwd("G:/Project/test.csv")

#######################################################################################


## Data Preprocessing 
#################################################################################
install.packages("car")
library(car)

library(MASS)
library(caret)
## reading file 

#######################################################################################
### Understanding the train data ###
dataTrain=read.csv("train.csv",header=T,sep=",")
summary(dataTrain)

str(dataTrain)
dim(dataTrain)
names(dataTrain)
sum(is.na(dataTrain)) ## '0' No missing values
#######################################################################################


#######################################################################################
### Understanding the test data ###
dataTest=read.csv("test.csv",header=T,sep=",")
summary(dataTest)
str(dataTest)
dim(dataTest)
names(dataTest)
sum(is.na(dataTest)) ## '0' No missing values


#######################################################################################
## Combine train and test dataset for analysis without target variable "revenue"
## As the data in train dataset is small , we need to combine the both
## to ensure there will not be any class imbalance
revenue <- dataTrain$revenue 
dataTrain <- dataTrain[,-c(1,3,43)]

Id <- dataTest$Id
dataTest <- dataTest[,-c(1,3)]
lgrevenue <- log10(revenue)
dataFinal=rbind(dataTrain,dataTest)

names(dataFinal)
str(dataFinal)
dim(dataFinal)
colnames(dataFinal)

## Converting the Open date to Number of years from current day
dataFinal$Open.Date=Sys.Date()-as.Date(as.character(dataFinal$Open.Date),'%m/%d/%Y')
dataFinal$Open.Date=as.numeric(dataFinal$Open.Date)/365
names(dataFinal)[names(dataFinal) == 'Open.Date'] <- 'NoOfYrs'

#Renaming DT to FC and MB to IL
plot(dataFinal$Type)
dataFinal$Type<- recode(dataFinal$Type,"'DT'='FC'")
dataFinal$Type<- recode(dataFinal$Type,"'MB'='IL'")
plot(dataFinal$Type)
str(dataFinal$Type)
levels(dataFinal$Type)
sum(is.na(dataFinal))
str(dataFinal)
##Seperate Numeric and Categorical attributes
dataNum=dataFinal[,c(1,5,6,7,16,29,30,31,32)]
dataCat=dataFinal[,c(2,3,4,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,
                     
                     33,34,35,36,37,38,39,40)]

dataCat=data.frame(apply(dataCat,2,function(x){as.factor(x)}))
str(dataCat) ## All Categorical variables converted to Factor

## Standardize the Numercial attributes
library(vegan)
dataNumStd <- decostand(dataNum,"standardize")
summary(dataNumStd)

## Create dummies for the Categorical variables
library(dummies)
function(x){
  dataCatDummy=data.frame(dummy(dataCat$x))
  
  
  dataCat=subset(dataCat,select=-c(x))
  dataCat=data.frame(dataCat,dataCatDummy)}
dataCatDummy=dummy.data.frame(dataCat) ## Create dummies for Categorrical variables

## Combine standardized Numerical and Categorical data frames
dataFinal=data.frame(dataNumStd,dataCatDummy)
str(dataFinal) ## 257 Variables
dim(dataFinal)
nrow(dataFinal)
names(dataFinal)


## Seperate train and test datasets
Train <- dataFinal[1:137, ]
Test <- dataFinal[138:nrow(dataFinal), ]
Train = data.frame(Train,revenue) ## Binding the revenue column from train datase
names(Train)[names(Train) == 'dataTrain.revenue'] <- 'revenue'

Train$revenue=log(Train$revenue)  ## Converting revenue to log as the value looks large

dim(Train)
dim(Test)
names(Train)
names(Test)
nrow(Train)
nrow(Test)
sum(is.na(Train))
sum(is.na(Test))
Train[1,]
## Identify the outliers in Revenue and remove them for analysis
## After removing outliers Adjusted R square was positive
boxplot(Train$revenue)
abline(h = mean(Train$revenue), lty=2)
IQ = IQR(Train$revenue)
Q1 = quantile(Train$revenue,0.25)
Q3 = quantile(Train$revenue,0.75)
lowerinnerfence =  Q1 - 1.5*IQ
upperinnerfence = Q3 + 1.5*IQ
lowerouterfence = Q1 - 3*IQ
upperouterfence = Q3 + 3*IQ
outliers = boxplot(Train$revenue, plot=FALSE)$out

#Extract the outliers from the original data frame
Train=Train[!(Train$revenue %in% outliers),]

#### Split the data into Train and Test sets
smp_size <- floor(0.7 * nrow(Train))
set.seed(123)
tr_data <- sample(seq_len(nrow(Train)), size = smp_size)
train <- Train[tr_data, ]

test <- Train[-tr_data, ]

#############################################################################


## Model building

#############################################################################
## Normal multivariate regression
## Was unsatisfactory - not working at all
MultLM <- lm(train$revenue~., data=train,na.action=NULL)
summary(MultLM)

## Model evaluation
library(DMwR)
Lmpred <- fitted(MultLM)
regr.eval(train$revenue,fitted(MultLM))  #error on train data

pred <- predict.lm(MultLM, test) 
regr.eval(test$revenue, pred)#error on test data


##################linear regression predictions#####################

Linearpredictions <- predict.lm(MultLM, Test)
#regr.eval(final_test, prediction) # 1.47
#mae          mse         rmse         mape 
#4.520635e+11 2.113490e+18 1.453785e+09 5.257039e+12 

Linearpredictions <-as.data.frame(Linearpredictions)
Linearpredictions <- exp(Linearpredictions)
head(Linearpredictions)

id=0:99999
id<-as.data.frame(id)
sample_submission<-cbind(id,Linearpredictions)
head(sample_submission)
write.csv(sample_submission,"sampleSubmission_final_linearreg.csv",row.names = F)




#############################################################################
## Lasso Reggression
#Converted the data into matrix form to input into glm model
trainL = as.matrix(train)
testL = as.matrix(test)

#Target Varaible
y=train$revenue
ytest = test$revenue
#install.packages("glmnet")
library(glmnet)

#######################################################
#cv.glmnet will help you choose lambda
cv <- cv.glmnet(trainL,y)

#lambda.min - value of lambda that gives minimum cvm - mean cross-validated error
###################################################
# Lasso Regression  using glmnet - L1 norm

fit1=glmnet(trainL,y,lambda=cv$lambda.min,alpha=0)
predict(fit1,trainL)
library(DMwR)
LASSOtrain = regr.eval(y, predict(fit1,trainL))
LASSOtest = regr.eval(ytest, predict(fit1,testL))
LASSOtrain
LASSOtest

#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(trainL,y)
plot(cv.lasso)
coef(cv.lasso)




#############################################################################
# Ridge Regression  using glmnet  - L2 norm
## Using Ridge regression also, the evaluation metrics on train and test 
## are same as Lasso regression

library(glmnet)
# fit model
fit2=glmnet(trainL,y,lambda=cv$lambda.min,alpha=0)
predict(fit2,trainL)
library(DMwR)
RIDGEtrain = regr.eval(y, predict(fit2,trainL))
RIDGEtest = regr.eval(ytest, predict(fit2,testL))
RIDGEtrain
RIDGEtest
#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(trainL,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

################################################################


finalerros <- data.frame(rbind(LASSOtrain,LASSOtest,
                               RIDGEtrain,RIDGEtest))
finalerros
###mae          mse       rmse         mape
###LASSOtrain 0.01241992 0.0004832456 0.02198285 0.0008209368
###LASSOtest  0.08824402 0.0118138379 0.10869148 0.0057887049
###RIDGEtrain 0.01241992 0.0004832456 0.02198285 0.0008209368
###RIDGEtest  0.08824402 0.0118138379 0.10869148 0.0057887049

###################################################################################
## If we observe, Multivariate Linear reggression was not working
## Of Lasso,Ridge both were providing same RMSE values in train and test
###################################################################################


