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


###################################################################################
##Let us try Non Linear models 
## Random Forests
###################################################################################
library(randomForest)


revenue_rf <- randomForest(revenue ~ ., data=train,trControl=trainControl(method="repeatedcv",number=10,repeats = 5), 
                           keep.forest=TRUE, strata=T,ntree=50,replace=T) 
# 20 - Tried with 20 but, the % var and and error was less
# 30 - Evaluation metrics improved
# 35 - Evaluation metrics decreased compared to that for 30
# 40 - Evaluation metrics decreased compared to that for 30
## Observing that with same number of trees, different metrics for different runs ***
## Adding sample,replace and ntree=50, 

# View results and understand important attributes
print(revenue_rf)
revenue_rf$predicted 
revenue_rf$importance # gives 1st column (accuracy will reduce if imp var are removed) 
round(importance(revenue_rf), 2)   

# Extract and store important variables obtained from the random forest model
Imp_revenue_rf <- data.frame(revenue_rf$importance)
Imp_revenue_rf <- data.frame(row.names(Imp_revenue_rf),Imp_revenue_rf[,1])
colnames(Imp_revenue_rf) = c('Attributes','Importance')
Imp_revenue_rf <- Imp_revenue_rf[order(Imp_revenue_rf$Importance , decreasing = TRUE),]
Imp_revenue_rf <- Imp_revenue_rf[1:6,]

# plot (directly prints the important attributes) 
varImpPlot(revenue_rf)

# Predict on Train data 
pred_model_train <-predict(revenue_rf,train[,-258], norm.votes=TRUE)
rf_pred <- pred_model_train
result_train <- data.frame(train$revenue,pred_model_train);

result_train
RMSETrain=mean((result_train[1] - result_train[2])^2)
RMSETrain 
## 0.03663608 - without replace
## 0.02706133 - with replace

# Predicton Test Data
pred_model_test <-predict(revenue_rf,test[,-258], norm.votes=TRUE)
result_test <- data.frame("actual _values"= test$revenue,pred_model_test);
result_test
RMSETest=mean((result_test[1] - result_test[2])^2)
RMSETest 
## 0.1369601 - without replace
## 0.1262862 - with replace

##################################################################################
## Create sampleSubmission with Random Forest Model
dataTest<-cbind(Id,dataTest)
names(dataTest)
pred_model_Test <-predict(revenue_rf,Test, norm.votes=TRUE)
sampleSubmission <- data.frame(dataTest[1],exp(pred_model_Test));## As the revenue was
## converted to log in the beginning the prediction will be exponential
names(sampleSubmission)[names(sampleSubmission) == 'exp.pred_model_Test.'] <- 'revenue'
write.csv(sampleSubmission,"sampleSubmission_RF1.csv",row.names = F)
##################################################################################

###################################################################################
## Let us try SVM
###################################################################################
#### Split the data into Train and Test sets
library(tm) 
library(e1071)
revenue_svm  =  svm(revenue~.,data=train,scale=F,epsilon=0.4,cost=3)
summary(revenue_svm)
pred_svm_train=predict(revenue_svm,train)
svm_pred <- pred_svm_train
res_svm_train=data.frame(train$revenue,pred_svm_train)
RMSETrainSvm=mean((res_svm_train[1] - res_svm_train[2])^2)
RMSETrainSvm  
## 0.1184927 - without performance tuning
## 0.09922642 - After tuning the SVM epsilon=0.4,cost=3
## 0.09565834 - After tuning the SVM  epsilon=0.1,cost=2

pred_svm_test=predict(revenue_svm,test)
res_svm_test=data.frame(test$revenue,pred_svm_test)
RMSETestSvm=mean((res_svm_test[1] - res_svm_test[2])^2)
RMSETestSvm 
## 0.1385927 - without performance tuning the SVM
## 0.1276617 - After tuning the SVM epsilon=0.4,cost=3
## 0.1324936 - After tuning the SVM epsilon=0.1,cost=2

#### Trying to tune the SVM for optimum parameters

tuneResult <- tune(svm, revenue~.,data = Train,scale=F,
                   ranges = list(epsilon = seq(0,0.5,0.1), cost = 1:3))
print(tuneResult)
# Parameter tuning of 'svm':   
#   - sampling method: 10-fold cross validation # 
# - best parameters:
#   epsilon cost
# 0.4    3
# - best performance: 0.1443543 

plot(tuneResult) ## Darker the region, lower the error

##################################################################################
## Create sampleSubmission with SVM Model
pred_svm_Test <-predict(revenue_svm,Test, norm.votes=TRUE)
sampleSubmission <- data.frame(dataTest[1],exp(pred_svm_Test));## As the revenue was
## converted to log in the beginning the prediction will be exponential
names(sampleSubmission)[names(sampleSubmission) == 'exp.pred_svm_Test.'] <- 'revenue'
write.csv(sampleSubmission,"sampleSubmission_SVM.csv",row.names = F)
##################################################################################

##### Ensemble learning #########################
Lmpred1 <-  exp(Lmpred)
rf_pred1 <- exp(rf_pred)
svm_pred1 <- exp(svm_pred)
ensemble <- cbind(Lmpred1,rf_pred1,svm_pred1,revenue)
ensemble <- data.frame(ensemble)   
                  
EnsLM <- lm(ensemble$revenue~., data=ensemble,na.action=NULL)
summary(EnsLM)                 
                  
vif(EnsLM)


library(randomForest)


revenue_rf <- randomForest(revenue ~ ., data=ensemble,trControl=trainControl(method="repeatedcv",number=10,repeats = 5), 
                           keep.forest=TRUE, strata=T,ntree=50,replace=T) 


summary(revenue_rf)



randomforest <- revenue_rf$predicted

regr.eval(ensemble$revenue, randomforest) # 0

##################### average
rf <- exp(pred_model_Test)
svm <- exp(pred_svm_Test)
finalensemble <- rf + svm
finalensemble <- finalensemble/2


 id=0:99999
 id<-as.data.frame(id)
 sample_submission<-cbind(id,finalensemble)
 head(sample_submission)
 write.csv(sample_submission,"sampleSubmission_final_ensemble.csv",row.names = F)
###=================================================================================================================
#### Ensemble stacking  
  
# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

# Load the dataset


# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions=TRUE, classProbs=TRUE)
#algorithmList <- c('C5.0', 'rf', 'knn', 'svmRadial')
algorithmList <- c('rf', 'knn', 'svmRadial')
set.seed(1234)
models <- caretList(revenue~., data=Train, methodList=algorithmList)
#trControl=control
results <- resamples(models)
View(results$values)
summary(results)
dotplot(results)
View(round(predict(models, Test)))
preds <- round(predict(models, Test))
exp(preds)
final_preds <- round(apply(preds, mean, MARGIN = 1))
table(Test$revenue, final_preds)

# correlation between results
modelCor(results)
splom(results)


