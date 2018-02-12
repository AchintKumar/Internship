######### Setting path in master code ################

# Clearing path environment
rm(list=ls())

# Getting the path 
getwd()

# Setting the path
setwd('/media/achint/INSOFE/Internship')

################# Data Exploration ###################

#Reading the data
derm_raw<-read.csv('dermatology_data.csv')

#Viewing the data
View(derm_raw)

#Summary of data
summary(derm_raw)

#Dimensions of dataset
dim(derm_raw)

#Structure of dataset
str(derm_raw)

#Checking the number of missing values
sum(is.na(derm_raw))
#No missing values as values not known denoted by ?

#Viewing rows with values corresponding to ?
View(derm_raw[derm_raw$age=='?',])

########### Data Type Conversion #######################

#To change to numeric,first converting factor to character
#Subsetting age column in new dataframe 
agenum<-subset(derm_raw,select=c(age))
#Changing column to character
agenum$age<-as.character(agenum$age)
#Changing column from character to numeric
agenum$age<-as.numeric(agenum$age)
#This automatically replaces ? to missing (NA) values

#Factorizing the rest of the variables

#Making a common function for factorizing the columns
factorize<-function(x){as.factor(x)}

#Applying the function on the entire dataframe
derm<-apply(derm_raw,2,factorize)

#Converting it back to dataframe
derm<-as.data.frame(derm)

#Checking the structure again
str(derm)

#Removing the only numeric variable to combine with agenum dataset
derm$age<-NULL

#Combining original dataset and converted age column
derm<-cbind(derm,agenum)

#Rechecking the structure for confirmation
str(derm)

#Checking the number of missing values
sum(is.na(derm))

################ Imputation Testing #######################

#Testing which imputation to choose
#Commemted because testing not related to research

#Introducing newer na values in some places

#Replacing every 5th element with missing value in erythema column
#for(i in seq(1,366,5))
#{
#  derm$erythema<-replace(derm$erythema,i,NA)
#}

#Replacing every 7th element in koebner column
#for(j in seq(1,366,7))
#{
#  derm$koebner<-replace(derm$koebner,j,NA)
#}

#Replacing every 9th element in exocytosis column
#for(k in seq(1,366,9))
#{
#  derm$exocytosis<-replace(derm$exocytosis,k,NA)
#}

#Replacing every 15th element in thinning_Supra column
#for(l in seq(1,366,15))
#{
#  derm$thinning_supra<-replace(derm$thinning_supra,l,NA)
#}

#Checking number of missing values in dataframe
#sum(is.na(derm))

#Writing a new csv file to export it for python
#For checking the imputation methods in python
#write.csv(derm2,file='derm_missing2.csv',row.names=FALSE)

#Testing different types of imputations

#KNN Imputation
#library(DMwR)

#Making a new dataframe to safe-keep the original
#derm_knn<-derm
#str(derm_knn)
#sum(is.na(derm_knn))

#Logging the start time of KNN Imputation
#start_knn<-Sys.time()

#KNN Imputation
#derm_knn<-knnImputation(derm_knn,k=5)

#Logging the end time 
#end_knn<-Sys.time()

#Calculating the total time taken
#time_knn<-end_knn - start_knn

#Comapring the dataframe with original dataframe
#compare_knn<-data.frame(derm_raw$erythema==derm_knn$erythema)

#Central Imputation

#Making new dataframe
#derm_central<-derm

#Logging the start time
#start_cen<-Sys.time()

#Central imputation
#derm_central<-centralImputation(derm_central)

#Logging the end time
#end_cen<-Sys.time()

#Calculating the time
#time_cen<-end_cen - start_cen

#Comparing the dataframe
#compare_central<-data.frame(derm_raw$erythema==derm_central$erythema)

#missForest Package
#library(missForest)

#New dataframe
#derm_miss<-derm

#Logging the start time
#start_miss<-Sys.time()

#missForest Imputation
#derm_miss<-missForest(derm_miss)

#Logging the end time
#end_miss<-Sys.time()

#Calculating the time
#time_miss<-end_miss - start_miss

#Creating the imputed dataframe
#derm_miss_data<-derm_miss$ximp

#Comparing the data frame
#compare_miss<-data.frame(derm_raw$erythema==derm_miss_data$erythema)

#Comparing the time taken by all the imputations
#time<-data.frame(time_knn,time_cen,time_miss)

#Checking the number of missing values in each imputed dataframe
#sum(is.na(derm_knn))
#sum(is.na(derm_central))
#sum(is.na(derm_miss_data))

#Checking whether the dimensions have been reduced or not
#dim(derm_knn)
#dim(derm_central)
#dim(derm_miss_data)
#dim(derm)

##################### Imputation ####################

#KNN Imputation
library(DMwR)

#Making a new dataframe to safe-keep the original
derm_knn<-derm
str(derm_knn)
sum(is.na(derm_knn))

#Logging the start time of KNN Imputation
start_knn<-Sys.time()

#KNN Imputation
derm_knn<-knnImputation(derm_knn,k=5)

#Logging the end time 
end_knn<-Sys.time()

#Calculating the total time taken
time_knn<-end_knn - start_knn

#Comapring the dataframe with original dataframe
compare_knn<-data.frame(derm_raw$erythema==derm_knn$erythema)

#Central Imputation

#Making new dataframe
derm_central<-derm

#Logging the start time
start_cen<-Sys.time()

#Central imputation
derm_central<-centralImputation(derm_central)

#Logging the end time
end_cen<-Sys.time()

#Calculating the time
time_cen<-end_cen - start_cen

#Comparing the dataframe
compare_central<-data.frame(derm_raw$erythema==derm_central$erythema)

#missForest Package
library(missForest)

#New dataframe
derm_miss<-derm

#Logging the start time
start_miss<-Sys.time()

#missForest Imputation
derm_miss<-missForest(derm_miss)

#Logging the end time
end_miss<-Sys.time()

#Calculating the time
time_miss<-end_miss - start_miss

#Creating the imputed dataframe
derm_miss_data<-derm_miss$ximp

#Comparing the data frame
compare_miss<-data.frame(derm_raw$erythema==derm_miss_data$erythema)

#Comparing the time taken by all the imputations
time<-data.frame(time_knn,time_cen,time_miss)

#Checking the number of missing values in each imputed dataframe
sum(is.na(derm_knn))
sum(is.na(derm_central))
sum(is.na(derm_miss_data))

#Checking whether the dimensions have been reduced or not
dim(derm_knn)
dim(derm_central)
dim(derm_miss_data)
dim(derm)

################ Splitting of Data Frame ##############

#splitting into train, validation and test data

#Using caret package for splitting the data frame
library(caret)

#Creating a floor value for splitting into train_val and test 
train_rows<-sample(1:nrow(derm_miss_data),nrow(derm_miss_data)*0.8)

#Splitting into train_val and test
train_val<-derm_miss_data[train_rows,]
test_miss<-derm_miss_data[-train_rows,]

#Creating another floor value for splitting into train and validation
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)

#Splitting into validation and train
val_miss<-train_val[train_rows2,]
train_miss<-train_val[-train_rows2,]

#Checking the dimensions of all three splits
dim(train_miss)
dim(val_miss)
dim(test_miss)

#Splitting for KNN-Imputed dataframe
train_rows<-sample(1:nrow(derm_knn),nrow(derm_knn)*0.8)
train_val<-derm_knn[train_rows,]
test_knn<-derm_knn[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_knn<-train_val[train_rows2,]
train_knn<-train_val[-train_rows2,]

#Checking dimensions of all 3 splits
dim(train_knn)
dim(val_knn)
dim(test_knn)

#Splitting for mean imputed dataframe
train_rows<-sample(1:nrow(derm_central),nrow(derm_central)*0.8)
train_val<-derm_central[train_rows,]
test_cen<-derm_central[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_cen<-train_val[train_rows2,]
train_cen<-train_val[-train_rows2,]

#Checking dimensions of all 3 splits
dim(train_cen)
dim(val_cen)
dim(test_cen)

#Splitting the original dataframe (with missing values)
train_rows<-sample(1:nrow(derm),nrow(derm)*0.8)
train_val<-derm[train_rows,]
test_raw<-derm[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_raw<-train_val[train_rows2,]
train_raw<-train_val[-train_rows2,]

#Checking dimensions of all 3 splits
dim(train_raw)
dim(val_raw)
dim(test_raw)

################### Model Building ########################

###################### Multinomial Regression

#Loading the library
library(nnet)

#Classification for Knn set

#Training the model on train set
model_knn_glm<-multinom(disease_type~.,data=train_knn)

#Checking the summary
summary(model_knn_glm)

#Subsetting target value in validation set
val_knn_notarget<-subset(val_knn,select=-c(disease_type))

#Predicting and tuning in validation set
pred_knn_glm<-predict(model_knn_glm,val_knn_notarget,type='class')

#Making a confusion matrix for validation set
metric_knn_glm<-confusionMatrix(pred_knn_glm,val_knn$disease_type)

#Checking the error metrics for valdation set
metric_knn_glm$byClass

#Creating a table for comparing actual values with predicted values
table(pred_knn_glm,val_knn$disease_type)

#Subsetting target variable in test set
test_knn_notarget<-subset(test_knn,select=-c(disease_type))

#Predicting results in test set
pred_knn_glm_test<-predict(model_knn_glm,test_knn_notarget,type='class')

#Making confusion matrix for test set
metric_knn_glm_test<-confusionMatrix(pred_knn_glm_test,test_knn$disease_type)

#Checking error metrics for test set
metric_knn_glm_test$byClass

#Creating a table for comparison
table(pred_knn_glm_test,test_knn$disease_type)


#Classification on Mean imputed set

#Training
model_cen_glm<-multinom(disease_type~.,data=train_cen)
summary(model_cen_glm)

#Validation Tuning
val_cen_notarget<-subset(val_cen,select=-c(disease_type))
pred_cen_glm<-predict(model_cen_glm,val_cen_notarget,type='class')
metric_cen_glm<-confusionMatrix(pred_cen_glm,val_cen$disease_type)
metric_cen_glm$byClass[,5]
table(pred_cen_glm,val_cen$disease_type)

#Test Comparison
test_cen_notarget<-subset(test_cen,select=-c(disease_type))
pred_cen_glm_test<-predict(model_cen_glm,test_cen_notarget,type='class')
metric_cen_glm_test<-confusionMatrix(pred_cen_glm_test,test_cen$disease_type)
metric_cen_glm_test$byClass[,5]
table(pred_cen_glm_test,test_cen$disease_type)


#missForest set

#Training
model_miss_glm<-multinom(disease_type~.,data=train_miss)
summary(model_miss_glm)

#Validation
val_miss_notarget<-subset(val_miss,select=-c(disease_type))
pred_miss_glm<-predict(model_miss_glm,val_miss_notarget,type='class')
metric_miss_glm<-confusionMatrix(pred_miss_glm,val_miss$disease_type)
metric_miss_glm$byClass[,5]
table(pred_miss_glm,val_miss$disease_type)

#Test
test_miss_notarget<-subset(test_miss,select=-c(disease_type))
pred_miss_glm_test<-predict(model_miss_glm,test_miss_notarget,type='class')
metric_miss_glm_test<-confusionMatrix(pred_miss_glm_test,test_miss$disease_type)
metric_miss_glm_test$byClass[,5]
table(pred_miss_glm_test,test_miss$disease_type)


#Raw model (no use because multinom doesn't deal with na values)
#model_raw_glm<-multinom(disease_type~.,data=train_raw)
#summary(model_raw_glm)
#val_raw_notarget<-subset(val_raw,select=-c(disease_type))
#pred_raw_glm<-predict(model_raw_glm,val_raw_notarget,type='class')
#metric_raw_glm<-confusionMatrix(pred_raw_glm,val_raw$disease_type)
#metric_raw_glm$byClass[,5]
#table(pred_raw_glm,val_raw$disease_type)
#test_raw_notarget<-subset(test_raw,select=-c(disease_type))
#pred_raw_glm_test<-predict(model_raw_glm,test_raw_notarget,type='class')
#metric_knn_raw_test<-confusionMatrix(pred_raw_glm_test,test_raw$disease_type)
#metric_knn_raw_test$byClass[,5]
#table(pred_raw_glm_test,test_raw$disease_type)

##################### c5.0 model

#Library for c50 model
library(C50)

#knn set

#Training
model_knn_c50<-C5.0(disease_type~.,data=train_knn)
summary(model_knn_c50)

#Validation
pred_knn_c50<-predict(model_knn_c50,val_knn_notarget,type='class')
metric_knn_c50<-confusionMatrix(pred_knn_c50,val_knn$disease_type)
metric_knn_c50$byClass
table(pred_knn_c50,val_knn$disease_type)

#Test
pred_knn_c50_test<-predict(model_knn_c50,test_knn_notarget,type='class')
metric_knn_c50_test<-confusionMatrix(pred_knn_c50_test,test_knn$disease_type)
metric_knn_c50_test$byClass
table(pred_knn_c50_test,test_knn$disease_type)

#central set

#Training
model_cen_c50<-C5.0(disease_type~.,data=train_cen)
summary(model_cen_c50)

#Validation
pred_cen_c50<-predict(model_cen_c50,val_cen_notarget,type='class')
metric_cen_c50<-confusionMatrix(pred_cen_c50,val_cen$disease_type)
metric_cen_c50$byClass[,5]
table(pred_cen_c50,val_cen$disease_type)

#Test
pred_cen_c50_test<-predict(model_cen_c50,test_cen_notarget,type='class')
metric_cen_c50_test<-confusionMatrix(pred_cen_c50_test,test_cen$disease_type)
metric_cen_c50_test$byClass[,5]
table(pred_cen_c50_test,test_cen$disease_type)

#missforest set

#Training
model_miss_c50<-C5.0(disease_type~.,data=train_miss)
summary(model_miss_c50)

#Validation
pred_miss_c50<-predict(model_miss_c50,val_miss_notarget,type='class')
metric_miss_c50<-confusionMatrix(pred_miss_c50,val_miss$disease_type)
metric_miss_c50$byClass[,5]
table(pred_miss_c50,val_miss$disease_type)

#Test
pred_miss_c50_test<-predict(model_miss_c50,test_miss_notarget,type='class')
metric_miss_c50_test<-confusionMatrix(pred_miss_c50_test,test_miss$disease_type)
metric_miss_c50_test$byClass[,5]
table(pred_miss_c50_test,test_miss$disease_type)


#raw set

#Training
model_raw_c50<-C5.0(disease_type~.,data=train_raw)
summary(model_raw_c50)

#Validation
pred_raw_c50<-predict(model_raw_c50,val_raw_notarget,type='class')
metric_raw_c50<-confusionMatrix(pred_raw_c50,val_raw$disease_type)
metric_raw_c50$byClass[,5]
table(pred_raw_c50,val_raw$disease_type)

#Test
pred_raw_c50_test<-predict(model_raw_c50,test_raw_notarget,type='class')
metric_raw_c50_test<-confusionMatrix(pred_raw_c50_test,test_raw$disease_type)
metric_raw_c50_test$byClass[,5]
table(pred_raw_c50_test,test_raw$disease_type)


################### CART model

#Loading library
library(rpart)

#knn set

#Training
model_knn_rpart<-rpart(disease_type~.,data=train_knn)
summary(model_knn_rpart)

#Validation
pred_knn_rpart<-predict(model_knn_rpart,val_knn_notarget,type='class')
metric_knn_rpart<-confusionMatrix(pred_knn_rpart,val_knn$disease_type)
metric_knn_rpart$byClass
table(pred_knn_rpart,val_knn$disease_type)

#Test
pred_knn_rpart_test<-predict(model_knn_rpart,test_knn_notarget,type='class')
metric_knn_rpart_test<-confusionMatrix(pred_knn_rpart_test,test_knn$disease_type)
metric_knn_rpart_test$byClass		
table(pred_knn_rpart_test,test_knn$disease_type)

#central set

#Training
model_cen_rpart<-rpart(disease_type~.,data=train_cen)
summary(model_cen_rpart)

#Validation
pred_cen_rpart<-predict(model_cen_rpart,val_cen_notarget,type='class')
metric_cen_rpart<-confusionMatrix(pred_cen_rpart,val_cen$disease_type)
metric_cen_rpart$byClass[,5]
table(pred_cen_rpart,val_cen$disease_type)

#Test
pred_cen_rpart_test<-predict(model_cen_rpart,test_cen_notarget,type='class')
metric_cen_rpart_test<-confusionMatrix(pred_cen_rpart_test,test_cen$disease_type)
metric_cen_rpart_test$byClass[,5]
table(pred_cen_rpart_test,test_cen$disease_type)

#missforest set

#Training
model_miss_rpart<-rpart(disease_type~.,data=train_miss)
summary(model_miss_rpart)

#Validation
pred_miss_rpart<-predict(model_miss_rpart,val_miss_notarget,type='class')
metric_miss_rpart<-confusionMatrix(pred_miss_rpart,val_miss$disease_type)
metric_miss_rpart$byClass[,5]
table(pred_miss_rpart,val_miss$disease_type)

#Test
pred_miss_rpart_test<-predict(model_miss_rpart,test_miss_notarget,type='class')
metric_miss_rpart_test<-confusionMatrix(pred_miss_rpart_test,test_miss$disease_type)
metric_miss_rpart_test$byClass[,5]
table(pred_miss_rpart_test,test_miss$disease_type)

#raw set

#Training
model_raw_rpart<-rpart(disease_type~.,data=train_raw)
summary(model_raw_rpart)

#Validation
pred_raw_rpart<-predict(model_raw_rpart,val_raw_notarget,type='class')
metric_raw_rpart<-confusionMatrix(pred_raw_rpart,val_raw$disease_type)
metric_raw_rpart$byClass[,5]
table(pred_raw_rpart,val_raw$disease_type)

#Test
pred_raw_rpart_test<-predict(model_raw_rpart,test_raw_notarget,type='class')
metric_raw_rpart_test<-confusionMatrix(pred_raw_rpart_test,test_raw$disease_type)
metric_raw_rpart_test$byClass[,5]
table(pred_raw_rpart_test,test_raw$disease_type)

################# Bagging CART set

#Loading library
install.packages('ipred')
library(ipred)

#knn set

#Training
model_knn_bag<-bagging(disease_type~.,data=train_knn)
summary(model_knn_bag)

#Validation
pred_knn_bag<-predict(model_knn_bag,val_knn_notarget,type='class')
metric_knn_bag<-confusionMatrix(pred_knn_bag,val_knn$disease_type)
metric_knn_bag$byClass
table(pred_knn_bag,val_knn$disease_type)

#Test
pred_knn_bag_test<-predict(model_knn_bag,test_knn_notarget,type='class')
metric_knn_bag_test<-confusionMatrix(pred_knn_bag_test,test_knn$disease_type)
metric_knn_bag_test$byClass
table(pred_knn_bag_test,test_knn$disease_type)

#central set

#Training
model_cen_bag<-bagging(disease_type~.,data=train_cen)
summary(model_cen_bag)

#Validation
pred_cen_bag<-predict(model_cen_bag,val_cen_notarget,type='class')
metric_cen_bag<-confusionMatrix(pred_cen_bag,val_cen$disease_type)
metric_cen_bag$byClass[,5]
table(pred_cen_bag,val_cen$disease_type)

#Test
pred_cen_bag_test<-predict(model_cen_bag,test_cen_notarget,type='class')
metric_cen_bag_test<-confusionMatrix(pred_cen_bag_test,test_cen$disease_type)
metric_cen_bag_test$byClass[,5]
table(pred_cen_bag_test,test_cen$disease_type)

#missforest set

#Training
model_miss_bag<-bagging(disease_type~.,data=train_miss)
summary(model_miss_bag)

#Validation
pred_miss_bag<-predict(model_miss_bag,val_miss_notarget,type='class')
metric_miss_bag<-confusionMatrix(pred_miss_bag,val_miss$disease_type)
metric_miss_bag$byClass[,5]
table(pred_miss_bag,val_miss$disease_type)

#Test
pred_miss_bag_test<-predict(model_miss_bag,test_miss_notarget,type='class')
metric_miss_bag_test<-confusionMatrix(pred_miss_bag_test,test_miss$disease_type)
metric_miss_bag_test$byClass[,5]
table(pred_miss_bag_test,test_miss$disease_type)

#raw set

#Training
model_raw_bag<-bagging(disease_type~.,data=train_raw)
summary(model_raw_bag)

#Validation
pred_raw_bag<-predict(model_raw_bag,val_raw_notarget,type='class')
metric_raw_bag<-confusionMatrix(pred_raw_bag,val_raw$disease_type)
metric_raw_bag$byClass[,5]
table(pred_raw_bag,val_raw$disease_type)

#Test
pred_raw_bag_test<-predict(model_raw_bag,test_raw_notarget,type='class')
metric_raw_bag_test<-confusionMatrix(pred_raw_bag_test,test_raw$disease_type)
metric_raw_bag_test$byClass[,5]
table(pred_raw_bag_test,test_raw$disease_type)


############### Random Forest

#Loading library
library(randomForest)

#knn set

#Training
model_knn_for<-randomForest(disease_type~.,data=train_knn)
summary(model_knn_for)

#Validation
pred_knn_for<-predict(model_knn_for,val_knn_notarget,type='class')
metric_knn_for<-confusionMatrix(pred_knn_for,val_knn$disease_type)
metric_knn_for$byClass[,5]
table(pred_knn_for,val_knn$disease_type)

#Test
pred_knn_for_test<-predict(model_knn_for,test_knn_notarget,type='class')
metric_knn_for_test<-confusionMatrix(pred_knn_for_test,test_knn$disease_type)
metric_knn_for_test$byClass
table(pred_knn_for_test,test_knn$disease_type)


#central set

#Training
model_cen_for<-randomForest(disease_type~.,data=train_cen)
summary(model_cen_for)

#Validation
pred_cen_for<-predict(model_cen_for,val_cen_notarget,type='class')
metric_cen_for<-confusionMatrix(pred_cen_for,val_cen$disease_type)
metric_cen_for$byClass[,5]
table(pred_cen_for,val_cen$disease_type)

#Test
pred_cen_for_test<-predict(model_cen_for,test_cen_notarget,type='class')
metric_cen_for_test<-confusionMatrix(pred_cen_for_test,test_cen$disease_type)
metric_cen_for_test$byClass[,5]
table(pred_cen_for_test,test_cen$disease_type)

#missforest set

#Training
model_miss_for<-randomForest(disease_type~.,data=train_miss)
summary(model_miss_for)

#Validation
pred_miss_for<-predict(model_miss_for,val_miss_notarget,type='class')
metric_miss_for<-confusionMatrix(pred_miss_for,val_miss$disease_type)
metric_miss_for$byClass[,5]
table(pred_miss_for,val_miss$disease_type)

#Test
pred_miss_for_test<-predict(model_miss_for,test_miss_notarget,type='class')
metric_miss_for_test<-confusionMatrix(pred_miss_for_test,test_miss$disease_type)
metric_miss_for_test$byClass[,5]
table(pred_miss_for_test,test_miss$disease_type)

#raw set
#Can't be made because of missing values

######### Gradient Boosting Model

#Loading the libraries
install.packages('gbm')
library(gbm)

#knn set

#Training
model_knn_gbm<-gbm(disease_type~.,data=train_knn,distribution='multinomial',n.trees=500)
summary(model_knn_gbm)

#Validation
pred_knn_gbm<-predict(model_knn_gbm,val_knn_notarget,type='response',n.trees=500)
pred_knn_gbm<-apply(pred_knn_gbm,1,which.max)
metric_knn_gbm<-confusionMatrix(pred_knn_gbm,val_knn$disease_type)
metric_knn_gbm$byClass[,5]
table(pred_knn_gbm,val_knn$disease_type)

#Test
pred_knn_gbm_test<-predict(model_knn_gbm,test_knn_notarget,type='response',n.trees=500)
pred_knn_gbm_test<-apply(pred_knn_gbm_test,1,which.max)
metric_knn_gbm_test<-confusionMatrix(pred_knn_gbm_test,test_knn$disease_type)
metric_knn_gbm_test$byClass
table(pred_knn_gbm_test,test_knn$disease_type)


#central set

#Training
model_cen_gbm<-gbm(disease_type~.,data=train_cen,n.trees=500)
summary(model_cen_gbm)

#Validation
pred_cen_gbm<-predict(model_cen_gbm,val_cen_notarget,type='response',n.trees=500)
pred_cen_gbm<-apply(pred_cen_gbm,1,which.max)
metric_cen_gbm<-confusionMatrix(pred_cen_gbm,val_cen$disease_type)
metric_cen_gbm$byClass[,5]
table(pred_cen_gbm,val_cen$disease_type)

#Test
pred_cen_gbm_test<-predict(model_cen_gbm,test_cen_notarget,type='response',n.trees=500)
pred_cen_gbm_test<-apply(pred_cen_gbm_test,1,which.max)
metric_cen_gbm_test<-confusionMatrix(pred_cen_gbm_test,test_cen$disease_type)
metric_cen_gbm_test$byClass[,5]
table(pred_cen_gbm_test,test_cen$disease_type)


#missforest set

#Training
model_miss_gbm<-gbm(disease_type~.,data=train_miss,n.trees=500)
summary(model_miss_gbm)

#Validation
pred_miss_gbm<-predict(model_miss_gbm,val_miss_notarget,type='response',n.trees=500)
pred_miss_gbm<-apply(pred_miss_gbm,1,which.max)
metric_miss_gbm<-confusionMatrix(pred_miss_gbm,val_miss$disease_type)
metric_miss_gbm$byClass[,5]
table(pred_miss_gbm,val_miss$disease_type)

#Test
pred_miss_gbm_test<-predict(model_miss_gbm,test_miss_notarget,type='response',n.trees=500)
pred_miss_gbm_test<-apply(pred_miss_gbm_test,1,which.max)
metric_miss_gbm_test<-confusionMatrix(pred_miss_gbm_test,test_miss$disease_type)
metric_miss_gbm_test$byClass[,5]
table(pred_miss_gbm_test,test_miss$disease_type)

#raw set
#Can't be made because of missing values
#####################################################################################################