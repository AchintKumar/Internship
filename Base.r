#Setting path in master code
rm(list=ls())
#library(ggplot2)
getwd()
#setwd('/media/achint/INSOFE/Internship')
setwd('F:/Internship')

#Reading data
derm_raw<-read.csv('dermatology_data.csv')
View(derm_raw)
summary(derm_raw)
dim(derm_raw)
str(derm_raw)
sum(is.na(derm_raw))
agenum<-subset(derm_raw,select=c(age))
View(derm_raw[derm_raw$age=='?',])
#to change to num,first convert factor to char
agenum$age<-as.character(agenum$age)
agenum$age<-as.numeric(agenum$age)

#factorizing all the variables
factorize<-function(x){as.factor(x)}
derm<-apply(derm_raw,2,factorize)
derm<-as.data.frame(derm)
str(derm)
#removing the only numeric variable
derm$age<-NULL

derm<-cbind(derm,agenum)
str(derm)

#Missing Values
sum(is.na(derm))

#introducing newer na values in some places
#for(i in seq(1,366,5))
#{
#  derm$erythema<-replace(derm$erythema,i,NA)
#}
#derm2<-derm
#for(i in seq(1,366,5))
#{
#  derm2$erythema<-replace(derm2$erythema,i,'')
#}
#str(derm2)
#for(j in seq(1,366,7))
#{
#  derm$koebner<-replace(derm$koebner,j,NA)
#}
#for(k in seq(1,366,9))
#{
#  derm$exocytosis<-replace(derm$exocytosis,k,NA)
#}
#for(l in seq(1,366,15))
#{
#  derm$thinning_supra<-replace(derm$thinning_supra,l,NA)
#}
#sum(is.na(derm))
#write.csv(derm2,file='derm_missing2.csv',row.names=FALSE)

#doing different types of imputations
#KNN,central,missForest,rpart,amelia,mice
library(DMwR)

derm_knn<-derm
str(derm_knn)
sum(is.na(derm_knn))
start_knn<-Sys.time()
derm_knn<-knnImputation(derm_knn,k=5)
end_knn<-Sys.time()
time_knn<-end_knn - start_knn
compare_knn<-data.frame(derm_raw$erythema==derm_knn$erythema)
#compare_knn$derm_raw.erythema....derm_knn.erythema<-as.factor(compare_knn$derm_raw.erythema....derm_knn.erythema)

derm_central<-derm
start_cen<-Sys.time()
derm_central<-centralImputation(derm_central)
end_cen<-Sys.time()
time_cen<-end_cen - start_cen
compare_central<-data.frame(derm_raw$erythema==derm_central$erythema)

library(missForest)
derm_miss<-derm
start_miss<-Sys.time()
derm_miss<-missForest(derm_miss)
end_miss<-Sys.time()
time_miss<-end_miss - start_miss
derm_miss_data<-derm_miss$ximp
compare_miss<-data.frame(derm_raw$erythema==derm_miss_data$erythema)

time<-data.frame(time_knn,time_cen,time_miss)


sum(is.na(derm_knn))
sum(is.na(derm_central))
sum(is.na(derm_miss_data))


dim(derm_knn)
dim(derm_central)
dim(derm_miss_data)
dim(derm)


#splitting into train val and test
library(caret)
train_rows<-sample(1:nrow(derm_miss_data),nrow(derm_miss_data)*0.8)
train_val<-derm_miss_data[train_rows,]
test_miss<-derm_miss_data[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_miss<-train_val[train_rows2,]
train_miss<-train_val[-train_rows2,]

dim(train_miss)
dim(val_miss)
dim(test_miss)


train_rows<-sample(1:nrow(derm_knn),nrow(derm_knn)*0.8)
train_val<-derm_knn[train_rows,]
test_knn<-derm_knn[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_knn<-train_val[train_rows2,]
train_knn<-train_val[-train_rows2,]

dim(train_knn)
dim(val_knn)
dim(test_knn)



train_rows<-sample(1:nrow(derm_central),nrow(derm_central)*0.8)
train_val<-derm_central[train_rows,]
test_cen<-derm_central[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_cen<-train_val[train_rows2,]
train_cen<-train_val[-train_rows2,]

dim(train_cen)
dim(val_cen)
dim(test_cen)



train_rows<-sample(1:nrow(derm),nrow(derm)*0.8)
train_val<-derm[train_rows,]
test_raw<-derm[-train_rows,]
train_rows2<-sample(1:nrow(train_val),nrow(train_val)*0.25)
val_raw<-train_val[train_rows2,]
train_raw<-train_val[-train_rows2,]

dim(train_raw)
dim(val_raw)
dim(test_raw)

#Multinomial Regression
library(nnet)
#Knn model
model_knn_glm<-multinom(disease_type~.,data=train_knn)
summary(model_knn_glm)
val_knn_notarget<-subset(val_knn,select=-c(disease_type))
pred_knn_glm<-predict(model_knn_glm,val_knn_notarget,type='class')
metric_knn_glm<-confusionMatrix(pred_knn_glm,val_knn$disease_type)
metric_knn_glm$byClass[,5]
table(pred_knn_glm,val_knn$disease_type)
test_knn_notarget<-subset(test_knn,select=-c(disease_type))
pred_knn_glm_test<-predict(model_knn_glm,test_knn_notarget,type='class')
metric_knn_glm_test<-confusionMatrix(pred_knn_glm_test,test_knn$disease_type)
metric_knn_glm_test$byClass[,5]
table(pred_knn_glm_test,test_knn$disease_type)


#Central Model
model_cen_glm<-multinom(disease_type~.,data=train_cen)
summary(model_cen_glm)
val_cen_notarget<-subset(val_cen,select=-c(disease_type))
pred_cen_glm<-predict(model_cen_glm,val_cen_notarget,type='class')
metric_cen_glm<-confusionMatrix(pred_cen_glm,val_cen$disease_type)
metric_cen_glm$byClass[,5]
table(pred_cen_glm,val_cen$disease_type)
test_cen_notarget<-subset(test_cen,select=-c(disease_type))
pred_cen_glm_test<-predict(model_cen_glm,test_cen_notarget,type='class')
metric_cen_glm_test<-confusionMatrix(pred_cen_glm_test,test_cen$disease_type)
metric_cen_glm_test$byClass[,5]
table(pred_cen_glm_test,test_cen$disease_type)


#missforest model
model_miss_glm<-multinom(disease_type~.,data=train_miss)
summary(model_miss_glm)
val_miss_notarget<-subset(val_miss,select=-c(disease_type))
pred_miss_glm<-predict(model_miss_glm,val_miss_notarget,type='class')
metric_miss_glm<-confusionMatrix(pred_miss_glm,val_miss$disease_type)
metric_miss_glm$byClass[,5]
table(pred_miss_glm,val_miss$disease_type)
test_miss_notarget<-subset(test_miss,select=-c(disease_type))
pred_miss_glm_test<-predict(model_miss_glm,test_miss_notarget,type='class')
metric_miss_glm_test<-confusionMatrix(pred_miss_glm_test,test_miss$disease_type)
metric_miss_glm_test$byClass[,5]
table(pred_miss_glm_test,test_miss$disease_type)


#Raw model (no use because multinom doesn't deal with na values)
model_raw_glm<-multinom(disease_type~.,data=train_raw)
summary(model_raw_glm)
val_raw_notarget<-subset(val_raw,select=-c(disease_type))
pred_raw_glm<-predict(model_raw_glm,val_raw_notarget,type='class')
metric_raw_glm<-confusionMatrix(pred_raw_glm,val_raw$disease_type)
metric_raw_glm$byClass[,5]
table(pred_raw_glm,val_raw$disease_type)
test_raw_notarget<-subset(test_raw,select=-c(disease_type))
pred_raw_glm_test<-predict(model_raw_glm,test_raw_notarget,type='class')
metric_knn_raw_test<-confusionMatrix(pred_raw_glm_test,test_raw$disease_type)
metric_knn_raw_test$byClass[,5]
table(pred_raw_glm_test,test_raw$disease_type)


#c5.0 model
library(C50)
#knn model
model_knn_c50<-C5.0(disease_type~.,data=train_knn)
summary(model_knn_c50)
pred_knn_c50<-predict(model_knn_c50,val_knn_notarget,type='class')
metric_knn_c50<-confusionMatrix(pred_knn_c50,val_knn$disease_type)
metric_knn_c50$byClass[,5]
table(pred_knn_c50,val_knn$disease_type)
pred_knn_c50_test<-predict(model_knn_c50,test_knn_notarget,type='class')
metric_knn_c50_test<-confusionMatrix(pred_knn_c50_test,test_knn$disease_type)
metric_knn_c50_test$byClass[,5]
table(pred_knn_c50_test,test_knn$disease_type)



#central model
model_cen_c50<-C5.0(disease_type~.,data=train_cen)
summary(model_cen_c50)
pred_cen_c50<-predict(model_cen_c50,val_cen_notarget,type='class')
metric_cen_c50<-confusionMatrix(pred_cen_c50,val_cen$disease_type)
metric_cen_c50$byClass[,5]
table(pred_cen_c50,val_cen$disease_type)
pred_cen_c50_test<-predict(model_cen_c50,test_cen_notarget,type='class')
metric_cen_c50_test<-confusionMatrix(pred_cen_c50_test,test_cen$disease_type)
metric_cen_c50_test$byClass[,5]
table(pred_cen_c50_test,test_cen$disease_type)

#missforest model
model_miss_c50<-C5.0(disease_type~.,data=train_miss)
summary(model_miss_c50)
pred_miss_c50<-predict(model_miss_c50,val_miss_notarget,type='class')
metric_miss_c50<-confusionMatrix(pred_miss_c50,val_miss$disease_type)
metric_miss_c50$byClass[,5]
table(pred_miss_c50,val_miss$disease_type)
pred_miss_c50_test<-predict(model_miss_c50,test_miss_notarget,type='class')
metric_miss_c50_test<-confusionMatrix(pred_miss_c50_test,test_miss$disease_type)
metric_miss_c50_test$byClass[,5]
table(pred_miss_c50_test,test_miss$disease_type)


#raw model
model_raw_c50<-C5.0(disease_type~.,data=train_raw)
summary(model_raw_c50)
pred_raw_c50<-predict(model_raw_c50,val_raw_notarget,type='class')
metric_raw_c50<-confusionMatrix(pred_raw_c50,val_raw$disease_type)
metric_raw_c50$byClass[,5]
table(pred_raw_c50,val_raw$disease_type)
pred_raw_c50_test<-predict(model_raw_c50,test_raw_notarget,type='class')
metric_raw_c50_test<-confusionMatrix(pred_raw_c50_test,test_raw$disease_type)
metric_raw_c50_test$byClass[,5]
table(pred_raw_c50_test,test_raw$disease_type)


#CART model
library(rpart)
#knn model
model_knn_rpart<-rpart(disease_type~.,data=train_knn)
summary(model_knn_rpart)
pred_knn_rpart<-predict(model_knn_rpart,val_knn_notarget,type='class')
metric_knn_rpart<-confusionMatrix(pred_knn_rpart,val_knn$disease_type)
metric_knn_rpart$byClass[,5]
table(pred_knn_rpart,val_knn$disease_type)
pred_knn_rpart_test<-predict(model_knn_rpart,test_knn_notarget,type='class')
metric_knn_rpart_test<-confusionMatrix(pred_knn_rpart_test,test_knn$disease_type)
metric_knn_rpart_test$byClass[,5]
table(pred_knn_rpart_test,test_knn$disease_type)


#central model
model_cen_rpart<-rpart(disease_type~.,data=train_cen)
summary(model_cen_rpart)
pred_cen_rpart<-predict(model_cen_rpart,val_cen_notarget,type='class')
metric_cen_rpart<-confusionMatrix(pred_cen_rpart,val_cen$disease_type)
metric_cen_rpart$byClass[,5]
table(pred_cen_rpart,val_cen$disease_type)
pred_cen_rpart_test<-predict(model_cen_rpart,test_cen_notarget,type='class')
metric_cen_rpart_test<-confusionMatrix(pred_cen_rpart_test,test_cen$disease_type)
metric_cen_rpart_test$byClass[,5]
table(pred_cen_rpart_test,test_cen$disease_type)

#missforest model
model_miss_rpart<-rpart(disease_type~.,data=train_miss)
summary(model_miss_rpart)
pred_miss_rpart<-predict(model_miss_rpart,val_miss_notarget,type='class')
metric_miss_rpart<-confusionMatrix(pred_miss_rpart,val_miss$disease_type)
metric_miss_rpart$byClass[,5]
table(pred_miss_rpart,val_miss$disease_type)
pred_miss_rpart_test<-predict(model_miss_rpart,test_miss_notarget,type='class')
metric_miss_rpart_test<-confusionMatrix(pred_miss_rpart_test,test_miss$disease_type)
metric_miss_rpart_test$byClass[,5]
table(pred_miss_rpart_test,test_miss$disease_type)

#raw model
model_raw_rpart<-rpart(disease_type~.,data=train_raw)
summary(model_raw_rpart)
pred_raw_rpart<-predict(model_raw_rpart,val_raw_notarget,type='class')
metric_raw_rpart<-confusionMatrix(pred_raw_rpart,val_raw$disease_type)
metric_raw_rpart$byClass[,5]
table(pred_raw_rpart,val_raw$disease_type)
pred_raw_rpart_test<-predict(model_raw_rpart,test_raw_notarget,type='class')
metric_raw_rpart_test<-confusionMatrix(pred_raw_rpart_test,test_raw$disease_type)
metric_raw_rpart_test$byClass[,5]
table(pred_raw_rpart_test,test_raw$disease_type)



#Bagging CART model
install.packages('ipred')
library(ipred)
#knn model
model_knn_bag<-bagging(disease_type~.,data=train_knn)
summary(model_knn_bag)
pred_knn_bag<-predict(model_knn_bag,val_knn_notarget,type='class')
metric_knn_bag<-confusionMatrix(pred_knn_bag,val_knn$disease_type)
metric_knn_bag$byClass[,5]
table(pred_knn_bag,val_knn$disease_type)
pred_knn_bag_test<-predict(model_knn_bag,test_knn_notarget,type='class')
metric_knn_bag_test<-confusionMatrix(pred_knn_bag_test,test_knn$disease_type)
metric_knn_bag_test$byClass[,5]
table(pred_knn_bag_test,test_knn$disease_type)


#central model
model_cen_bag<-bagging(disease_type~.,data=train_cen)
summary(model_cen_bag)
pred_cen_bag<-predict(model_cen_bag,val_cen_notarget,type='class')
metric_cen_bag<-confusionMatrix(pred_cen_bag,val_cen$disease_type)
metric_cen_bag$byClass[,5]
table(pred_cen_bag,val_cen$disease_type)
pred_cen_bag_test<-predict(model_cen_bag,test_cen_notarget,type='class')
metric_cen_bag_test<-confusionMatrix(pred_cen_bag_test,test_cen$disease_type)
metric_cen_bag_test$byClass[,5]
table(pred_cen_bag_test,test_cen$disease_type)

#missforest model
model_miss_bag<-bagging(disease_type~.,data=train_miss)
summary(model_miss_bag)
pred_miss_bag<-predict(model_miss_bag,val_miss_notarget,type='class')
metric_miss_bag<-confusionMatrix(pred_miss_bag,val_miss$disease_type)
metric_miss_bag$byClass[,5]
table(pred_miss_bag,val_miss$disease_type)
pred_miss_bag_test<-predict(model_miss_bag,test_miss_notarget,type='class')
metric_miss_bag_test<-confusionMatrix(pred_miss_bag_test,test_miss$disease_type)
metric_miss_bag_test$byClass[,5]
table(pred_miss_bag_test,test_miss$disease_type)

#raw model
model_raw_bag<-bagging(disease_type~.,data=train_raw)
summary(model_raw_bag)
pred_raw_bag<-predict(model_raw_bag,val_raw_notarget,type='class')
metric_raw_bag<-confusionMatrix(pred_raw_bag,val_raw$disease_type)
metric_raw_bag$byClass[,5]
table(pred_raw_bag,val_raw$disease_type)
pred_raw_bag_test<-predict(model_raw_bag,test_raw_notarget,type='class')
metric_raw_bag_test<-confusionMatrix(pred_raw_bag_test,test_raw$disease_type)
metric_raw_bag_test$byClass[,5]
table(pred_raw_bag_test,test_raw$disease_type)


#Random Forest
library(randomForest)
#knn model
model_knn_for<-randomForest(disease_type~.,data=train_knn)
summary(model_knn_for)
pred_knn_for<-predict(model_knn_for,val_knn_notarget,type='class')
metric_knn_for<-confusionMatrix(pred_knn_for,val_knn$disease_type)
metric_knn_for$byClass[,5]
table(pred_knn_for,val_knn$disease_type)
pred_knn_for_test<-predict(model_knn_for,test_knn_notarget,type='class')
metric_knn_for_test<-confusionMatrix(pred_knn_for_test,test_knn$disease_type)
metric_knn_for_test$byClass[,5]
table(pred_knn_for_test,test_knn$disease_type)


#central model
model_cen_for<-randomForest(disease_type~.,data=train_cen)
summary(model_cen_for)
pred_cen_for<-predict(model_cen_for,val_cen_notarget,type='class')
metric_cen_for<-confusionMatrix(pred_cen_for,val_cen$disease_type)
metric_cen_for$byClass[,5]
table(pred_cen_for,val_cen$disease_type)
pred_cen_for_test<-predict(model_cen_for,test_cen_notarget,type='class')
metric_cen_for_test<-confusionMatrix(pred_cen_for_test,test_cen$disease_type)
metric_cen_for_test$byClass[,5]
table(pred_cen_for_test,test_cen$disease_type)

#missforest model
model_miss_for<-randomForest(disease_type~.,data=train_miss)
summary(model_miss_for)
pred_miss_for<-predict(model_miss_for,val_miss_notarget,type='class')
metric_miss_for<-confusionMatrix(pred_miss_for,val_miss$disease_type)
metric_miss_for$byClass[,5]
table(pred_miss_for,val_miss$disease_type)
pred_miss_for_test<-predict(model_miss_for,test_miss_notarget,type='class')
metric_miss_for_test<-confusionMatrix(pred_miss_for_test,test_miss$disease_type)
metric_miss_for_test$byClass[,5]
table(pred_miss_for_test,test_miss$disease_type)

#raw model
#Can't be made because of missing values


#Gradient Boosting Model
install.packages('gbm')
library(gbm)
#knn model
model_knn_gbm<-gbm(disease_type~.,data=train_knn,distribution='multinomial',n.trees=500)
summary(model_knn_gbm)
pred_knn_gbm<-predict(model_knn_gbm,val_knn_notarget,type='response',n.trees=500)
pred_knn_gbm<-apply(pred_knn_gbm,1,which.max)
metric_knn_gbm<-confusionMatrix(pred_knn_gbm,val_knn$disease_type)
metric_knn_gbm$byClass[,5]
table(pred_knn_gbm,val_knn$disease_type)
pred_knn_gbm_test<-predict(model_knn_gbm,test_knn_notarget,type='response',n.trees=500)
pred_knn_gbm_test<-apply(pred_knn_gbm_test,1,which.max)
metric_knn_gbm_test<-confusionMatrix(pred_knn_gbm_test,test_knn$disease_type)
metric_knn_gbm_test$byClass[,5]
table(pred_knn_gbm_test,test_knn$disease_type)


#central model
model_cen_gbm<-gbm(disease_type~.,data=train_cen,n.trees=500)
summary(model_cen_gbm)
pred_cen_gbm<-predict(model_cen_gbm,val_cen_notarget,type='response',n.trees=500)
pred_cen_gbm<-apply(pred_cen_gbm,1,which.max)
metric_cen_gbm<-confusionMatrix(pred_cen_gbm,val_cen$disease_type)
metric_cen_gbm$byClass[,5]
table(pred_cen_gbm,val_cen$disease_type)
pred_cen_gbm_test<-predict(model_cen_gbm,test_cen_notarget,type='response',n.trees=500)
pred_cen_gbm_test<-apply(pred_cen_gbm_test,1,which.max)
metric_cen_gbm_test<-confusionMatrix(pred_cen_gbm_test,test_cen$disease_type)
metric_cen_gbm_test$byClass[,5]
table(pred_cen_gbm_test,test_cen$disease_type)


#missforest model
model_miss_gbm<-gbm(disease_type~.,data=train_miss,n.trees=500)
summary(model_miss_gbm)
pred_miss_gbm<-predict(model_miss_gbm,val_miss_notarget,type='response',n.trees=500)
pred_miss_gbm<-apply(pred_miss_gbm,1,which.max)
metric_miss_gbm<-confusionMatrix(pred_miss_gbm,val_miss$disease_type)
metric_miss_gbm$byClass[,5]
table(pred_miss_gbm,val_miss$disease_type)
pred_miss_gbm_test<-predict(model_miss_gbm,test_miss_notarget,type='response',n.trees=500)
pred_miss_gbm_test<-apply(pred_miss_gbm_test,1,which.max)
metric_miss_gbm_test<-confusionMatrix(pred_miss_gbm_test,test_miss$disease_type)
metric_miss_gbm_test$byClass[,5]
table(pred_miss_gbm_test,test_miss$disease_type)


#raw model
#Can't be made because of missing values




