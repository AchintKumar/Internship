#Setting path in master code
rm(list=ls())
library(ggplot2)
getwd()
setwd('/media/achint/INSOFE/Internship')

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
for(i in seq(1,366,5))
{
  derm$erythema<-replace(derm$erythema,i,NA)
}
derm2<-derm
for(i in seq(1,366,5))
{
  derm2$erythema<-replace(derm2$erythema,i,'')
}
str(derm2)
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
sum(is.na(derm))
write.csv(derm2,file='derm_missing2.csv',row.names=FALSE)

#doing different types of imputations
#KNN,central,missForest,rpart,amelia,mice
library(DMwR)
derm_knn<-derm
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
