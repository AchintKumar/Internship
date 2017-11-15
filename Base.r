#Setting path in master code
rm(list=ls())
library(ggplot2)
getwd()
setwd('/media/achint/INSOFE/Internship')

#Reading data
derm_raw<-read.csv('dermatology_data.csv')
View(derm_raw)
summary(derm_raw)
str(derm_raw)
agenum<-subset(derm_raw,select=c(age))

#factorizing all the variables
factorize<-function(x){as.factor(x)}
derm<-apply(derm_raw,2,factorize)
derm<-as.data.frame(derm)
str(derm)
#removing the only numeric variable
derm$age<-NULL

#changing age into numeric
str(agenum)
agenum$age<-as.numeric(agenum$age)
derm<-cbind(derm,agenum)
str(derm)

#Erythema
str(derm$erythema)
#plot(derm$erythema)
#jpeg('erythema_plot.jpeg')
#dev.off()
#Trying for all the plots
var_list<-combn(names(derm)[1:34],2,simplify=FALSE)
plot_list<-list()
for (i in 1:34){
	p=plot(derm[var_list[i]])
	plot_list[[i]]=p
} 
pdf('plots.pdf')
for (i in 1:34) {
	print(plot_list[[i]])
}
dev.off()