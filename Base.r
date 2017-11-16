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
sum(is.na(derm_raw))
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

#Missing Values
sum(is.na(derm))
miss<-function(x){sum(is.na(x))/length(x)}
apply(derm,2,miss)

#Erythema
str(derm)
#plot(derm$erythema)
#jpeg('erythema_plot.jpeg')
#dev.off()
#Trying for all the plots
#var_list<-combn(names(derm)[1:34],2,simplify=FALSE)
#plot_list<-list()
#for (i in 1:34){
#	p=plot(derm[var_list[i]])
#	plot_list[[i]]=p
#} 
#pdf('plots.pdf')
#for (i in 1:34) {
#	print(plot_list[[i]])
#}
#dev.off()

for (i in 1:ln-1) 
{
    jpeg(paste('jpeg', names(derm)[i], '.jpeg', sep='')
    plot(derm[,i], ylab=names(derm[i]),)
    dev.off()
}



dfplot <- function(data.frame)
{
  df <- data.frame
  ln <- length(names(data.frame))
  for(i in 1:ln){
    mname <- substitute(df[,i])
    jpeg(names(df)[i],'.jpeg')
    if(is.factor(df[,i])){
        plot(df[,i],main=names(df)[i])}
    else{hist(df[,i],main=names(df)[i])}
        dev.off()
  }
}
dfplot(derm)


mypalette = rainbow(ncol(derm))
matplot(y = derm$erythema, type = 'b',pch=15:19, col = mypalette)


ln<-length(names(derm))
for (i in 1:ln-1) {
	jpeg(file='plot.jpeg')
	#dev.copy(jpeg,filename=derm[i],'plot.jpeg',sep='')
	plot(derm[,i],xlab=names(derm[i]))
	dev.off()
}


for(i in 1:ln-1)
{
	dev.copy(jpeg,filename=paste(names(derm[i])),'plot.jpeg',sep='')
	plot(derm[,i],xlab=names(derm[i]),ylab=)
	dev.off()
}



#new commit