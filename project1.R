data1
####delete one obserrvation#####
data1=data1[order(data1$Run),]
###drop the 7th observation
data1[-7,]
data1
######Descriptive satatistics for each measurement
summary(data1$Temp)###summary of the tempt
sd(data1$Temp)
summary(data1$Conv)
sd(data1$Conv)
#Temp by Reactor 
tapply(data1$Temp, data1$Reactor, summary)
tapply(data1$Temp, data1$Reactor, sd)
#Conv by reacor
tapply(data1$Conv, data1$Reactor, summary)
tapply(data1$Conv,data1$Reactor,sd)
#overall correlation between the temp and the conv
cor(data1$Temp, data1$Conv)
#corelation by reactor a
dataA= data1[which(data1$Reatcor=='A'),]
dataA
cor(dataA$Temp,dataA$Conv)
dataB= data1[which(data1$Reatcor=='B'),]
dataB
cor(dataB$Temp, dataB$Conv)

####Comparision hist
#Temp
par(mfrow=c(1,2))
hist(dataA$Temp, xlab="Temp",main="Histogram if temporature \n Reactor Type A", freq=F)
hist(dataB$Temp, xlab ="Temp", main ="Histgram of Temprature \n Reactor Type B", fre=F)

###Conv
par(mfrow=c(1,2))
hist(dataA$Temp, xlab="Conv",main="Histogram if Conversion \n Reactor Type A", freq=F)
hist(dataB$Temp, xlab ="Conv", main ="Histgram of Conversion \n Reactor Type B", fre=F)
##scatter plot of y=conversion vs. x = temp
plot(data1$Temp, data1$Conv, pch = levels(data1$Reactor), xlab="Temperature", ylab="Conversion", main="scatter plot")
######A normal probability plot of the deviations about the individual reacot means
meanTempA=mean(dataA$Temp)
meanTempB=mean(dataA=B$Temp)
devTemp=data$Temp-meanTempA
devTempA=dataA$Temp-meanTempA
devTempB=dataB$Temp-meanTempB


####
meanConvA=mean(dataA$Conv)
meanConvB=mean(dataB$Conv)
devConvA=dataA$Conv-meanConvA
devConvB=dataB$Conv-meanConvB

#####
par(mfrow=c(2,2))
qqnorm(devTempA, main="Normality plot for Temp w/ Type A")
qqline(devTempA)
qqnorm(devTempB, main="Normality plot for Temp w/ Type B")
qqline(devTempB)
qqnorm(devConvA, main="Normality plot for Temp w/ Type A")
qqline(ConvTempA)
qqnorm(devConvB, main="Normality plot for Temp w/ Type B")
qqline(ConvTempB)
