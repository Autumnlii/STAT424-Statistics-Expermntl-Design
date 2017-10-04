sigmaS = 0.035
sigmaSqS = sigmaS^2
alpha3S = ( exp(sigmaSqS) + 2 )*sqrt( exp(sigmaSqS) - 1 )
alpha3S # alpha is less than 0.25


sigmaM = 0.25
sigmaSqM = sigmaM^2
alpha3M = ( exp(sigmaSqM) + 2 )*sqrt( exp(sigmaSqM) - 1 )
alpha3M # 0.25 < alpha < 1.0


sigmaL = 0.75
sigmaSqL = sigmaL^2
alpha3L = ( exp(sigmaSqL) + 2 )*sqrt( exp(sigmaSqL) - 1 )
alpha3L # alpha is greater than 1.0



#### Create a plot of the population model
x = seq(0,2,.001)
plot(x, dlnorm(x, 0, sigmaS), type= "l", col ="blue")
lines(x, dlnorm(x, 0, sigmaM), type ="l", col = "red")
lines(x, dlnorm(x, 0, sigmaL), type ="l", col = "green")
legend("topright",c("alpha3 < 0.25","0.25< alpha3 < 1", "1 < alpha3"),lty=1,col=c("blue", "red", "green"))


#### Calculate 1,000 sample means, sd, and skew coefficient


#n = 10
meanS10 = rep(0, 1000)
meanM10 = rep(0, 1000)
meanL10 = rep(0, 1000)

sdS10 = rep(0, 1000)
sdM10 = rep(0, 1000)
sdL10 = rep(0, 1000)

scS10 = rep(0, 1000)
scM10 = rep(0, 1000)
scL10 = rep(0, 1000)

for (i in 1:1000) {
  z = rlnorm(10, mean=0, sigmaS) 
  meanS10[i] = mean(z)
  sdS10[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:10) {
    estAlpha = estAlpha + (z[j] - meanS10[i])^3 
  }
  estAlpha = estAlpha/10
  scS10[i] = estAlpha/(sdS10[i]^3)
}

for (i in 1:1000) {
  z = rlnorm(10, mean=0, sigmaM) 
  meanM10[i] = mean(z)
  sdM10[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:10) {
    estAlpha = estAlpha + (z[j] - meanM10[i])^3 
  }
  estAlpha = estAlpha/10
  scM10[i] = estAlpha/(sdM10[i]^3)
  
}

for (i in 1:1000) {
  z = rlnorm(10, mean=0, sigmaL) 
  meanL10[i] = mean(z)
  sdL10[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:10) {
    estAlpha = estAlpha + (z[j] - meanL10[i])^3 
  }
  estAlpha = estAlpha/10
  scL10[i] = estAlpha/(sdL10[i]^3)
  
}


# n = 30

meanS30 = rep(0, 1000)
meanM30 = rep(0, 1000)
meanL30 = rep(0, 1000)

sdS30 = rep(0, 1000)
sdM30 = rep(0, 1000)
sdL30 = rep(0, 1000)

scS30 = rep(0, 1000)
scM30 = rep(0, 1000)
scL30 = rep(0, 1000)

for (i in 1:1000) {
  z = rlnorm(30, mean=0, sigmaS) 
  meanS30[i] = mean(z)
  sdS30[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:30) {
    estAlpha = estAlpha + (z[j] - meanS30[i])^3 
  }
  estAlpha = estAlpha/30
  scS30[i] = estAlpha/(sdS30[i]^3)
}

for (i in 1:1000) {
  z = rlnorm(30, mean=0, sigmaM) 
  meanM30[i] = mean(z)
  sdM30[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:30) {
    estAlpha = estAlpha + (z[j] - meanM30[i])^3 
  }
  estAlpha = estAlpha/30
  scM30[i] = estAlpha/(sdM30[i]^3)
  
}

for (i in 1:1000) {
  z = rlnorm(30, mean=0, sigmaL) 
  meanL30[i] = mean(z)
  sdL30[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:30) {
    estAlpha = estAlpha + (z[j] - meanL30[i])^3 
  }
  estAlpha = estAlpha/30
  scL30[i] = estAlpha/(sdL30[i]^3)
  
}


n = 100

meanS100 = rep(0, 1000)
meanM100 = rep(0, 1000)
meanL100 = rep(0, 1000)

sdS100 = rep(0, 1000)
sdM100 = rep(0, 1000)
sdL100 = rep(0, 1000)

scS100 = rep(0, 1000)
scM100 = rep(0, 1000)
scL100 = rep(0, 1000)

for (i in 1:1000) {
  z = rlnorm(100, mean=0, sigmaS) 
  meanS100[i] = mean(z)
  sdS100[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:100) {
    estAlpha = estAlpha + (z[j] - meanS100[i])^3 
  }
  estAlpha = estAlpha/100
  scS100[i] = estAlpha/(sdS100[i]^3)
}

for (i in 1:1000) {
  z = rlnorm(100, mean=0, sigmaM) 
  meanM100[i] = mean(z)
  sdM100[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:100) {
    estAlpha = estAlpha + (z[j] - meanM100[i])^3 
  }
  estAlpha = estAlpha/100
  scM100[i] = estAlpha/(sdM100[i]^3)
  
}

for (i in 1:1000) {
  z = rlnorm(100, mean=0, sigmaL) 
  meanL100[i] = mean(z)
  sdL100[i] = sd(z)
  
  estAlpha = 0
  for (j in 1:100) {
    estAlpha = estAlpha + (z[j] - meanL100[i])^3 
  }
  estAlpha = estAlpha/100
  scL100[i] = estAlpha/(sdL100[i]^3)
  
}



#### Calculate the sample skew coefficient of the 1,000 means

# n = 10
meanScS10 = 0
meanScM10 = 0
meanScL10 = 0

for (i in 1:1000) {
  meanScS10 = meanScS10 + (meanS10[i] - mean(meanS10) )^3 
}
meanScS10 = meanScS10/1000
meanScS10 = meanScS10/(sd(meanS10)^3)

for (i in 1:1000) {
  meanScM10 = meanScM10 + (meanM10[i] - mean(meanM10) )^3 
}
meanScM10 = meanScM10/1000
meanScM10 = meanScM10/(sd(meanM10)^3)

for (i in 1:1000) {
  meanScL10 = meanScL10 + (meanL10[i] - mean(meanL10) )^3 
}
meanScL10 = meanScL10/1000
meanScL10 = meanScL10/(sd(meanL10)^3)


# n = 30
meanScS30 = 0
meanScM30 = 0
meanScL30 = 0

for (i in 1:1000) {
  meanScS30 = meanScS30 + (meanS30[i] - mean(meanS30) )^3 
}
meanScS30 = meanScS30/1000
meanScS30 = meanScS30/(sd(meanS30)^3)

for (i in 1:1000) {
  meanScM30 = meanScM30 + (meanM30[i] - mean(meanM30) )^3 
}
meanScM30 = meanScM30/1000
meanScM30 = meanScM30/(sd(meanM30)^3)

for (i in 1:1000) {
  meanScL30 = meanScL30 + (meanL30[i] - mean(meanL30) )^3 
}
meanScL30 = meanScL30/1000
meanScL30 = meanScL30/(sd(meanL30)^3)


# n = 100
meanScS100 = 0
meanScM100 = 0
meanScL100 = 0

for (i in 1:1000) {
  meanScS100 = meanScS100 + (meanS100[i] - mean(meanS100) )^3 
}
meanScS100 = meanScS100/1000
meanScS100 = meanScS100/(sd(meanS100)^3)

for (i in 1:1000) {
  meanScM100 = meanScM100 + (meanM100[i] - mean(meanM100) )^3 
}
meanScM100 = meanScM100/1000
meanScM100 = meanScM100/(sd(meanM100)^3)

for (i in 1:1000) {
  meanScL100 = meanScL100 + (meanL100[i] - mean(meanL100) )^3 
}
meanScL100 = meanScL100/1000
meanScL100 = meanScL100/(sd(meanL100)^3)


#### Calculate the simulated standard error by finding the standard deviation of your 1000 means and
#### compare to the theoretical value of sd/sqrt(n)

#n = 10 (CLT are true because the two values are similar)
sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(10) #theoratical value
sd(meanS10)

sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(10) 
sd(meanM10)

sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(10) 
sd(meanL10)

#n = 30
sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(30) 
sd(meanS30)

sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(30) 
sd(meanM30)

sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(30) 
sd(meanL30)

#n = 100
sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(100) 
sd(meanS100)

sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(100) 
sd(meanM100)

sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(100) 
sd(meanL100)


#### Creat histograms and normal probability plots for the mean.

par(mfrow=c(3,3))
###comparing the historm plot, means follow theoretical distribution, CLT true
#n = 10
hist(	meanS10, col="yellow", xlab="x", main="Histogram of Mean with \n n = 10 & Small alpha3", freq=FALSE  )
x = seq(min(meanS10),max(meanS10),length=1000)
lines(x,dnorm(x, mean(meanS10), sd=sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(10)), col="red", lwd=2)


hist(	meanM10, col="yellow", xlab="x", main="Histogram of Mean with \n n = 10 & Medium alpha3", freq=FALSE )
x = seq(min(meanM10),max(meanM10),length=1000)
lines(x,dnorm(x, mean(meanM10), sd=sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(10)), col="red", lwd=2)

hist(	meanL10, col="yellow", xlab="x", main="Histogram of Mean with \n n = 10 & Large alpha3", freq=FALSE )
x = seq(min(meanL10),max(meanL10),length=1000)
lines(x,dnorm(x, mean(meanL10), sd=sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(10)), col="red", lwd=2)


#n = 30
hist(	meanS30, col="yellow", xlab="x", main="Histogram of Mean with \n n = 30 & Small alpha3", freq=FALSE  )
x = seq(min(meanS30),max(meanS30),length=1000)
lines(x,dnorm(x, mean(meanS30), sd=sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(30)), col="red", lwd=2)

hist(	meanM30, col="yellow", xlab="x", main="Histogram of Mean with \n n = 30 & Medium alpha3", freq=FALSE )
x = seq(min(meanM30),max(meanM30),length=1000)
lines(x,dnorm(x, mean(meanM30), sd=sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(30)), col="red", lwd=2)

hist(	meanL30, col="yellow", xlab="x", main="Histogram of Mean with \n n = 30 & Large alpha3", freq=FALSE )
x = seq(min(meanL30),max(meanL30),length=1000)
lines(x,dnorm(x, mean(meanL30), sd=sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(30)), col="red", lwd=2)


#n = 100
hist(	meanS100, col="yellow", xlab="x", main="Histogram of Mean with \n n = 100 & Small alpha3", freq=FALSE  )
x = seq(min(meanS100),max(meanS100),length=1000)
lines(x,dnorm(x, mean(meanS100), sd=sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(100)), col="red", lwd=2)

hist(	meanM100, col="yellow", xlab="x", main="Histogram of Mean with \n n = 100 & Medium alpha3", freq=FALSE )
x = seq(min(meanM100),max(meanM100),length=1000)
lines(x,dnorm(x, mean(meanM100), sd=sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(100)), col="red", lwd=2)

hist(	meanL100, col="yellow", xlab="x", main="Histogram of Mean with \n n = 100 & Large alpha3", freq=FALSE )
x = seq(min(meanL100),max(meanL100),length=1000)
lines(x,dnorm(x, mean(meanL100), sd=sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(100)), col="red", lwd=2)




#### Creat histograms and Chi-square probability plots for the variance.

ChiS10 = rep(0, 1000)
ChiM10 = rep(0, 1000)
ChiL10 = rep(0, 1000)
ChiS30 = rep(0, 1000)
ChiM30 = rep(0, 1000)
ChiL30 = rep(0, 1000)
ChiS100 = rep(0, 1000)
ChiM100 = rep(0, 1000)
ChiL100 = rep(0, 1000)

for (i in 1:1000) {
  ChiS10[i] = (10-1)*sdS10[i]^2/(exp(2*sigmaS^2)-exp(sigmaS^2))
  ChiM10[i] = (10-1)*sdM10[i]^2/(exp(2*sigmaM^2)-exp(sigmaM^2))
  ChiL10[i] = (10-1)*sdL10[i]^2/(exp(2*sigmaL^2)-exp(sigmaL^2))
  
  ChiS30[i] = (30-1)*sdS30[i]^2/(exp(2*sigmaS^2)-exp(sigmaS^2))
  ChiM30[i] = (30-1)*sdM30[i]^2/(exp(2*sigmaM^2)-exp(sigmaM^2))
  ChiL30[i] = (30-1)*sdL30[i]^2/(exp(2*sigmaL^2)-exp(sigmaL^2))
  
  ChiS100[i] = (100-1)*sdS100[i]^2/(exp(2*sigmaS^2)-exp(sigmaS^2))
  ChiM100[i] = (100-1)*sdM100[i]^2/(exp(2*sigmaM^2)-exp(sigmaM^2))
  ChiL100[i] = (100-1)*sdL100[i]^2/(exp(2*sigmaL^2)-exp(sigmaL^2))
}


par(mfrow=c(3,3))

#n = 10
hist(	ChiS10, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 10 & Small alpha3" ), freq=FALSE  )
x = seq(min(ChiS10),max(ChiS10),length=1000)
lines(x,dchisq(x, 10-1, ncp = 0), col="red", lwd=2)

hist(	ChiM10, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 10 & Medium alpha3" ), freq=FALSE  )
x = seq(min(ChiM10),max(ChiM10),length=1000)
lines(x,dchisq(x, 10-1, ncp = 0), col="red", lwd=2)

hist(	ChiL10, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 10 & Large alpha3" ), freq=FALSE  )
x = seq(min(ChiL10),max(ChiL10),length=1000)
lines(x,dchisq(x, 10-1, ncp = 0), col="red", lwd=2)


#n = 30

hist(	ChiS30, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 30 & Small alpha3" ), freq=FALSE  )
x = seq(min(ChiS30),max(ChiS30),length=1000)
lines(x,dchisq(x, 30-1, ncp = 0), col="red", lwd=2)

hist(	ChiM30, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 30 & Medium alpha3" ), freq=FALSE  )
x = seq(min(ChiM30),max(ChiM30),length=1000)
lines(x,dchisq(x, 30-1, ncp = 0), col="red", lwd=2)

hist(	ChiL30, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 30 & Large alpha3" ), freq=FALSE  )
x = seq(min(ChiL30),max(ChiL30),length=1000)
lines(x,dchisq(x, 30-1, ncp = 0), col="red", lwd=2)


#n = 100
hist(	ChiS100, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 100 & Small alpha3" ), freq=FALSE  )
x = seq(min(ChiS100),max(ChiS100),length=1000)
lines(x,dchisq(x, 100-1, ncp = 0), col="red", lwd=2)

hist(	ChiM100, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 100 & Medium alpha3" ), freq=FALSE  )
x = seq(min(ChiM100),max(ChiM100),length=1000)
lines(x,dchisq(x, 100-1, ncp = 0), col="red", lwd=2)

hist(	ChiL100, col="yellow", xlab="x", main=expression("Histogram of "~(n-1)~S^{2}~"/"~sigma^{2}~ " with n = 100 & Large alpha3" ), freq=FALSE  )
x = seq(min(ChiL100),max(ChiL100),length=1000)
lines(x,dchisq(x, 100-1, ncp = 0), col="red", lwd=2)


#### Generate 95% confidence intervals for the population mean, and count the number that cover the mean

# n = 10
S10_LeftCI = exp(0.5*sigmaS^2) - qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(10)
S10_RightCI = exp(0.5*sigmaS^2) + qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(10)
cntS10 = length( which(S10_LeftCI <= meanS10 & meanS10 <= S10_RightCI ) )


M10_LeftCI = exp(0.5*sigmaM^2) - qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(10)
M10_RightCI = exp(0.5*sigmaM^2) + qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(10)
cntM10 = length( which(M10_LeftCI <= meanM10 & meanM10 <= M10_RightCI ) )


L10_LeftCI = exp(0.5*sigmaL^2) - qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(10)
L10_RightCI = exp(0.5*sigmaL^2) + qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(10)
cntL10 = length( which(L10_LeftCI <= meanL10 & meanL10 <= L10_RightCI ) )
cntS10
cntM10
cntL10

# n = 30
S30_LeftCI = exp(0.5*sigmaS^2) - qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(30)
S30_RightCI = exp(0.5*sigmaS^2) + qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(30)
cntS30 = length( which(S30_LeftCI <= meanS30 & meanS30 <= S30_RightCI ) )


M30_LeftCI = exp(0.5*sigmaM^2) - qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(30)
M30_RightCI = exp(0.5*sigmaM^2) + qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(30)
cntM30 = length( which(M30_LeftCI <= meanM30 & meanM30 <= M30_RightCI ) )


L30_LeftCI = exp(0.5*sigmaL^2) - qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(30)
L30_RightCI = exp(0.5*sigmaL^2) + qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(30)
cntL30 = length( which(L30_LeftCI <= meanL30 & meanL30 <= L30_RightCI ) )
cntS30
cntM30
cntL30

# n = 100
S100_LeftCI = exp(0.5*sigmaS^2) - qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(100)
S100_RightCI = exp(0.5*sigmaS^2) + qnorm(0.975)*sqrt((exp(2*sigmaS^2)-exp(sigmaS^2)))/sqrt(100)
cntS100 = length( which(S100_LeftCI <= meanS100 & meanS100 <= S100_RightCI ) )


M100_LeftCI = exp(0.5*sigmaM^2) - qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(100)
M100_RightCI = exp(0.5*sigmaM^2) + qnorm(0.975)*sqrt((exp(2*sigmaM^2)-exp(sigmaM^2)))/sqrt(100)
cntM100 = length( which(M100_LeftCI <= meanM100 & meanM100 <= M100_RightCI ) )


L100_LeftCI = exp(0.5*sigmaL^2) - qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(100)
L100_RightCI = exp(0.5*sigmaL^2) + qnorm(0.975)*sqrt((exp(2*sigmaL^2)-exp(sigmaL^2)))/sqrt(100)
cntL100 = length( which(L100_LeftCI <= meanL100 & meanL100 <= L100_RightCI ) )
cntS100
cntM100
cntL100

#### Generate 95% confidence intervals for the population standard deviation, and count the number that cover the standard deviation

# n = 10
S10_RightCI_SD = sqrt( (10-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.025, 10-1) )
S10_LeftCI_SD = sqrt( (10-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.975, 10-1) )
cntS10_SD = length( which(S10_LeftCI_SD <= sdS10 & sdS10 <= S10_RightCI_SD ) )


M10_RightCI_SD = sqrt( (10-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.025, 10-1) )
M10_LeftCI_SD = sqrt( (10-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.975, 10-1) )
cntM10_SD = length( which(M10_LeftCI_SD <= sdM10 & sdM10 <= M10_RightCI_SD ) )


L10_RightCI_SD = sqrt( (10-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.025, 10-1) )
L10_LeftCI_SD = sqrt( (10-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.975, 10-1) )
cntL10_SD = length( which(L10_LeftCI_SD <= sdL10 & sdL10 <= L10_RightCI_SD ) )
cntS10_SD
cntM10_SD
cntL10_SD

# n = 30
S30_RightCI_SD = sqrt( (30-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.025, 30-1) )
S30_LeftCI_SD = sqrt( (30-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.975, 30-1) )
cntS30_SD = length( which(S30_LeftCI_SD <= sdS30 & sdS30 <= S30_RightCI_SD ) )

M30_RightCI_SD = sqrt( (30-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.025, 30-1) )
M30_LeftCI_SD = sqrt( (30-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.975, 30-1) )
cntM30_SD = length( which(M30_LeftCI_SD <= sdM30 & sdM30 <= M30_RightCI_SD ) )

L30_RightCI_SD = sqrt( (30-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.025, 30-1) )
L30_LeftCI_SD = sqrt( (30-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.975, 30-1) )
cntL30_SD = length( which(L30_LeftCI_SD <= sdL30 & sdL30 <= L30_RightCI_SD ) )
cntS30_SD
cntM30_SD
cntL30_SD

# n = 100
S100_RightCI_SD = sqrt( (100-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.025, 100-1) )
S100_LeftCI_SD = sqrt( (100-1)*(exp(2*sigmaS^2)-exp(sigmaS^2))/qchisq(0.975, 100-1) )
cntS100_SD = length( which(S100_LeftCI_SD <= sdS100 & sdS100 <= S100_RightCI_SD ) )

M100_RightCI_SD = sqrt( (100-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.025, 100-1) )
M100_LeftCI_SD = sqrt( (100-1)*(exp(2*sigmaM^2)-exp(sigmaM^2))/qchisq(0.975, 100-1) )
cntM100_SD = length( which(M100_LeftCI_SD <= sdM100 & sdM100 <= M100_RightCI_SD ) )

L100_RightCI_SD = sqrt( (100-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.025, 100-1) )
L100_LeftCI_SD = sqrt( (100-1)*(exp(2*sigmaL^2)-exp(sigmaL^2))/qchisq(0.975, 100-1) )
cntL100_SD = length( which(L100_LeftCI_SD <= sdL100 & sdL100 <= L100_RightCI_SD ) )
cntS100_SD
cntM100_SD
cntL100_SD




