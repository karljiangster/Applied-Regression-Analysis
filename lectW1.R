### CDS data visualization
## From Slide 13
data <- read.csv("Credit.csv")
names(data)

# R's summary function is pretty clever
summary(data[,c(2:4,6,7)])


# Histograms
par(mfrow=c(1,3)) # break the plot into 1x3 matrix
hist(data$CDS, col=grey(.4), border=grey(.9),xlab="CDS Spread",main="Histogram of CDS Spread")
hist(data$Vol, xlab="Volatility",main="Histogram of Volatility", col=grey(.4), border=grey(.9))
hist(data$Beta, col=grey(.4), border=grey(.9),xlab="Beta",main="Histogram of Beta")

# Scatterplots
par(mfrow=c(1,2))
# B&W
plot(data$Vol,data$CDS,pch=20,xlab="Volatility",ylab="CDS Spread")
plot(data$Beta,data$CDS,pch=20,xlab="Beta",ylab="CDS Spread")

# Color
par(mfrow=c(1,2))
plot(data$Vol,data$CDS,pch=20, col=data$Grade,xlab="Volatility",ylab="CDS Spread")
legend("topleft", fill=c(1:2), legend=levels(data$Grade), cex=.7) 
plot(data$Beta,data$CDS,pch=20, col=data$Grade,xlab="Beta",ylab="CDS Spread")
legend("topleft", fill=c(1:2), legend=levels(data$Grade), cex=.7) 

# 'pairs' scatterplot matrix
pairs(data[,2:4], pch=20, col=data$Rating)

### Boxplots with the credit data
## Slide 18

boxplot(data$CDS)

par(mfrow=c(1,2))
boxplot(data$CDS ~ data$Grade,ylab="CDS Spread", col=8, main="Credit Grade")
boxplot(data$CDS ~ data$Type,ylab="CDS Spread", col=8, main="Industry Type",names=c("B","CO","CC","CN","E","F","I","T","U"))

### ANOVA with simulated dataS
## From Slide 28
par(mfrow=c(1,1))
boxplot( Y <- data.frame(Y0 = rnorm(n=10, mean=0),
                         Y1 = rnorm(n=10, mean=5),
                         Y2 = rnorm(n=10, mean=10)) )
# sapply applies mean calculation to each column of list data.frame Y
print( groupYbar<-sapply(Y,mean) )

# unlist removes this cleverness
print( totalYbar <- mean( unlist(Y) ) )

#Sums of Squares:
print( SSR <- sum( 10*(groupYbar - totalYbar)^2 ) )
print( SST <- sum( (unlist(Y) - totalYbar)^2 ) )
SSR/SST # proportion of explained variability
print(SSE<-sum((Y$Y0-groupYbar[1])^2)+sum((Y$Y1-groupYbar[2])^2)+sum((Y$Y2-groupYbar[3])^2))

### ANOVA example: back to Credit.csv
## From Slide 31
boxplot(data$CDS ~ data$Grade, ylab="CDS Spread")
# Our first real modeling!
# lm stands for 'linear model'
print(model <- lm(data$CDS ~ data$Grade))
mean(data$CDS[data$Grade=="Investment"])
mean(data$CDS[data$Grade=="Speculative"])
#
anova(model)
round( 3282834/(3282834+3946809), 3 )


From Slides 40

rm(list=ls())
data <- read.csv('obama.csv')
Dprob<-diff(data$Prob[90:455])
RetGreen<-diff(log(data$Index[90:455]))
RetSPX<-diff(log(data$SPX[90:455]))

cor(RetGreen,Dprob)
cor(RetSPX,Dprob)

## From Slide 42:
data <- read.csv("Credit.csv")
Y <- (data$CDS[data$Grade=="Speculative"])
X <- (data$Vol[data$Grade=="Speculative"])
# These are the coefficients we found in class
round(b1 <- cor(X,Y)*sd(Y)/sd(X),2) # Use correlation for slope
round(b0 <- mean(Y) - mean(X)*b1,2) # Put [X.bar,Y.bar] on the line
# Plot the data and our line
plot(X,Y)

xx <- seq(10,80,length=4)
lines(xx, b0 + b1*xx, col=2)
## Plot the residuals Y - Y.hat
e <- Y - (b0+b1*X)
plot(X, e, xlab="Volatility", ylab="Residuals",  pch=20)
abline(h=0, col=8, lwd=2) # Just to make it pretty


## Extra
Y <- (data$CDS[data$Grade=="Investment"])
X <- (data$Vol[data$Grade=="Investment"])
Z <- (data$Beta[data$Grade=="Investment"])
reg<-(model <- lm(Y~X+Z))
