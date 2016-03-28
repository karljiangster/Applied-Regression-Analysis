#### Again, House price v. size data.
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)
n <- 15

#### Run the regression
house.reg <- lm(price ~ size)

####  Look at the summary:
summary(house.reg)

#### Estimated error variance (s^2)
## from slide 7
summary(house.reg)$sigma
sqrt(sum(house.reg$resid^2)/(n-2))

sqrt(var(house.reg$resid)) #this is wrong because n- 1d dgerees of freedom 

sqrt(var(house.reg$resid)*(n-1)/(n-2))

## Slide 8


beta0<--2.5
beta1<-2
x<-c(-2.8, -2.5, -1.5,-1,2,3.5)
sigma<-2.0
y<-beta0+beta1*x+rnorm(length(x))*sigma
plot(x,y,pch=20,xlim=c(-4,4),ylim=c(-10,6),main=paste("beta0 = ",as.character(beta0),", beta1 = ",as.character(beta1)))
abline(a=beta0,b=beta1,col="blue")
reg<-lm(y~x)
abline(a=reg$coeff[1],b=reg$coeff[2],col="red",lty=2)
text(x=-3,y=5,labels=paste("b0 = ",as.character(round(reg$coeff[1],2)),", b1 = ",as.character(round(reg$coeff[2],2))))


xx<-c(-2.89,-2.86,-2.81,-2.70, -2.67, -2.55,-2.45, -2.41, -2.35, -1.87, -1.83, -1.65,-1.64, -1.56, -1.52, -1.50, -1.38, -1.35,-1.21,-1.14, -1.00, -0.85, -0.44, -0.24,-0.05,  0.19,  0.28,  0.35,  0.38,  0.65,0.74,  0.77,  0.78,  0.93,  1.13,  1.14,1.29,  1.33,  1.38,  1.77,  1.80,  1.84,1.90,  2.31,  2.59,  2.59,  2.60,  2.66,2.89,2.95)
yy<-beta0+beta1*xx+rnorm(length(xx))*sigma
plot(xx,yy,xlim=c(-4,4),ylim=c(-12,6),pch=20,main=paste("beta0 = ",as.character(beta0),", beta1 = ",as.character(beta1)))
abline(a=beta0,b=beta1,col="blue")
reg2<-lm(yy~xx)
abline(a=reg2$coeff[1],b=reg2$coeff[2],col="red",lty=2)
text(x=-3,y=5,labels=paste("b0 = ",as.character(round(reg2$coeff[1],2)),", b1 = ",as.character(round(reg2$coeff[2],2))))

# Slides 17
x<-apply(matrix(rexp(2 * 1000), ncol = 1000), 2, mean)
hist(x,60,main="1000 means from n=2 samples")

x<-apply(matrix(rexp(10 * 1000), ncol = 1000), 2, mean)
hist(x,60,main="1000 means from n=10 samples")

x<-apply(matrix(rexp(100 * 1000), ncol = 1000), 2, mean)
hist(x,60,main="1000 means from n=100 samples")

x<-apply(matrix(rexp(1000 * 1000), ncol = 1000), 2, mean)
hist(x,60, main="1000 means from n=1000 samples")

#Slides 27
hist(rt(100000, 10),60)


## Slide 37, confidence intervals
## Read the following values off of the summary:
b0 <- 38.885
sb0 <- 9.094
b1 <- 35.386
sb1 <- 4.494

#### Confidence intervals example (alpha = .05)
## Calculate everything yourself:
t025 <- qt(.975, df=n-2) # this is t_{n-p,alpha/2}... 
c(b0 - sb0*t025, b0 + sb0*t025) # 95% CI for intercept
b1 + c(-1,1)*sb1*t025 # 95% CI for slope (using c(-1,1) to get + or -)
 # 95% CIs using R's function:
confint(house.reg,level=0.95)


#### AAPL CAPM example ####
## from lide 46
corp <- read.csv("Beta.csv")
len<-dim(corp)[1]
RetSPX<-52*diff(log(corp$SPX))-corp$TBILL[2:(len)]/100      
RetAAPL<-52*diff(log(corp$AAPL))-corp$TBILL[2:(len)]/100  
n<-len-1
## Testing for zero intercept
summary(capm <- lm(RetAAPL ~ RetSPX))
## Read the values off of the summary:
b0 <- 0.2621
sb0 <-0.1552 
b1 <- 1.0750 
sb1 <-0.1105

## R's "t-value" is our z_b from class, with null beta=0 
zb0 <- b0/sb0 # number of standard errors away from the null
## To get p-value, find the area out in the tails of the standard t_{n-p}
2*pt(-abs(zb0),df=n-2) # p-value = 2 * area in left tail
2*(1-pt(abs(zb0),n-2)) # p-valu = 2 * area in right tail
qt(0.975, n-2) ## the boundary

## Now consider testing for beta=1
## slide 48
zb1 <- (b1 - 1)/sb1  # number of standard errors away from the null
2*pt(-abs(zb1), df=n-2) # p-value = 2 * area in left tail


#### Back to the Housing Data ####
## slide 57

## Prediction for places of size Xf = 1.48, 2, or 3
## You must use the same name as the data ('size')
Xf <- data.frame(size=1.48)
n<-15
## This gives you the 95% prediction interval
predict(house.reg, newdata=Xf, interval="prediction", level=.95)
## You can use the same function to get the se.fit
## (i.e. standard error of Y.hat at Xf)
summary(house.reg)
predict(house.reg, newdata=Xf, se.fit=TRUE)
## calculate the same things ourself...
round(s <- summary(house.reg)$sigma, 4) # s
round(sfit <- s*sqrt( 1/n + (Xf$size-mean(size))^2/((n-1)*var(size)) ), 3) # se(Yhat)
## pred interval at Xf=1.48: 
house.reg$coeff[1] + ho0use.reg$coeff[2]*1.48 + c(0,-1, 1)*qt(.975, df=n-2)*sqrt(s^2+sfit^2) 
X_f

