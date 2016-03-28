## slide 6
weather <- read.csv("weather.csv")
plot(weather$temp, xlab="day", ylab="temp", type="l", col=2, lwd=2)

## slide 7
DJIdata<-read.csv("DJI_week.csv")
DJI<-ts(DJIdata[,7],start=c(1929,1),freq=52)
plot(DJI)

## slide 8  
beer <- read.csv("beer.csv")
plot(beer$prod, xlab="month", ylab="beer", type="l", col=4, lwd=2)

## slide 9
plot(rnorm(200), xlab="t", ylab="Y_t", type="l", col=6, lwd=2)

## slide 10
plot(weather$temp[1:58], weather$temp[2:59], pch=20, col=4,
     main="Daily Temp at O'Hare", xlab="temp(t-1)",
     ylab = "temp(t)")
text(x=5,y=45, col=2, cex=1.5,
     labels=paste("Corr =", round(cor(weather$temp[1:58],
       weather$temp[2:59]),2)))

## slide 11
par(mfrow=c(1,2))
plot(weather$temp[1:57], weather$temp[3:59], pch=20, col=4,
     main="", xlab="temp(t-2)", ylab = "temp(t)")
text(x=5,y=45, col=2, 
     labels=paste("Lag 2 Corr =", round(cor(weather$temp[1:57],
       weather$temp[3:59]),2)))
plot(weather$temp[1:56], weather$temp[4:59], pch=20, col=4,
     main="", xlab="temp(t-3)", ylab = "temp(t)")
text(x=5,y=45, col=2, 
     labels=paste("Lag 3 Corr =", round(cor(weather$temp[1:56],
       weather$temp[4:59]),2)))
       
## slide 12
print(acf(weather$temp))


## slide 14
print(acf(DJIdata[,7],10))

## slide 15
acf(beer$prod, lag.max=30)

## slide 16
acf(rnorm(40), lag.max=40)

## slide 19
DJIret<-log(DJI[2:length(DJI)])-log(DJI[1:(length(DJI)-1)])
plot(ts(DJIret,start=c(1929,2),freq=52),type="l",col="red",ylab="DJI Returns")
acf(DJIret)

## slide 16
summary( tempreg <- lm( weather$temp[2:59] ~ weather$temp[1:58] ) )

## slide 17
par(mfrow=c(1,1))
acf(tempreg$residuals)

#slides 21

beerret<-beer$pro[13:72]-beer$pro[1:60]
par(mfrow=c(2,1))
plot(beerret,type="l")
acf(beerret)


## slide 24
summary( tempreg <- lm( weather$temp[2:59] ~  weather$temp[1:58] ) )
acf(tempreg$resid)

## not in slides
## shows how 1-year lag would work; 
summary( beerreg12 <- lm( beer$prod[13:72] ~  beer$prod[1:60] ) )
acf(beerreg12$residuals, lag.max=30)
summary( beerreg <- lm( beer$prod[2:72] ~  beer$prod[1:71] ) )
acf(beerreg$residuals, lag.max=30)


## slide 28
random.walk <- rnorm(1)
for(i in 2:200){
  random.walk <- c(random.walk, random.walk[i-1]+rnorm(1))
}
plot(random.walk, pch=20, col=2)
lines(random.walk, col=4)

## slide 29
acf(random.walk, lwd=2)


## slide 31
summary(ARdj <- lm(DJI[2:length(DJI)] ~ DJI[1:(length(DJI)-1)]))

## slide 32
summary( lm(DJIret[2:(length(DJI)-1)] ~ DJIret[1:(length(DJI)-2)]) )

## slide 33
exploding.series <- rnorm(1)
for(i in 2:200){
  exploding.series <- c(exploding.series, 1.02*exploding.series[i-1]+rnorm(1))
}
plot(exploding.series, pch=20, col=2)
lines(exploding.series, col=4)

## slide 34
stationary.series <- rnorm(1)
for(i in 2:200){
  stationary.series <- c(stationary.series, 0.8*stationary.series[i-1]+rnorm(1))
}
plot(stationary.series, pch=20, col=2)
lines(stationary.series, col=4)
abline(h=0, lty=2, col=8)

## slide 35
acf(stationary.series, lwd=2)

## slide 37
negcor.series <- rnorm(1)
for(i in 2:100){
  negcor.series <- c(negcor.series, -0.8*negcor.series[i-1]+rnorm(1))
}
plot(negcor.series, pch=20, col=2)
lines(negcor.series, col=4)
abline(h=0, lty=2, col=8)

## airline passengers example below

###### Airline Passenger Numbers ######

## slide 39
airline <- read.csv("airline.csv")
plot(airline$Passengers, xlab="year", ylab="monthly passengers",
     type="l", col=3, lwd=2, xaxt="n")
axis(1, at=(0:12)*12, labels=1949:1961)

## slide 40
acf(airline$Passengers, lag.max=100)

## slide 41
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers",
     type="l", col=4, lwd=2, xaxt="n")
axis(1, at=(0:12)*12, labels=1949:1961)

## slide 42
t <- 2:nrow(airline)
YX <- data.frame(logY=log(airline$Passengers[2:144]),
                 logYpast=log(airline$Passengers[1:143]),
                 t=t, sin12 =  sin(2*pi*t/12), cos12 = cos(2*pi*t/12))
summary(airlm <- lm(logY ~ logYpast + t + sin12 + cos12, data=YX))

## slide 43
## plot predictions
plot(log(airline$Passengers), xlab="year",
     ylab="log monthly passengers", type="l", col=4, lty=2,
     xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
lines(t, airlm$fitted, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

## slide 44
## look at the residuals
par(mfrow=c(1,2))
plot(airlm$resid, xlab="year", ylab="residual", type="l",
     col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
acf(airlm$resid, lwd=2)

## slide 45
## We find a month effect!
par(mfrow=c(1,1))
boxplot(airlm$resid ~ airline$Month[t], xlab="month",
        ylab="residuals", col=7)

## slide 46
## note that the dummies are indexed by 't' so that things line up properly
## i.e., mar|jun|jul|aug|dec|
YX$holidays <- airline$Month[t] %in% c(3,6,7,8,12) 
YX$jan <- airline$Month[t]==1
YX$nov <- airline$Month[t]==11
YX$jul <- airline$Month[t]==7

## slide 47
summary(airlm2 <- lm(logY ~ t + logYpast + sin12 + cos12
                     + holidays + nov + jan + jul, data=YX))

## slide 48
## plot predictions
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers",
     type="l", col=4, lty=2, xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
lines(t, airlm2$fitted, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

## slide 49
## look at the residuals
par(mfrow=c(1,2))
plot(airlm2$resid, xlab="year", ylab="residual", type="l", col=4,
     main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
acf(airlm2$resid, lwd=2)

## not in slides
par(mfrow=c(1,1))
boxplot(airlm2$resid ~ airline$Month[t], xlab="month",
        ylab="residuals", col=7)
        
# slides 54-57
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=500)   
pacf(sim.ar, main="PACF of AR(2)")     
summary(lm(sim.ar[3:500]~sim.ar[2:499]+sim.ar[1:498]))

fit<-arima(sim.ar,order=c(2,0,0))
pred.ar<-predict(fit,n.ahead=8)
plot.ts(sim.ar)
lines(pred.ar$pred,col="red")
lines(pred.ar$pred+2*pred.ar$se,col="red",lty=3)
lines(pred.ar$pred-2*pred.ar$se,col="red",lty=3)


## slides 60-63

DJIdata<-read.csv("DJI_Week.csv")
DJI<-DJIdata[3705:4328,7] # Cutting the series from Jan 2000
u<-diff(log(DJI))
mean(u^4)/(mean(u^2))^2
mean(u^3)/(mean(u^2))^(1.5)
par(mfrow=c(1,2))
acf(u)
acf(u^2)


par(mfrow=c(1,1))
xgrid<-seq(-8,4,0.5)
hist(u/sd(u),freq=FALSE,150)
lines(xgrid,dnorm(xgrid),col="red")


par(mfrow=c(1,2))
pacf(u)
pacf(u^2)

## slides 65
summary(lm(u[4:623]^2~u[3:622]^2+u[2:621]^2+u[1:620]^2))



# slides 66-68
macrodata<-read.csv("macro.csv")
usinfl1<-100*diff(log(macrodata[1:69,3])) # US inflation before 
# 1982
usinfl2<-100*diff(log(macrodata[69:161,3]))
jpgdp1<-log(macrodata[1:68,8])
jpgdp2<-log(macrodata[69:160,8])
summary(lm(usinfl1~jpgdp1))
summary(lm(usinfl2~jpgdp2))

usinfl<-100*diff(log(macrodata[1:161,3]))
jpgdp<-(log(macrodata[1:160,8]))


plot(ts(usinfl,start=c(1965,1),freq=4),ylab="Inflation Rate",col=4)
plot(ts(jpgdp,start=c(1965,1),freq=4),ylab="Log GDP",col=4)

# slides 69
x.ar<-arima.sim(list(ar=c(0.99)),n=200) 
y.ar<-arima.sim(list(ar=c(0.99)),n=200)     
summary(lm(y.ar~x.ar))