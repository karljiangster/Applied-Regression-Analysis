#### Housing data: just price (in $100,000) vs size (in 1000 sq.ft.)
## From Slide 6
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)
plot(size, price, pch=20)
print( n <- length(size) )

#### Simple regression; compare R's calculations to our own
## From Slide 17
reg <- lm(price ~ size) 
b1 <- cor(price,size)*sd(price)/sd(size)
b0 <- mean(price) - mean(size)*b1
cbind(b0,b1)

#### Plots of fitted values and residuals
## From Slide 23
plot(size, reg$fitted, pch=20, xlab="X", ylab="Fitted Values")
text(x=3, y=80, paste("corr(y.hat, x) =", cor(size, reg$fitted)), col=2, cex=1.5)
#
plot(size, reg$fitted-price, pch=20, xlab="X", ylab="Residuals")
text(x=3.2, y=25, paste("corr(e, x) =",
              round(cor(size, reg$fitted-price),2)), col=2, cex=1.5)
text(x=3.2, y=17,
     paste("mean(e) =", round(mean(reg$fitted-price),0)), col=4, cex=1.5)
abline(h=0, col=8, lty=2)

#### "Crazy line" illustration
## From Slide 25
plot(size, price, pch=20, xlab="X", ylab="Y")
abline(a=b0, b=b1, col=4)
abline(a=10, b=50, col=2)
text(x=3, y=80, paste("LS line:", round(b0,1), "+", round(b1,1), "X"),
     col=4, cex=1.25)
text(x=1.5, y=140, "Crazy line: 10 + 50 X", col=2, cex=1.25)
#
crazyresid <- price - (10 + 50*size)
#
plot(size, crazyresid, pch=20, xlab="X", ylab="Crazy Residuals")
text(x=3, y=20, paste("corr(e, x) =",
            round(cor(size, crazyresid),1)), col=2, cex=1.5)
text(x=3, y=13, paste("mean(e) =",
            round(mean(crazyresid),1)), col=4, cex=1.5)
lines(size, lm(crazyresid ~ size)$fitted.values, col=2)
abline(h=0, col=8, lty=2)

#### ANOVA for Regression and R^2
## From Slide 35
anova(reg)
var(reg$fitted)*(n-1)
var(reg$resid)*(n-1)
#
summary(reg)
var(reg$fitted)/var(price)

#### CAPM Example #####
## From Slide 51

corp <- read.csv("Beta.csv")
summary(corp)
print(len<-dim(corp)[1])
RetSPX<-diff(log(corp$SPX))*52-corp$TBILL[2:(len)]/100       ## or log(corp$SPX[2:len])-log(corp$SPX[1:len-1])
RetAAPL<-diff(log(corp$AAPL))*52-corp$TBILL[2:(len)]/100        
plot(RetSPX,RetAAPL,xlab="SPX Returns",ylab="AAPL Returns")
reg<-lm(RetAAPL~RetSPX)
xx <- seq(range(RetAAPL)[1],range(RetAAPL)[2],length=4)
lines(xx, reg$coeff[1] + reg$coeff[2]*xx, col=2)

Retcorp<-52*log(corp[2:len,2:(dim(corp)[2]-2)])-52*log(corp[1:len-1,2:(dim(corp)[2]-2)])-corp$TBILL[2:(len)]/100     

## Run the regression for each of the 5 corps
print(CAPM <- lm(as.matrix(Retcorp) ~ RetSPX))
CAPM
plot(CAPM$coeff[2,], CAPM$coeff[1,],
     ylab="alpha", xlab="beta", col = 0)
text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(corp)[2:(dim(corp)[2]-2)], col=2)
CAPM$coeff[1]
