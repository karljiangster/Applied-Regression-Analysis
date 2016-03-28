#1.1 
#i) 
x <- rnorm(100, -1, sqrt(2.5))
e <- rnorm(100, 0, sqrt(3))
y <- 2.5 + 2 * x + e
par(mfrow = c(1,1))
plot(x, y, xlab = "x", ylab = "y", pch = 20, main = "1.1 Y versus X")
abline(model <- lm(y ~ x)) 

#ii)  
abline(mod25 <- lm(y[1:25] ~ x[1:25]), col = "red") 
abline(mod25 <- lm(y[26:100] ~ x[26:100]), col = "green")
#not the same because the data the regression is based off of is different. 

#iii)
mean(y)
#true: 0.5 (just plug via equation)

#iv) 

bound <- qnorm(c(0.075), 0, sqrt(3))
print(bound)
abline(model$coefficients[1] + bound, model$coefficients[2], lty = 2)
abline(model$coefficients[1] - bound, model$coefficients[2], lty = 2)

#percent that fall in. Makes sense since we did an 85% interval. 
length(model$residuals[model$residuals^2 < bound^2])

#1.2) Leverage Effect. 
#i) 
lev <- read.csv("leverage.csv")
summary(lev)
spxr <- diff(log(lev$SPX))
vixr <- diff(log(lev$VIX))
plot(vixr, spxr, xlab = "VIX Returns", ylab = "SPX Returns", pch = 20, main = "SPX vs VIX Returns")

summary(spx.model <- lm(spxr ~ vixr) )
print(b1 <- cor(spxr, vixr) * sd(spxr) / sd(vixr) )
print(b0 <- mean(spxr) - b1 * mean(vixr))

abline(spx.model)

#iii)
print(lev.aov <- anova(spx.model) )
#SST is 2938350, SSE is 890392, SSR = SST - SSE = 2047958
lev.aov$'Sum Sq'[1] / (lev.aov$'Sum Sq'[1] + lev.aov$'Sum Sq'[2])
(lev.aov$'Sum Sq'[1] + lev.aov$'Sum Sq'[2])
sum(spx.model$residual^2)
cor(vixr, spxr)^2
#variance explained above: 0.3030245

#iv) Not too sure what they are asking here.... 
x <- c(0, 0.1, 0.2, 0.3, 0.4)
y <- c(1, 2, 3, 4, 5)
summary(lm(y ~ x) )

#v) Assuming that the VIX level is in the range of the data set... 
bound <- qnorm(0.05, sd= 0.025)
print( spxlow <- b0 + b1 * 0.1 + bound )
print( spxlow <- b0 + b1 * 0.1 - bound )

#1.3
#i) 
van <- read.csv("vanguard.csv")
summary(van)
print(len<-dim(van)[1])
RetSPX <- diff(log(van$SPX))*52-van$TBILL[2:(len)]/100
Retvan <- 52*log(van[2:len,2:(dim(van)[2]-2)])-52*log(van[1:len-1,2:(dim(van)[2]-2)])-van$TBILL[2:(len)]/100     
summary(CAPM <- lm(as.matrix(Retvan) ~ RetSPX, data = van))
plot(CAPM$coefficients[2,], CAPM$coeff[1,], col = 0, xlab = "beta", ylab = "alpha")
text(x=CAPM$coeff[2,], y=CAPM$coeff[1,], labels=names(van)[2:(dim(van)[2]-2)], col=2)
