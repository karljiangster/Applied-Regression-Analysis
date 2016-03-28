#1.1 CAPM 
#variables) 
b0 <- -0.008194
b1 <- 1.690067
rf <- 0.0003/52
n <- 103
s <- 0.03734
Xf <- 0.027
X_bar <- 0.0001
sx <- 0.02711201

#iii) 
print(Yf <- b0 + b1 * Xf + rf)

#iiv) 
print( s_pred <- s * sqrt(1 + 1/n + (Xf - X_bar)^2/(n - 1)/ sx^2 ) )
print( interval <- qnorm(c(0.05, 0.95), mean = Yf, sd = s_pred) )

#v) 
t_025 <- qt(0.975, df = n - 2)
sb1 <- 0.136379
b1 + c(-1, 1)*sb1*t_025

#vi) 
zb1 <- (b1 - 1.52) / sb1
print( p_val <- 2 * pt(abs(zb1), df = n - 2, lower.tail = FALSE) ) 

#1.3 Leverage
lev <- read.csv("leverage.csv")
summary(lev)
spxr <- diff(log(lev$SPX))
vixr <- diff(log(lev$VIX))
n <- 503

#i) 
plot(vixr, spxr, xlab = "VIX Returns", ylab = "SPX Returns", pch = 20, main = "SPX vs VIX Returns")

summary( spx.model <- lm(spxr ~ vixr) ) 

print( s <- summary(spx.model)$sigma )
print( sfit <- s*sqrt( 1/n + (0.1-mean(vixr))^2/((n-1)*var(vixr)) ) )
s.pred <-  sqrt(s^2 + sfit^2)
spx.model$coeff[1] + 0.1 * spx.model$coeff[2] + c(-1, 1) * qt(0.95, df = 501) * s.pred
#qnorm
spx.model$coeff[1] + 0.1 * spx.model$coeff[2] + c(-1, 1) * qnorm(0.95, sd = s.pred)

#ii) 
new_data <- data.frame(vixr = seq(-0.2, 0.2, 0.01) )
range <- predict(spx.model, newdata = new_data, interval = "prediction", level = 0.9)
range_test <- predict(spx.model, newdata = data.frame(vixr = 0.1), interval = "prediction", level = 0.9)
range_test
lines(seq(-0.2, 0.2, 0.01), range[,2], col = "red" )
lines(seq(-0.2, 0.2, 0.01), range[,3], col = "blue" )
abline(spx.model)

range[range[,3] > 0.03]
range[1:8, 2]
range
