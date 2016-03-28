
#1a) these all three lines give the same ans  
pnorm(2, mean = -4, sd = 4, lower.tail = FALSE)
1 - pnorm(2, mean = -4, sd = 4)
pnorm(6/4, lower.tail = FALSE)

#b) 
pnorm(2) - pnorm(1)

#c) 
pnorm(1, lower.tail = FALSE) + pnorm(-1)
1- pnorm(1) + pnorm(-1/2)

#d) 
pnorm(1) + pnorm(7/4, lower.tail = FALSE)

#2) 
a = pnorm(100, sd = 24/5, mean = 414/5)
b = pnorm(85, sd = 24/5, mean = 414/5)
d = pnorm(90, sd = 24/5, mean = 414/5)
a - b 
a - d
pnorm(43/12) - pnorm(11/24)

#3)
7/10 * 8^3/5^5

options <- read.csv("options.csv")
names(options)
strk <- options$Strike
price <- options$Price

plot(strk, price, xlab = "Strike", ylab = "Price", main = "Strike and Price with SLR")
slr.mdl <- lm(price ~ strk)
summary(slr.mdl)
abline(slr.mdl)

log.mdl <- lm(log(price) ~ strk)
summary(log.mdl)

plot(strk, price, xlab = "Strike", ylab = "Price", main = "Exponential Regression")

strikevals <- seq(0, 2000, 1)
exponentials <- exp(predict(log.mdl, list(strk=strikevals)))
lines(strikevals, exponentials, col = "red")
