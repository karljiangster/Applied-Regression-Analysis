#Problem 0.1 
ansi = pnorm(c(-10, -20), mean = -10, sd = sqrt(25)) 
ansi #part a and b, note part c is zero since normal dist. 

alternative <- pnorm(c(z_score(c(-10, -20), -10, sqrt(25))))
alternative

z_score <- function(x, mean = 0, sd = 1) {  
  return  ( (x - mean) / sd ) 
}


zscrs = z_score(c(-22, -12), -10, sqrt(25) ) 
zscrs[2] - zscrs[1] #part d) 

#Problem 0.2 
#Some important properties to keep note of: 
# E[x] = sum(x * f(x)) 
# 1) E[c] = c 
# 2) E[c(x + y)] = cE[x] + cE[y]

# var(X) = E[X^2] + (E[X])^2 = E[(X - avg)^2] = sum((x - avg)^2 f(x))
# note that "x" means x sub i 
# sum(x^2 * p(x)) + -2sum(avg * xp(x)) + sum(avg^2 * p(x))
# = E[X] - 2avg * avg + avg (note that sum(p(x) = 1))
# so var(X) = E[(X - avg)^2] = E[X] - avg 
# var(c) = 0; Var(cX) = c^2Var(X) ; var(X + c) = var(X); 

#COVARIANCE 
# cov(X, y) = E[XY] - E(X)E(Y) = E[ (X - E[X])(Y - E[Y]) ]
# 1) cov(x, y) = cov(y, x)
# 2) Var(X) = Cov(X, X) And var(X+ Y) = var[X] + var[Y] + 2cov(X, Y)
# 3) http://www.math.uiuc.edu/~hildebr/461/variance.pdf Useful link 

#1.1 Ciggarette Consumptions 
#b)correlation matrix and plots
cig_data = read.csv("Cigarette.csv")
summary(cig_data)

cig_corr <- data.frame(cor(cig_data[,2:6]))

write.csv(cig_corr, "Cig Correlation Matrix.csv")
par(mfrow = c(2, 2))
plot(cig_data$Age, cig_data$Sales, pch=20, xlab="Age",ylab="Cig Sales", main = "Age vs Sales")
plot(cig_data$HS, cig_data$Sales, pch=20, xlab="HS",ylab="Cig Sales", main = "HS vs Sales")
plot(cig_data$Income, cig_data$Sales, pch=20, xlab="Income",ylab="Cig Sales", main = "Income vs Sales")
plot(cig_data$Price, cig_data$Sales, pch=20, xlab="Price",ylab="Cig Sales", main = "Price vs Sales")

#1.2 AIG 
aig_data <- read.csv("aig.csv")
summary(aig_data)

time <- aig_data$Time
price <- aig_data$Price
minute <- time - 93500

#i) 
var(price)
pricemean <- mean(price)
residualsi <- price - pricemean 
sum(residualsi^2) / 59



b1 <- cor(minute, price) * sd(price)/ sd(time)
b1 #anoher way to find the slope of regre line 

#for ii) 
q2modl <- function(time_s) { 
  return(4 - 0.005 * (time_s))
}
new_p <- q2modl(minute)
mean(price)
new_e <- new_p - price
sum(new_e^2) / 60

lines(minute, new_p)
lines(mean(price))


#iii) 
m.aig <- lm(price ~ minute, data = aig_data)
aov(m.aig)
summary(m.aig)

par(mfrow = c(1, 1))
plot(aig_data$Time - 93500, aig_data$Price, pch = 20,
     xlab = "Seconds after 9:35:00", ylab = "Price", 
     main = "AIG Price from 9:35 - 9:36" )
abline(m.aig)
cor(minute, price)

#iv) 
m.aig$coefficients[1]
aig_reg <- function(time) { 
  return(m.aig$coefficients[1] + time * m.aig$coefficients[2])
}
m.aig$residuals
aig.res <- price - aig_reg(minute)
m.aig$residual - aig.res
mean(aig.res)
mean_resi <- mean(m.aig$residuals)
mean_resi
var_aig <- sum(m.aig$residuals^2)/59
var_aig

#vi) 
price_adj <- diff(aig_data$Price)
alt.p.model <- aig_data$Price
alt.p.model[2:length(alt.p.model)] <- alt.p.model[1:length(alt.p.model) - 1]
aig_data$Price
   alt.p.model
sen_resi <- alt.p.model - aig_data$Price
var_alt <- sum(sen_resi^2) / 59
var_alt


#Problem 1.3 
rent <- read.csv("rent.csv")
summary(rent)

par(mfrow = c(1,3))
boxplot(rent$Rent, ylab = "Rent", col = 8, main = "Rent")
boxplot(rent$Rent ~ rent$AC, xlab = "With or without AC", 
        ylab = "Rent", col = 8, main = "Rent by AC")
boxplot(rent$Rent ~ rent$Rooms, xlab = "Num of Rooms", ylab = "Rent", col = 8, main = "Rent by Num of Rooms")

summary(ac <- lm(rent$Rent ~ rent$AC))
ac.aov <- anova(ac)
ac.aov
ac.aov[1,2] / (ac.aov[2,2] + ac.aov[1,2])

summary(rooms <- lm(rent$Rent ~ factor(rent$Rooms)))
rooms.aov <- anova(rooms)
rooms.aov
rooms.aov[1,2] / (rooms.aov[2,2] + rooms.aov[1,2])

#iii) 
par(mfrow = c(1,1))
summary(sqft <- lm(rent$Rent ~ rent$SqFt))
plot(rent$SqFt, rent$Rent, xlab = "Area in Square Feet", pch = 20, 
     ylab = "Rent price", main = "Area on Rent")
abline(sqft, col = "red")
abline(adj.mdl, col = "blue")

#iv) 
par(mfrow = c(1,2))
hist(sqft$residuals, col=gray(.4), border=gray(.9),xlab="Residuals",
    main="Histogram of Sqft Residuals")
plot(rent$SqFt, sqft$residuals, xlab = "Sqft", ylab = "Residual from regression", 
     main = "Residuals along sqft")

adj.sqft <- rent$SqFt[rent$SqFt < 20]
adj.rent <- rent$Rent[rent$SqFt < 20]
adj.mdl <- lm(adj.rent ~ adj.sqft)
summary(adj.mdl)

plot(rent$SqFt, rent$Rent, xlab = "Area in Square Feet", pch = 20, 
     ylab = "Rent price", main = "Area on Rent")
abline(sqft, col = "red")
abline(adj.mdl, col = "blue")


