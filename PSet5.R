#1) Beef 

beef <- read.csv("beef.csv")
summary(beef)

yes = beef$YES
size = beef$SIZE
val = beef$VAL

par(mfrow = c(1, 3))
plot(size, yes, xlab = "size", ylab = "percent yes", main = "Pro Beef based on Size")
plot(val, yes, xlab = "val", ylab = "percent yes", main= "Pro Beef based on value")
plot(size, val, xlab = "size", ylab = "value", main= "Size and Value")

yes.mdl <- lm(yes ~ size + log(val))
summary(yes.mdl)
yes.mdl.fixed <- lm(yes ~ log(val) * size - log(val))
summary(yes.mdl.fixed)

par(mfrow = c(1, 2))
cor(beef)
cor(size, log(val))

plot(yes.slr$residuals, ylab = "residuals", main = "resid plot")
hist(yes.slr$residuals, xlab = "residuals", main = "resid histogram")

#Wine 
wine <- read.csv("winequality.csv.")
summary(wine)

names <- colnames(wine)
par(mfrow = c(2,2))

for(i in 2: 5) { 
  plot(wine[,i], wine_fixed$quality, xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 

for(i in 6: 9) { 
  plot(wine[,i], wine_fixed$quality, xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 

for(i in 10: 11) { 
  plot(wine[,i], wine_fixed$quality, xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 
boxplot(wine_fixed$quality ~ wine_fixed$color, xlab = "Wine Color", 
        ylab = "quality", col = 8, main = "Red vs White")

#fixed wine plots 
wine_fixed = wine
wine_fixed = wine_fixed[ wine_fixed$fixed.acidity < 11.5, ]
wine_fixed = wine_fixed[ wine_fixed$volatile.acidity < 1, ]
wine_fixed = wine_fixed[ wine_fixed$citric.acid < 0.8, ]
wine_fixed = wine_fixed[ wine_fixed$residual.sugar < 22,]
wine_fixed = wine_fixed[ wine_fixed$chlorides < 0.13, ] 
wine_fixed = wine_fixed[ wine_fixed$free.sulfur.dioxide < 80, ]  
wine_fixed = wine_fixed[ wine_fixed$total.sulfur.dioxide < 270, ]
wine_fixed = wine_fixed[ wine_fixed$density < 1, ] 
wine_fixed = wine_fixed[ wine_fixed$pH < 3.8, ] 
wine_fixed = wine_fixed[ wine_fixed$pH > 2.8, ] 
wine_fixed = wine_fixed[ wine_fixed$sulphates < 1, ]

#plots now fixed
par(mfrow = c(2,2))

for(i in 2: 5) { 
  plot(wine_fixed[,i], log(wine_fixed$quality), xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 
 

for(i in 6: 9) { 
  plot(wine_fixed[,i], log(wine_fixed$quality), xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 

for(i in 10: 11) { 
  plot(wine_fixed[,i], log(wine_fixed$quality), xlab = names[i], ylab = "Wine Quality", 
       main = paste("Wine Quality and ", names[i])) 
} 
boxplot(wine_fixed$quality ~ log(wine_fixed$color), xlab = "Wine Color", 
        ylab = "quality", col = 8, main = "Red vs White")


par(mfrow = c(2,2))

plot(wine_fixed[,2], wine_fixed$quality, xlab = names[2], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[2])) 
plot(log(wine_fixed[,3]), wine_fixed$quality, xlab = paste("log. ", names[3]), ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[3])) 
plot(wine_fixed[,4], wine_fixed$quality, xlab = names[4], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[4])) 
plot(log(wine_fixed[,5]), wine_fixed$quality, xlab = paste("log.", names[5]), ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[5])) 
plot(log(wine_fixed[,6]), wine_fixed$quality, xlab = paste("log. ", names[6]), ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[6])) 
plot(wine_fixed[,7], wine_fixed$quality, xlab = names[7], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[7])) 
plot(wine_fixed[,8], wine_fixed$quality, xlab = names[8], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[8])) 
plot(wine_fixed[,9], wine_fixed$quality, xlab = names[9], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[9])) 
plot(wine_fixed[,10], wine_fixed$quality, xlab = names[10], ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[10])) 
plot(log(wine_fixed[,11]), wine_fixed$quality, xlab = paste("log ", names[11]), ylab = "Wine Quality", 
     main = paste("Wine Quality and ", names[11])) 
boxplot(wine_fixed$quality ~ wine_fixed$color, xlab = "Wine Color", 
        ylab = "quality", col = 8, main = "Red vs White")

citric2 <- wine_fixed[,4]^2

wine.mdl <- lm(wine_fixed$quality ~ wine_fixed[,2] + log(wine_fixed[,3]) + wine_fixed[,4] + citric2 +
      log(wine_fixed[,5]) + log(wine_fixed[,6]) + wine_fixed[,7] + wine_fixed[,8] + 
        wine_fixed[,9] + wine_fixed[,10] + log(wine_fixed[,11]) + wine_fixed$color)

summary(wine.mdl)

wine_n = data.frame(wine_fixed$quality, wine_fixed[,2], log(wine_fixed[,3]), wine_fixed[,4], citric2 +
                      log(wine_fixed[,5]), log(wine_fixed[,6]), wine_fixed[,7], wine_fixed[,8], 
                      wine_fixed[,9], wine_fixed[,10], log(wine_fixed[,11]), wine_fixed$color)
cor(wine_n[1:11]) > 0.5
#5 and 9, 7 and 8, 6 and 9. 
names[5]
names[6]
names[9]
names[7]
names[8]

wine.mdl.2 <- lm(wine_fixed$quality ~ wine_fixed[,2] + log(wine_fixed[,3]) + wine_fixed[,4] + citric2 +
                 wine_fixed[,10] + log(wine_fixed[,11]) + wine_fixed$color)
summary(wine.mdl.2)

wine.mdl.cit <- lm(wine_fixed$quality ~ wine_fixed[,2] + log(wine_fixed[,3]) +  citric2 + 
                   wine_fixed[,10] + log(wine_fixed[,11]) + wine_fixed$color)


fwdwine <- step(wine.mdl.2, scope= ~ . + .^2, direction="forward", k=log(1000))
summary(fwdwine)
fwdcit <- step(wine.mdl.cit)

fwdwine$coefficients

wine.mdl.4 <- lm( wine_fixed[, 2] + log(wine_fixed[, 3]) + wine_fixed[, 4] + citric2 + wine_fixed[, 10] +
  log(wine_fixed[, 11]) + wine_fixed$color + wine_fixed[, 4] * wine_fixed$color + 
  wine_fixed[, 10] * log(wine_fixed[, 11]))

wine.mdl.4 <- lm(wine_fixed$quality ~ wine_fixed[, 2] + log(wine_fixed[, 3]) + 
                   citric2 + wine_fixed[, 4] + )

#now for the residual plots 
hist(rstudent(wine.mdl), main = "Studentized residuals Original" )
qqnorm(rstudent(wine.mdl), col = 4)


hist(rstudent(wine.mdl.2), main = "Studentized residuals Simplified" )
qqnorm(rstudent(wine.mdl.2), col = 4)

hist(rstudent(fwdwine), main = "Student Forward" )
qqnorm(rstudent(fwdwine), col = 4)

hist(rstudent(fwdcit), main = "Student Forward Simple" )
qqnorm(rstudent(fwdcit), col = 4)

#model compare
summary(wine.mdl)
summary(wine.mdl.2)
summary(fwdwine)
summary(fwdcit)


