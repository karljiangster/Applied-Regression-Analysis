
################  Wage Data Examples  ##############
#### Census income data.
## slide 3
census <- read.csv("census2000.csv")
workers <- census$income > 1000 # remove folks earning less than $1000
log.wagerate <- log(census$income/census$hours)[workers]
edu <- census$education[workers]
plot(edu, log.wagerate, col=7, xlab="Education Level", ylab="log Hourly Rate")

## slide 5
summary( reg <- lm(log.wagerate ~ edu) )
## slide 11
anova(reg)


#### Supervisor Performance Data
## slide 13
attach(supervisor <- read.csv("supervisor.csv"))
pairs(supervisor, col=4, pch=20)

## slide 14
summary(bosslm <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 ))
## slide 15
summary( bosslm2 <- lm(Y ~ X1 + X2))
## slide 22
anova(bosslm2, bosslm)
## slide 24 -- augmenting the summary from two lines above
anova(bosslm1 <- lm(Y~X1), bosslm2)

## slide 29
par(mfrow=c(1,3))
plot(X2,X3,col=2,pch=20, main=paste("r =", round(cor(X2,X3),1)))
plot(X2,X4,col=2,pch=20, main=paste("r =", round(cor(X2,X4),1)))
plot(X3,X4,col=2,pch=20, main=paste("r =", round(cor(X3,X4),1)))

## slide 30
summary(lm(Y~ X2 + X3 + X4))
## slide 31
summary(lm(Y ~ X2))
summary(lm(Y ~ X3))
summary(lm(Y ~ X4))


## census data:
## only include folks working more than 500 hours AND
## earning more than $5000 AND less than age 60

#### Income Data ####

census <- read.csv("census2000.csv")

## slide 32
## A good habit: build a dataframe with your relevant variables.
YX <- data.frame(log.WR = log(census$income/census$hours))
YX$age <- census$age
YX$age2 <- census$age^2
YX$sex <- census$sex
## Use relevel to make "White" and "Married" the intercept
YX$race <- relevel(census$race, "White") 
YX$marital <- relevel(census$marital, "Married")

## slide 33
## create a bunch of education indicator variables
YX$hs <- census$edu=="3.hsgrad"
YX$assoc <- census$edu=="4.assoc"
YX$coll <- census$edu=="5.bachs"
YX$grad <- as.numeric(census$edu)>6
## only include folks working more than 500 hours AND
## earning more than $5000 AND less than age 60
YX <- YX[(census$hours > 500)&(census$income > 5000)&(census$age < 60), ]

## slide 34
## only main effects
summary(reg1 <- lm(log.WR ~ age*sex + age2*sex + ., data=YX) ) 

## slide 35
## some interactions
summary(reg2 <- lm(log.WR ~ age*sex + age2*sex + marital +
                   (hs+assoc+coll+grad)*age + race*age , data=YX) )
anova(reg1, reg2)  

## slide 36
## even more interactions -- 3-way
summary(reg3 <- lm(log.WR ~ race*age*sex + age2*sex + marital +
                   (hs+assoc+coll+grad)*age, data=YX) ) 
anova(reg2, reg3)

## slide 37
## can we get away without race effects?
summary(reg4 <- lm(log.WR ~ race*age*sex - race + age2*sex +
                   marital + (hs+assoc+coll+grad)*age, data=YX))
anova(reg3, reg4)

## slide 43
n <- nrow(YX)
## AIC
print(AIC <- c(reg1=extractAIC(reg1)[2],
               reg2=extractAIC(reg2)[2],
               reg3=extractAIC(reg3)[2],
               reg4=extractAIC(reg4)[2]))

## BIC
print(BIC <- c(reg1=extractAIC(reg1, k=log(n))[2],
               reg2=extractAIC(reg2, k=log(n))[2],
               reg3=extractAIC(reg3, k=log(n))[2],
               reg4=extractAIC(reg4, k=log(n))[2]))
extractAIC(reg1, k=log(n))
## slide 44
## Model probabilities
print(eBIC <- exp(-.5*(BIC-min(BIC))))
round(probs <- eBIC/sum(eBIC), 2)

## slide 49
## Forward Stepwise Regression
null <- lm(log.WR ~ age*sex + age2*sex, data=YX)
full <- lm(log.WR ~ . + .^2, data=YX)

## slide 50
fwd <- step(null, scope=formula(full), direction="forward", k=log(n))

## 
## Again, Model probabilities
BIC <-  c(BIC, reg5 = extractAIC(fwd, k=log(n))[2])
eBIC <- exp(-.5*(BIC-min(BIC)))
round(probs <- eBIC/sum(eBIC), 2)
