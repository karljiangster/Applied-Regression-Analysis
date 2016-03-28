

########################
##### Crime Data #######
########################

## slide 5
crime <- read.csv("CommunityCrime.csv")
crime$logCR <- log(crime$ViolentCR)
## you need to get rid of the "label" column, and ViolentCR
print (XY <- crime[,-(1:2)] )
## create training and validation sets
index <- sample(1:nrow(crime),1000)

training <- XY[index,]
validation <- XY[-index,]

## slide 6
## define the scope from small model to big
null <- lm(logCR ~ 1, data=training)
summary(null)
## build a regression model with the BIC
fwdBIC <- step(null, scope=formula(full), direction="forward", k=log(1000))

## slide 7
## again with the BIC, but searching all variables AND interactions
fwdBICinteract <- step(fwdBIC, scope= ~ . + .^2, direction="forward", k=log(1000))

## slide 8

## then with the AIC (just for curiosity, you don't need to do this one on your assignment)
fwdAIC <- step(null, scope=formula(full), direction="forward")      

### Calculate the probabilities for each
bics <- cbind(null = extractAIC(null, k=log(1000)),
              fwdBIC = extractAIC(fwdBIC, k=log(1000)),
              fwdBICinteract = extractAIC(fwdBICinteract, k=log(1000)),
              fwdAIC  = extractAIC(fwdAIC, k=log(1000)),
              full = extractAIC(full, k=log(1000)))
ebic <- exp(-.5*(bics[2,]-min(bics[2,])))
round(probs <- ebic/sum(ebic),4)

## slide 9
## Plot the fits
par(mfrow=c(1,4))
plot(fwdBIC$fitted, training$logCR, pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("BIC fit: p =",length(fwdBIC$coef)))
lines(fwdBIC$fitted, fwdBIC$fitted, lty=2, col=2)
plot(fwdBICinteract$fitted, training$logCR, pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("BIC2 fit: p =",length(fwdBICinteract$coef)))
lines(fwdBICinteract$fitted, fwdBICinteract$fitted, lty=2, col=2)
plot(full$fitted,training$logCR, pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("Full model: p =",length(full$coef)))
lines(full$fitted, full$fitted, lty=2, col=2)
plot(fwdAIC$fitted, training$logCR, pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("AIC fit: p =",length(fwdAIC$coef)))
lines(fwdAIC$fitted, fwdAIC$fitted, lty=2, col=2)

## slide 10
## Get the MSE's on left out data
errorNull <- predict(null, newdata=validation)-validation$logCR
errorBIC <- predict(fwdBIC, newdata=validation)-validation$logCR
errorBICinteract <- predict(fwdBICinteract, newdata=validation)-validation$logCR
errorAIC <- predict(fwdAIC, newdata=validation)-validation$logCR
errorFull <- predict(full, newdata=validation)-validation$logCR

## comparing the errors
mean(errorNull^2)
mean(errorBIC^2)
mean(errorBICinteract^2)
mean(errorAIC^2)
mean(errorFull^2)



# Cigarettes Example

attach(read.csv('CigarettesSW.csv'))
## Define additional variables

# Real taxes and prices
salestax = taxs - tax
realprice = price / cpi
realtaxs = taxs / cpi
realsalestax = salestax / cpi

# Other
pcincome = income / population
realpcincome = pcincome / cpi
logrealpcincome = log(realpcincome)
logpacks = log(packs)
logrealprice = log(realprice)

#slides 34-35
## TSLS
summary(TSLS1 <- lm(logrealprice[year==1995] ~ realsalestax[year==1995]))
summary(TSLS2<-lm(logpacks[year==1995]~fitted(TSLS1)))

##This requires sem package
#slides 36
require(sem)
summary(tsls(logpacks~logrealprice, ~realsalestax, subset = which(year == 1995)))
#slides 38
summary(tsls(logpacks ~ logrealprice + logrealpcincome, ~realsalestax + logrealpcincome, subset = which(year == 1995)))
#slides 39
summary(tsls(logpacks ~ logrealprice + logrealpcincome, ~realsalestax + realtaxs + logrealpcincome, subset = which(year == 1995)))


## Estimation (not on slides)
# Note: R's ivreg() and tsls() assumes constant variances when calculating SEs.
#       We need to use vcovHC(..., type = 'HC1') to account for that. This may require the AER package (windows).
# 
TSLS <- ivreg(logpacks ~ logrealprice | realsalestax, subset = which(year == 1995))
cbind(Estimate = coef(TSLS),
    'Std. Error' = sqrt(diag(vcovHC(TSLS, type = 'HC1'))),
    't value' = coef(TSLS) / sqrt(diag(vcovHC(TSLS, type = 'HC1'))),
    'Pr(>|t|)' = 2*pt(abs(coef(TSLS) / sqrt(diag(vcovHC(TSLS, type = 'HC1')))), df = 46, lower.tail = F))
    
#  
TSLS <-ivreg(logpacks ~ logrealprice + logrealpcincome | realsalestax + logrealpcincome, subset = which(year == 1995))
TSLS <-ivreg(logpacks ~ logrealprice + logrealpcincome | realsalestax + realtaxs + logrealpcincome, subset = which(year == 1995))