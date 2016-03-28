oj <- read.csv("OJ.csv")
summary(oj)

par(mfrow = c(2,2))

plot(log(oj$minuteprice) ,log(oj$minutevol), main = "Minute Sales vs Price")
plot(log(oj$tropicprice), log(oj$minutevol), main = "Minute Sales vs Tropic Price")
plot(log(oj$dmnckprice), log(oj$minutevol), main = "Minute Sale vs Dominick Price")

boxplot(log(oj$minutevol) ~ oj$minutead, main = "Minute Ad effect on Minute Sales",
        names = c("Without", "With"))
boxplot(log(oj$minutevol) ~ oj$tropicad, main = "Tropic Ad effect on Minute Sale",
        names = c("Without", "With"))
boxplot(log(oj$minutevol) ~ oj$dmnckad, main = "Dominick Ad effect on Minute Sale",
        names = c("Without", "With"))

cor(oj[1:4] )

cor(oj$dmnckprice * oj$tropicprice, oj$minuteprice)
cor(log(oj$dmnckprice) * log(oj$tropicprice), log(oj$minuteprice))
cor(oj$tropicprice, oj$minuteprice)

ojnull <- lm(log(oj$minutevol) ~ 1, data = oj)
ojfull <- lm(log(oj$minutevol) ~ ., data = oj)
fwdoj <- step(null, scope = formula(ojfull), direction="forward", k=log(1000)) 
summary(fwdoj)

#ii) 
#simplest 
summary(mdl.one <- lm(log(oj$minutevol) ~ oj$minutead))
summary(mdl.one <- lm(log(oj$minutevol) ~ log(oj$minuteprice)))

#mdl 2
price_effect <- log(oj$tropicprice) * log(oj$minuteprice) * log(oj$dmnckprice)
summary(mdl.two <- lm(log(oj$minutevol) ~ oj$minutead + price_effect))

#mdl 3 
summary(mdl.three <- lm(log(oj$minutevol) ~ oj$minutead + log(oj$minuteprice) + 
                        price_effect))

#mdl 4
ad_effect <- oj$dmnckad * oj$tropicad 

summary(mdl.four <- lm(log(oj$minutevol) ~ oj$minutead + log(oj$minuteprice) + 
                        price_effect + oj$dmnckad + oj$tropicad ))

#iii) Finding the best model with training, validation 
oj$log_sales <- log(oj$minutevol)
oj_new <- oj[,-1]
summary(oj_new)
oj_index <- sample(1:nrow(oj), 1000)
oj_training <- oj_new[oj_index,]
oj_validation <- oj_new[-oj_index,]

null_oj <- lm(oj_training$log_sales ~ 1, data=oj_training)
full_oj <- lm(oj_training$log_sales ~ ., data = oj_training)
oj_fwdBIC <- step(null_oj, scope = formula(full_oj) , direction="forward", k=log(1000))
oj_fwdBICinteract <- step(oj_fwdBIC, scope= ~ . + .^2, direction="forward", k=log(1000))
 

#simple 
summary(mdl.one <- lm(training$log_sales ~ training$minutead, data = training))
        
#mdl 2
price_effect <- log(training$tropicprice) * log(training$minuteprice) * log(training$dmnckprice)
summary(mdl.two <- lm(training$log_sales ~ training$minutead + price_effect, data = training))
        
#mdl 3 
summary(mdl.three <- lm(training$log_sales ~ training$minutead + log(training$minuteprice) + 
                                  price_effect))
#mdl 4
ad_effect <- training$dmnckad * training$tropicad 
summary(mdl.four <- lm(training$log_sales ~ training$minutead + log(training$minuteprice) + 
                                 price_effect + training$dmnckad + training$tropicad ))


errorOne <- predict(oj_fwdBIC, newdata = oj_validation)
errorTwo <- predict(oj_fwdBICinteract, newdata = oj_validation)

mseOne <- mean(errorOne^2)
mseTwo <- mean(errorTwo^2)

mseOne
mseTwo

summary(mdl.one)


#2) 
fert <- read.csv("Fertility.csv")
summary(fert)
#i)
summary(labor.kids <- lm(fert$weeksm1 ~ fert$morekids) ) 
summary(labor.age <- lm(fert$weeksm1 ~ fert$agem1) ) 

#iii) 
summary(labor.same <- lm(fert$morekids ~ fert$samesex) ) 
summary(labor.same <- glm(fert$morekids ~ fert$samesex) )

#v) 
install.packages("sem")
require(sem)
summary(tsls(fert$weeksm1 ~ fert$morekids, ~fert$samesex))

#vi) agem1, black, hispan, and othrace
agem1 <- fert$agem1
black <- fert$black
hispan <- fert$hispan
othrace <- fert$othrace
summary(tsls(fert$weeksm1 ~ fert$morekids, 
             ~fert$samesex + agem1 + black + hispan + othrace))

#3) 
bike <- read.csv("BikeSharing.csv")
summary(bike)
#i) 


