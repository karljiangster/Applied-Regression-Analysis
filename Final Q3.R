smsa <- read.csv("smsa.csv")
summary(smsa)

smsa$Mortality
par(mfrow = c(2,2))
#Group 1
plot(smsa$JanT, smsa$Mortality, xlab = "Jan Temperature",
        ylab= "Mortality Rate", main = "Mortality and Jan Temp")
plot(smsa$JulyT, smsa$Mortality, xlab = "July Temperature",
     ylab= "Mortality Rate", main = "Mortality and July Temp")
plot(smsa$RelHum, smsa$Mortality, xlab = "Relative Humidity",
     ylab= "Mortality Rate", main = "Mortality and Humidity") #looks like no fit at all 
plot(smsa$Rain, smsa$Mortality, xlab = "rainfall (inches)",
      ylab= "Mortality Rate", main = "Mortality and Rainfall") 
#Group 2
plot(smsa$Edu, smsa$Mortality, xlab = "Median education", 
     ylab= "Mortality Rate", main = "Mortality and Education") 
plot(smsa$PopD, smsa$Mortality, xlab = "Population Density",
     ylab= "Mortality Rate", main = "Mortality and Population Density") #use log, looks like log(x)
plot(smsa$NonWht, smsa$Mortality, xlab = "Percentage of non whites",
     ylab= "Mortality Rate", main = "Mortality and Non Whites") 
plot(smsa$WC, smsa$Mortality, xlab = "Percentage of white collar workers",
     ylab= "Mortality Rate", main = "Mortality and White Collar") #use log, -log(x)
#Group 3
plot(smsa$Pop, smsa$Mortality, xlab = "Population",
     ylab= "Mortality Rate", main = "Mortality and Population") #def use log here, clustering 
plot(smsa$Income, smsa$Mortality, xlab = "Median Income",
     ylab= "Mortality Rate", main = "Mortality and Income") #use log, looks like negative log(x) 
plot(smsa$HCPot, smsa$Mortality, xlab = "Pollution Potential",
     ylab= "Mortality Rate", main = "Mortality and Pollution Potential") #use log - data too clustered 
plot(smsa$NOxPot, smsa$Mortality, xlab = "Nitrous Oxide Pollution Potential",
     ylab= "Mortality Rate", main = "Mortality and NO Potential") #use log, data too clustered = low spread 
#Group 4
par(mfrow = c(1,1))
plot(smsa$S02Pot, smsa$Mortality, xlab = "Sulfur Dioxide pollution potential",
     ylab= "Mortality Rate", main = "Mortality and SD Potential") #use log, low spread

#ii) 
mort_mdl <- lm(smsa$Mortality ~ smsa$JanT + smsa$JulyT + smsa$RelHum + smsa$Rain + smsa$Edu
   + log(smsa$PopD) + smsa$NonWht + log(smsa$WC) + log(smsa$Pop) + log(smsa$Income) 
   + log(smsa$HCPot) + log(smsa$NOxPot) + log(smsa$S02Pot) )
summary(mort_mdl)

#iii) The variable selection 
cor(smsa[,2:ncol(smsa)]) > 0.5
mort_mdl_simple <- lm(smsa$Mortality ~ smsa$JanT + smsa$Rain + smsa$NonWht 
                       + log(smsa$NOxPot) )
summary(mort_mdl_simple)
anova(mort_mdl, mort_mdl_simple)

#Compare with ii) 
smsa_morts <- cbind(mort_mdl = extractAIC(mort_mdl, k = log( nrow(smsa) )),
                   mdl_simple = extractAIC(mort_mdl_simple, k = log( nrow(smsa) )))
smsa_morts_ebic <- exp(-.5* (smsa_morts[2,] - min(smsa_morts[2,])))
round(probs <- smsa_morts_ebic/sum(smsa_morts_ebic),4)


#iv) 
null_smsa <- lm(smsa$Mortality ~ 1, data = smsa)
fwd_smsa <- step(null_smsa, scope = formula(mort_mdl), direction = "forward", 
                 k = log( nrow(smsa) ))
summary(fwd_smsa)

### Calculate the probabilities for BIC step and the one from iii)
smsa_bics <- cbind(fwd_smsa = extractAIC(fwd_smsa, k = log( nrow(smsa) )),
                   mdl_simple = extractAIC(mort_mdl_simple, k = log( nrow(smsa) )))
smsa_ebic <- exp(-.5* (smsa_bics[2,] - min(smsa_bics[2,])))
round(probs <- smsa_ebic/sum(smsa_ebic),4)

#v) Adding interactions from BIC Regre
fwd_interact_smsa <- step(fwd_smsa, scope = ~. + .^2, direction = "forward", 
                          k = log( nrow(smsa) ))
summary(fwd_interact_smsa)
smsa_bics <- cbind(mort_mdl = extractAIC(mort_mdl, k = log( nrow(smsa) )), 
                   fwd_smsa = extractAIC(fwd_smsa, k = log( nrow(smsa) )),
                   mdl_simple = extractAIC(mort_mdl_simple, k = log( nrow(smsa) )),
                   fwd_interact = extractAIC(fwd_interact_smsa, k = log( nrow(smsa) ) ) )
smsa_ebic <- exp(-.5* (smsa_bics[2,] - min(smsa_bics[2,])))
round(probs <- smsa_ebic/sum(smsa_ebic),4)

#vi) Out of sample prediction
v_mse_ii <- c()
v_mse_iii <- c()
v_mse_bic <- c() 
v_mse_bic_2 <- c()

for(i in 1:1000) {
sample_index <- sample(1:nrow(smsa), nrow(smsa) * 0.75)
length(sample_index)
samples <- smsa[sample_index,]
valid <- smsa[-sample_index,]

ii <- lm(Mortality ~ JanT + JulyT + RelHum + Rain + Edu
         + log(PopD) + NonWht + log(WC) + log(Pop) + log(Income) 
         + log(HCPot) + log(NOxPot) + log(S02Pot),
         data = samples)
iii <- lm(Mortality ~ JanT + Rain + NonWht 
          + log(NOxPot), data = samples )
null_sample <- lm(Mortality ~ 1, data = samples)
bic_sample <- lm(Mortality ~ NonWht + Edu + log(S02Pot), data = samples)
bic_sample_interact <- lm(Mortality ~ NonWht + 
                            Edu + log(S02Pot) * NonWht, data = samples)

error_ii <- predict(ii, newdata = valid) - valid$Mortality
error_iii <- predict(iii, newdata = valid) - valid$Mortality
error_bic <- predict(bic_sample, newdata = valid) - valid$Mortality
error_bic_interact <- predict(bic_sample_interact, newdata = valid) - valid$Mortality

#MSE's 
v_mse_ii <- c(v_mse_ii, mean(error_ii^2) ) 
v_mse_iii <- c(v_mse_iii, mean(error_iii^2) ) 
v_mse_bic <- c(v_mse_bic, mean(error_bic^2) ) 
v_mse_bic_2 <- c(v_mse_bic_2, mean(error_bic_interact^2) ) 
}
mean(v_mse_ii)
mean(v_mse_iii)
mean(v_mse_bic)
mean(v_mse_bic_2)
