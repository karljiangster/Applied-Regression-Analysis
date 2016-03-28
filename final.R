market <- read.csv("market.csv")
summary(market)

#declare dv vectors 
sbux <- market$SBUX - market$RF
amzn <- market$AMZN - market$RF


#one factor 
s_one <- lm(sbux ~ market$MKTminusRF)
a_one <- lm(amzn ~ market$MKTminusRF)
summary(s_one)
summary(a_one)

#three factor
s_three <- lm(sbux  ~ market$MKTminusRF + market$SMB + 
                market$HML + market$CMA)
a_three <- lm(amzn  ~ market$MKTminusRF + market$SMB + 
                market$HML + market$CMA)
summary(s_three)
summary(a_three)

s_five <- lm(sbux  ~ market$MKTminusRF + market$SMB + 
               market$HML + market$CMA + market$MOM)
a_five <- lm(amzn  ~ market$MKTminusRF + market$SMB + 
               market$HML + market$CMA + market$MOM)
summary(s_five) 
summary(a_five) 

#finding best: 
n = nrow(market)

s_models <- list(s_one = s_one, s_three = s_three, s_five = s_five)
#BIC 
s_bic <- sapply(s_models, extractAIC, k = log(n))[2,]
s_ebic <- exp(-.5 * (s_bic - min(s_bic) ) )
round(s_ebic / sum(s_ebic), 4)

#AIC for fun 
s_aic <- sapply(s_models, extractAIC)[2,]
s_eaic <- exp(-.5 * (s_aic - min(s_aic) ) )
round(s_eaic / sum(s_eaic), 4)

a_models <- list(a_one = a_one, a_three = a_three, a_five = a_five)
a_bic <- sapply(a_models, extractAIC, k = log(n))[2,]
a_ebic <- exp(-.5 * (a_bic - min(a_bic) ) )
round(a_ebic / sum(a_ebic), 4)

#AIC 
a_aic <- sapply(a_models, extractAIC)[2,]
a_eaic <- exp(-.5 * (a_aic - min(a_aic) ) )
round(a_eaic / sum(a_eaic), 4)


