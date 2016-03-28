seat <- read.csv("seatbelt.csv")
summary(seat)
seat$risk <- seat$risk / seat$kms)
par(mfrow = c(2, 2))

plot(seat$risk , xlab = "months" , ylab = "risk", main = "driver risk v time" )
boxplot(seat$risk ~ seat$law, names = c("no law", "law"))
plot(seat$petrol, seat$risk, main = "risk vs petrol")
acf(seat$risk)
boxplot(seat$risk ~ seat$month)

seat$t <- seq(1:nrow(seat))

par(mfrow = c(1,1))
seat_two <- lm(seat$risk ~ law + petrol + t, data = seat)
summary(seat_two)
acf(seat_two$residual)

#Above was playing with data, now let's do the time series 
seat$q2 <- (m == 4 | m == 5 | m == 6)
seat$q3 <- (m == 7 | m == 8 | m == 9)
seat$q4 <- (m == 10 | m == 11 | m == 12)

seat_three <- lm(seat$risk ~ seat$law + seat$petrol 
              + seat$q2 + seat$q3 + seat$q4 + t, data = seat)
summary(seat_three)
acf(seat_three$residuals, main = "Model 2 ACF")

seat_year_lag <- seat[seat$t > 12, ]
seat_year <- seat[seat$t <= 180,]

seat_four <- lm( seat_year$risk ~ seat_year$law + seat_year$petrol 
                 + seat_year$q2 + seat_year$q3 + seat_year$q4 + seat_year$t
                  + seat_year_lag$risk )
summary(seat_four)
acf(seat_four$residuals, main = "Model 3 ACF")

seat_year$feb <- seat_year$month == 2
seat_year$april <- seat_year$month == 4 

seat_five <- lm( seat_year$risk ~ seat_year$law + seat_year$petrol 
                  + seat_year$april + seat_year$q2 + seat_year$q3 
                   + seat_year$q4 + t
                   + seat_year_lag$risk, data = seat_year)
summary(seat_five)
acf(seat_five$residuals) #This model is useless, don't include it... 

seat_year$sin <- sin(seat_year$t * 2 * pi / 12)
seat_year$cos <- cos(seat_year$t * 2 * pi / 12)

seat_six <- lm( seat_year$risk ~ seat_year$law + seat_year$petrol 
                  + seat_year_lag$risk + seat_year$sin
                + seat_year$cos + seat_year$t + seat_year$q4
                + seat_year$q2 + seat_year$q3) 
              
summary(seat_six)
acf(seat_six$residuals, mai = "Model Four Residuals")

boxplot(seat$risk ~ seat$month)

seat_year$jun <- seat_year$month == 6
seat_month_lag <- seat[2:(nrow(seat) - 11),]

seat_seven <- lm( risk ~ law + petrol 
                  + seat_year_lag$risk + cos 
                  + q2 + q3 + q4 + t + seat_month_lag$risk + sin
                  , data = seat_year)
acf(seat_seven$resid, main = "Model 5 ACF")
summary(seat_seven)


seat_eight <- lm( seat_year$risk 
                  ~  seat_year$law + seat_year$petrol 
                  + seat_year_lag$risk + seat_year$cos 
                  + seat_year$q4 + seat_year$t
                  + seat_month_lag$risk )
acf(seat_eight$resid) #This model is also useless, dont use it on final. 

boxplot(seat$risk ~ seat$month, xlab = "month", ylab = "driver risk", 
        main = "driver risk over year")

jan <- seat_year$month == 1
nov <- seat_year$month == 11
dec <- seat_year$month == 12
feb <- seat_year$month == 2
mar <- seat_year$month == 3
oct <- seat_year$month == 10
seat_year$winter <- jan | nov | dec |feb

seat_nine <- lm( risk 
                  ~  law + petrol / kms - petrol
                  + seat_year_lag$risk + cos 
                  + winter + t 
                  + seat_month_lag$risk, data = seat_year)
acf(seat_nine$resid, main = "Model 6 ACF")
summary(seat_nine)

seat_ten <- lm( risk 
                   ~  law +
                   + seat_year_lag$risk + cos 
                   + winter + t 
                   + seat_month_lag$risk, data = seat_year)
acf(seat_ten$resid, main = "Model 7 ACF")
summary(seat_ten)

#compare models using BIC 
seat_rows <- nrow(seat_year)
seat_bics <- cbind(mdl_one = extractAIC(seat_two, k = log(seat_rows) ), 
                   mdl_two = extractAIC(seat_three, k = log(seat_rows) ),
                   mdl_three = extractAIC(seat_four, k = log(seat_rows) ),
                   mdl_four = extractAIC(seat_six, k = log(seat_rows) ),
                   mdl_five = extractAIC(seat_seven, k = log(seat_rows) ),
                   mdl_six = extractAIC(seat_nine, k = log(seat_rows) ) ) 
  
seat_ebic <- exp(-.5* (seat_bics[2,] - min(seat_bics[2,])))
round(probs <- seat_ebic/sum(seat_ebic),4)

#Try AIC too 
seat_aics <- cbind(mdl_one = extractAIC(seat_two  ), 
                   mdl_two = extractAIC(seat_three  ),
                   mdl_three = extractAIC(seat_four  ),
                   mdl_four = extractAIC(seat_six  ),
                   mdl_five = extractAIC(seat_seven  ),
                   mdl_six = extractAIC(seat_nine  ) ) 

seat_eaic <- exp(-.5* (seat_aics[2,] - min(seat_aics[2,])))
round(probs <- seat_eaic/sum(seat_eaic),4)
