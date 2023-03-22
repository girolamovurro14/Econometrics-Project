## Part 1

# Impot dataset

library(readr)
fish <- read_csv("C:/Users/Utente/Downloads/fish.csv")
View(fish)
summary(fish)
str(fish)
dim(fish)
head(fish)
tail(fish)
colnames(fish)
View(fish)

# Add inspection of the dataset in the latex file.

library(stargazer)
stargazer(fish)


# a)

regr1<-lm(lavgprc ~ mon + tues + wed + thurs + t, data = fish)
summary(regr1)


# Do not reject the null hypothesis that coefficients of the days are equal to 0.
# There is not a statistical significance that price varies within the week
# t is significant with alpha = 0.01 meaning that as the time increases by one unit, 
# the price would fall by  -0.3991177 % (semi-elasticity).

# F-test


library(car)

linearHypothesis(regr1, c('mon=0','tues=0', 'wed=0', 'thurs=0'), test = 'F')

# semi-elasticity (from coefficient of t)

summary(regr1)$coefficients[6]*100

#---------------------------------------------------------------------------------

# b)


regr2<-lm(lavgprc ~ mon + tues + wed + thurs + t + wave2 + wave3, data = fish)

summary(regr2)


# Individually wave2 significant, wave3 significant with alpha = 0.01

# Computing the F-test

anova(regr1,regr2)
linearHypothesis(regr2, c("wave2=0", "wave3=0"), test = "F")

# Jointly significant

# The coefficients of waves2 and waves3 are semi - elasticities. They indicate 
# the percentage increase of the price if the average level of waves increases by 1.
# Intuitively they have a positive impact on the prices because if the waves are high it is harder to 
# fish, therefore the supply of fish will be more scarse because it would be harder to fish with higher waves.



#---------------------------------------------------------------------------------

# c) t is no more statistically significant

# correlation between t and wave2/wave3
cor(fish$t,fish$wave2)
cor(fish$t,fish$wave3)

# Thus, \beta_{6} in the first model contained anomitted variable bias.


#---------------------------------------------------------------------------------

# d)  Preform a Breusch - Godfrey test

install.packages('lmtest')
library(zoo)
library(lmtest)
bgtest(regr2)

# Alternatively we could perform also a Durbin Watson as there is no lagged dependent variable
library(car)

durbinWatsonTest(regr2)

# Reject the null hypothesis that rho equals 0 as the p-value is very small in both tests (9.882e-10 for Breusch - Godfrey)
# There is autocorrelation.


# A4 is violated, however the sufficient conditions for OLS to be consistent are A7 and A6 that still hold
# Sufficient conditions for unbiaseness are A1 A2 that still hold.

# The standard errors are autocorrelated.

# OLS is therefore still unbiased and consistent.

#---------------------------------------------------------------------
# For the Durbin-Watson
# The test statistic is 0.745231, while the p-value is 0 so we can 
# conclude that the residuals in this regression model are 
# autocorrelated.
#---------------------------------------------------------------------


#---------------------------------------------------------------------------------
# e)


install.packages('sandwich')
library(sandwich)

# Robust variance covariance matrix of the coefficients that considers autocorrelation.

# A good procedure is to use H = T^(1/4) or H = 0.75*T^(1/3) lags.


covNW<-NeweyWest(regr2, lag = 4, prewhite = FALSE, adjust = TRUE)

NW_C<-coeftest(regr2, vcov= covNW)
NW_C
# library(lmtest)
coeftest(regr2)

# The t-statistic on wave2 decreases in compartison with the OLS model because the estimated standard error
# for this coefficient is now higher (as it takes into account the autocorrelation); whereas the t-statistic
# for wave3 increases slightly as the the estimated value of its coefficient stays the same in 
# both models but in the second the standard error is now smaller than before.
# We expect that the new t-statistics are more reliable than before because, provided that there is autocorrelation,
# they are obtained from the standard errors that are estimated considering autocorrelation.
# Moreover, also the CI are more likely to include the true value of the respective coefficients with the new st. errors estimates.

# Table for t-statistics and st.  errors comparison
library(stargazer)

# Compare OLS with Newey-West t-statistics

coeftest(regr2)[7:8,2:3]
NW_C[7:8,2:3]
stargazer(NW_C[7:8,2:3], coeftest(regr2)[7:8,2:3])




#---------------------------------------------------------------------------------

# f)

install.packages('prais')
library(prais)

pw1<-prais_winsten(lavgprc ~ mon + tues + wed + thurs + t + wave2 + wave3, data = fish, index = 't')

summary(pw1)
library(stargazer)
stargazer(summary(pw1)$coefficients)
summary(pw1)





# The mangnitude of the autocorrelation in the error term is 0.6874.


coef(pw1)

# Var-cov matrix of coefficients
vcovHC(pw1)

install.packages('aod')
library(aod)

# Prepare for Wald test

L=matrix(c(c(rep(0,6),1,0),c(rep(0,7),1)), nrow = 2, ncol = 8, byrow = TRUE)
# tryal 
L%*%coef(pw1)

# H0 vector

H0 = c(0,0)

library(prais)

wald.test(Sigma=vcovHC(pw1), b=coef(pw1), L=L, H0=H0)

# As the p-value is lower than 0.05 therefore the null hypothesis can be rejected and the coefficients of wave2 and wave3 are jointly significant


# Regressions of point a and b table

library(stargazer)
stargazer(regr1, regr2)

#---------------------------------------------------------------------------------------

# Part 2

# g)

regr3<- lm(ltotqty ~ lavgprc + mon + tues + wed + thurs + t, data = fish)

summary(regr3)

# Logprice's coefficient is an elasticity: in particular it indicates by wich percentage decrease
# the level of quantity, if the prices incrises by 1% (-0.548897%).
# Simultaneity and reverse causality -> A2 would be violated as the correlation between the dependent variable and the errors is not equal to 0
# Therefore the estimated coefficient is not a consistent estimate.
#---------------------------------------------------------------------------------------------


# h)

# In order to be considered an instrument, wave2 must be correlated with X of this model and 
# it only affects Y (ltotqty) throughout its effect on X.
# The relevance condition would be fulfilled as we have seen in the previous part that wave2 
# had a statistically significative impact on the prices; the exclusion restriction would also be satisfied if we 
# assume that the the demand of fish (therefore the total quantity sold) would be uneffected by the high of the waves
# therefore it has no impact on our Y variable. To sum up, the hight of waves impacts the supply
# therefore the price, but does not impact the demand of fish.

#-------------------------------------------------------------------------------------------

# i)

regr4<-lm(lavgprc ~ wave2 + mon + tues + wed + thurs + t, data = fish)

summary(regr4)

# As the t-statistic for wave2 is higher than 3.16 (4.758) it respects the rule of thumb so it is a strong instrument.

#---------------------------------------------------------------------------------------------------------

# j)

regr5<-lm(ltotqty ~ wave2 + mon + tues + wed + thurs + t, data = fish)

# Check the result manually

B2_IV<-coef(regr5)[2]/coef(regr4)[2]
B2_IV

install.packages('ivreg')
library(ivreg)

ivregr<-ivreg(ltotqty ~ lavgprc + mon + tues + wed + thurs + t | wave2 + mon + tues + wed + thurs + t, data = fish)

summary(ivregr)

summary(regr3)



# The IV estimate is higher in absolute value than the OLS estimate.
# According to the new IV estimate, an increase of 1% of the price reduces the total quantity by -0.960347% (elasticity).

# The OLS estimate was not able to isolate the effect (``good variation'') of the price on the quantity 
# sold but indeed it was  underestimating (in absolute values) this effect. The OLS estimates included 
# also the effect that the quantity sold had on the prices which is intuitively positive as when quantity sold increases, 
# also prices should increase and this produced an attenuated effect (smaller coefficient in absolute value) if compared to the one 
# obtained with the IV model, where the sea conditions help us to isolate the good variation in price that we can use to find the 
#true effect of price on quantity.


# The magnitude of the IV standard errors is in general larger than the OLS ones

#------------------------------------------------------------------------------------------------


# k)

ivregr2<-ivreg(ltotqty ~ lavgprc + mon + tues + wed + thurs + t | speed3 + mon + tues + wed + thurs + t, data = fish)

summary(ivregr2)

first_stage<-lm(lavgprc ~ speed3 + mon + tues + wed + thurs + t, data = fish)

summary(first_stage)

# Rule of thumb not respected as the t-value is smaller than 3.16 (2.565)

#---------------------------------------------------------------------------------------------

# l)

ivregr3<-ivreg(ltotqty ~ lavgprc + mon + tues + wed + thurs + t | wave2 + speed3 + mon + tues + wed + thurs + t, data = fish)

summary(ivregr3)

first_stage2<-lm(lavgprc ~ wave2 + speed3 + mon + tues + wed + thurs + t, data = fish)

library(car)

linearHypothesis(first_stage2, c("wave2=0", "speed3=0"), test = 'F')

# Rule of thumb F-stat is > 10. 

# m)

summary(ivregr3, diagnostics = T)



# p???value = 0.161 therefore wecould not reject the null hypothesis under which all the restrictions are valid
# (i.e.  all instruments are indeedexogenous).

library(stargazer)
stargazer(regr3, regr4,  ivregr, ivregr2, ivregr3)

# n)

# see from summary(ivregr3, diagnostics = T) in point m

# t???value= 2.821 and ap???value= 0.097 therefore with apha =  5%  we  cannot  reject
# the  null  hypothesis,  i.e.   we  cannot  reject  the  hypothesis  that log(price)
#is exogenous

# Table to summarize the last three points.

stargazer(summary(ivregr3, diagnostics = T)$diagnostics)


#-----------------------------------------------------------------------------------

# Bonus question


library(sandwich)

# Still utilizing the same rule of thumb used in the
# point e.

# Estimate the robust var-covar matrix that considers autocorrelation.

NWmatr<-NeweyWest(ivregr3, lag = 4, prewhite = FALSE, adjust = TRUE)



# Apply it to reestimate the coefficients


library(lmtest)
coeftest(ivregr3, vcov = NWmatr)
coeffBonus[,1:4]

# Create a table with the coefficients that are the same as expected but with different 
# standard errors that consider the autocorrelation.

stargazer(coeffBonus[,1:4]) 




