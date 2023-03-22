# Girolamo Vurro, SCIPER: 341816
# Filippo Rondoni, SCIPER: 338506

library(readr)
vehiclesales <- read_csv("C:/Users/Utente/Downloads/vehiclesales.csv")
View(vehiclesales)

#---------------------------------------------------------------------------------
# Point a)

# Inspect the dataset
View(vehiclesales)
summary(vehiclesales)
str(vehiclesales)
dim(vehiclesales)
head(vehiclesales)
tail(vehiclesales)
colnames(vehiclesales)
View(vehiclesales)


stargazer(summary(vehiclesales))

# Be careful to omit the NA from the dataset!!!
 
# ggplot automatically ingores the NA

library(ggplot2)

ggplot(data=vehiclesales, aes(x=1:180, y=log(vehiclesales))) +
  geom_line()+
  geom_point()+ xlab("Time periods (# of Quarters from 1976:Q1 to 2018:Q4)") + ylab("log(vehicles sales)")+theme_light()+ggtitle("Plot of the time series of log(vehicles sales)")+
  geom_smooth(method=lm, se=FALSE)




# We can observe that the series does not appear stationary. Indeed in some time intervals 
# the series seems to increase for a while, in some it seems to decrease for a while. Moreover,
# give to the fact that there are downwards peacks, we might think that the the series
# steers away from its mean which supports the idea of non stationarity. Furthermore, we plotted the
# regression line and we noticed its upwards slope, thus, we could hypothesize that there could be a 
# time trend.


#------------------------------------------------------------------------------------------
# Point b)


install.packages('aTSA')
library(aTSA)


ADF = adf.test(log(na.omit(vehiclesales$vehiclesales)), nlag = 13, output = TRUE)

# Need this vector to select 0,2,4,...,12 lags
c=seq(from=1, to=13, by=2)


# Take 'type3' which considers the model with a constant and allows for a time trend

########################################################################################
# Could also do KPSS test to see if they lead to the same conclusion.
# Here the null hypothesis is stationarity.
library(tseries)
kpss.test(log(vehiclesales$vehiclesales))

# As the p-value is lower than 0.05 the null hypothesis of stationarity is rejected.
# It leads to the same conclusion that we did with ADF.
########################################################################################
A_Dickey_Fuller_1=ADF$type3[c,]

library(stargazer)
stargazer(ADF$type3[c,])

# After having computed the Augmented D-F test from 0 to 12 lags, we obtained that the 
# p-values of all the computations except the one obtained for 6 and 12 lags, that are smaller than
# alpha = 0.05, are bigger than 0.05 and therefore we did not reject the null hypothesis of unit root and no time trend for 5/7
# of the computations. For the one with 6 and 12 lags, as the p-value is strictly smaller than 0.05,
# we could reject the null hypothesis, however as in the majority of the computations we did not reject the null hypothesis it
# is not unlikely that the model might have a unit root.

# From this conclusion we could also say that, as expected in the point a),
# there is not a statisical evidence that the time series is stationary.

# From the theory we know that if a time series has a unit root (and we did not reject the hypothesis),
# Then, by taking the first differences we should obtain a stationary series. 

#-------------------------------------------------------------------------------------------

# Point c)

par(mfrow=c(1,2))

acf(log(na.omit(vehiclesales$vehiclesales)), plot = TRUE, main='ACF of log(vehicles sales)')
# We can observe that the first 11 lags are significative


pacf(log(na.omit(vehiclesales$vehiclesales)), lag.max = NULL, plot = TRUE, main='PACF of log(vehicles sales)')
# Only the first two lags are significative 



# After plotting the ACF and PACF we made the following observations:
# 1. The Autocorrelations tail off gradually, indeed only after high number of lags (11)
# they become statistically not significant.
# 2. The partial autocorrelations become statistically not significant after only two lags (which can
# be interpreted as the cutoff, i.e. p = 2)
# After these observations we could conclude that the clues provided by ACF and PACF plots
# suggest that we adopt a AR(2) model which presents autocorrelations and partial autocorrelations plot
# very affine to those that we observe.


#--------------------------------------------------------------------------------------------
# Point d)

# We now estimate the candidate models:
# Start with AR(2) or ARIMA(2,0,0)

AR2=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(2, 0,0), include.mean = TRUE)



# See the estimated coefficients that we obtain
library(lmtest)

coeftest(AR2)

install.packages('forecast')
library(forecast)
par(mfrow=c(1,1))

library(ggplot2)
library(forecast)

autoplot(AR2)+
  ggtitle("Inverse AR(2) roots")+
  theme_light()


# One of the inverse roots appears very close to the cirle unit as it is
# shown in the last figure.

# See that the abs. value of the sum of the theta is close to 1 so we might still support what we noticed in 
# the previous point, i.e. the model may have a unit root as we saw in theory.

# |SARC| < 1 but still high 
sum(coeftest(AR2)[1:2,1])

# We perform the the following tests on the model:


# i) Overfitting
# ARMA(p,0,q+1)

ARMA21=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(2, 0, 1))
library(lmtest)
coeftest(ARMA21)

# We observe that the ma coefficient is not statistically significative.


# ARMA(p+1,0,q)

ARMA30=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(3, 0, 0))
library(lmtest)
coeftest(ARMA30)

# We observe that the ar3 coefficient is not statistically significative.

stargazer(coeftest(AR2), coeftest(ARMA21), coeftest(ARMA30))

# Check which of the models performs better with AIC and BIC

AIC_BIC21=c(AIC(ARMA21),BIC(ARMA21))

AIC_BIC30=c(AIC(ARMA30),BIC(ARMA30))

AIC_BIC2=c(AIC(AR2),BIC(AR2))

AIC_BIC=rbind(AIC_BIC2,AIC_BIC21,AIC_BIC30)
AIC_BIC

# Therefore, the AR(2) model has a better performance also in terms of AIC and BIC (lower values)
# if compared to the values of the overfitted model.

library(stargazer)
stargazer(AIC_BIC)

# ii) Ljung-Box test 
# First we do it with 6 lags

BOX=Box.test(AR2$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 2)

# We do not reject the null hypothesis that the errors are not autocorrelated in all of the three models. However, we need to be cautious 
# With the model AR(2) because its p-value is 0.05263 and it is very close to the threshold of 0.05.

# We re-do the test with 12 lags.

BOX2=Box.test(AR2$residuals, lag = 12, type = c("Ljung-Box"), fitdf = 2)

library(stargazer)
stargazer(unlist(BOX), unlist(BOX2))


# Here again we do not reject the null hypothesis for all of the three models. In particular,
# the statistics relative to the AR(2) model provides a higher p-value now so we can be more
# confident that the model passes the test.


# As only the AR(2) model passed both the tests, we can take it as our preferred model.
# This is also the model that has the best AIC and BIC.


library(stargazer)
stargazer(AR2, ARMA21, ARMA30)


#----------------------------------------------------------------------------------------
# point e)

# Here we use our preferred model to predict the vehicle sales over 2019:Q1-2020:Q4

# We first predict the log(vehicle sales)

install.packages('forecast')
library(forecast)


summary(forecast(AR2, 8))

# These are the predicted log(vehicle sales)
f1=forecast(AR2, 8)

# Predicted levels
exp=exp(f1$mean+1/2*0.003718)

exp

# In order to get the quarterly vehicles sales, we needed to transform the logs into 
# levels and therefore we did this as we saw in the first 
# part of the course using the exp(pred) + 1/2*sigma^2.

library(aTSA)
library(stargazer)
stargazer(f1)


#------------------------------------------------------------------------------------

# poinf f)

# From this function we can also obtain the confidence interval at 95% and we also plotted our
# forecasts in logs, with the respective confidende intervals indicated by the blue shadow in
# the graph.

library(forecast)
forecast(AR2, 8)

library(ggplot2)
# To plot the real data on top of the predicted one we define a vector tv with the true values
# and a vector with its time values that starts at 173 and ends at 180. With then plot it with the 
# forcasts
tv= c(3.95328, 3.964027, 3.969461, 3.953664, 3.834667, 3.550938, 3.860077, 3.911062)
tim_tv = c(173:180)
d1=data.frame(x=tim_tv,y=tv)
autoplot(forecast(AR2, 8)) +ylab('log(vehicle sales)')+ 
  geom_line(data=d1,aes_string(x='x',y='y'), col='red')+ 
  theme_light()


#------------------------------------------------------------------------------------------
##################
# Bonus question #
##################
#-----------------------------------------------------------------------------------------

# As we said in the second point, the fact that we did not reject the null hypothesis the we have a unit root
# led us to the intuition that (as we also saw in the theory) that we could take the first differences
# of the logs of vehicle sales and obtain stationarity (if we consider the assumption that it has one unit root).
# We proceed as follows:


# We create a new dataset that contains the differences and the number of quarters. (Obviously we lose one observation) 

Diff=diff(log(na.omit(vehiclesales$vehiclesales)))
N_Qarters=1:length(Diff)
Differences=data.frame(Diff,N_Qarters)

library(ggplot2)

ggplot(data=Differences, aes(x=N_Qarters, y = Diff)) +
  geom_line()+
  geom_point()+ xlab("Time periods (# of Quarters)") + ylab("log(growth sales)")+theme_light()+ggtitle("Plot of the time series")+
  geom_smooth(method=lm, se=FALSE) 


# Thus we also checked that our assumption of stationarity holds throughout the Augmented D-F test
# but, as from the series we notice that it does not suggest for a time trend,
# we only focus our attention of the output "type1" that does not allow for a time trend, neither for a drift, as our models.

library(aTSA)
adf.test(Differences$Diff, nlag = 13, output = TRUE)
library(tseries)
kpss.test(Differences$Diff)

# As we can see we always reject the null hypothesis here for every lag (the p-values are always strictly
# smaller than 0.05), whereas we do not reject the null hypothesis of stationarity for the kpss.

# In order to decide which would be a possible candidate for our model, we recompute the
# ACF and PACF plots.

par(mfrow=c(1,2))

acf(Differences$Diff, plot = TRUE, main='ACF of log(growth sales)')

pacf(Differences$Diff, plot = TRUE, main='PACF of log(growth sales)')



# From the two graphs we could infer p,q <= 1.
# So three possible candidate coul be ARIMA(1,1,0), ARIMA(0,1,1) ARIMA(1,1,1)
# Therefore we estimate our model

ARIMA110=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(1, 1,0), include.mean = TRUE)
ARIMA011=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(0, 1,1), include.mean = TRUE)
ARIMA111=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(1, 1,1), include.mean = TRUE)

# See where we get significative coefficients
library(lmtest)
coeftest(ARIMA110)
coeftest(ARIMA011)
coeftest(ARIMA111)

# We see that ARIMA(1,1,0) and ARIMA(0,1,1) produce statistically significant estimates and we
# take them as candidate models; 
# ARIMA(1,1,1) (which would be one overfitted model in common for both the previous models) 
# produces less significative estimates.
# We perform the the following diagnostics on the two candidate models: 

# i) Overfitting ARIMA(2,1,0) and ARIMA(0,1,2) 

ARIMA210=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(2, 1,0), include.mean = TRUE)
ARIMA012=arima(log(na.omit(vehiclesales$vehiclesales)), order = c(0, 1,2), include.mean = TRUE)

# Check if the coefficients are significative

coeftest(ARIMA210)
coeftest(ARIMA012)

library(stargazer)

stargazer(ARIMA110,ARIMA011, ARIMA111, ARIMA210, ARIMA012)

# In both the overfitted models the additional parameters are not statistically significant,
# therefore we can proceed with the next test.

# ii) Ljung-Box test for both our candidate models


BOX_BON1=Box.test(ARIMA110$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 1)
BOX_BON2=Box.test(ARIMA011$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 1)

BOX_BON11=Box.test(ARIMA110$residuals, lag = 12, type = c("Ljung-Box"), fitdf = 1)
BOX_BON22=Box.test(ARIMA011$residuals, lag = 12, type = c("Ljung-Box"), fitdf = 1)


stargazer(unlist(BOX_BON1), unlist(BOX_BON2), unlist(BOX_BON11), unlist(BOX_BON22))

# Here you can also see their distribution and the ACF plot
checkresiduals(ARIMA110, lag = 6)
checkresiduals(ARIMA011, lag = 6)

# We do not reject the null hypothesis that the errors are not serially autocorrelated in both the 
# candidate models with the 6 lags. However, the p-value for ARIMA(0,1,1) is closer to the threshold 0.05


checkresiduals(ARIMA110, lag = 12)
checkresiduals(ARIMA011, lag = 12)

# Also with 12 lags we do not reject the null hypothesis for both the models, however we have again that the p-value
# for the ARIMA(0,1,1) is closer to the 0.05 threshold.


# Now we compare the AIC and BIC

AIC_BIC110=c(AIC(ARIMA110),BIC(ARIMA110))


AIC_BIC011=c(AIC(ARIMA011),BIC(ARIMA011))

AIC_BIC=rbind(AIC_BIC110,AIC_BIC011)
AIC_BIC

stargazer(AIC_BIC)

# If we compare the two models based on the information criteria, we see that ARIMA(1,1,0)
# has better AIC and BIC.

# See that the inverse of its characteristic root lies within the unit circle
autoplot(ARIMA110)+
  ggtitle("Inverse ARIMA(1,1,0) roots")+
  theme_light()

# Now we make our prediction with our preferred model ARIMA(1,1,0) and compare it 
# with the true values as we did for point f)

par(mfrow=c(1,1))

install.packages('forecast')
library(forecast)

f2=forecast(ARIMA110,8)

library(ggplot2)
autoplot(f2)+ylab('log(vehicle sales)')+ 
  geom_line(data=d1,aes_string(x='x',y='y'), col='red')+
  theme_light()


# Levels of the vehicle sales

exp2=exp(f2$mean+1/2*0.003812)
exp2


# Only to print our tables
library(aTSA)
library(stargazer)
stargazer(forecast(ARIMA110,8))


