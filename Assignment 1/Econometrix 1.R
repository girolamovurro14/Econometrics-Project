# Import the dataset


# Set working directory
library(readr)
sevs <- read_csv("sevs.csv")
View(sevs)
#------------------------------------------------------------
# Point a


summary(sevs)
str(sevs)
dim(sevs)
head(sevs)
tail(sevs)
colnames(sevs)
View(sevs)

na_values=NULL
for (i in 1:length(sevs[1,])){
  na_values[i] <- c(sum(is.na(sevs[,i])))
}

na_values_in_sevs<-data.frame(c(colnames(sevs)),c(na_values))

print(na_values_in_sevs)

# We notice that the datataset containes many missing values.
# These were all located into the wages column (535).

# Therefore we remove the observations with at least one missing
# value from the sample.

# Inspect the dataset

CLEANEDDATASET<-na.omit(sevs)
summary(CLEANEDDATASET)
str(CLEANEDDATASET)
dim(CLEANEDDATASET)
head(CLEANEDDATASET)
tail(CLEANEDDATASET)
colnames(CLEANEDDATASET)
View(CLEANEDDATASET)




#-----------------------------------------------------------
#Point b

# Import library 

library(ggplot2)

# Histogram

SEX<-CLEANEDDATASET$sex
WPH<-CLEANEDDATASET$wph
for (i in 1:length(SEX)) {
  if(SEX[i]==1){
    SEX[i]<-"Women"
  }else{
    SEX[i]<-'Men'
  }
}

Dataset_wages<-data.frame(SEX,WPH)

#Create the tables 1 and 2
DataWoman<-Dataset_wages[SEX=="Women",]
DataMen<-Dataset_wages[SEX=="Men",]
stargazer(DataWoman, DataMen)
summary(Dataset_wages)

ggplot(Dataset_wages, aes(x = WPH, col = SEX, fill= SEX)) +                       
  geom_histogram( position = "identity", alpha = 0.2, bins = 50)+
  theme_classic()

# Bokplots
ggplot(Dataset_wages, aes(y = WPH, col = SEX)) +                       
  geom_boxplot()+
  theme_classic()


#-----------------------------------------------------------
#Point c



install.packages("stargazer")

library(stargazer)

regression1<-lm(log(wph) ~ edu + exp + I(exp^2), data = CLEANEDDATASET)
summary(regression1)




#-----------------------------------------------------------
# Point d
summary(regression1)

# As the p-value is much less than 0.05, we reject the null hypothesis that Beta_3 = 0. Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.

coefficients1<-regression11$coefficients
coefficients1
#----------------------------------------------------------
# Point e 

# Calculate the optimal level of Experience

OptEXP<-(-coefficients1[3]/(2*coefficients1[4]))
#----------------------------------------------------------
# Point f

new_data <- data.frame(edu=17, exp=1)
lnWph_MFE <- predict(regression1, new_data)
MFE_WPH<- exp(lnWph_MFE+0.5*sd(regression1$residuals)^2)
MFE_WPH

#------------------------------------------------------------
# Point g

## i) collinearity
install.packages('car')
library(car)
vif(regression1)
# We can observe that the variables exp and exp^2 have a high 
# VIF value as expected as they are highly correlated.




regressTrial<-lm(log(wph) ~ edu + exp, data = CLEANEDDATASET)
#Now VIF lower because WE HAVE NOT CORRELATED INDEP. VARIABLES 

vif(regressTrial)

# Check that the R_k are high and therefore also the VIF relative
# to those variables will be high as the denominator of its formula
#is very small.


regressionTrial2<-lm(exp^2 ~ edu + exp, data = CLEANEDDATASET)

summary(regressTrial)
summary(regressionTrial2)

# Note that the R_4^2 is very high and this will make the denominator of the formula 
# utilized to calculate the VIF very small. Therefore we expect Beta_4 to 
# Have a high value of the VIF.



# Third Trial
regressionTrial3<-lm(exp ~ edu + I(exp^2), data = CLEANEDDATASET)

summary(regressionTrial3)

# Same high R_3^2 will have the same implication as the one above.
# So both the high VIF values for Beta_3 and Beta_4 are explained.




## ii) outliers/influential observations
dfbetas(regression1)
DEFBEAS<-dfbetas(regression1)

# Check that none of the single observation is an outlier


# Fix a critical value that depends also on the number of 
# observations.

critical_value<-1

# We could also fix the critical value at 2/sqrt(N) if we want to consider the dimension of the sample


for(j in 1:ncol(DEFBEAS)){
for (i in 1:nrow(DEFBEAS)) {
  if(abs(DEFBEAS[i,j])>critical_value){
    print(c(i,j)) # prints the position of potential outliers in the matrix
  }
}
}
# with critical value = 1 we do not spot outliers


# Visualize the positions of the potential outliers for each variable

par(mfrow=c(1,2))
hist(CLEANEDDATASET$hrs, main="Histogram for working hours per week", 
     xlab="working hours per week", 
     border="blue", 
     col="green",
     breaks=40)




hist(CLEANEDDATASET$edu, main="Histogram for years of schooling", 
     xlab="education in years of schooling", 
     border="blue", 
     col="green",
     breaks=40)

avPlots(regression1)


## iii) RESET test
install.packages('lmtest')
library(lmtest)
resettest(regression1)


# Pvalue is smaller than 0.05 thus we can reject the null hypothesis


#------------------------------------------------------------

# Point h

regressionAge<-lm(log(wph) ~ edu + exp + I(exp^2) + age, data = CLEANEDDATASET)
summary(regressionAge)

# Given that EXP is a linear combination of AGE and EDU, it turns that 
# Age does not add additional information and therefore the matrix of independent
# variables has not rank = K. EXP contains the information of AGE. COLLINEARITY.
# R omits the added variable AGE and makes the regression as the previous.
## SLIDE 10

#------------------------------------------------------------------

# Point i)


regressionSex<-lm(log(wph) ~ edu + exp + I(exp^2) + sex, data = CLEANEDDATASET)

summary(regressionSex)

stargazer(regressionSex,title="Age & Sex regression")
# Being a woman reduces the salary by approximately 28% CHECK

#-------------------------------------------------------------
# Point j
## The test is called Chow test

regressionFinal<-lm(log(wph) ~ edu + exp + I(exp^2) + sex + I(edu*sex) + I(exp*sex) + I(exp^2*sex), data = CLEANEDDATASET)
summary(regressionFinal)

library(stargazer)
stargazer(regression1,regressionSex, regressionFinal,title="Regressions")

# We use the standard F-test for the so called the Chow test

anova(regression1,regressionFinal)

# We reject the null hypothesis 

#------------------------------------------------------------
# Bonus question

regressionFinal<-lm(log(wph) ~ edu + exp + I(exp^2) + sex + I(edu*sex) + I(exp*sex) + I(exp^2*sex), data = CLEANEDDATASET)
summary(regressionFinal)
AIC(regressionFinal)
BIC(regressionFinal)


# We can remove the iteration between edu and sex as it is not statistically sigificant

# We add the variable hrs in the regression:

regression2<-lm(log(wph) ~ edu + exp + I(exp^2) + sex + I(exp*sex) + I(exp^2*sex) + hrs , data = CLEANEDDATASET)

summary(regression2)




AIC(regression2)<AIC(regression1)
BIC(regression2)<BIC(regression1)


# Adj. R squared, AIC and BIC better than the previous model

vif(regression2)


# Adding the variables hrs and hi:

regression3<-lm(log(wph) ~ edu + exp + I(exp^2) + sex  + I(exp*sex) + I(exp^2*sex) + hrs + hi, data = CLEANEDDATASET)
summary(regression3)
summary(regression2)
AIC(regression3)<AIC(regression2)
BIC(regression3)<BIC(regression2)
# Better than the previous one 

# Adding the iteration of kt and sex, as well as hrs, hi and kt:
regression4<-lm(log(wph) ~ edu + exp + I(exp^2) + sex  + I(exp*sex) + I(exp^2*sex) + hrs + hi + kt + I(kt*sex) , data = CLEANEDDATASET)
summary(regression4)
summary(regression3)
AIC(regression4)<AIC(regression2)
BIC(regression4)<BIC(regression2)
# The Adj. R squared here is better than the regression2 and also
# the AIC as well as BIC.
stargazer(regression4, c(AIC(regression4), BIC(regression4)))

