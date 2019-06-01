#Load Libraries
library(car)
library(TSA)

#Set working directory
setwd("~/RMIT/Semester 4/Regression Analysis/Project")

#Read Data
data = read.csv("Life Expectancy Data.csv")
str(data)

#Linear Regression Model
lm.fit_1<-lm(Life.expectancy ~. -(Year + Status), data)
summary(lm.fit_1)

#Very high adjusted r sqaure value
#P value is less than 0.05 and hence the regression is significant

#Using ANOVA to check significance of variables
anova(lm.fit_1)

#Check multicollinearity
vif(lm.fit_1)

#Since all predictors have values less than 5/10 we can say that multicollinearity does not exit.

#Residual check.options
par(mfrow=c(2,2))
plot(lm.fit_1)

#The residuals seem to have a lot of outliers and it might not follow normality

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lm.fit_1)

#Since p value is greater than 0.05 and hence we fail to reject the null hypothesis.
#Thus the errors do have constant variance.

# Test for Autocorrelated Errors
acf(lm.fit_1$residuals)
durbinWatsonTest(lm.fit_1)

#Since p value is smaller than 0.05 we reject the null hypothesis and errors are correlated.
#Also, ACF plot shows a strong correlation in lag 1.

# Test for Normally Distributed Errors
shapiro.test(lm.fit_1$residuals)

#Since p value is smaller than 0.05 we reject the null hypothesis and errors are not normally distributed.