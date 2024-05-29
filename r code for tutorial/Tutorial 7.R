setwd("/Users/tiffanieleong/Downloads/ST3131")

library(MASS)
library(ggplot2)
library(olsrr)

data = read.csv("data-table-B3.csv")
newdata = na.omit(data)
y = newdata$mileage
x1 = newdata$displacement
x2 = newdata$horsepower
x3 = newdata$torqne
x4 = newdata$compression_ratio
x5 = newdata$real_axel_ratio
x6 = newdata$length
x7 = newdata$width
x8 = newdata$weight
x9 = newdata$transmission 

fitted_model = lm(y~ x1+x4)

#Q1.1 
fitted = predict(fitted_model)
resid = y-fitted

plot(fitted,resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1,resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x4,resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)


#Q1.2 
MSres = (summary(fitted_model)$sigma)^2
std.resid = resid/sqrt(MSres)

plot(fitted,std.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1,std.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x4,std.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)


#Q1.3 
stu.resid = studres(fitted_model)
plot(fitted,stu.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1,stu.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x4,stu.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)


#Q1.4
Hii = hatvalues(fitted_model, fullHatMatrix = FALSE)
press.resid = resid/(1-Hii)
plot(fitted, press.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1, press.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x4, press.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)


#Q1.5
S2 = ((length(y)-3)*MSres - resid^2/(1-Hii)) / (length(y) -4)
rstu_resid = resid/sqrt(S2 *(1-Hii))
plot(fitted, rstu_resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1, rstu_resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x4, rstu_resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)


#Q1.6 
# There is some evidence that σ2 is not constant.
# No outliers can be found.


#Q1.7 
qqnorm(resid,pch=16,col="red", ylab = "Residuals", xlab= "Normal Percentiles", main = "Normal QQ Plot")
qqline(resid)
# The plot is approximately linear. 
# There is little evidence that the residuals are not normally distributed.

#Q1.8
ols_test_normality(fitted_model)
# p value is 0.5016 which is large, 
# Therefore do not reject H0 and conclude there is little evidence that the residuals are not normally distributed.



#Q2.1 
fitted_model_2 = lm(y~x5)
plot(x5, y, main = "Scatter Plot of y vs. x5", xlab = "x5", ylab = "y")
# Add the least-squares regression line
abline(fitted_model_2, col = "red")
# The fit is not satisfactory, a curve could provide a better fit.


#Q2.2 
fitted2 = predict(fitted_model_2)
resid2 = y-fitted2

plot(fitted2,resid2, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x5,resid2, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
# The residuals are not uniformly distributed. The residuals appear to be U-shaped.


#Q2.3
ols_pure_error_anova(fitted_model_2)
# The p-value is 0.0004 which is very small, so we reject H0 
# There is evidence of lack of fit 



#Q3.1 
data3 = read.csv("vapor.csv")
fitted_model_3 = lm(data3$vapor~data3$temp)
plot(data3$temp, data3$vapor, main = "Scatter Plot of vapor vs. temp", xlab = "temp", ylab = "vapor")
# Add the least-squares regression line
abline(fitted_model_3, col = "red")


fitted3 = predict(fitted_model_3)
resid3 = data3$vapor-fitted3

plot(fitted3,resid3, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(data3$temp,resid3, pch=15, col="red")
abline(h=0, col="blue", lty = 6)

#Q3.2 
# Use log transformation 
logvapor = log(data3$vapor)
logtemp = log(data3$temp)
plot(logtemp, logvapor, main = "Scatter Plot of vapor vs. temp", xlab = "temp", ylab = "vapor")
abline(lm(logvapor~logtemp), col = "red")
# The fit has greatly improved but the residual plots are still not uniformly distributed.

#Q4.1 
data4 = read.csv("chemical_yield.csv")
y_qn4 = data4$yield
x1_qn4 = data4$time
fitted_model_4 = lm(y_qn4~x1_qn4)
plot(x1_qn4, y_qn4 , main = "Scatter Plot of yield vs. time", xlab = "time", ylab = "yield")
# Add the least-squares regression line
abline(fitted_model_4, col = "red")

fitted4 = predict(fitted_model_4)
resid4 = y_qn4-fitted4

plot(fitted4,resid4, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1_qn4,resid4, pch=15, col="red")
abline(h=0, col="blue", lty = 6)

# A straight line model is reasonable.\
# The residual plots show that the variance of residuals increases with fitted response and also time.

#Q4.2 
# use 1/yield 
inverseyield = 1/y_qn4
plot(x1_qn4, inverseyield , main = "Scatter Plot of yield vs. time", xlab = "time", ylab = "yield")
abline(lm(inverseyield~x1_qn4), col = "red")

# The fit is good and the residuals are now much more uniform



#Q6
standard_normal = runif(100, min=0, max = 1 )
nrandom = qnorm(standard_normal)
qqnorm(nrandom,pch=16,col="red", ylab = "y", xlab= "Normal Percentiles", main = "Normal QQ Plot")
qqline(nrandom)
