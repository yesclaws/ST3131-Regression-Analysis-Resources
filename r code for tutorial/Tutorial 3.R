# Q4

library(tidyverse)

data = read.csv("data-table-B3.csv")

linear_model = lm(horsepower ~ displacement, data=data)
summary(linear_model)
# There is a strong linear relationship between horsepower and displacement. 
# As displacement increases, horsepower increases.

linear_model_2 = lm(horsepower ~ 0 + displacement, data = data)
# Purpose for fitting thru the origin is that no engine capacity == cannot generate horsepower???
# greater misfit -> not as good a model compared to the original one 

plot(data$displacement, data$horsepower, main="Scatterplot of y against x", 
     xlab="Displacement", ylab="Horsepower", ylim=c(0,250), xlim = c(0,600)) + # Add the regression line to the plot
  abline(linear_model, col='blue', lwd=2) +
  abline(linear_model_2, lty='dashed', col='red', lwd=2)


summary(linear_model)
confint(linear_model,level=0.95)
# ci of beta0 is [19.144242, 47.8680566]
# ci of beta1 is [0.315939,  0.4093451]

summary(linear_model_2)
confint(linear_model_2,level=0.95)
# ci of beta1 is [0.4408183, 0.4864571]

# A large confidence interval suggests that the sample does not provide a precise representation of the population mean, 
# whereas a narrow confidence interval demonstrates a greater degree of precision.


# Q5.1 
pairs(data[,1:3], pch = 19)
corr = cor(data$displacement, data$horsepower)
corr # 0.945208

# Q5.2 
lm5.2 = lm(mileage ~ displacement, data = data)
summary(lm5.2)
anova(lm5.2)
COEF = coef(lm5.2)
beta1_5.2 = COEF[2]
beta1_5.2 # -0.04735958 
# The p-value is very small, indicating a very strong evidence that β1 is different from zero

# Q5.3 
lm5.3 = lm(mileage ~ horsepower, data = data)
summary(lm5.3)
anova(lm5.3)
COEF = coef(lm5.3)
beta1_5.3 = COEF[2]
beta1_5.3 # -0.1133394
# The p-value is very small, indicating a very strong evidence that β2 is different from zero.

# Q5.4 
lm5.4 = lm(mileage ~ displacement + horsepower, data = data)
anova(lm(mileage~1, data= data), lm5.4)
# The p-value is very small, we reject H0 : β1 = β2 = 0. The overall model is significant.

# Q5.5 
summary(lm5.4)
# β1 is significantly different from zero but β2 is not. 
# The reason is that x1 and x2 are highly correlated 
# This results in β2 not significantly different from zero. 
# This phenomenon is called multicollinearity

