install.packages("scatterplot3d")
install.packages("psychometric")
library(scatterplot3d)
library(psychometric)
library(MASS)

library(tidyverse)
data = read.csv("data-table-B3.csv")

#Q1.1 
data = data %>% mutate(mileage_kmlitre = mileage*0.425 , 
                       displacement_cc = displacement*16.387)
linear_model = lm(mileage ~ displacement, data=data)
summary(linear_model)

yhat = predict(linear_model)
yhat


residual = list()

for (i in 1:32){
  residual = append(residual, (data$mileage[i]-yhat[i]))
}
residual = data.frame(residual)
residual


sum(residual*data$displacement) # 5.323386e-11 (roughly equals 0)
sum(yhat*residual) # 3.249401e-12 (roughly equals 0)

#Q1.2 
par(mfcol=c(1, 2))

plot(data$displacement, data$mileage, main="Scatterplot of y against x", xlab="Displacement", ylab="Mileage") + # Add the regression line to the plot
  abline(linear_model, col="red") 

for (i in 1:32) lines(c(data$displacement[i],data$displacement[i]), c(data$mileage[i],yhat[i]))

plot(data$displacement, residual, main="Residuals Plot", xlab="x", ylab="Residuals")


# There is no evidence 

#Q1.3 
COEF = coef(linear_model)
beta0 = COEF[1] 
beta0
beta1 = COEF[2]
beta1

SST = sum(data$mileage ^2) - ( (sum(data$mileage))^2 ) / length(data$mileage)
SST

sxy = sum(data$mileage*data$displacement) - sum(data$mileage)*sum(data$displacement)/length(data$mileage)
SSR = beta1*sxy
SSR

SSres = SST - SSR
SSres

MSres = SSres/(length(data$mileage)-2)
MSres


#Q1.4 & Q1.5
summary(linear_model)
confint(linear_model,level=0.99)
# ci of beta0 is [29.75195035, 37.69340302]
# ci of beta1 is [0.06027187, -0.03444729]


#Q1.6
n = length(data$mileage)
(n-2)*MSres/qchisq(0.5/100, df=n-2)
(n-2)*MSres/qchisq(99.5/100, df=n-2)
# ci is [5.250868 , 20.44173]

#Q1.7 - help

sum(data$displacement_cc)/32


#Q1.8 
R2 = SSR/SST
R2
# R2 is the proportion of variation explained by the regressor x 


#Q1.9 
# r > 0 indicates a positive association
# r < 0 indicates a negative association. 
# Values of r near 0 indicate a very weak linear relationship. 
# The strength of the linear relationship increases as r moves away from 0 toward -1 or 1.


# The R-squared value, denoted by R 2, is the square of the correlation r.
sxx = 0
x_bar = sum(data$displacement)/n
for (i in 1:length(data$displacement)){
  sxx = sxx + (data$displacement[i] - x_bar)^2
}

r = sxy/ ( (sxx*SST)^0.5) 
r 
# r is -0.8787896 which indicates a negative association 


#Q1.10 
corr = cor(data$displacement, data$mileage)
CIr(corr, n,level=0.99)
# [-0.9516278, -0.7124335]

