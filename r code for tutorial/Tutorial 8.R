library(MASS)
library(ggplot2)
library(olsrr)
library(DescTools)

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

# Q1.1

plot(x1, y, 
     xlab = "Displacement",
     ylab = "Mileage",
     main = "Mileage vs. Displacement")


fitted_model <- lm(y ~ x1)
abline(fitted_model, col = "red")

fitted = predict(fitted_model)
resid = y-fitted
stu.resid = studres(fitted_model)
plot(fitted,stu.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1,stu.resid, pch=15, col="red")
abline(h=0, col="blue", lty = 6)

MSres = (summary(fitted_model)$sigma)^2  # 9.736062
# Both residual plots show that the residuals are not uniformly distributed about 0 against yˆ and x1.

# Q1.2 

geom.mean = exp(sum(log(y))/length(y) )
geom.mean

msres_values = as.numeric()
lambda = seq(-2,2, by = 0.01)
for (i in 1:length(lambda)) {
  if (abs(lambda[i])<0.00001){
    y.lambda = geom.mean*log(y)
  }else{
    y.lambda = (y^lambda[i]-1)/(lambda[i]*geom.mean^(lambda[i]-1))
  }
  msres_values[i] = (summary(lm(y.lambda~x1))$sigma)^2
  
}

plot(lambda,msres_values, pch=16, col="red")
index = msres_values==min(msres_values)
lambda[index] #-0.25




# Q1.3 
y_lambda = y^-0.25
plot(x1, y_lambda, 
     xlab = "Displacement",
     ylab = "Mileage",
     main = "Mileage vs. Displacement")


fitted_model_lambda <- lm(y_lambda ~ x1)
abline(fitted_model_lambda, col = "red")

fitted_lambda = predict(fitted_model_lambda)
resid_lambda = y_lambda-fitted_lambda
stu.resid_lambda = studres(fitted_model_lambda)
plot(fitted_lambda,stu.resid_lambda, pch=15, col="red")
abline(h=0, col="blue", lty = 6)
plot(x1,stu.resid_lambda, pch=15, col="red")
abline(h=0, col="blue", lty = 6)

MSres_lambda = (summary(fitted_model_lambda)$sigma)^2 
MSres_lambda # 0.0002596275


# Q3 
wine = read.csv("wine.csv")
wine_y = wine$Quality
wine_x1 = wine$Flavor
wine_x2 = wine$Aroma 
wine_x3 = wine$Body
wine_x4 = wine$Oakiness
wine_x5 = wine$Clarity
wine_x6 = wine$Region
fitted_model_4 = lm(wine_y ~ wine_x1 + wine_x2 + wine_x4)
confint(fitted_model_4, level = 1- 0.05/3)




# Q5
fitted_model_5 = lm(y~ x1+x4)
Hii = hatvalues(fitted_model_5, fullHatMatrix = FALSE)
obs = seq(1:length(y))
data.frame(obs,Hii)
# p = 3,  n = 30, 2p/n = 0.2
# observations exceeding 0.2 are 10, 17, 23

print(influence.measures(fitted_model_5))
# None of the Cook’s distances is greater than one, therefore none of the observations is influential
# Cook's distance for obs 10 is 0.000479 < 1 hence not influential 
# Cook's distance for obs 17 is 0.137 < 1 hence not influential 
# Cook's distance for obs 23 is 0.0316 < 1 hence not influential 


