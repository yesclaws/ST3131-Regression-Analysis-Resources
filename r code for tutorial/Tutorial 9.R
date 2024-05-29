setwd("/Users/tiffanieleong/Downloads/ST3131")

library(ggplot2)
install.packages("rgl")
library(rgl)
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
install.packages("np")
library(np)

data = read.csv("data-table-B13.csv")
y = data$y
x1 = data$x1
x2 = data$x2
x3 = data$x3
x4 = data$x4
x5 = data$x5
x6 = data$x6

fitted_model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)


#Q1.1
print(influence.measures(fitted_model))
# Leverage points are obs 6, 9, 10, 11, 20 
# Points only influential if Di > 1 
# Influential : Obs 20 
# Not influential : Obs 6, 9, 10, 11 


#Q1.2 
cutoff = 2/sqrt(40)
# cutoff value is 0.316227766
print(influence.measures(fitted_model))
# Leverage points are obs 6, 9, 10, 11, 20 
# Points only influential if DFBETAS > 0.316227766
# Obs 6 : Not influential for all betas 
# Obs 9 : Not influential for all betas
# Obs 10 : Not influential for all betas
# Obs 11 : Influential for all betas except beta2, beta5 
# Obs 20 : Influential for all betas 


#Q1.3 
cuttoff_2 = 2*sqrt((7/40))
# cutoff value is 0.83666
print(influence.measures(fitted_model))
# Leverage points are obs 6, 9, 10, 11, 20 
# Points only influential if DFFITS > 0.83666
# Influential : Obs 11 , 20 
# Not influential : Obs 6, 9, 10 


#Q1.4 
1 + 3*(7/40)
1 - 3*(7/40)
# Cuttoff values are 1.525 and 0.475 
print(influence.measures(fitted_model))
# Leverage points are obs 6, 9, 10, 11, 20 
# Points only influential if COVRATIO > 1.525 or COVRATIO < 0.475 
# Influential : Obs 6, 9, 10
# Not influential : Obs  11, 20 


#Q2.1 & Q2.2 
data2 = read.csv("mercedes_car_prices.csv")
data2 = data2[order(data2$age),]


y = data2$price
x = data2$age
fitted_model_2 = lm(y~x + I(x^2))
plot(x, y, 
     xlab = "Age",
     ylab = "Price",
     main = "Price vs. Age")
xxx = seq(0,10, by=0.01)
xx = data.frame(x = xxx)
fitted_quad = predict(fitted_model_2, xx)
par(new=TRUE)
plot(xxx, fitted_quad, axes= FALSE, type= "l", lwd = 1, col= "red")

# Does not take into account the discontinuity (price diff between old and new)

anova(lm(y~1), lm(y~x + I(x^2)))


#2.3 
lenx <- length (x) 
x2 <- rep(0,lenx)

for (i in 1:lenx) {
  if(x[i]>5) x2[i] <- (x[i]-5)^2
}
data.spline <- data.frame(y, x, x2)
data.spline
# fit model
model.spline <- lm (y~x+I(x^2)+x2)
plot(x, y,pch=16, col="black",xlim=c(0,10), ylim=c (50,400))
# plot model
fitted <- predict (model.spline)
par (new = TRUE)
plot(x, fitted, axes= FALSE, pch=15, lwd=2,
     type="l",col="red",xlim=c(0,10), ylim=c(50,400))
# test hypotheses
anova (lm (y-1), model.spline)



lenx <- length (x) 
x2 <- rep(0,lenx) 
x3 <- rep(0,lenx)
x4 <- rep(0,lenx) 
for (i in 1:lenx) { 
  if(x[i]>5) {
    x2[i] <- 1
    x3[i] <- x[i]-5
    x4 [i] <- (x[i]-5)^2
  }
}

data.spline2 <- data.frame (y, x, x2, x3, x4)
data.spline2
# fit model
model.spline2 <- lm(y~ x+I(x^2)+x2+x3+x4)
plot(x, y,pch=16, col="black",xlim=c(0,10), ylim=c(50,400)) # plot model
fitted <- predict(model.spline2)
xx1 <- x[(x<5)]
xx2 <- x[(x>=5)]
yy1 <- fitted [(x<5)]
yy2 <- fitted [(x>=5)]
par (new = TRUE)
plot(xx1,yy1, axes=FALSE, pch=15, lwd=2,
     type="l",col="red",xlim=c(0,10), ylim=c(50,400))
par (new = TRUE)
plot(xx2,yy2, axes= FALSE, pch=15, lwd=2,
     type="l",col="blue",xlim=c(0,10), ylim=c(50,400))

# The spline model with no continuity restriction is preferred 
# because the model has accounted for the price difference between new and old models of the car.

#2.4
lenx <- length (x) 
x2 <- rep(0,lenx)

for (i in 1:lenx) {
  if(x[i]>5) x2[i] <- (x[i]-5)
}
data.spline <- data.frame(y, x, x2)
data.spline
# fit model
model.spline <- lm (y~x+x2)
plot(x, y,pch=16, col="black",xlim=c(0,10), ylim=c (50,400))
# plot model
fitted <- predict (model.spline)
par (new = TRUE)
plot(x, fitted, axes= FALSE, pch=15, lwd=2,
     type="l",col="red",xlim=c(0,10), ylim=c(50,400))
# test hypotheses
anova (lm (y-1), model.spline)



lenx <- length (x) 
x2 <- rep(0,lenx) 
x3 <- rep(0,lenx)
x4 <- rep(0,lenx) 
for (i in 1:lenx) { 
  if(x[i]>5) {
    x2[i] <- 1
    x3[i] <- x[i]-5
    x4 [i] <- (x[i]-5)
  }
}

data.spline2 <- data.frame (y, x, x2, x3, x4)
data.spline2
# fit model
model.spline2 <- lm(y~ x+x2+x3+x4)
plot(x, y,pch=16, col="black",xlim=c(0,10), ylim=c(50,400)) # plot model
fitted <- predict(model.spline2)
xx1 <- x[(x<5)]
xx2 <- x[(x>=5)]
yy1 <- fitted [(x<5)]
yy2 <- fitted [(x>=5)]
par (new = TRUE)
plot(xx1,yy1, axes=FALSE, pch=15, lwd=2,
     type="l",col="red",xlim=c(0,10), ylim=c(50,400))
par (new = TRUE)
plot(xx2,yy2, axes= FALSE, pch=15, lwd=2,
     type="l",col="blue",xlim=c(0,10), ylim=c(50,400))


#2.5
npmodel = npreg(y~x, ckertype="gaussian", bws=0.5)
summary(npmodel)
plot(npmodel, col="red", lwd=2, xlim = c(0,10), ylim = c(50,400))
points(x,y,pch =16, col="blue")

#2.6 
loess.model= loess(y~x)
yhat = predict(loess.model)
plot(x, yhat, col="red", type = "l", lty=1, lwd=2, xlim = c(0,10), ylim = c(50,400))
points(x,y,pch =16, col="blue")


#2.7
# We know that there is a significant price difference between new and old models of the car,
# therefore any model that does not account for this price difference is not a satisfactory model. 
# Between the quadratic spline and linear spline models with no continuity restriction,
# the quadratic spline model is preferred because the rate of decrease of the price is not a constant.

#Q3.1 
data3 = read.csv("carbonation.csv")
clevel = data3$y
temp = data3$x1
pressure = data3$x2

scatterplot3d(temp, pressure, clevel, angle = 20, xlab = "Temperature", ylab = "Pressure", zlab = "Carbonation Level")


pairs(data3)
#carbonation level increases with temperature and pressure 


