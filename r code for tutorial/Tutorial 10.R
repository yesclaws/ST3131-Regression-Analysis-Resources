library(ggplot2)

data = read.csv("mercedes_car_prices.csv")
data = data[order(data$age),]
data$Car_Type <- ifelse(data$age <= 5, 0, 1)


y = data$price
x = data$age
x2 = data$Car_Type


#Q1.1 
fitted_model_2 = lm(y~x + I(x^2))

plot(x[x2 == 0], y[x2 == 0], 
     xlab = "Age",
     ylab = "Price",
     main = "Price vs. Age",
     col = "blue",
     xlim = c(0,10),
     ylim = c(0,400),
     pch = 19)

# Create a scatter plot for old cars
points(x[x2 == 1], y[x2 == 1], 
       col = "red",
       pch = 19)

xxx = seq(0,10, by=0.01)
xx = data.frame(x = xxx)
fitted_quad = predict(fitted_model_2, xx)
par(new=TRUE)
plot(xxx, fitted_quad, axes= FALSE, type= "l", lwd = 1, col= "black")
# The fit is good 
# but the model has not taken into consideration the price difference between new and old models of the car.


#Q1.2 
fitted_model2 = lm(y ~ x + I(x^2) + x2)

plot(x[x2 == 0], y[x2 == 0], 
     xlab = "Age",
     ylab = "Price",
     main = "Price vs. Age",
     col = "blue",
     xlim = c(0,10),
     ylim = c(0,400),
     pch = 19)

# Create a scatter plot for old cars
points(x[x2 == 1], y[x2 == 1], 
       col = "red",
       pch = 19)
summary(fitted_model2) #y = 338.318 - 29.053*x - 24.224(x2)

xxx <- seq(0, 5, by = 0.01)
lines(xxx, 338.318 - 29.053 * xxx, col = "blue") 
xxx <- seq(5, 10, by = 0.01)
lines(xxx, 338.318 - 29.053 * xxx -24.224, col = "red")



#Q1.3 
anova(lm(y~x + I(x^2) ), fitted_model2)
# The p-value is 0.02978 which is small, hence reject H0 and conclude that x2 is significant.


#Q1.4
fitted_model3 = lm(y ~ x + x2 + x*x2)
plot(x[x2 == 0], y[x2 == 0], 
     xlab = "Age",
     ylab = "Price",
     main = "Price vs. Age",
     col = "blue",
     xlim = c(0,10),
     ylim = c(0,400),
     pch = 19)

# Create a scatter plot for old cars
points(x[x2 == 1], y[x2 == 1], 
       col = "red",
       pch = 19)
summary(fitted_model3) # y = 353.977 -34.580 -109.644(x2) + 15.229x*x2

xxx <- seq(0, 5, by = 0.01)
lines(xxx, 353.977 -34.580 * xxx, col = "blue") 
xxx <- seq(5, 10, by = 0.01)
lines(xxx, 353.977 -34.580 * xxx -109.644 + 15.229*xxx, col = "red")


#Q1.5 
anova(lm(y ~ x + x2 ), fitted_model3)
# Interaction is significant


#2.1 
data2 = read.csv("data-table-B3.csv")
newdata = na.omit(data2)
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
x10 = newdata$carburetor
x11 = newdata$n_speeds

plot(x1, y)

plot(x1[x5 < 2.6], y[x5  < 2.6], 
     col = "blue",
     xlim = c(0,600),
     ylim = c(0,50),
     pch = 19)

points(x1[2.6 <= x5 & x5 < 3], y[2.6 <= x5  & x5 < 3], 
       col = "red",
       pch = 19)

points(x1[3 <= x5 & x5 < 3.5], y[3 <= x5  & x5 < 3.5], 
       col = "green",
       pch = 19)

points(x1[x5 >= 3.5], y[x5 >= 3.5], 
       col = "brown",
       pch = 19)

#2.2 
fitted_model_dat2 = lm(y~x1 + I(x1^2))
xxx = seq(0,700, by=0.01)
xx = data.frame(x1 = xxx)
fitted_quad = predict(fitted_model_dat2, xx)
par(new=TRUE)
plot(xxx, fitted_quad, axes= FALSE, type= "l", lwd = 1, col= "black")



#Q3 
popcorn_data = read.csv("popcorn.csv")
ggplot(popcorn_data, aes(x = Oil, y = Unpopped)) +
  geom_boxplot() +
  labs(x = "Oil Type", y = "Number of Unpopped Kernels") +
  ggtitle("Comparison of Unpopped Kernels by Oil Type")
