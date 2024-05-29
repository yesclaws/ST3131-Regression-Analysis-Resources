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

# Q1.1 
anova(lm(y~1), lm (y~x1))
# reject H0 (pvalue small)


# Q1.2 
anova(lm(y~x1), lm(y~x1+x2+x3))
# do not reject H0 (pvalue is big )
# No evidence that the horsepower and torque contribute significantly to the mileage aft 
# including displacement in the model


#Q1.3 
plot(x1, y, 
     xlab = "Displacement (cc)",
     ylab = "Mileage (mpg)",
     main = "Mileage vs. Displacement")
# Mileage decreases as displacement increases n the rate of change decreases 
# RS between mileage and displacement is not linear but a curved RS

# Add a trendline 
lm_fit <- lm(y ~ x1)
abline(lm_fit, col = "red")


# Q1.4
anova(lm(y~x1), lm(y~x1 + I(x1^2)+ I(x1^3)))
# reject H0 (pvalue small) 
# Strong evidence that either beta_2 =/ 0 or beta_3 =/0 or both not equal 0 

summary(lm(y~x1 + I(x1^2)+ I(x1^3)))


# Q1.5
plot(x1, y, xlab = "Displacement", ylab = "Mileage", main = "Mileage vs. Displacement")

# Fit a straight line (linear) regression model
model_linear <- lm(y ~ x1)
abline(model_linear, col = "blue")

# Fit a quadratic regression model
model_quadratic <- lm(y ~ x1 + I(x1^2))
xxx = seq(80,500, by=1)
xx = data.frame(x1 = xxx)
fitted_quad = predict(model_quadratic, xx)
par(new=TRUE)
plot(xxx, fitted_quad, x_lab="", y_lab = "", axes= FALSE, type= "l", lwd = 1, col= "red")



# Fit a cubic regression model
model_cubic <- lm(y ~ x1 + I(x1^2) + I(x1^3))

model_quadratic <- lm(y ~ x1 + I(x1^2))
xxx = seq(80,500, by=1)
xx = data.frame(x1 = xxx)
fitted_cubic = predict(model_cubic, xx)
par(new=TRUE)
plot(xxx, fitted_cubic, x_lab="", y_lab = "", axes= FALSE, type= "l", lwd = 1, col= "green")


# Add a legend to the plot
legend("topright", legend = c("Linear", "Quad", "Cubic"), col = c("blue", "red", "green"), lty = 1)

# Q1.6
summary(lm(model))
# Multiple R-squared:  0.7845,	Adjusted R-squared:  0.7396  
summary(lm(model_quadratic))
# Multiple R-squared:  0.8198,	Adjusted R-squared:  0.8064  
summary(lm(model_cubic))
# Multiple R-squared:  0.8435,	Adjusted R-squared:  0.8254 
# Cubic model is the best according to the adjusted R-squared

# Q1.7 
x67 = x6 + x7
anova(lm(y ~ x1 + I(x1^2) + I(x1^3) + x67 ) , lm(y ~ x1 + I(x1^2) + I(x1^3) + x6 + x7))
# do not reject H0 (pvalue is big )
# Insufficient evidence to indicate that beta_6 /= beta_7 
# Insufficient evidence to indicate that the length and width of a car affect mileage in a similar way 


# Q1.8 
model_cubic <- lm(y ~ x1 + I(x1^2) + I(x1^3))
confint(model_cubic, level = 0.95)
# -4.047704e-01,  -9.553927e-02
# 8.455934e-05,   1.264723e-03
# -1.330292e-06,  2.277854e-08

new = data.frame(x1 = 300)
predict.lm(model_cubic, new, interval = "confidence", level = 0.95)
# 16.62748, 19.28796


predict.lm(model_cubic, new, interval = "prediction", level = 0.95)
# 12.41336,  23.50207






