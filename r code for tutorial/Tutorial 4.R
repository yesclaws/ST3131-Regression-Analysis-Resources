getwd()
setwd("/Users/tiffanieleong/Downloads/ST3131")
data = read.csv("data-table-B3.csv")
newdata = na.omit(data)

#Q1.1 
# There are 30 observations 

#Q1.2 
model = lm(mileage~ displacement+horsepower + torqne + compression_ratio + real_axel_ratio, data = newdata)
one = rep(1,length(newdata$mileage))
X = array(c(one,newdata$displacement, newdata$horsepower, newdata$torqne, newdata$compression_ratio, newdata$real_axel_ratio ),
          dim = c(length(newdata$mileage), 6))
X
y = array(newdata$mileage, dim=c(length(newdata$mileage), 1))
y 
XPX = t(X) %*% X
XPX
XPy = t(X) %*% y
XPy
solve(XPX)
betahat = solve(XPX, XPy)
betahat 

SSres = (t(y) %*% y) - (t(betahat) %*% XPy)
SSres

MSres = SSres/(length(y)-6)
anova(model)
MSres 
variance = var(y)[1,1] 
var_betahat_2 = variance * (solve(XPX)[3,3]) 
var_betahat_2

cov_betahat_2 = variance * (solve(XPX) [3,4])
cov_betahat_2


# Q1.3 
lm(model)

# Q1.4 
vcov(model)


# Q1.5 
y = data$mileage
x1 = data$displacement
x2 = data$horsepower
x3 = data$torqne
x4 = data$compression_ratio
x5 = data$real_axel_ratio
anova(lm(y~1), lm(y~x1+x2+x3+x4+x5))




# Q1.7 
summary(lm(model))
# Multiple R-squared:  0.7845,	Adjusted R-squared:  0.7396 
# A higher Adjusted R-squared value is preferred 
# when comparing models with different numbers of predictors,
# as it accounts for model complexity.


# Q1.8
matrix = cbind(y,X[,-1])
cor(matrix)
pairs(matrix, pch = 19, cex = 0.7, col = "darkgreen")

# Q1.9
x2 <- x1
x2[1] <- 351
summary(lm(y~x1+x2+x3+x4+x5))
# The variances are now highh and the regression coefficients are completely different (poorly estimated). 
# When two regressors are highly correlated, the X′X matrix becomes nearly singular 
# and inversion of this matrix will yield (X′X)−1 with large absolute values.




