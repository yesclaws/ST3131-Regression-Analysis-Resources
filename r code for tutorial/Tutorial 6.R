library(MASS)
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

cardata = data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9)
fitted_model = lm(y~ x1 + x2 +  x3 +  x4 + x5 + x6 + x7 + x8 + x9, data=cardata)
allout_car = ols_step_all_possible(fitted_model)
plot(allout_car)
write.csv(allout_car, file= "/Users/tiffanieleong/Downloads/ST3131/allout_car.csv")

# Adjusted R square greatly decreases when the number of regressor variables increase beyond 3 
# Note that x1, x2, x3 and x8 are highly correlated and affect mileage in similar manner 
# Best model either (10) containing x1 and x4 or 
# (46) containing x5, x6 and x8 
# Based on these 2 models, (10) seems to be the best becoz of the highest adjusted R squared value and 
# the two regressor variables seem more meaningful 



qn2_data = read.csv("wine.csv")
y = qn2_data$Quality
x1 = qn2_data$Flavor
x2 = qn2_data$Aroma
x3 = qn2_data$Body
x4 = qn2_data$Oakiness
x5 = qn2_data$Clarity
winedata = data.frame(y, x1, x2, x3, x4, x5)
fitted_model2 = lm(y~ x1 + x2 +  x3 +  x4 + x5, data=winedata)
allout_wine = ols_step_all_possible(fitted_model2)
write.csv(allout_wine, file= "/Users/tiffanieleong/Downloads/ST3131/allout_wine.csv")


