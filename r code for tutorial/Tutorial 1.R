
install.packages("tidyverse")
library(tidyverse)


data = read.csv("data-table-B3.csv")

#Q1.1 
# 12 variables in the data set 

#Q1.2 
# Mileage is the response variable while displacement is the regressor variable 
ggplot(data, aes(x=displacement, y=mileage)) +
       geom_point() +
       geom_smooth(method=lm, se=FALSE)


#Q1.3 
data = data %>% mutate(mileage_kmlitre = mileage*0.425 , 
                       displacement_cc = displacement*16.387)
ggplot(data, aes(x=displacement_cc, y=mileage_kmlitre)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
#pattern of variable did not change --> linear transformation will not affect pattern 

#Q1.4 
linear_model = lm(mileage_kmlitre ~ displacement_cc, data=data)
summary(linear_model)

COEF = coef(linear_model)
beta0 = COEF[1] 
beta0
beta1 = COEF[2]
beta1

#Q1.5 
x_bar = sum(data$displacement_cc)/ length(data$displacement_cc)
x_bar
y_bar = sum(data$mileage_kmlitre)/ length(data$mileage_kmlitre)
y_bar

sxx = 0
for (i in 1:length(data$displacement_cc)){
  sxx = sxx + (data$displacement_cc[i] - x_bar)^2
}
sxx 

sxy = 0 
for (i in 1:length(data$displacement_cc)){
  sxy = sxy + data$mileage_kmlitre[i]*(data$displacement_cc[i] - x_bar)
}
sxy
beta_1_hat = sxy/sxx
beta_1_hat

beta_0_hat = y_bar - (beta_1_hat*x_bar)
beta_0_hat


#Q1.6 
yhat = predict(linear_model)
yhat
residual = list()

for (i in 1:32){
  residual = append(residual, (data$mileage_kmlitre[i]-yhat[i]))
}
residual = data.frame(residual)
residual
sum(residual)

e_square = data.frame(residual^2)
sum(e_square)

#Q1.7
plot(y =residual, x = data$displacement_cc) +
  abline(h = 0)

#Q1.8
predict(linear_model, data.frame(displacement_cc = 1998))

#Q1.9 
predict(linear_model, data.frame(displacement_cc = 1991))

