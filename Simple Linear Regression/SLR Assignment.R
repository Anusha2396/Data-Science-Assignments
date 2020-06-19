Calories_consumed <- read.csv(file.choose())
View(Calories_consumed)
summary(Calories_consumed)
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)
var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)

library(lattice)
dotplot(Calories_consumed$Calories.Consumed, main="Dot Plot of Calories Consumed")
dotplot(Calories_consumed$Weight.gained..grams., main="Dot Plot of Weight gained in grams")
boxplot(Calories_consumed$Calories.Consumed,col="blueviolet")
boxplot(Calories_consumed$Weight.gained..grams.,col="mediumorchid3", horizontal = T)

cor(Calories_consumed$Weight,Calories_consumed$Calories.Consumed)
plot(Calories_consumed$Weight,Calories_consumed$Calories.Consumed,main="Calories Consumed v/s Weight Gained", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="Weight.gained..grams", 
     ylab="Calories.Consumed", pch=20)
WeightGainModel <- lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WeightGainModel)

# Conclusion: As the p-value is < 0.05 and are significant. 
# Even R^2 is also > 0.7 i.e., 0.8968 so the model is a good one 
# As it predicts 89.68% of the right output.

Delivery_time <- read.csv(file.choose())
View(Delivery_time)
summary(Delivery_time)
var(Delivery_time$Delivery.Time)
sd(Delivery_time$Delivery.Time)
var(Delivery_time$Sorting.Time)
sd(Delivery_time$Sorting.Time)

dotplot(Delivery_time$Delivery.Time, main="Dot Plot of Delivery Time")
dotplot(Delivery_time$Sorting.Time, main="Dot Plot of Sorting time")
boxplot(Delivery_time$Delivery.Time,col="blueviolet")
boxplot(Delivery_time$Sorting.Time,col="mediumorchid3", horizontal = F)

cor(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time)
plot(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time,main="Calories Consumed v/s Weight Gained", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="Delivery time", 
     ylab="Sorting time", pch=20)
DeliveryTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = Delivery_time)
summary(DeliveryTimeModel)

# P-value is <0.05 but R^2 value is <0.7. So it is not good model so inorder to make it a perfect one, we need to perform transformations.
# To increase R^2, we need to find the influencing data points in the given data
library(mvinfluence)
infIndexPlot(DeliveryTimeModel)
DeliveryTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = Delivery_time[c(-5,-9,-21),])
summary(DeliveryTimeModel)
plot(DeliveryTimeModel)

# Conclusion: As the p-value is < 0.05 and are significant.And also by removing the influential data points 
# R^2 is increased and is also > 0.7 i.e., 0.8332 so the model is a good one 
# As it predicts 83.32% of the right output.

Emp_data <- read.csv(file.choose())
View(Emp_data)
summary(Emp_data)
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)

library(lattice)
dotplot(Emp_data$Salary_hike, main="Dot Plot of Salary Hike")
dotplot(Emp_data$Churn_out_rate, main="Dot Plot of Churnout Rate")
boxplot(Emp_data$Salary_hike,col="blueviolet")
boxplot(Emp_data$Churn_out_rate,col="mediumorchid3", horizontal = T)

cor(Emp_data$Salary_hike,Emp_data$Churn_out_rate)
plot(Emp_data$Churn_out_rate,Emp_data$Salary_hike,main="Salary Hike v/s Churnout rate", col="violetred", 
     col.main="violetred", col.lab="violetred", xlab="Weight.gained..grams", ylab="Calories.Consumed", pch=20)
SalaryHikeModel <- lm(Salary_hike ~ Churn_out_rate, data = Emp_data)
summary(SalaryHikeModel)

# Conclusion: As the p-value is < 0.05 and are significant. 
# Even R^2 is also > 0.7 i.e., 0.8312 so the model is a good one 
# As it predicts 83.12% of the right output.



