SalaryTrain <- read.csv(file.choose())
str(SalaryTrain)
View(SalaryTrain)
SalaryTrain$educationno <- as.factor(SalaryTrain$educationno)
class(SalaryTrain)

# Data(Test)
SalaryTest <- read.csv(file.choose())
str(SalaryTest)
View(SalaryTest)
SalaryTest$educationno <- as.factor(SalaryTest$educationno)
class(SalaryTest)

library(naivebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

# Plots of individual discrete variables wrt Salary using Plot
plot(SalaryTrain$workclass,SalaryTrain$Salary)
plot(SalaryTrain$education,SalaryTrain$Salary)
plot(SalaryTrain$educationno,SalaryTrain$Salary)
plot(SalaryTrain$maritalstatus,SalaryTrain$Salary)
plot(SalaryTrain$occupation,SalaryTrain$Salary)
plot(SalaryTrain$relationship,SalaryTrain$Salary)
plot(SalaryTrain$race,SalaryTrain$Salary)
plot(SalaryTrain$sex,SalaryTrain$Salary)
plot(SalaryTrain$native,SalaryTrain$Salary)

#Plots of individual continuous variables wrt Salary using ggplot
ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$age, fill = SalaryTrain$Salary)) +geom_boxplot() +ggtitle("Box Plot")
# The boxplot tells us about the outliers and hence as outliers are more, it is not normally distributed
ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$capitalgain, fill = SalaryTrain$Salary)) + geom_boxplot() + ggtitle("Box Plot")
ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$capitalloss, fill = SalaryTrain$Salary)) + geom_boxplot() + ggtitle("Box Plot")
ggplot(data=SalaryTrain,aes(x=SalaryTrain$Salary, y = SalaryTrain$hoursperweek, fill = SalaryTrain$Salary)) + geom_boxplot() + ggtitle("Box Plot")

#Density Plot helps in plotting distribution of a numeric variable
ggplot(data=SalaryTrain,aes(x = SalaryTrain$age, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.5, color = 'dodgerblue4') + ggtitle("Density Plot of Age")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$workclass, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Workclass")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$education, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Education")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$educationno, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Educationno")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$maritalstatus, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of martial status")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$occupation, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Occupation")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$sex, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Gender")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$relationship, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Relationship")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$race, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Race")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$capitalgain, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Capitalgain")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$capitalloss, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Capitalloss")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$hoursperweek, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Hoursperweek")
ggplot(data=SalaryTrain,aes(x = SalaryTrain$native, fill = SalaryTrain$Salary)) + geom_density(alpha = 0.7, color = 'dodgerblue4') + ggtitle("Density Plot of Native")

# Naive Bayes Model 
Model <- naiveBayes(SalaryTrain$Salary ~ ., data = SalaryTrain)
Model
Model_pred <- predict(Model,SalaryTest)
mean(Model_pred==SalaryTest$Salary)
confusionMatrix(Model_pred,SalaryTest$Salary)

