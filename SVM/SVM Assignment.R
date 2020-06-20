# Salary Prediction using SVM 
# To import the train data of salary
SalaryTrain <- read.csv(file.choose())
View(SalaryTrain)
summary(SalaryTrain)
str(SalaryTrain)
dim(SalaryTrain)
class(SalaryTrain)
SalaryTrain$educationno <- as.factor(SalaryTrain$educationno)

# To import the test data of salary
SalaryTest <- read.csv(file.choose())
View(SalaryTest)
summary(SalaryTest)
str(SalaryTest)
dim(SalaryTest)
class(SalaryTest)
SalaryTest$educationno <- as.factor(SalaryTest$educationno)

install.packages("kernlab")
library(kernlab) # To make use of ksvm function
install.packages("caret")
library(caret) # To solve classification and regression related problems.
install.packages("plyr")
library(plyr) # Used for spliting, applying and combining data
install.packages("psych")
library(psych) # Helps in doing many factor analysis
install.packages("e1071")
library(e1071) # It enables to perform SVM method.

# Building Salary prediction model of SVM by using ksvm function

model1<-ksvm(SalaryTrain$Salary~., data= SalaryTrain, kernel = "vanilladot")
model1
SalaryPrediction <- predict(model1, SalaryTest)

table(SalaryPrediction,SalaryTest$Salary)
agreement <- SalaryPrediction == SalaryTest$Salary
table(agreement)
prop.table(table(agreement))

# Using different kernels gives different average values
Rfdotmodel<-ksvm(SalaryTrain$Salary~., data= SalaryTrain,kernel = "rbfdot")
Rfdotpred<-predict(Rfdotmodel,newdata=SalaryTest)
mean(Rfdotpred==SalaryTest$Salary) #
Vanillamodel<-ksvm(SalaryTrain$Salary~., data= SalaryTrain,kernel = "vanilladot")
Vanillapred<-predict(Vanillamodel,newdata=SalaryTest)
mean(Vanillapred==SalaryTest$Salary)
polymodel<-ksvm(SalaryTrain$Salary~., data= SalaryTrain,kernel = "polydot")
polypred<-predict(polymodel,newdata=SalaryTest)
mean(polypred==SalaryTest$Salary)

# Forest Fire to predict the size of burned area using SVM 
# To import the Forest Fire dataset
ForestFire <- read.csv(file.choose())
View(ForestFire)
summary(ForestFire)
str(ForestFire)
dim(ForestFire)
class(ForestFire)

library(kernlab) # To make use of ksvm function
library(caret) # To solve classification and regression related problems.
library(plyr) # Used for spliting, applying and combining data
library(psych) # Helps in doing many factor analysis
library(e1071) # It enables to perform SVM method.
library(dplyr)

hist(ForestFire$area) # Shows maximum number of zero values
rug(ForestFire$area) # Represents 1D plot of data

# Transforming area values to Y so that zero values can be varied 
ForestFire1 <- mutate(ForestFire, y = log(area + 1))  # default is to the base e, y is lower case
hist(ForestFire1$y)
summary(ForestFire) 

# Normalizing the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

ForestFire$temp = normalize(ForestFire$temp)
ForestFire$RH   = normalize(ForestFire$RH)
ForestFire$wind = normalize(ForestFire$wind)
ForestFire$rain = normalize(ForestFire$rain)

# To predict the size of burned area

attach(ForestFire)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(ForestFire), replace = TRUE, prob = c(0.7,0.3)) # Setting the test and train probabilities
ForestFireTrain <- ForestFire[ind==1,]
ForestFireTest  <- ForestFire[ind==2,]

# Building size of burned area prediction model of SVM by using ksvm function
model1<-ksvm(size_category~temp+rain+wind+RH, data= ForestFireTrain, kernel = "vanilladot")
model1
PredictedArea <- predict(model1, ForestFireTest)

table(PredictedArea,ForestFireTest$size_category)
agreement <- PredictedArea == ForestFireTest$size_category
table(agreement)
prop.table(table(agreement))

# The size prediction of burned area only depends on Temp,Rain,Wind & RH variables
# Model prediction using different kernels to check the accuracy
kernel = "rbfdot"
Rbfdotmodel<-ksvm(size_category~temp+rain+wind+RH, data= ForestFireTrain,kernel = "rbfdot")
RbfdotPred<-predict(Rfdotmodel,newdata=ForestFireTest)
mean(RbfdotPred==ForestFireTest$size_category) 

# kernel = vanilladot
Vanillamodel<-ksvm(size_category~temp+rain+wind+RH, data= ForestFireTrain,kernel = "vanilladot")
VanillaPred<-predict(Vanillamodel,newdata=ForestFireTest)
mean(VanillaPred==ForestFireTest$size_category) 

# kernal = besseldot
Besseldotmodel<-ksvm(size_category~temp+rain+wind+RH, data= ForestFireTrain,kernel = "besseldot")
BesselPred<-predict(Besseldotmodel,newdata=ForestFireTest)
mean(BesselPred==ForestFireTest$size_category) 

# kernel = polydot
Polymodel<-ksvm(size_category~temp+rain+wind+RH, data= ForestFireTrain,kernel = "polydot")
PolyPred<-predict(Polymodel,newdata = ForestFireTest)
mean(PolyPred==ForestFireTest$size_category) 