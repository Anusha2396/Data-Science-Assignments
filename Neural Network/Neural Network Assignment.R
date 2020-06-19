# Analysis on Startup data using NN method
Startups50 <- read.csv(file.choose())
View(Startups50)
summary(Startups50)
class(Startups50)

install.packages("neuralnet")
library(neuralnet)  # To perform Neural network regression
install.packages("nnet")
library(nnet) # To perform Neural network classification
library(plyr)
install.packages("NeuralNetTools")
library(NeuralNetTools) # To use other tools of Neural Network

# To make the levels of State as 0,1 & 2
Startups50$State <- as.numeric(revalue(Startups50$State, c("New York"="1", "California"="2", "Florida"="3")))
str(Startups50)

# Normalizing the Startups data
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
Startups_norm <- as.data.frame(lapply(Startups50, normalize))
summary(Startups_norm)
Startups50 <- as.data.frame(Startups50)
attach(Startups50)

# To check the normality of variables wrt Profit
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
windows()

pairs(Startups50) # Correlation of Each variable wrt other variables

cor(Startups50) # Correlation coefficient - Strength & Direction of correlation
summary(Startups50) # Summarizes the normalized data.
summary(Startups_norm$Profit) # Normalized form of profit
summary(Startups50$Profit) # Profit value without normalization

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.8,0.2)) # Spliting the data into Test & Train
StartupsTrain <- Startups_norm[ind==1,] # 80% of Training data
StartupsTest  <- Startups_norm[ind==2,] # 20% of Testing data

# Creating a neural network model on training data
StartupsModel <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = StartupsTrain)
str(StartupsModel) 
plot(StartupsModel)
summary(StartupsModel)
par(mar = numeric(4), family = 'serif')
plotnet(StartupsModel, alpha = 0.6)

# Evaluating model performance
set.seed(123)
ResultingModel <- compute(StartupsModel,StartupsTest[1:4])
ProfitPredicted <- ResultingModel$net.result

# Predicted profit Vs Actual profit of test data.
cor(ProfitPredicted,StartupsTest$Profit)

# since the prediction is in Normalized form, we need to de-normalize it # to get the actual prediction on profit
StrMax <- max(Startups50$Profit)
StrMin <- min(Startups50$Profit)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
ActualPred <- unnormalize(ProfitPredicted,StrMin,StrMax)
head(ActualPred)

# Improve the model performance :
set.seed(123)
StartupsModel2 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = StartupsTrain,hidden = 2)
plot(StartupsModel2 ,rep = "best")
summary(StartupsModel2)
ResultingModel2<-compute(StartupsModel2,StartupsTest[1:4])
ProfitPredicted2<-ResultingModel2$net.result
cor(ProfitPredicted2,StartupsTest$Profit)
plot(ProfitPredicted2,StartupsTest$Profit)
par(mar = numeric(4), family = 'serif')
plotnet(StartupsModel2, alpha = 0.6)# Error has reduced and training steps had been increased as the number of neurons  under hidden layer are increased


# Analysis on Concrete data using NN method 

ConcreteData = read.csv(file.choose())
View(ConcreteData)
summary(ConcreteData)
class(ConcreteData)

library(neuralnet) # To perform Neural network regression
library(nnet) # To perform Neural network classification
library(NeuralNetTools) # To use other tools of Neural Network
library(plyr)

# Normalizing the Concrete data
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Applying normalization to entire data
Concrete_norm <- as.data.frame(lapply(ConcreteData, normalize))
summary(Concrete_norm)

# Spliting the data into test and train
ConcreteTrain <- Concrete_norm[1:773, ] # Train data
ConcreteTest <- Concrete_norm[774:1030, ] # Test data

# BUilding model using train data
ConcreteModel <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,data = ConcreteTrain)

# Visualizing the Concrete model
plot(ConcreteModel)

# Evaluating model performance
set.seed(123)
ResultingModel <- compute(ConcreteModel,ConcreteTest[1:8])
StrengthPred <- ResultingModel$net.result

# Predicted strength Vs Actual Strength of test data.
cor(StrengthPred,ConcreteTest$strength)

# As the predicted results are in Normalized form, we need to de-normalize it  so as to get the actual predicted values of strength.
StrMax <- max(ConcreteData$strength) # Maximum value of strength variable
StrMin <- min(ConcreteData$strength) # Minimum value of strength variable

# Unnormalizing the actual data
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrengthPred <- unnormalize(StrengthPred,StrMin,StrMax)
head(ActualStrengthPred)

# Improving the model performance :
set.seed(123)
ConcreteModel2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= ConcreteTrain,hidden = 5)
plot(ConcreteModel2, rep = "best")
summary(ConcreteModel2)
ResultingModel2<-compute(ConcreteModel2,ConcreteTest[1:8])
StrengthPred2<-ResultingModel2$net.result
cor(StrengthPred2,ConcreteTest$strength)
plot(StrengthPred,ConcreteTest$strength)
par(mar = numeric(4), family = 'serif')
plotnet(ConcreteModel2, alpha = 0.6) # Error has reduced and training steps had been increased as the number of neurons  under hidden layer are increased


# Analysis on Forestfire dataset using NN method

ForestFire <- read.csv(file.choose())
View(ForestFire)
summary(ForestFire)
class(ForestFire)
str(ForestFire)

library(neuralnet) # To perform Neural network regression
library(nnet) # To perform Neural network classification
library(NeuralNetTools) # To use other tools of Neural Network
library(plyr)

ForestFire$size_category <- as.numeric(revalue(ForestFire$size_category, c("small"="0", "large"="1")))
ForestFire$month <- as.numeric(revalue(ForestFire$month,c('jan'='0','feb'='1','mar'='2','apr'='3','may'='4','jun'='5','jul'='6','aug'='7','sep'='8','oct'='9','nov'='10','dec'='11')))
ForestFire$day <- as.numeric(revalue(ForestFire$day,c('mon'='0','tue'='1','wed'='2','thu'='3','fri'='4','sat'='5','sun'='6')))
ForestFire <- as.data.frame(ForestFire)
attach(ForestFire)
windows()
pairs(ForestFire)
cor(ForestFire)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
ForestFireNorm<-as.data.frame(lapply(ForestFire,FUN=normalize))
summary(ForestFireNorm)
summary(ForestFire$area)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(ForestFireNorm), replace = TRUE, prob = c(0.7,0.3))
ForestFireTrain <- ForestFireNorm[ind==1,]
ForestFireTest  <- ForestFireNorm[ind==2,]

ForestFireModel <- neuralnet(formula = area ~ temp+DMC+wind+ISI+RH+DC,data = ForestFireTrain)
str(ForestFireModel)
plot(ForestFireModel)
summary(ForestFireModel)

set.seed(123)
ResultingModel <- compute(ForestFireModel,ForestFireTest[1:12])
AreaPred <- ResultingModel$net.result

# Predicted profit Vs Actual profit of test data.
cor(AreaPred,ForestFireTest$area)
StrMax <- max(ForestFire$area)
StrMin <- min(ForestFire$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualAreaPred <- unnormalize(AreaPred,StrMin,StrMax)
head(ActualAreaPred)

set.seed(12345)
ForestFireModel2 <- neuralnet(area~temp+DMC+wind+ISI+RH+DC,data = ForestFireTrain,hidden = 2)
plot(ForestFireModel2)
summary(ForestFireModel2)
ResultingModel2<-compute(ForestFireModel2,ForestFireTest[1:12])
AreaPred2<-ResultingModel2$net.result
cor(AreaPred2,ForestFireTest$area)
plot(AreaPred2,ForestFireTest$area)
par(mar = numeric(4), family = 'serif')
plotnet(ForestFireModel2, alpha = 0.6)