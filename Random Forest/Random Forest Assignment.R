# Analysis of FraudCheck data using Random Forest method
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
summary(FraudCheck)
dim(FraudCheck)

# Inorder to perform Random Forest method we need to install randomForest package
install.packages("randomForest")
library(randomForest)
hist(FraudCheck$Taxable.Income)
hist(FraudCheck$Taxable.Income, main = "Taxable Income",xlim = c(10000,99999),
     breaks=c(seq(40,80,120)), col = c("dodgerblue4","darkgoldenrod3", "darkmagenta","violet"))
RiskyGood = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good") # Prints Risky if income is <=30000 or else Good.
TempFC= data.frame(FraudCheck,RiskyGood) # Creates a new variable RiskyGood and prints 1 for Good and 0 for Risky
Fraud = TempFC[,c(1:7)]
str(Fraud) # Defines Structure of Fraud variable
table(Fraud$RiskyGood)   # Gives a table format of Risky and Good(476 good customers and 124 risky customers)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(Fraud), replace = TRUE, prob = c(0.7,0.3)) # Helps in spliting train and test data
FCTrain <- Fraud[ind==1,] # 70% train data
FCTest  <- Fraud[ind==2,] # 30% test data

set.seed(123)
RFFraud <- randomForest(RiskyGood~., data=FCTrain) # Performing Random Forest on Train data
RFFraud  # Description of the random forest with no of trees
attributes(RFFraud) # Gives different aspects involved in Random forest

# Building a prediction and confusion matrix for training data 
pred1 <- predict(RFFraud,FCTrain)
head(pred1)
head(FCTrain$RiskyGood) # Retuns first few rows of data
confusionMatrix(pred1, FCTrain$RiskyGood)   # 100 % accuracy on training data

# Building a prediction and confusion matrix for testing data
pred2 <- predict(RFFraud, FCTest)
confusionMatrix(pred2, FCTest$RiskyGood) # 100 % accuracy on test data 

# Error rate can be seen in the plot
plot(RFFraud) # From the plot we can say that Accuracy is maximum.

# Tuning Random Forest Model for better visualization and classification
tune <- tuneRF(FCTrain[,-6], FCTrain[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
RFFraud1 <- randomForest(RiskyGood~., data=FCTrain, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
RFFraud1
pred1 <- predict(RFFraud1, FCTrain)
confusionMatrix(pred1, FCTrain$RiskyGood)  # 100 % accuracy on training data 

# Testing prediction using the Tuned RFFraud1 model
pred2 <- predict(RFFraud1, FCTest)
confusionMatrix(pred2, FCTest$RiskyGood) # 100 % accuracy on test data 

# To check number of nodes in the trees
hist(treesize(RFFraud1), main = "No of Nodes for the trees", col = "darkmagenta")
# Variable Importance gives how much accuracy decreases if we remove variables
varImpPlot(RFFraud1)
varImpPlot(RFFraud1 ,Sort = T, n.var = 5, main = "Variable Importance of Top-5 variables")

# Quantitative values 
importance(RFFraud1)
varUsed(RFFraud)   # Variables that are used by Random forest

# Partial Dependence Plot 
partialPlot(RFFraud1, FCTrain, Taxable.Income, "Good")
tr1 <- getTree(RFFraud1, 2, labelVar = TRUE)

# Multidimensional scaling plot is used to find similarity/dissimilarity and it is a proximity Matrix
MDSplot(RFFraud1, Fraud$RiskyGood) 




# Analysis of Company data using Random Forest method
CompanyData <- read.csv(file.choose())
View(CompanyData)
summary(CompanyData)
dim(CompanyData)

# Inorder to perform Random Forest method we need to install randomForest package
library(randomForest)
hist(CompanyData$Sales)
hist(CompanyData$Sales, main = "Sales",xlim = c(0,17),
     breaks=c(seq(20,40,60)), col = c("dodgerblue4","darkgoldenrod3", "darkmagenta","violet"))
SalesHigh = ifelse(CompanyData$Sales <= 9, "No", "Yes")  # If sales is greater than 9 then the sales is high
CompanySales = data.frame(CompanyData[1:11], SalesHigh)
str(CompanySales) # Defines Structure of Company data variables
table(CompanySales$SalesHigh) # Gives a table format of  High sales whether its Yes and No

# Data Partition
set.seed(123)
ind <- sample(2, nrow(CompanySales), replace = TRUE, prob = c(0.7,0.3)) # Helps in spliting train and test data
CDTrain <- CompanySales[ind==1,]# 70% train data
CDTest  <- CompanySales[ind==2,] # 30% test data

set.seed(213)
RFCompany <- randomForest(SalesHigh~., data=CDTrain) # Performing Random Forest on Train data
RFCompany  # Description of the random forest with no of trees
attributes(RFCompany) # Gives different aspects involved in Random forest

# Building a prediction and confusion matrix for training data  
pred1 <- predict(RFCompany, CDTrain)
head(pred1) 
head(CDTrain$SalesHigh) # Retuns first 5 rows of data
confusionMatrix(pred1, CDTrain$SalesHigh)   # 100 % accuracy on training data 

# Building a prediction and confusion matrix for testing data 
pred2 <- predict(RFCompany, CDTest)
confusionMatrix(pred2, CDTest$SalesHigh) # 98% accuracy on test data 

# Error rate can be seen in the plot
plot(RFCompany)  # From the plot we can say that Accuracy is maximum.

# Tuning Random Forest Model for better visualization and classification 
tune <- tuneRF(CDTrain[,-11], CDTrain[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
RFCompany1 <- randomForest(SalesHigh~., data=CDTrain, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
RFCompany1
pred1 <- predict(RFCompany1, CDTrain)
confusionMatrix(pred1, CDTrain$SalesHigh)  # 100 % accuracy on training data 

# Testing prediction using the Tuned RFCompany1 model
pred2 <- predict(RFCompany1, CDTest)
confusionMatrix(pred2, CDTest$SalesHigh) # 84.35 % accuracy on test data 

# To check number of nodes in the trees
hist(treesize(RFCompany1), main = "No of Nodes for the trees", col = "burlywood1")

# Variable Importance gives how much accuracy decreases if we remove variables
varImpPlot(RFCompany1)
varImpPlot(RFCompany1 ,Sort = T, n.var = 5, main = "Variable Importance")

# Quantitative values 
importance(RFCompany1)
varUsed(RFCompany)   # Variables that are used by Random forest

# Partial Dependence Plot 
partialPlot(RFCompany1, CDTrain, Price, "Yes")

# Extract single tree from the forest :
getTree(RFCompany, 1, labelVar = TRUE)

# Multidimensional scaling plot is used to find similarity/dissimilarity and it is a proximity Matrix
MDSplot(RFCompany1, CompanySales$SalesHigh)
