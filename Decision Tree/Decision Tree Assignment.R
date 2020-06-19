# Fraud data to check on the probability of Risky Vs Good

install.packages("party")
library(party)
install.packages("caret")
library(caret)
install.packages("C50")
library(C50)
install.packages("tree")
library(tree)
install.packages("gmodels")
library(gmodels)
install.packages("knitr")
library(knitr)
install.packages("png")
library(png)

FraudCheck <- read.csv(file.choose())
View(FraudCheck)

# Data is split into train and test based on sales

hist(FraudCheck$Taxable.Income)
RiskyGoodAnalysis = ifelse(FraudCheck$Taxable.Income <= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
FC_train <- FC[1:300,]# View(CD_train)
FC_test <- FC[301:600,]# View(CD_test)

# Using Party Function 
png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
# From the above tree, there are 20% of Risky patients and 80% good patients
png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)
mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df)
install.packages("e1071")
library(e1071)
confusionMatrix(FC_test$Risky_Good,pred_test_df)

pred_tree1 <- as.data.frame(predict(op_tree,newdata=FC_train))
pred_tree1["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_train)
mean(pred_test_df==FC_train$Risky_Good) # Accuracy = 76.6 %
CrossTable(FC_train$Risky_Good,pred_test_df)
install.packages("e1071")
library(e1071)
confusionMatrix(FC_train$Risky_Good,pred_test_df)


# Analysis on Company dataset to know what factor produces high sale
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
CompanyData <- read.csv(file.choose())

# Data is split into train and test based on sales

hist(CompanyData$Sales)
High = ifelse(CompanyData$Sales<10, "No", "Yes")
CD = data.frame(CompanyData, High)
#CD <- CompanyData[,2:12]
# View(CD)
CD_train <- CD[1:200,]
# View(CD_train)
CD_test <- CD[201:400,]
# View(CD_test)#Using Party Function 
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(op_tree)
plot(op_tree)
pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test)
mean(pred_test_df==CD$High) # Accuracy = 79.5%
CrossTable(CD_test$High,pred_test_df)
confusionMatrix(CD_test$High,pred_test_df)

pred_tree <- as.data.frame(predict(op_tree,newdata=CD_train))
pred_tree["final"] <- NULL
pred_train_df <- predict(op_tree,newdata=CD_train)
mean(pred_train_df==CD$High) # Accuracy = 63.5%
CrossTable(CD_train$High,pred_test_df)
confusionMatrix(CD_test$High,pred_train_df)

##### Using tree function 
cd_tree_org <- tree(High~.-Sales,data=CD)
plot(cd_tree_org)
text(cd_tree_org,pretty = 0)
summary(cd_tree_org)
# Using the training data#Using tree function 
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)
cd_tree <- tree(High~.-Sales,data=CD_test)
summary(cd_tree)
