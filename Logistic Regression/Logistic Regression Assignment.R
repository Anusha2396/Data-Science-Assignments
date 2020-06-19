#Performing logistic regression using bank dataset
BankData <- read.csv(file.choose())
summary(BankData)
View(BankData)
str(BankData) #str provides Structure of dataset

install.packages("AER") # Applied Econometrics in R 
library(AER)

# Creating dummies
BankData$y <- as.factor(BankData$y)
BankData$y <- factor(BankData$y,levels = c("yes" , "no"),labels = c(0,1))
summary(BankData$y)
BankData$job <- factor(BankData$job,levels = c("admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                               "blue-collar","self-employed","retired","technician","services"),labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
BankData$marital <- factor(BankData$marital,levels = c( "married","divorced","single"),labels = c(0, 1, 2))
BankData$education <- factor(BankData$education,levels = c( "unknown","secondary","primary","tertiary"),labels = c(0, 1, 2,3))
BankData$default <- factor(BankData$default,levels = c("yes" , "no"),labels = c(0,1))
BankData$housing <- factor(BankData$housing, levels = c("yes","no"),labels = c(0,1))
BankData$loan <- factor(BankData$loan,levels = c("yes","no"),labels = c(0,1))
BankData$contact <- factor(BankData$contact,levels = c("unknown","telephone","cellular"),labels = c(0,1,2))
BankData$month <- factor(BankData$month,levels = c("jan", "feb", "mar", "apr" , "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), labels = c(0,1,2,3,4,5,6,7,8,9,10,11))
BankData$poutcome <- factor(BankData$poutcome,levels = c("unknown","other","failure","success"),labels = c(0,1,2,3))
View(BankData)

BankData1 <- na.omit(BankData) # Bankdata dataset with NA will be omitted.

# Logistic Regression is done using glm(generalised linear model) function 
logit <- glm(y ~ age + factor(job)  + factor(marital) + factor(education) + factor(default) + balance + factor(housing) + factor(loan) + factor(month)
            + duration + pdays + previous +factor(poutcome),family= "binomial",data=BankData)
summary(logit)

# Confusion Matrix Table gives the values of FP,FN,TP,TN.
prob <- predict(logit,type=c("response"),BankData)
prob
confusion <- table(prob>0.7,BankData$y)
confusion

# Model Accuracy
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

pred_values  = NULL
yes_NO = NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>= 0.5,1,0)
  yes_NO[i] <- ifelse(prob[i]>= 0.5,'Yes',"no")
}
BankData[,"prob"] <- prob
BankData[,"pred_values"] <- pred_values
BankData[,"yes_NO"] <- yes_NO

# ROC Curve 
install.packages("ROCR")
library(ROCR)

rocrpred<-prediction(prob,BankData$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)
# If there is more area under the ROC Curve then we can say better logistic regression model