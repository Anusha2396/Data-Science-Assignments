#Predicting KNN using Glass Dataset
GlassData <- read.csv(file.choose()) #Importing Glass dataset
View(GlassData)
summary(GlassData) 
str(GlassData) # Structure of glass dataset with levels

# table of type 
table(GlassData$Type)

# table of type which gives % of each type
round(prop.table(table(GlassData$Type))*100,1)

# Summarizing any three numeric features
summary(GlassData[c("Ca","Fe","K")])

#Creating a normalization function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#Test normalization to make the results identical
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))

#Applying the normalization function to glass dataset
GlassData1 <- as.data.frame(lapply(GlassData[1:9], norm))
View(GlassData1)
summary(GlassData1[c("Ca","Fe","K")])
View(GlassData1)

#Inorder to evaluate better results, classify into train and test data
set.seed(123) # Used to specify seed(single value may be integer or null)
index <- sample(2, nrow(GlassData1), replace = TRUE, prob = c(0.7,0.3))
GlassData_train <- GlassData1[index == 1,]
GlassData_test <-  GlassData1[index == 2,]

#Labelling for training and test datasets
set.seed(123) # Used to specify seed(single value may be integer or null)
index1 <- sample(2, nrow(GlassData), replace = TRUE, prob = c(0.7,0.3))
GlassData_train_labels <- GlassData[index1==1,10]
GlassData_test_labels <-  GlassData[index1==2,10]

# Build a KNN model on dataset
install.packages("class")
library(class)
GlassData_test_pred <- knn(train = GlassData_train, test = GlassData_test, cl = GlassData_train_labels, k=3)
table(GlassData_test_pred,GlassData_test_labels)
mean(GlassData_test_pred == GlassData_test_labels) 

install.packages("gmodels")
library(gmodels)
CrossTable(x= GlassData_test_labels,y= GlassData_test_pred,prop.chisq = FALSE) 



#Predicting KNN using zoo dataset
ZooData <- read.csv(file.choose())
View(ZooData)
summary(ZooData) 
str(ZooData) # Structure of zoo dataset with levels

# Drop the animal.name feature
ZooData1 <- ZooData[-1]
str(ZooData1) #Structure of Zoo data with 17 levels

table(ZooData1$type)

round(prop.table(table(ZooData1$type))*100,1)

summary(ZooData1[c("hair","feathers","eggs")])
#Creating a normalization function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#Test normalization to make the results identical
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))

#Applying the normalization function to glass dataset
ZooData2 <- as.data.frame(lapply(ZooData1[1:17], norm))
View(ZooData2)
summary(ZooData2[c("hair","feathers","eggs")])
View(ZooData2)

#Inorder to evaluate better results, classify into train and test data
set.seed(123) # Used to specify seed(single value may be integer or null)
index2 <- sample(2, nrow(ZooData2), replace = TRUE, prob = c(0.7,0.3))
ZooData_train <- ZooData2[index2 == 1,]
ZooData_test <-  ZooData2[index2 == 2,]

#Labelling for training and test datasets
set.seed(123) # Used to specify seed(single value may be integer or null)
index3 <- sample(2, nrow(ZooData1), replace = TRUE, prob = c(0.7,0.3))
ZooData_train_labels <- ZooData1[index3==1,10]
ZooData_test_labels <-  ZooData1[index3==2,10]

# Build a KNN model on dataset
install.packages("class")
library(class)
ZooData_test_pred <- knn(train = ZooData_train, test = ZooData_test, cl = ZooData_train_labels, k=3)
table(ZooData_test_pred,ZooData_test_labels)
mean(ZooData_test_pred == ZooData_test_labels) 

install.packages("gmodels")
library(gmodels)
CrossTable(x= ZooData_test_labels,y= ZooData_test_pred,prop.chisq = FALSE)
