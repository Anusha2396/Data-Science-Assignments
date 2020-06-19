#Airlines Passengers Data Analysis with Time Series Forecast
Airlines<-read.csv(file.choose()) # Import the Airlines dataset
View(Airlines)
summary(Airlines)
var(Airlines$Passengers)
sd(Airlines$Passengers)
hist(Airlines$Passengers) # Right skewed

# Packages needed to perform Forecasting
library(forecast) # Performs Forecasting based on Time Series Analysis
library(fpp) # Forecasting Principles and Practice
library(smooth) # Helps in Smoothing operations in forecasting

windows() #To view a better plot
plot(Airlines$Passengers,type="o") # Based on plot we can tell it is Upward linear trend with multiplicative seasonality

# So based on Domain knowledge looking at the dataset, As it is a Time series forecast we need to create 12 dummy variables for 12 months 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummy variables for 12 months and for 96 observations
colnames(X)<-month.abb # Assigning month names for different Variables
AirlinesData<-cbind(Airlines,X) # Combining Airlines dataset with X
View(AirlinesData)
colnames(AirlinesData)
AirlinesData["t"]<- 1:96 # Creating a t variable to count each observations
View(AirlinesData)
AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"]) # To perform Exponential Trend
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"] # To perform Quadratic Trend
attach(AirlinesData)

# Splitting the data into train and test data
AirlinesTrain<-AirlinesData[1:84,] # We build a model using training data
AirlinesTest<-AirlinesData[85:96,] # We predict a model using testing data


# Building different trends and seasonality to conclude on the type of model this Time series Forecast belongs to
# Trend says whether it has a Overall downward or upward pattern
# Linear Trend
linear_model<-lm(Passengers~t,data=AirlinesTrain) # Building a training model
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =AirlinesTest)) # Predicting a model using test data
View(linear_pred)
rmse_linear<-sqrt(mean((AirlinesTest$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear 

# Exponential Trend
expo_model<-lm(log_Passenger~t,data=AirlinesTrain)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=AirlinesTest))
rmse_expo<-sqrt(mean((AirlinesTest$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

# Quadratic Trend
Quad_model<-lm(Passengers~t+t_square,data=AirlinesTrain)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=AirlinesTest))
rmse_Quad<-sqrt(mean((AirlinesTest$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

# Seasonality says about upward and downward fluctuations
# Additive Seasonality 
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

# Additive Seasonality with Linear 
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

# Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 

# Multiplicative Seasonality 
multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

# Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and different trend and seasonality's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)
pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)
Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)
# Therefore based on rmse values and model, we conclude that it is a Multiplicative Seasonality with linear trend


#Cocacola Sales Data Analysis with Time Series Forecast
Cocacola <- read.csv(file.choose()) # Import Cocacola Dataset
View(Cocacola) 
summary(Cocacola) 
var(Cocacola$Sales)
sd(Cocacola$Sales)
hist(Cocacola$Sales) # Right Skewed

#Packages needed to perform forecasting
library(forecast)
library(fpp)
library(smooth)

windows()
plot(Cocacola$Sales,type="o")
#   Converting the quarters into 0s and 1s
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0') # grepl function returns true or false
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

# So creating 4 dummy variables as we have 4 Quarters
CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)
CocacolaData["t"]<- 1:42 # Creating a t variable to keep a count of observations
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"]) # To perform exponential trend
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"] # To perform Quadratic trend
attach(CocacolaData)

#Splitting the data into test and train
CocacolaTrain<-CocacolaData[1:36,] # We build a model using training data
CocacolaTest<-CocacolaData[37:40,] # We predict a model using testing data


# Building different trends and seasonality to conclude on the type of model this Time series Forecast belongs to
# Trend says whether it has a Overall downward or upward pattern
# Linear Trend
linear_model<-lm(Sales~t,data=CocacolaTrain)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =CocacolaTest))
View(linear_pred)
rmse_linear<-sqrt(mean((CocacolaTest$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 

# Exponential trend
expo_model<-lm(log_Sales~t,data=CocacolaTrain)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=CocacolaTest))
rmse_expo<-sqrt(mean((CocacolaTest$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

# Quadratic trend
Quad_model<-lm(Sales~t+t_square,data=CocacolaTrain)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=CocacolaTest))
rmse_Quad<-sqrt(mean((CocacolaTest$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

# Additive Seasonality 
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=CocacolaTrain)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=CocacolaTest,interval='predict'))
rmse_sea_add<-sqrt(mean((CocacolaTest$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

# Additive Seasonality with Linear 
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=CocacolaTrain)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=CocacolaTest))
rmse_Add_sea_Linear<-sqrt(mean((CocacolaTest$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

# Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaTrain)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=CocacolaTest))
rmse_Add_sea_Quad<-sqrt(mean((CocacolaTest$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 

# Multiplicative Seasonality 
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = CocacolaTrain)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,interval='predict',newdata=CocacolaTest))
rmse_multi_sea<-sqrt(mean((CocacolaTest$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

# Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data=CocacolaTrain)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=CocacolaTest,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((CocacolaTest$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 

# Preparing table on model of different trend and seasonality's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Additive Seasonality with Quadratic Trend  has the least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values
View(new_model_fin)
pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)
Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter", col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",col.axis="Green",type="s")
View(Final)


#Plastic Sales Analysis with Time Series Forecast
PlasticSales <- read.csv(file.choose()) # Import Plastic Sales Dataset
View(PlasticSales) 
summary(PlasticSales) 
var(PlasticSales$Sales)
sd(PlasticSales$Sales)
hist(PlasticSales$Sales) 

#Packages needed to perform forecasting
library(forecast)
library(fpp)
library(smooth)

windows() # To view a better plot
plot(PlasticSales$Sales,type="o")

# So creating 12 dummy variables based on Time series Forecast
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummy variables for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
PlasticsData<-cbind(PlasticSales,X) # Combining Plastic Sales with X
View(PlasticsData)
colnames(PlasticsData)
PlasticsData["t"]<- 1:60 # Creating a table to perform linear model for 60 obseravations
View(PlasticsData)
PlasticsData["log_Sales"]<-log(PlasticsData["Sales"]) # Perform Exponential trend
PlasticsData["t_square"]<-PlasticsData["t"]*PlasticsData["t"] # Perform Quadratic model
attach(PlasticsData)

# Splitting the data into train and test dataset
PlasticsTrain<-PlasticsData[1:48,] # We build a model using training data
PlasticsTest<-PlasticsData[49:60,] # We predict a model using testing data

# Building different trends and seasonality to conclude on the type of model this Time series Forecast belongs to
# Trend says whether it has a Overall downward or upward pattern
# Linear Trend
linear_model<-lm(Sales~t,data=PlasticsTrain)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =PlasticsTest))
View(linear_pred)
rmse_linear<-sqrt(mean((PlasticsTest$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 

# Exponential Trend
expo_model<-lm(log_Sales~t,data=PlasticsTrain)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=PlasticsTest))
rmse_expo<-sqrt(mean((PlasticsTest$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

# Quadratic Trend
Quad_model<-lm(Sales~t+t_square,data=PlasticsTrain)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=PlasticsTest))
rmse_Quad<-sqrt(mean((PlasticsTest$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

# Additive Seasonality
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=PlasticsTrain)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=PlasticsTest,interval='predict'))
rmse_sea_add<-sqrt(mean((PlasticsTest$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

# Additive Seasonality with Linear Trend
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=PlasticsTrain)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=PlasticsTest))
rmse_Add_sea_Linear<-sqrt(mean((PlasticsTest$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

# Additive Seasonality with Quadratic Trend
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=PlasticsTrain)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=PlasticsTest))
rmse_Add_sea_Quad<-sqrt(mean((PlasticsTest$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 

# Multiplicative Seasonality
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = PlasticsTrain)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=PlasticsTest,interval='predict'))
rmse_multi_sea<-sqrt(mean((PlasticsTest$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

# Multiplicative Seasonality with Linear trend
multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = PlasticsTrain)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=PlasticsTest,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((PlasticsTest$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model of different trends and seasonality's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Multiplicative Seasonality with Linear trend  has the least RMSE value

new_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = PlasticsData)
new_model_pred<-data.frame(predict(new_model,newdata=PlasticsData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)
Month <- as.data.frame(PlasticsData$Month)
Final <- as.data.frame(cbind(Month,PlasticsData$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months", col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months", col.axis="Green",type="s")
View(Final)