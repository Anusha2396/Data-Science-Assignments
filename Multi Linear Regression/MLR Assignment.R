Startups_50 <- read.csv(file.choose())
View(Startups_50)
summary(Startups_50)
dim(Startups_50) # Tells us about how many rows and columns are available in the dataset

# Variance
var(Startups_50$R.D.Spend)
var(Startups_50$Administration)
var(Startups_50$Marketing.Spend)
var(Startups_50$Profit)

# Standard Deviation
sd(Startups_50$R.D.Spend)
sd(Startups_50$Administration)
sd(Startups_50$Marketing.Spend)
sd(Startups_50$Profit)
class(Startups_50)

# Dummy variable creation
Dummyvariable <- data.frame(numbers = 1:3,
                                  state  = c("New York", "California", "Florida"),
                                  stringsAsFactors = FALSE)
View(Dummyvariable)
levels(State)
install.packages("fastDummies")
library(fastDummies)
knitr::kable(Dummyvariable)
results <- fastDummies::dummy_cols(Dummyvariable)
knitr::kable(results)
results <- fastDummies::dummy_cols(Dummyvariable, select_columns = "numbers")
knitr::kable(results)
StartupsState_50 <- cbind(Startups_50,ifelse(Startups_50$State=="New York",1,0), ifelse(Startups_50$State=="California",2,0),  ifelse(Startups_50$State=="Florida",3,0))
View(StartupsState_50)

# To check whether the data is normally distributed or not by using QQ plot
qqnorm(Profit)
qqline(Profit)
qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Administration)
qqline(Administration)

install.packages("data.table")
library(data.table)

plot(R.D.Spend,Profit) # Plot relationship between each X with Y
plot(Administration,Profit)
plot(Marketing.Spend,Profit)

## To check the correlation for all pairs of variables
pairs(Startups_50)   # Scatter plot for all pairs
cor(Startups_50[,-4]) # correlation matrix
cor(R.D.Spend,Profit) # Correlaion is very high between this pair

library(corpcor)
cor2pcor(cor(Startups_50[,-4]))

# Linear Model of interest
Profitmodel <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State) # lm(Y ~ X)
summary(Profitmodel)

# As p value is <0.05 only for R.D.Spend so now performing linear model of interest for individual pairs
Profitmodel1 <- lm(Profit~R.D.Spend)
summary(Profitmodel1)

Profitmodel2 <- lm(Profit~Administration)
summary(Profitmodel2)

Profitmodel3 <- lm(Profit~Marketing.Spend)
summary(Profitmodel3)

Profitmodel4 <- lm(Profit~State)
summary(Profitmodel4)

Profitmodel5 <- lm(Profit~R.D.Spend+Administration)
summary(Profitmodel5)

Profitmodel6 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(Profitmodel6)

Profitmodel7 <- lm(Profit~R.D.Spend+State)
summary(Profitmodel7)

Profitmodel8 <- lm(Profit~Administration+Marketing.Spend)
summary(Profitmodel8)

Profitmodel9 <- lm(Profit~Administration+State)
summary(Profitmodel9)

Profitmodel10 <- lm(Profit~Marketing.Spend+State)
summary(Profitmodel10)

Profitmodel11 <- lm(Profit~R.D.Spend+Administration+State)
summary(Profitmodel11)

Profitmodel12 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(Profitmodel12)

Profitmodel13 <- lm(Profit~Administration+Marketing.Spend+State)
summary(Profitmodel13)

influence.measures(Profitmodel)
install.packages("car")
library(car)
infIndexPlot(Profitmodel)
influencePlot(Profitmodel)

ProfitModelInfluence <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = Startups_50[-c(50,49,47,46),])
summary(ProfitModelInfluence)

### Variance Inflation Factors
vif(Profitmodel)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(Profitmodel, col="red")

library("MASS")
stepAIC(Profitmodel) # backward
plot(Profitmodel)

# Final model depends on only R.D.spend and two pairs depending on p-value and R-squared value
Final <- lm(Profit ~ R.D.Spend+Administration+Marketing.Spend)
summary(Final)

Final1 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(Final1)

Final2 <- lm(Profit~R.D.Spend)
summary(Final2)

plot(Final2)
plot(R.D.Spend,Profit,main = "Profit with R.D.Spend")

ComputerData <- read.csv(file.choose())
View(ComputerData)
summary(ComputerData)
dim(ComputerData) # Tells us about how many rows and columns are available in the dataset

# Variance
var(ComputerData$X)
var(ComputerData$price)
var(ComputerData$speed)
var(ComputerData$hd)
var(ComputerData$ram)
var(ComputerData$screen)
var(ComputerData$ads)
var(ComputerData$trend)

# Standard Deviation
sd(ComputerData$X)
sd(ComputerData$price)
sd(ComputerData$speed)
sd(ComputerData$hd)
sd(ComputerData$ram)
sd(ComputerData$screen)
sd(ComputerData$ads)
sd(ComputerData$trend)

ComputerData$cd1 <- ifelse(ComputerData$cd=="yes",1,0)
ComputerData$multi1 <- ifelse(ComputerData$multi=='yes',1,0)
ComputerData$premium1 <- ifelse(ComputerData$premium=='yes',1,0)
View(ComputerData)

library(data.table)
pairs(ComputerData)
Computermodel <- lm(price ~ speed+hd+ram+screen+ads+trend+cd1+multi1+premium1, data = ComputerData)
summary(Computermodel)
influenceIndexPlot(Computermodel, id.n=5)
vif(Computermodel)
avPlots(Computermodel)
library(MASS)
stepAIC(Computermodel)
Finalmodel <- lm(price ~ speed+hd+ram+screen+ads+trend+cd1+multi1+premium1, data = ComputerData)
summary(Finalmodel)
# As all the p-values are significant, R-squared is also significant so therefore its a good model

# Predicting Toyato Corolla cars price
ToyatoCorolla <- read.csv(file.choose())
View(ToyatoCorolla)
summary(ToyatoCorolla)
dim(ToyatoCorolla)
levels(ToyatoCorolla$Model)
levels(ToyatoCorolla$Color)
levels(ToyatoCorolla$Fuel_Type)

# Inorder to predict the price, following variables Age,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight plays a important role.
var(ToyatoCorolla$Age_08_04)
var(ToyatoCorolla$KM)
var(ToyatoCorolla$HP)
var(ToyatoCorolla$cc)
var(ToyatoCorolla$Doors)
var(ToyatoCorolla$Gears)
var(ToyatoCorolla$Quarterly_Tax)
var(ToyatoCorolla$Weight)

sd(ToyatoCorolla$Age_08_04)
sd(ToyatoCorolla$KM)
sd(ToyatoCorolla$HP)
sd(ToyatoCorolla$cc)
sd(ToyatoCorolla$Doors)
sd(ToyatoCorolla$Gears)
sd(ToyatoCorolla$Quarterly_Tax)
sd(ToyatoCorolla$Weight)

plot(Toyato)
cor(Toyato)
Toyato <- ToyatoCorolla[c('Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight')]
Toyatomodel <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Toyato)
summary(Toyatomodel)
influenceIndexPlot(Toyatomodel)
influencePlot(Toyatomodel)
vif(Toyatomodel)
avPlots(Toyatomodel)
stepAIC(Toyatomodel)
Final_Model <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Toyato)
summary(Final_Model)
