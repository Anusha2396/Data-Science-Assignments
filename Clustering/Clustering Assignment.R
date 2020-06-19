#Analysis of Crime data using Hierarchial Clustering

Crimedata <- read.csv(file.choose())
View(Crimedata)
summary(Crimedata)
install.packages("cluster")
library(cluster)

#As the given variables does not form a normal distribution when plotting an histogram, we need to normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_data<-scale(as.data.frame(lapply(Crimedata[,2:5],normalize)))
View(normalized_data)
summary(normalized_data)
cor(normalized_data)

d <- dist(normalized_data, method = "euclidean")
fit <- hclust(d, method="complete")

plot(fit)
plot(fit, hang=-3) #hang if provided with negative values will be on the same line

groups <- cutree(fit, k=6) 
table(groups)
rect.hclust(fit, k=6, border="dodgerblue4")

Crimedata_membership<-as.matrix(groups)

Crimeanalysis <- data.frame(Crimedata, Crimedata_membership)
View(Crimeanalysis)
Crimenanalysis1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(Crimeanalysis1)

aggregate(Crimedata[,2:5], by=list(final$Crimedata_membership), mean)

# Analysis of Crime data using Kmeans Clustering

Crimedata <- read.csv(file.choose())
View(Crimedata)
summary(Crimedata)
str(Crimedata)

install.packages("plyr")
library(plyr)
install.packages("animation")
library(animation)

normalized_data<-scale(Crimedata[,2:5])
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Using Scree-Plot")

fit <- kmeans(normalized_data, 4) # 4 cluster solution
plot(fit)

Crimeanalysis2<- data.frame(Crimedata, fit$cluster) # append cluster membership
View(Crimeanalysis2)

aggregate(Crimedata[,2:5], by=list(fit$cluster), FUN=mean)
table(fit$cluster)

#Analysis of Airlines data using Hierarchial Clustering

Airlines <- read.csv(file.choose())
View(Airlines)
summary(Airlines)
install.packages("cluster")
library(cluster)

#As the given variables does not form a normal distribution when plotting an histogram, we need to normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_data<-scale(as.data.frame(lapply(Airlines[,2:12],normalize)))
View(normalized_data)
summary(normalized_data)
cor(normalized_data)

d <- dist(normalized_data, method = "euclidean")
fit <- hclust(d, method="complete")

plot(fit)
plot(fit, hang=-3) #hang if provided with negative values will be on the same line

groups <- cutree(fit, k=6) 
table(groups)
rect.hclust(fit, k=6, border="dodgerblue4")

Airlines_membership<-as.matrix(groups)

AirlinesAnalysis <- data.frame(Airlines, Airlines_membership)
View(AirlinesAnalysis)
AirlinesAnalysis1 <- AirlinesAnalysis[,c(ncol(AirlinesAnalysis),1:(ncol(AirlinesAnalysis)-1))]
View(AirlinesAnalysis1)

aggregate(Airlines[,2:12], by=list(AirlinesAnalysis$Airlines_membership), mean)

# Analysis of Airlines data using Kmeans Clustering

Airlines <- read.csv(file.choose())
View(Airlines)
summary(Airlines)
str(Airlines)

install.packages("plyr")
library(plyr)
install.packages("animation")
library(animation)

normalized_data<-scale(Crimedata[,2:12])
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 4) # 4 cluster solution
plot(fit)

AirlinesAnalysis2 <- data.frame(Airlines, fit$cluster) # append cluster membership
View(AirlinesAnalysis2)

aggregate(Airlines[,2:12], by=list(fit$cluster), FUN=mean)
table(fit$cluster)
