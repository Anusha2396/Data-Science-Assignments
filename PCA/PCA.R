# Performing PCA using Wine dataset

library(fpc)
library(cluster)
library(NbClust)
library(factoextra)
Wine <- read.csv(file.choose())
View(Wine)
summary(Wine)

# To drop the type feature
Wine <- mydata[-1]
# mydata[-1] 
View(Wine)
attach(Wine)
cor(Wine) #Using correlation matrix to get pca scores

# Princomp function is used to perform PCA on numeric data
Wine.pca <-princomp(Wine[-1],cor = TRUE,scores = TRUE,covmat = NULL)
summary(Wine.pca) #Gives the summary of each prinicipal component
str(Wine.pca)
loadings(Wine.pca) # Defines correlation between factors and variables.
plot(Wine.pca) #Plot showing the importance of pca
biplot(Wine.pca)  # Shows the increase of variance with concurrent pca values so that the right pca values can be chosen.

Wine.pca$scores[,1:3] # # Represents top 3 PCA scores

Wine<-cbind(Wine,Wine.pca$scores[,1:3]) # Combines pca scores with Wine dataset.
View(Wine) 

#Hierarchical clustering
library(NbClust)
library(cluster)
clus_data<-Wine[,14:16] # Building a hiereachial model
norm_clus<-scale(clus_data) # Normalizing the values of clus_data using Scale function

# Finding the distance using Eucidean method
dist1<-dist(norm_clus,method = "euclidean")

# Clustering the data using hclust function 
fit1<-hclust(dist1,method="complete") 

# Displaying Dendrogram
plot(fit1,hang =-3)
rect.hclust(fit1, k=7, border="red")

# Cutting the dendrogram for 7 clusters
groups<-cutree(fit1,7) 

#cluster numbering
membership_1<-as.matrix(groups)

View(membership_1)

final1<-cbind(membership_1,Wine) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,15:17)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wines data on membership_1

write.csv(final1,file="wine.csv",row.names = F,col.names = F)
getwd()

#K means clustering
library(plyr)
str(Wine)
View(Wine)

normalized_data<-scale(Wine[,8:10])
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 Clusters
fviz_cluster(fit, geom = "point", ellipse.type = "norm")

final2<- data.frame(fit$cluster,Wine) # Appending cluster membership
View(final2)
aggregate(Wine[,2:14], by=list(fit$cluster), FUN=mean)
table(fit$cluster)
