# Importing the books dataset to perform recommendation 
books <- read.csv(file.choose())
View(books)
summary(books)
str(books)

#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE) # To perform recommendation operations
install.packages("Matrix") 
library("recommenderlab")
library("Matrix")
library(caTools)

# Distribution of ratings
hist(books$ratings) # It has got a long tail towards right

# Inorder to build recommendation engine, the datatype should be of realRatingMatrix
books_rating_matrix <- as(books,'realRatingMatrix')

#Popularity based books which are popular in the market, we need to improve the model by using  user collaborative filtering
recomm_model1 <- Recommender(books_rating_matrix,method="popular")

#User Based Collaborative Filtering
model2 <- Recommender(books_rating_matrix,method="UBCF") # UBCF accesses all the data for prediction

#Predictions for two users, First based on popularity
recomm_item1 <- predict(recomm_model1,books_rating_matrix[413:414], n=5, data=NULL, type = "topNList")
as(recomm_item1,"list")

#Predictions for two users, second based on UBFC 
recomm_item2 <- predict(model2,books_rating_matrix[413:414],n=5, data=NULL, type = "topNList")
as(recomm_item2,"list")
